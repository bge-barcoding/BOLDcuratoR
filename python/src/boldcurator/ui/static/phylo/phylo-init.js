/* Minimal, dependency-free Newick tree viewer for the Phylogeny tab.
 *
 * A real JS tree library (phylotree.js) was tried first and dropped: its
 * published browser bundle needs jQuery, underscore *and* lodash loaded as
 * separate globals (its UMD wrapper reads `global._` for underscore and a
 * second, distinctly-named global for lodash -- two libraries that both
 * normally claim `window._`), which is not something a handful of <script>
 * tags with no bundler can resolve safely. Rather than risk a fragile,
 * hard-to-debug vendoring, this file is a small rectangular-cladogram
 * renderer written directly against the one thing it actually needs: a
 * Newick string and a list of per-tip colours/labels. No external JS
 * dependency, no build step -- a few hundred lines of plain SVG and DOM
 * events plugs into the app's existing "load static files as <script>
 * tags" convention (see ui/app.py's global script/style blocks) exactly
 * the way phylotree.js was originally meant to.
 *
 * Supports: pan (drag), zoom (wheel), collapse/expand an internal node
 * (click its connector), a hover tooltip, and colouring/marking tips from
 * per-tip metadata Python already computed (BAGS grade colour, whether the
 * tip's species is a non-monophyletic grade-C species).
 */
(function (global) {
  "use strict";

  // -- Newick parsing ------------------------------------------------------
  //
  // Handles the one thing Biopython's writer actually needs: single-quoted
  // labels (with '' as an escaped quote) for any name containing a space,
  // parenthesis or other Newick-special character -- BOLDcuratoR's tip
  // labels ("processid-Genus species-Country") routinely do.

  function parseNewick(text) {
    let i = 0;
    let nextId = 0;

    function skipWs() {
      while (i < text.length && /\s/.test(text[i])) i++;
    }

    function parseLabel() {
      skipWs();
      if (text[i] === "'") {
        i++;
        let out = "";
        while (i < text.length) {
          if (text[i] === "'") {
            if (text[i + 1] === "'") {
              out += "'";
              i += 2;
              continue;
            }
            i++;
            break;
          }
          out += text[i++];
        }
        return out;
      }
      const start = i;
      while (i < text.length && ",():;".indexOf(text[i]) === -1 && !/\s/.test(text[i])) {
        i++;
      }
      return text.slice(start, i);
    }

    function parseBranchLength() {
      skipWs();
      if (text[i] === ":") {
        i++;
        skipWs();
        const start = i;
        while (i < text.length && /[0-9eE+\-.]/.test(text[i])) i++;
        const value = parseFloat(text.slice(start, i));
        return Number.isNaN(value) ? 0 : value;
      }
      return 0;
    }

    function parseNode() {
      skipWs();
      const node = { id: nextId++, children: [], length: 0, name: "" };
      if (text[i] === "(") {
        i++;
        node.children.push(parseNode());
        skipWs();
        while (text[i] === ",") {
          i++;
          node.children.push(parseNode());
          skipWs();
        }
        if (text[i] === ")") i++;
        node.name = parseLabel();
      } else {
        node.name = parseLabel();
      }
      node.length = parseBranchLength();
      return node;
    }

    const root = parseNode();
    skipWs();
    if (text[i] === ";") i++;
    return root;
  }

  // -- Layout ---------------------------------------------------------------
  //
  // A standard rectangular cladogram: x is cumulative branch length from the
  // root, y is leaf order (internal nodes sit at the mean of their visible
  // children). Negative branch lengths -- NJ can produce them -- are
  // clamped to 0 so a node never lands to the left of its own parent.

  function layout(root, collapsed) {
    const leaves = [];
    let maxDepth = 0;

    function assignX(node, parentX) {
      const length = Math.max(0, node.length || 0);
      node.x = parentX + length;
      maxDepth = Math.max(maxDepth, node.x);
      if (collapsed.has(node.id) || node.children.length === 0) {
        node._visibleLeaf = true;
        return;
      }
      // Must be reset explicitly: `root` is reused across every re-render
      // (collapsing/expanding mutates the same node objects in place, not a
      // fresh tree), so a node that was collapsed in a previous render and
      // has since been expanded again would otherwise keep last render's
      // stale `_visibleLeaf = true`.
      node._visibleLeaf = false;
      node.children.forEach((child) => assignX(child, node.x));
    }
    assignX(root, 0);

    function collectLeaves(node) {
      if (node._visibleLeaf) {
        leaves.push(node);
        return;
      }
      node.children.forEach(collectLeaves);
    }
    collectLeaves(root);
    leaves.forEach((leaf, index) => {
      leaf.y = index;
    });

    function assignY(node) {
      if (node._visibleLeaf) return node.y;
      const ys = node.children.map(assignY);
      node.y = (Math.min(...ys) + Math.max(...ys)) / 2;
      return node.y;
    }
    assignY(root);

    return { leaves, maxDepth };
  }

  function collectDisplayLabel(node, byName) {
    if (node._visibleLeaf && node.children.length > 0) {
      // A collapsed internal node stands in for every tip beneath it.
      const tipNames = [];
      (function walk(n) {
        if (n.children.length === 0) tipNames.push(n.name);
        else n.children.forEach(walk);
      })(node);
      return `${tipNames.length} tips (collapsed)`;
    }
    return node.name;
  }

  // -- Rendering --------------------------------------------------------

  const NS = "http://www.w3.org/2000/svg";

  function svgEl(tag, attrs) {
    const el = document.createElementNS(NS, tag);
    for (const key in attrs) el.setAttribute(key, attrs[key]);
    return el;
  }

  function render(container, root, tipsByName, collapsed, state) {
    container.innerHTML = "";
    const { leaves, maxDepth } = layout(root, collapsed);

    const rowHeight = 18;
    const leftMargin = 16;
    const labelWidth = 260;
    const xScale = maxDepth > 0 ? 480 / maxDepth : 1;
    const width = leftMargin + maxDepth * xScale + labelWidth + 24;
    const height = Math.max(60, leaves.length * rowHeight + 24);

    const svg = svgEl("svg", {
      width: "100%",
      height: Math.min(height, 2000),
      viewBox: `0 0 ${width} ${height}`,
      style: "background:#fff;cursor:grab;user-select:none;",
    });
    const viewport = svgEl("g", { id: "bc-phylo-viewport" });
    svg.appendChild(viewport);

    function px(x) {
      return leftMargin + x * xScale;
    }
    function py(y) {
      return 12 + y * rowHeight;
    }

    function drawNode(node) {
      if (node.parent) {
        viewport.appendChild(
          svgEl("line", {
            x1: px(node.parent.x), y1: py(node.y),
            x2: px(node.x), y2: py(node.y),
            stroke: "#888", "stroke-width": 1.3,
          })
        );
      }
      if (node._visibleLeaf) {
        const meta = tipsByName[node.name];
        const collapsedHere = node.children.length > 0 && collapsed.has(node.id);
        const color = meta ? meta.color : (collapsedHere ? "#6c757d" : "#495057");
        const circle = svgEl("circle", {
          cx: px(node.x), cy: py(node.y), r: 4,
          fill: color, stroke: meta && meta.monophyletic === false ? "#dc3545" : "none",
          "stroke-width": 2,
        });
        if (node.children.length > 0) {
          circle.style.cursor = "pointer";
          circle.addEventListener("click", () => {
            collapsed.delete(node.id);
            render(container, root, tipsByName, collapsed, state);
          });
        }
        viewport.appendChild(circle);

        const label = collectDisplayLabel(node, tipsByName);
        const text = svgEl("text", {
          x: px(node.x) + 8, y: py(node.y) + 4,
          "font-size": 11, fill: "#212529",
        });
        text.textContent = label;
        viewport.appendChild(text);

        if (meta) {
          circle.addEventListener("mouseenter", (evt) => showTooltip(state, evt, meta));
          circle.addEventListener("mouseleave", () => hideTooltip(state));
        }
        return;
      }

      // Vertical connector across this node's (visible) children.
      const childYs = node.children.map((c) => c.y);
      const line = svgEl("line", {
        x1: px(node.x), y1: py(Math.min(...childYs)),
        x2: px(node.x), y2: py(Math.max(...childYs)),
        stroke: "#888", "stroke-width": 1.3,
      });
      line.style.cursor = "pointer";
      line.addEventListener("click", () => {
        collapsed.add(node.id);
        render(container, root, tipsByName, collapsed, state);
      });
      viewport.appendChild(line);

      node.children.forEach((child) => {
        child.parent = node;
        drawNode(child);
      });
    }
    drawNode(root);

    container.appendChild(svg);
    wireZoomAndPan(svg, viewport, state);
  }

  // -- Zoom / pan ---------------------------------------------------------

  function wireZoomAndPan(svg, viewport, state) {
    let dragging = false;
    let lastX = 0;
    let lastY = 0;

    function apply() {
      viewport.setAttribute(
        "transform",
        `translate(${state.panX},${state.panY}) scale(${state.zoom})`
      );
    }
    apply();

    svg.addEventListener("wheel", (evt) => {
      evt.preventDefault();
      const factor = evt.deltaY < 0 ? 1.15 : 1 / 1.15;
      state.zoom = Math.min(8, Math.max(0.2, state.zoom * factor));
      apply();
    }, { passive: false });

    svg.addEventListener("mousedown", (evt) => {
      dragging = true;
      lastX = evt.clientX;
      lastY = evt.clientY;
      svg.style.cursor = "grabbing";
    });
    global.addEventListener("mousemove", (evt) => {
      if (!dragging) return;
      state.panX += evt.clientX - lastX;
      state.panY += evt.clientY - lastY;
      lastX = evt.clientX;
      lastY = evt.clientY;
      apply();
    });
    global.addEventListener("mouseup", () => {
      dragging = false;
      svg.style.cursor = "grab";
    });
  }

  // -- Tooltip --------------------------------------------------------------

  function showTooltip(state, evt, meta) {
    if (!state.tooltip) return;
    const lines = [
      meta.species || "Unknown species",
      meta.bin_uri ? `BIN: ${meta.bin_uri}` : null,
      meta.bags_grade ? `BAGS grade: ${meta.bags_grade}` : null,
      meta.monophyletic === false ? "Not monophyletic on this tree" : null,
    ].filter(Boolean);
    state.tooltip.innerHTML = lines.map((l) => `<div>${l}</div>`).join("");
    state.tooltip.style.display = "block";
    state.tooltip.style.left = `${evt.clientX + 12}px`;
    state.tooltip.style.top = `${evt.clientY + 12}px`;
  }

  function hideTooltip(state) {
    if (state.tooltip) state.tooltip.style.display = "none";
  }

  // -- Public entry point ---------------------------------------------------

  global.bcRenderPhylotree = function (containerId, newick, tips) {
    const container = document.getElementById(containerId);
    if (!container || !newick) return;

    const tipsByName = {};
    (tips || []).forEach((t) => {
      tipsByName[t.tip] = t;
    });

    let tooltip = document.getElementById("bc-phylo-tooltip");
    if (!tooltip) {
      tooltip = document.createElement("div");
      tooltip.id = "bc-phylo-tooltip";
      tooltip.className = "bc-phylo-tooltip";
      document.body.appendChild(tooltip);
    }

    const root = parseNewick(newick);
    const state = { zoom: 1, panX: 0, panY: 0, tooltip };
    render(container, root, tipsByName, new Set(), state);
  };
})(window);
