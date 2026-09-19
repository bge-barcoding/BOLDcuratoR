#!/usr/bin/env python3
"""Click through the running UI in a real browser, and report what happened.

**This is not optional colour.** Two controls in the Phase 3.1 spike -- the
descending toggle and the rows-per-page select -- rendered perfectly, accepted
clicks and did nothing, because their inputs were read inside
``reactive.isolate()`` and so took no reactive dependency on them. Every unit
test passed. The bugs were obvious on the first click and invisible to
everything else, and a third of the same family (a pager that kept reporting
the old page count) turned up on the next run.

Start the app, then drive it::

    python -m boldcurator.cli gui --snapshot fixture.duckdb --port 8765 &
    python tools/drive_ui.py --out /tmp/shots

Needs a browser, which the test suite deliberately does not::

    pip install playwright && playwright install chromium

Exits non-zero if any check fails.
"""

from __future__ import annotations

import argparse
import re
import time
from pathlib import Path

SETTLE = 2.0          # seconds to let Shiny round-trip after an interaction


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--url", default="http://127.0.0.1:8765/")
    parser.add_argument("--out", type=Path, default=Path("ui-screenshots"))
    parser.add_argument("--taxon", default="Lepidoptera")
    parser.add_argument("--browser", default=None,
                        help="path to a chromium binary, if playwright's own "
                             "download is not where it expects it")
    args = parser.parse_args(argv)
    args.out.mkdir(parents=True, exist_ok=True)

    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        print("playwright is not installed: pip install playwright && "
              "playwright install chromium")
        return 2

    failures: list[str] = []

    def check(name: str, ok: bool, detail: str = "") -> None:
        print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
              f"{'  -- ' + detail if detail else ''}")
        if not ok:
            failures.append(name)

    with sync_playwright() as pw:
        launch = {"executable_path": args.browser} if args.browser else {}
        browser = pw.chromium.launch(**launch)
        page = browser.new_page(viewport={"width": 1700, "height": 1050})
        js_errors: list[str] = []
        page.on("pageerror", lambda e: js_errors.append(str(e)))
        page.goto(args.url, wait_until="networkidle")

        def table_text(selector: str) -> str:
            """Only the table.

            The toolbar holds a flag <select> whose options include every flag
            name, so matching against the whole panel's text reports a
            success for an annotation that never rendered.
            """
            table = page.locator(f"{selector} table")
            return table.inner_text() if table.count() else ""

        def show(name: str, settle: float = 3.0) -> None:
            # has-text, not text-is: the priority grades carry a bullet in
            # their label, so an exact match finds nothing.
            page.click(f"a.nav-link:has-text('{name}')")
            time.sleep(settle)

        # -- the pre-check, before anything is fetched
        page.fill("#user", "Driver")
        page.fill("#taxa", args.taxon)
        page.click("#check")
        time.sleep(SETTLE)
        sized = page.locator("#estimate_box").inner_text()
        check("the pre-check sizes the search", "Matching records" in sized,
              " ".join(sized.split())[:90])
        page.screenshot(path=str(args.out / "00-input.png"), full_page=True)

        # a geographic filter must visibly narrow it
        page.fill("#countries", "France")
        page.click("#check")
        time.sleep(SETTLE)
        check("a country filter narrows the pre-check",
              page.locator("#estimate_box").inner_text() != sized)
        page.fill("#countries", "")
        page.check("input[name='continents'][value='Europe']")
        page.click("#check")
        time.sleep(SETTLE)
        check("a continent can be ticked",
              "Matching records" in page.locator("#estimate_box").inner_text())
        page.uncheck("input[name='continents'][value='Europe']")

        # -- search
        page.click("#search")
        time.sleep(SETTLE * 2.5)
        check("search lands on the species checklist",
              "Species" in page.locator("a.nav-link.active").inner_text())
        check("the checklist has rows", bool(table_text("#species_body")))
        page.screenshot(path=str(args.out / "01-species.png"), full_page=True)

        show("BINs")
        check("the BIN dashboard has rows", bool(table_text("#bins_body")))
        page.screenshot(path=str(args.out / "02-bins.png"), full_page=True)

        # -- the grade screens, and the group navigator
        for grade in ("E", "C", "A"):
            show(f"BAGS {grade}")
            groups = page.locator(f"#group_{grade} option").count()
            body = page.locator(f"#grade_{grade}_body").inner_text()
            if "No species graded" in body:
                check(f"grade {grade} reports an empty grade cleanly", True,
                      "no species at this grade")
                continue
            check(f"grade {grade} splits into groups", groups >= 1,
                  f"{groups} groups")
            first = page.locator(f"#grade_{grade}_body strong").first.inner_text()
            if groups > 1:
                page.click(f"#next_{grade}")
                time.sleep(SETTLE)
                moved = page.locator(f"#grade_{grade}_body strong").first.inner_text()
                check(f"grade {grade} Next moves to another problem",
                      moved != first, f"{first} -> {moved}")
                page.click(f"#prev_{grade}")
                time.sleep(SETTLE)
            page.screenshot(path=str(args.out / f"03-bags-{grade}.png"),
                            full_page=True)

        # -- annotate one group, and prove it stays in that group
        show("BAGS C") if page.locator("#group_C").count() else show("BAGS E")
        grade = "C" if page.locator("#group_C").count() else "E"
        first = page.locator(f"#grade_{grade}_body strong").first.inner_text()
        page.click(f"#selall_{grade}")
        time.sleep(SETTLE)
        page.select_option(f"#g{grade}_flag", "synonym")
        page.fill(f"#g{grade}_note", "driven by drive_ui")
        page.click(f"#g{grade}_apply")
        time.sleep(SETTLE * 1.5)
        annotated = table_text(f"#grade_{grade}_body")
        check("the flag lands in the group's table", "synonym" in annotated)
        check("the note lands in the group's table", "driven by drive_ui" in annotated)
        page.screenshot(path=str(args.out / "04-annotated.png"), full_page=True)

        if page.locator(f"#group_{grade} option").count() > 1:
            page.click(f"#next_{grade}")
            time.sleep(SETTLE * 1.5)
            other = table_text(f"#grade_{grade}_body")
            check("the next group is untouched",
                  "synonym" not in other and "driven by drive_ui" not in other)

            # -- a per-row check left in one group must not leak into
            # "Apply to checked" in another. Only "Check this group" (just
            # used above) replaces the whole checked set; the per-row
            # checkbox adds to it, so start clean in both groups first.
            page.click(f"#clear_{grade}")            # currently group 2
            time.sleep(SETTLE)
            page.click(f"#prev_{grade}")
            time.sleep(SETTLE)
            page.click(f"#clear_{grade}")             # and group 1
            time.sleep(SETTLE)
            page.locator(f"#grade_{grade}_body .bc-row-check").first.click()
            time.sleep(SETTLE)
            page.click(f"#next_{grade}")
            time.sleep(SETTLE)
            page.locator(f"#grade_{grade}_body .bc-row-check").first.click()
            time.sleep(SETTLE)
            page.select_option(f"#g{grade}_flag", "data_issue")
            page.click(f"#g{grade}_apply")
            time.sleep(SETTLE * 1.5)
            check("apply to checked reaches the current group's own check",
                  "data_issue" in table_text(f"#grade_{grade}_body"))
            page.click(f"#prev_{grade}")
            time.sleep(SETTLE * 1.5)
            check("a check left in a different group is not swept into that apply",
                  "data_issue" not in table_text(f"#grade_{grade}_body"))

        # -- the paged specimen table
        show("Specimens", settle=SETTLE * 1.5)
        header = page.locator("#specimens_body table thead").inner_text()
        check("the specimen table carries the BAGS grade once analysed",
              "BAGS" in header, header.replace("\n", " "))
        before = table_text("#specimens_body")[:200]
        page.click("#next_")
        time.sleep(SETTLE)
        check("paging moves to different rows",
              table_text("#specimens_body")[:200] != before)
        # Click-a-column-header sorting, not a dropdown: click the "Process
        # ID" header twice (ascending, then descending) and check the order
        # actually changes each time.
        header_cell = page.locator(
            "#specimens_body th.bc-sort-th", has_text="Process ID")
        header_cell.click()
        time.sleep(SETTLE * 1.5)
        ascending = table_text("#specimens_body")[:200]
        check("clicking a column header sorts the specimen table",
              bool(ascending) and ascending != before)
        header_cell.click()
        time.sleep(SETTLE * 1.5)
        descending = table_text("#specimens_body")[:200]
        check("clicking the same header again reverses the order",
              descending != ascending)
        page.screenshot(path=str(args.out / "05-specimens.png"), full_page=True)

        # -- click-a-column-header sorting on a BAGS group table too
        show("BAGS A")
        group_before = table_text("#grade_A_body")[:200]
        group_header = page.locator(
            "#grade_A_body th.bc-sort-th", has_text="Process ID")
        if group_header.count():
            group_header.first.click()
            time.sleep(SETTLE * 1.5)
            check("clicking a column header sorts a BAGS group table",
                  table_text("#grade_A_body")[:200] != group_before)
        else:
            check("clicking a column header sorts a BAGS group table", False,
                  "no sortable header found")

        # -- a checked/unchecked row must not reset the table's scroll
        # position, and the Rep./Check/Flag/Updated ID/Notes headers must
        # stay pinned to the top while scrolled, exactly like every other
        # header. Dispatching the click via JS on a checkbox already inside
        # the scrolled viewport (rather than page.click(), which scrolls an
        # off-screen element into view first) is what actually exercises
        # this -- a real curator only ever clicks what they can already see.
        show("Specimens", settle=SETTLE)
        scroll_div = page.locator("#specimens_body div.bc-scroll")
        scroll_div.evaluate("el => { el.scrollTop = 300; }")
        time.sleep(0.3)
        rep_header_y = page.locator(
            "#specimens_body thead th", has_text="Rep.").bounding_box()["y"]
        container_top_y = scroll_div.bounding_box()["y"]
        check("the Rep. header stays pinned to the top while scrolled",
              abs(rep_header_y - container_top_y) < 5,
              f"header y={rep_header_y}, container top y={container_top_y}")

        scroll_before = scroll_div.evaluate("el => el.scrollTop")
        clicked = scroll_div.evaluate("""
            (el) => {
                const rect = el.getBoundingClientRect();
                const box = Array.from(el.querySelectorAll('.bc-row-check'))
                    .find(b => {
                        const r = b.getBoundingClientRect();
                        return r.top >= rect.top && r.bottom <= rect.bottom;
                    });
                if (!box) return false;
                box.click();
                return true;
            }
        """)
        time.sleep(SETTLE)
        scroll_after = page.locator("#specimens_body div.bc-scroll").evaluate(
            "el => el.scrollTop")
        check("checking a visible row does not reset the table's scroll position",
              clicked and scroll_before > 0 and scroll_after == scroll_before,
              f"{scroll_before} -> {scroll_after}")

        # -- clearing the checked/working selection must not touch the
        # representative pick (auto-selected best per BIN x country) -- see
        # io.annotations's module docstring for why the two are separate.
        def value_box_count(label: str) -> int:
            show("Data Input", settle=1.5)
            body = page.locator("#search_summary").inner_text()
            match = re.search(rf"([\d,]+)\s*\n?{label}", body)
            return int(match.group(1).replace(",", "")) if match else -1

        before_rep = value_box_count("Representative")
        show("Specimens", settle=1.5)
        page.click("#select_all")    # "Check all"
        time.sleep(SETTLE)
        page.click("#clear_selection")   # "Clear checked"
        time.sleep(SETTLE)
        after_rep = value_box_count("Representative")
        check("clearing the checked selection leaves the representative pick alone",
              before_rep > 0 and before_rep == after_rep,
              f"{before_rep} -> {after_rep}")

        check("no javascript errors", not js_errors, "; ".join(js_errors))
        browser.close()

    print(f"\n{len(failures)} failed" if failures else "\nall checks passed")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
