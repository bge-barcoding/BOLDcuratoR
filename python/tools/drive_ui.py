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
    python tools/drive_ui.py --out /tmp/shots --url <the URL gui printed>

The URL carries the server's one-time launch token (``localguard.py``), so
it is the one ``gui`` prints ("Open BOLDcurator at ..."), and each run of
this script needs a fresh server.

Needs a browser, which the test suite deliberately does not::

    pip install playwright && playwright install chromium

Exits non-zero if any check fails.
"""

from __future__ import annotations

import argparse
import re
import time
from pathlib import Path
from urllib.parse import urlsplit

SETTLE = 2.0          # seconds to let Shiny round-trip after an interaction


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--url", required=True,
                        help="the URL `boldcurator gui` printed, token and all")
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
        # The launch token works once (localguard.py): later requests go to
        # the bare server and carry the cookie it set.
        base = "{0.scheme}://{0.netloc}".format(urlsplit(args.url))

        def table_text(selector: str) -> str:
            """Only the table(s), not the surrounding panel.

            The annotation form holds a <select> whose options include every
            flag name, so matching against the whole panel's text reports a
            success for an annotation that never rendered. The Species screen
            can hold two tables (gap analysis, then the checklist) when taxa
            were typed, so this joins every table's text rather than assuming
            there is exactly one.
            """
            tables = page.locator(f"{selector} table")
            return "\n".join(tables.nth(i).inner_text() for i in range(tables.count()))

        def tbody_text(selector: str) -> str:
            """Only the row data, not the header -- for a table wide enough
            (round 3's "show every column") that the header alone can exceed
            a short slice, making two different pages look identical."""
            bodies = page.locator(f"{selector} table tbody")
            return "\n".join(bodies.nth(i).inner_text() for i in range(bodies.count()))

        def column_values(selector: str, label: str) -> list[str]:
            """One column's cells, by its header text, in a table."""
            return page.evaluate("""([sel, label]) => {
                const table = document.querySelector(sel + ' table');
                if (!table) return [];
                const heads = Array.from(table.querySelectorAll('thead th'));
                const i = heads.findIndex(th => th.innerText.trim() === label);
                if (i < 0) return [];
                return Array.from(table.querySelectorAll('tbody tr'))
                    .map(tr => tr.children[i].innerText.trim());
            }""", [selector, label])

        def group_count(grade: str) -> int:
            """How many groups a screen has, from its "i of N" (the group
            list is a selectize, which keeps only the chosen option)."""
            pos = page.locator(f"#grade_{grade}_body .bc-group-pos")
            if not pos.count():
                return 0
            match = re.search(r"of ([\d,]+)", pos.first.inner_text())
            return int(match.group(1).replace(",", "")) if match else 0

        def show(name: str, settle: float = 3.0) -> None:
            # has-text, not text-is: the priority grades carry a bullet in
            # their label, so an exact match finds nothing.
            page.click(f"a.nav-link:has-text('{name}')")
            time.sleep(settle)

        # Round 9, item 7: the app opens on the Guide, which links the paper.
        check("the app opens on the Guide, linking the BAGS paper",
              "Guide" in page.locator("a.nav-link.active").inner_text()
              and page.locator("a[href='https://doi.org/10.1111/1755-0998.13262']")
              .first.is_visible())
        page.screenshot(path=str(args.out / "00-guide.png"), full_page=True)

        # -- the pre-check, before anything is fetched
        # Round 6, data input items 2/3: "Data" (snapshot/session) and, since
        # round 9, "Guide" come before "Search" -- the taxa field isn't on the
        # tab that's selected by default.
        show("Search", settle=1.0)
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

        show("Gap analysis")
        gap_text = table_text("#gap_body")
        check("gap analysis reports on the taxon actually typed",
              args.taxon in gap_text and ("Found" in gap_text or "Missing" in gap_text))
        page.screenshot(path=str(args.out / "01b-gap-analysis.png"), full_page=True)

        show("BINs")
        check("the BIN dashboard has rows", bool(table_text("#bins_body")))
        page.screenshot(path=str(args.out / "02-bins.png"), full_page=True)

        # -- the grade screens, and the group navigator
        for grade in ("E", "C", "A"):
            show(f"BAGS {grade}")
            groups = group_count(grade)
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

        def annotate(prefix: str, *, flag: str | None = None, **fields) -> str:
            """Open the annotation form, fill it in, press Apply. Returns
            the form's error text ("" once it closed)."""
            page.click(f"#{prefix}_apply")
            page.wait_for_selector("#ann_confirm", state="visible", timeout=10_000)
            time.sleep(SETTLE / 2)
            if flag is not None:
                page.select_option("#ann_id_flag", flag)
                time.sleep(SETTLE / 4)
            for name, value in fields.items():
                target = f"#ann_{name}"
                if isinstance(value, bool):
                    page.set_checked(target, value)
                elif page.locator(f"select{target}").count():
                    page.select_option(target, value)
                else:
                    page.fill(target, value)
                time.sleep(SETTLE / 4)
            page.click("#ann_confirm")
            time.sleep(SETTLE * 1.5)
            if page.locator("#ann_confirm").is_visible():
                return page.locator("#ann_errors").inner_text() or "(still open)"
            return ""

        # -- annotate one group, and prove it stays in that group
        show("BAGS C") if page.locator("#group_C").count() else show("BAGS E")
        grade = "C" if page.locator("#group_C").count() else "E"
        first = page.locator(f"#grade_{grade}_body strong").first.inner_text()
        page.click(f"#selall_{grade}")
        time.sleep(SETTLE)
        # A misidentification needs a method: refused, with why, until given.
        error = annotate(f"g{grade}", flag="misidentification",
                         new_id="Danaus drivenus", note="driven by drive_ui",
                         curator="Drive Tester")
        check("an incomplete form is refused and says what is missing",
              "Identification method is required" in error, error)
        page.select_option("#ann_method", "Morphology")
        page.set_checked("#ann_contamination", True)
        page.set_checked("#ann_other_issue", True)
        time.sleep(SETTLE / 2)
        box = page.locator("#shiny-modal .modal-dialog").bounding_box()
        check("the form fits the window without scrolling, every part shown",
              box is not None and box["y"] + box["height"]
              <= page.viewport_size["height"],
              f"{box and round(box['y'] + box['height'])} of "
              f"{page.viewport_size['height']}px")
        page.set_checked("#ann_contamination", False)
        page.set_checked("#ann_other_issue", False)
        time.sleep(SETTLE / 2)
        check("the rank is suggested from the name",
              page.input_value("#ann_rank") == "species",
              page.input_value("#ann_rank"))
        page.click("#ann_confirm")
        time.sleep(SETTLE * 1.5)
        annotated = table_text(f"#grade_{grade}_body")
        check("the flag lands in the group's table",
              "Misident." in annotated and "Danaus drivenus" in annotated)
        check("the note lands in the group's table", "driven by drive_ui" in annotated)
        page.screenshot(path=str(args.out / "04-annotated.png"), full_page=True)

        # The form reopens on what the records have; contamination adds to
        # the misidentification rather than replacing it.
        page.click(f"#g{grade}_apply")
        page.wait_for_selector("#ann_confirm", state="visible", timeout=10_000)
        time.sleep(SETTLE / 2)
        check("the form reopens on the records' own annotation, and the curator",
              page.input_value("#ann_id_flag") == "misidentification"
              and page.input_value("#ann_new_id") == "Danaus drivenus"
              and page.input_value("#ann_curator") == "Drive Tester",
              page.input_value("#ann_curator"))
        check("the header remembers the curator's name",
              page.input_value("#user") == "Drive Tester", page.input_value("#user"))
        page.click("#ann_cancel")
        time.sleep(SETTLE)
        error = annotate(f"g{grade}", contamination=True,
                         sequence_taxon="Homo sapiens")
        annotated = table_text(f"#grade_{grade}_body")
        check("contamination combines with the misidentification",
              not error and "Contam." in annotated
              and "Misident." in annotated and "Homo sapiens" in annotated,
              error)
        page.click(f"#g{grade}_clear_ann")
        time.sleep(SETTLE)
        page.click("#confirm_clear_ann")
        time.sleep(SETTLE * 1.5)
        annotated = table_text(f"#grade_{grade}_body")
        check('"Clear annotations" removes them, once confirmed',
              "driven by drive_ui" not in annotated
              and "Misident." not in annotated)

        # A synonym on one checked record is saved for its name, so it
        # reaches every record with that name -- here and on other tabs.
        page.click(f"#clear_{grade}")
        time.sleep(SETTLE)
        page.locator(f"#grade_{grade}_body .bc-row-check").first.click()
        time.sleep(SETTLE)
        idents = column_values(f"#grade_{grade}_body", "ID")
        error = annotate(f"g{grade}", flag="synonym", new_id="Danaus seniorus",
                         rank="species", citation="Driver 2026")
        updated = column_values(f"#grade_{grade}_body", "Updated ID")
        same = [u for i, u in zip(idents, updated) if i == idents[0]]
        check("a synonym reaches every record with the name, marked as a taxon update",
              not error and len(same) >= 1
              and all("Danaus seniorus" in u and "taxon" in u
                      for u in same), f"{error} {same[:3]}")
        page.screenshot(path=str(args.out / "04b-synonym.png"), full_page=True)

        if group_count(grade) > 1:
            page.click(f"#next_{grade}")
            time.sleep(SETTLE * 1.5)
            other = table_text(f"#grade_{grade}_body")
            check("the next group is untouched",
                  "Misident." not in other
                  and "driven by drive_ui" not in other)

            # -- a per-row check left in one group must not leak into
            # "Annotate checked" in another. Only "Check this group" (just
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
            error = annotate(f"g{grade}", other_issue=True,
                             note="lat/long swapped")
            check("annotating reaches the current group's own check",
                  not error and "Other" in column_values(f"#grade_{grade}_body",
                                                         "Other issue"),
                  error)
            page.click(f"#prev_{grade}")
            time.sleep(SETTLE * 1.5)
            check("a check left in a different group is not swept into that apply",
                  "lat/long swapped" not in table_text(f"#grade_{grade}_body"))

        # Round 9, item 2: Find records in a group -- the table shows only
        # the matches and "Check the N found" checks exactly those.
        page.select_option(f"#find_col_{grade}", "identified_by")
        time.sleep(SETTLE / 2)
        page.fill(f"#find_text_{grade}", "smith")
        page.click(f"#find_go_{grade}")
        time.sleep(SETTLE * 1.5)
        who = column_values(f"#grade_{grade}_body", "Identified by")
        check("Find records narrows a group to the matches",
              bool(who) and all("smith" in w.lower() for w in who),
              f"{len(who)} rows")
        banner = page.locator(f"#grade_{grade}_body .bc-filter-banner")
        check("a filtered table says so above it, and is outlined",
              banner.count() == 1 and "Filtered" in banner.inner_text()
              and f"showing {len(who)} of" in banner.inner_text()
              and page.locator(f"#grade_{grade}_body .bc-scroll.bc-filtered").count()
              == 1, banner.inner_text() if banner.count() else "no banner")
        page.click(f"#selall_{grade}")
        time.sleep(SETTLE)
        here = page.locator(f"#grade_{grade}_body").inner_text()
        check('"Check the N found" checks just the matches',
              f"{len(who)} checked here" in here,
              re.search(r"\d+ checked here", here).group(0) if
              re.search(r"\d+ checked here", here) else "")
        page.click(f"#find_clear_{grade}")
        time.sleep(SETTLE)
        check("Clear filter takes the banner away",
              page.locator(f"#grade_{grade}_body .bc-filter-banner").count() == 0)
        page.click(f"#clear_{grade}")
        time.sleep(SETTLE)

        # Round 12, item 4: groups marked reviewed, and annotated ones, show
        # in the group list; Next unreviewed skips the reviewed ones.
        # Round 14, item 1: Mark reviewed shares the group list's line.
        nav = page.evaluate(f"""() => {{
            const list = document.querySelector('#grade_{grade}_body .selectize-input')
                .getBoundingClientRect();
            const button = document.querySelector('#review_{grade}')
                .getBoundingClientRect();
            const mid = button.top + button.height / 2;
            return [mid >= list.top && mid <= list.bottom,
                    Math.round(list.top), Math.round(button.top)];
        }}""")
        check("Mark reviewed is on the same line as the group list",
              bool(nav[0]), str(nav))
        body = page.locator(f"#grade_{grade}_body").inner_text()
        page.click(f"#review_{grade}")
        time.sleep(SETTLE)
        body = page.locator(f"#grade_{grade}_body").inner_text()
        options = page.evaluate(
            f"Object.values(document.querySelector('#group_{grade}').selectize"
            ".options).map(o => o.label)")
        check("Mark reviewed marks the group and counts it",
              "1 reviewed" in body and "Reviewed ✓ (undo)" in body
              and any(o.startswith("✓") for o in options), str(options[:2]))
        check("groups holding annotated records carry ✎",
              any("✎" in o for o in options), str(options[:3]))
        if group_count(grade) > 1:
            before = page.locator(f"#grade_{grade}_body .bc-group-pos").inner_text()
            page.click(f"#next_unrev_{grade}")
            time.sleep(SETTLE)
            after = page.locator(f"#grade_{grade}_body .bc-group-pos").inner_text()
            check("Next unreviewed moves to an unreviewed group",
                  before != after and "Mark reviewed ✓" in
                  page.locator(f"#grade_{grade}_body").inner_text(),
                  f"{before} -> {after}")
            page.click(f"#prev_{grade}")
            time.sleep(SETTLE)

        # Round 12, item 3: grade with the curated IDs -- the synonym made
        # above renames a species, so the checklist gains the curated grade
        # beside the original and a row for the new name.
        show("Species", settle=SETTLE)
        page.check("#grades_from_species input[value='curated']")
        time.sleep(SETTLE * 2)
        species_text = page.locator("#species_body").inner_text()
        check("grading with curated IDs adds the curated grade and the new name",
              "BAGS (curated IDs)" in species_text
              and "Danaus seniorus" in species_text
              and "re-identified" in species_text)
        page.screenshot(path=str(args.out / "02b-species-curated.png"),
                        full_page=True)
        page.check("#grades_from_species input[value='original']")
        time.sleep(SETTLE * 1.5)
        check("and back to the original grades",
              "BAGS (curated IDs)" not in page.locator("#species_body").inner_text())

        # -- the paged specimen table
        show("Specimens", settle=SETTLE * 1.5)
        # The synonym made on a BAGS screen is listed here, downloads as its
        # own report, and can be removed (after a confirmation).
        check("the Specimens tab lists the taxon update",
              "Taxon updates (1)" in page.locator("#specimens_body").inner_text())
        href = page.get_attribute("#dl_taxon_updates", "href") or ""
        report = page.request.get(f"{base}/{href.lstrip('/')}")
        check("the taxon updates report downloads with the synonym",
              report.ok and "Danaus seniorus" in report.text()
              and "Driver 2026" in report.text())
        page.locator("#specimens_body summary").first.click()
        time.sleep(SETTLE / 2)
        page.click("#taxon_update_remove")
        time.sleep(SETTLE)
        page.click("#confirm_remove_taxon")
        time.sleep(SETTLE * 1.5)
        check("a taxon update can be removed",
              "Taxon updates (" not in page.locator("#specimens_body").inner_text()
              and "Danaus seniorus" not in table_text("#specimens_body"))
        pages_before = page.locator("#specimens_body").inner_text()
        page.select_option("#find_col_sp", "country.ocean")
        time.sleep(SETTLE / 2)
        page.fill("#find_text_sp", "Canada")
        page.press("#find_text_sp", "Enter")          # Enter searches too
        time.sleep(SETTLE * 1.5)
        countries = column_values("#specimens_body", "Country/Ocean")
        body = page.locator("#specimens_body").inner_text()
        found = re.search(r"Check all ([\d,]+) found", body)
        check("Find records narrows the specimen table, Enter to search",
              bool(countries) and set(countries) == {"Canada"} and bool(found),
              f"{len(countries)} rows on the page, {found.group(0) if found else 'no count'}")
        page.click("#select_all")
        time.sleep(SETTLE)
        checked = re.search(r"([\d,]+) checked",
                            page.locator("#specimens_body").inner_text())
        check('"Check all N found" checks just the matches',
              bool(found and checked) and checked.group(1) == found.group(1),
              checked.group(0) if checked else "")
        page.click("#clear_selection")
        time.sleep(SETTLE)
        page.click("#find_clear_sp")
        time.sleep(SETTLE * 1.5)
        pages_after = page.locator("#specimens_body").inner_text()

        def page_count(text: str) -> str:
            m = re.search(r"\b1 / ([\d,]+)", text)
            return m.group(1) if m else "?"
        check("Clear search shows every record again",
              not re.search(r"Check all [\d,]+ found", pages_after)
              and page_count(pages_after) == page_count(pages_before) != "?",
              f"pages {page_count(pages_before)} -> {page_count(pages_after)}")
        header = page.locator("#specimens_body table thead").inner_text()
        check("the specimen table carries the BAGS grade once analysed",
              "BAGS" in header, header.replace("\n", " "))
        before = tbody_text("#specimens_body")[:200]
        page.click("#next_")
        time.sleep(SETTLE)
        check("paging moves to different rows",
              tbody_text("#specimens_body")[:200] != before)
        # The annotation columns are sortable on the specimen table too,
        # not only a stored/physical one -- SpecimenTable.sort_by fetches
        # nothing from the database for these, it reads Annotations directly.
        for label in ("Rep.", "Check", "ID issue", "Updated ID", "Notes",
                      "Contam.", "Curator"):
            check(f'the "{label}" column header is sortable',
                  page.locator("#specimens_body th.bc-sort-th",
                              has_text=label).count() >= 1)

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

        # Sorting by "Rep." (the representative pick) must surface the
        # auto-selected rows, not silently do nothing -- this is the one
        # annotation column with a real answer to check for correctness
        # (True sorts after False; the representative picks are True).
        rep_header = page.locator("#specimens_body th.bc-sort-th", has_text="Rep.")
        rep_header.click()
        time.sleep(SETTLE)
        rep_header.click()   # descending: True (checked) first
        time.sleep(SETTLE * 1.5)
        first_row_rep = page.locator(
            "#specimens_body tbody tr").first.locator("td").first.locator(
            "input").is_checked()
        check("sorting by Rep. surfaces the representative picks",
              first_row_rep)
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
            show("Search", settle=1.5)
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

        # -- the Phylogeny tab: built from the already-selected representative
        # specimens (core.phylogeny), not every specimen -- see that module's
        # own docstring. This is exactly the class of bug this whole script
        # exists to catch: the tree is only ever drawn by a per-render
        # <script> tag Shiny's own HTML swap has to actually execute, which
        # no unit test can see.
        show("Phylogeny", settle=1.5)
        before_build = page.locator("#phylogeny_body").inner_text()
        check("the phylogeny tab reports a representative-specimen count",
              "representative specimens" in before_build, before_build[:120])
        page.click("#build_tree")
        tree_text = ""
        for _ in range(20):
            time.sleep(1)
            tree_text = page.locator("#phylogeny_body").inner_text()
            if "tips." in tree_text or "Could not build" in tree_text:
                break
        check("the tree finishes building", "tips." in tree_text, tree_text[:200])
        svg_count = page.locator("#phylo-tree-container svg").count()
        check("the tree renders as SVG", svg_count > 0)
        tip_count = page.locator("#phylo-tree-container circle").count()
        check("the tree has at least one tip", tip_count > 0, f"{tip_count} tips")
        # The fixture's own default taxon covers grade C (see
        # test_the_fixture_exercises_every_grade_the_screens_show) -- when the
        # searched taxon does too, the monophyly badge must appear.
        if page.locator("#group_C").count():
            check("a grade-C monophyly badge is shown",
                  "monophyletic" in tree_text.lower(), tree_text[:300])
        check("the tree hit-areas are wider than the visible shapes",
              page.evaluate("""
                  () => {
                      const hitLine = document.querySelector(
                          "#phylo-tree-container line[stroke='transparent']");
                      const visLine = document.querySelector(
                          "#phylo-tree-container line[stroke='#888']");
                      const hitCircleOk = Array.from(
                          document.querySelectorAll('#phylo-tree-container circle'))
                          .some(c => c.getAttribute('r') === '10');
                      return hitLine && visLine
                          && parseFloat(hitLine.getAttribute('stroke-width'))
                             > parseFloat(visLine.getAttribute('stroke-width')) * 5
                          && hitCircleOk;
                  }
              """))
        check("the Newick download button is present",
              page.locator("#dl_phylo_newick").count() > 0)
        tip_link = page.evaluate("""
            () => {
                const a = document.querySelector(
                    '#phylo-tree-container a.bc-phylo-tip-link');
                return a ? [a.getAttribute('href'), a.textContent] : null;
            }
        """)
        check("tip names start with the BIN and link to it on BOLD",
              bool(tip_link) and "[bin]" in tip_link[0]
              and tip_link[1].startswith("BOLD:"), str(tip_link))
        # Round 9, item 1: a link, but not underlined -- hovered too.
        page.locator("#phylo-tree-container a.bc-phylo-tip-link").first.hover()
        underline = page.evaluate("""
            () => {
                const a = document.querySelector(
                    '#phylo-tree-container a.bc-phylo-tip-link');
                const t = a && a.querySelector('text');
                return t ? [getComputedStyle(a).textDecorationLine,
                            getComputedStyle(t).textDecorationLine] : null;
            }
        """)
        check("tip names are not underlined, even hovered",
              bool(underline) and all(u == "none" for u in underline),
              str(underline))

        # -- reroot: right-click (dispatched directly -- see phylo-init.js's
        # own contextmenu listener; a real synthetic right-click through
        # Playwright is flaky at this pixel scale in practice, dispatching
        # the event directly on the element still exercises the same
        # listener and server round-trip) a tip and confirm the tree
        # actually changes, not just re-renders identically.
        before_reroot = page.locator("#phylogeny_body").inner_text()
        page.evaluate("""
            () => {
                // Each tip draws two circles: a small visible one (no
                // listeners) then a larger invisible "hit" one on top that
                // actually carries the click/contextmenu listeners -- see
                // phylo-init.js's own comment on why. Must target that one,
                // not just the first <circle> in document order.
                const c = document.querySelector(
                    "#phylo-tree-container circle[fill='transparent']");
                if (c) c.dispatchEvent(new MouseEvent('contextmenu', {bubbles: true}));
            }
        """)
        time.sleep(SETTLE)
        after_reroot = page.locator("#phylogeny_body").inner_text()
        check("right-clicking a tip reroots the tree",
              after_reroot != before_reroot)
        page.screenshot(path=str(args.out / "06-phylogeny.png"), full_page=True)

        # -- round 13: the tree, the BINs tab and the downloads after
        # curation. Re-identify one tip's record on the Specimens tab...
        tip_pid = (tip_link[1].split("-")[1] if tip_link else "")
        show("Specimens", settle=SETTLE)
        tops = page.evaluate(
            "Array.from(document.querySelectorAll('.bc-download-row .btn'))"
            ".map(b => Math.round(b.getBoundingClientRect().top))")
        check("the download buttons sit on one row",
              len(tops) == 8 and len(set(tops)) == 1, str(tops))
        page.fill("#find_text_sp", tip_pid)
        page.click("#find_go_sp")
        time.sleep(SETTLE * 1.5)
        page.click("#select_all")
        time.sleep(SETTLE)
        error = annotate("sp", flag="misidentification", new_id="Danaus drivenus",
                         method="Morphology", note="driven by drive_ui",
                         curator="Drive Tester")
        check("a tree tip's record can be re-identified", not error, error)
        page.click("#find_clear_sp")
        time.sleep(SETTLE)
        page.click("#clear_selection")
        time.sleep(SETTLE)
        # ...then grade with curated IDs: its tip reads "old → new".
        show("Phylogeny", settle=SETTLE)
        page.check("#grades_from_phylogeny input[value='curated']")
        time.sleep(SETTLE * 2)
        labels = page.evaluate(
            "Array.from(document.querySelectorAll('#phylo-tree-container text'))"
            ".map(t => [t.textContent, t.getAttribute('fill')])")
        renamed = [l for l in labels if "→ Danaus drivenus" in l[0]]
        # Purple -- unless the tip also carries an alignment warning, whose
        # amber (and ⚠) wins.
        check("with curated IDs the re-identified tip reads old → new, in purple",
              bool(renamed) and (renamed[0][1] == "#6f42c1"
                                 or renamed[0][0].endswith("\u26a0")),
              str(renamed[:1]))
        href = page.get_attribute("#dl_phylo_newick", "href") or ""
        newick = page.request.get(f"{base}/{href.lstrip('/')}").text()
        check("the Newick download carries the curated label",
              "-> Danaus drivenus" in newick, newick[:80])
        page.screenshot(path=str(args.out / "06b-phylogeny-curated.png"),
                        full_page=True)
        show("BINs", settle=SETTLE)
        check("the BINs tab adds concordance with curated IDs",
              "Concordance (curated IDs)" in page.locator("#bins_body").inner_text())
        page.check("#grades_from_bins input[value='original']")
        time.sleep(SETTLE * 1.5)
        check("and back to the original",
              "Concordance (curated IDs)" not in
              page.locator("#bins_body").inner_text())

        # -- round 8, item 1.1: a dropped connection (a sleeping machine)
        # reloads the page and restores the work, rather than leaving a dead
        # page whose buttons do nothing and whose downloads are an error.
        show("Specimens")
        old_session = page.evaluate("Shiny.shinyapp.config.sessionId")
        page.evaluate("Shiny.shinyapp.$socket.close()")
        time.sleep(1.0)
        check("a dropped connection says it is reconnecting",
              page.locator("#bc-reconnect").is_visible())
        page.wait_for_load_state("networkidle")
        time.sleep(SETTLE * 2)
        new_session = page.evaluate("Shiny.shinyapp.config.sessionId")
        active = page.locator(".nav-link.active").inner_text()
        check("it reconnects to a new session on the same tab, work restored",
              new_session != old_session and "Specimens" in active
              and page.locator("#specimens_body table").count() > 0,
              f"tab {active!r}")
        href = page.get_attribute("#dl_curation_report", "href") or ""
        download = page.request.get(f"{base}/{href.lstrip('/')}")
        check("a download after reconnecting is the file, not an error",
              download.ok and "json" not in download.headers.get("content-type", ""),
              f"{download.status} {download.headers.get('content-type')}")
        # Round 9, item 4 and round 10, item 3: Download All is an Excel
        # workbook, prepared in the background with a progress panel, and
        # its save starts by itself once it's written.
        with page.expect_download(timeout=120_000) as saved:
            page.click("#prep_all")
            page.wait_for_selector(".bc-prep", timeout=10_000)
            panel = page.locator(".bc-prep").inner_text()
        workbook = Path(saved.value.path()).read_bytes()
        check("Download All is prepared with a progress panel, then saves itself",
              # The fixture is small enough to be ready at once.
              ("Preparing all_specimens_" in panel or "is ready" in panel)
              and saved.value.suggested_filename.endswith(".xlsx")
              and workbook[:2] == b"PK",
              f"{saved.value.suggested_filename}, {len(workbook):,} bytes")
        time.sleep(SETTLE)
        check("the panel then says it's ready",
              "is ready" in page.locator(".bc-prep").inner_text())
        href = page.get_attribute("#dl_selected_ids", "href") or ""
        ids = page.request.get(f"{base}/{href.lstrip('/')}")
        lines = ids.text().splitlines()
        check("Selected process IDs is one processid per line",
              ids.ok and len(lines) > 0
              and all(re.fullmatch(r"[A-Za-z0-9-]+", line) for line in lines),
              f"{len(lines)} lines, first {lines[:2]}")

        # Round 9, item 9b: a client that didn't open the launch URL -- here
        # a fresh browser profile, so no cookie -- is refused.
        stranger = browser.new_context()
        refused = stranger.request.get(base + "/")
        check("a browser without the launch cookie is refused",
              refused.status == 403, str(refused.status))
        stranger.close()

        check("no javascript errors", not js_errors, "; ".join(js_errors))
        browser.close()

    print(f"\n{len(failures)} failed" if failures else "\nall checks passed")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
