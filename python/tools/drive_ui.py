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
import time
from pathlib import Path

SETTLE = 2.0          # seconds to let Shiny round-trip after an interaction


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--url", default="http://127.0.0.1:8765/")
    parser.add_argument("--out", type=Path, default=Path("ui-screenshots"))
    parser.add_argument("--taxon", default="Nymphalidae")
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
        page = browser.new_page(viewport={"width": 1500, "height": 950})
        js_errors: list[str] = []
        page.on("pageerror", lambda e: js_errors.append(str(e)))
        page.goto(args.url, wait_until="networkidle")

        def pager() -> str:
            return page.locator("#pager").inner_text().strip()

        def column(index: int, rows: int = 3) -> list[str]:
            body = page.locator("table tbody tr")
            return [body.nth(i).inner_text().split("\t")[index]
                    for i in range(min(body.count(), rows))]

        # -- search
        page.fill("#taxa", args.taxon)
        page.click("#search")
        page.wait_for_selector("table tbody tr", timeout=30_000)
        time.sleep(SETTLE)
        check("search renders a page", page.locator("table tbody tr").count() > 0,
              page.locator("#search_status").inner_text().strip())
        page.screenshot(path=str(args.out / "01-search.png"))
        first = column(4)
        pages_before = pager()

        # -- paging
        page.click("#next_")
        time.sleep(SETTLE)
        check("next page shows different rows", column(4) != first,
              f"{pages_before} -> {pager()}")
        page.screenshot(path=str(args.out / "02-page2.png"))

        # -- sorting, both directions
        page.select_option("#sort", "processid")
        time.sleep(SETTLE)
        page.click("#first")
        time.sleep(SETTLE)
        ascending = column(4)
        check("ascending sort orders the result", ascending == sorted(ascending),
              str(ascending))
        page.check("#descending")
        time.sleep(SETTLE)
        descending = column(4)
        check("descending sort reverses it",
              descending == sorted(descending, reverse=True) and descending != ascending,
              str(descending))
        page.uncheck("#descending")
        time.sleep(SETTLE)
        page.screenshot(path=str(args.out / "03-sorted.png"))

        # -- page size. The grid virtualises rows in the DOM, so the row count
        # is not the honest signal here; the page count is.
        before = pager()
        page.select_option("#page_size", "50")
        time.sleep(SETTLE)
        check("rows-per-page changes the page count", pager() != before,
              f"{before} -> {pager()}")
        page.select_option("#page_size", "25")
        time.sleep(SETTLE)

        # -- selection and bulk annotation
        page.click("#select_page")
        time.sleep(SETTLE)
        selected = page.locator("#selection_status").inner_text().strip()
        check("select page reports a selection", selected.startswith("25"), selected)
        page.select_option("#flag", "id_uncertain")
        page.fill("#note", "checked against the type series")
        page.click("#apply")
        time.sleep(SETTLE)
        body = page.locator("table tbody").inner_text()
        check("the flag appears in the grid", "id_uncertain" in body)
        check("the note appears in the grid", "type series" in body)
        page.screenshot(path=str(args.out / "04-annotated.png"))

        # -- select all, then annotate a page that was never rendered
        page.click("#select_all")
        time.sleep(SETTLE)
        page.click("#last")
        time.sleep(SETTLE)
        page.select_option("#flag", "synonym")
        page.click("#apply")
        time.sleep(SETTLE)
        check("select-all reaches a page never rendered",
              "synonym" in page.locator("table tbody").inner_text())
        page.screenshot(path=str(args.out / "05-select-all.png"))

        check("no javascript errors", not js_errors, "; ".join(js_errors))
        browser.close()

    print(f"\n{len(failures)} failed" if failures else "\nall checks passed")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
