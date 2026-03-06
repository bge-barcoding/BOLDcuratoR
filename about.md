## BOLDcuratoR

A Shiny application for checking and curating barcode data on [BOLD](https://www.boldsystems.org/) (Barcode of Life Data System). Built on the [BOLDconnectR](https://github.com/boldsystems-central/BOLDconnectR) R package.

**GitHub:** [github.com/bge-barcoding/BOLDcuratoR](https://github.com/bge-barcoding/BOLDcuratoR)

**Authors:** Ben Price, Claude Opus

---

### Getting Started

Follow these steps to use BOLDcuratoR:

1. **Enter your credentials** in the bar at the top of the page:
   - Your BOLD API key (or use the test key for exploration)
   - Your name and email address (used for session tracking and curation logs)

2. **Go to the Data Input tab** and enter your search criteria:
   - **Taxa:** One taxon per line. For species with synonyms, list the valid name first followed by synonyms separated by commas (e.g. `Valid name, Synonym A, Synonym B`). Higher-level taxa (families, orders) are also supported.
   - **Countries / Continents (optional):** Filter downloaded records geographically. These filters are applied after download but before BIN expansion, so BIN-sharing species from other regions are still captured.

3. **Click "Get Data"** to search BOLD and download specimen records.

4. **Review results** across the analysis tabs:
   - **Species Analysis** -- checklist of species found, gap analysis comparing your input taxa against results, and summary statistics.
   - **BIN Analysis** -- BIN concordance/discordance across your taxa.
   - **BAGS Grades A--E** -- species graded using the [BAGS framework](https://doi.org/10.1111/1755-0998.13262) with specimen-level detail.
   - **Specimens** -- full specimen table with ranking, selection, annotation, and download tools.

5. **Curate and download:**
   - Select representative specimens per species (auto-selected by quality score, adjustable manually).
   - Flag or annotate records for update on BOLD.
   - Download curated datasets as spreadsheets or sequences in FASTA format.

---

### Features

- Process multiple species-level taxa with their synonyms
- Process higher-level taxa (e.g. families, orders)
- Filter by multiple countries and/or continents
- Fetch all specimens matching search parameters
- Expand BINs to include all specimens within each BIN (even those outside original search terms)
- Analyse BIN content and concordance
- Rank specimens by metadata quality
- Grade species using [BAGS](https://doi.org/10.1111/1755-0998.13262) (Barcode, Audit & Grade System)
- Semi-automated representative specimen selection
- Download records as spreadsheets, annotated curation reports, and FASTA sequences
- Automatic session saving and restoration

---

### Session Management

Your work is automatically saved every 60 seconds and when you close the browser. To resume a previous session, expand the "Resume Previous Session" panel on the Data Input tab.
