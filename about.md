A shiny app to check and curate data on BOLD based on input taxonomy, geography, datasets and projects. Uses BOLDconnectR (https://github.com/boldsystems-central/BOLDconnectR)

### Features
- Process multiple species level taxa with their synonyms
- Process multiple higher level taxa (e.g. families)
- Filter based on multiple countries and/or continents
- Fetch all specimens based on search parameters
- Fetch all specimens within each downloaded BIN (even if they didn't match the original search terms)
- Analyze BIN content and concordance
- Rank all specimens based on specimen metadata
- Grade all species using BAGS (https://doi.org/10.1111/1755-0998.13262)
- Select representative specimens for curated datasets based on a semi-automated workflow
- Download all records in spreadsheet and sequences in fasta format
- Download all flagged/annotated records for update at BOLD

### Usage
- Enter your BOLD API key, or use the test API key
- Enter your name and email
- Resume a previous session or start a new one
- Input taxa (one per line, synonyms after valid names, separated by commas)
- Include any countries and/or continents you want to filter on
- Click 'Get Data' to start analysis
