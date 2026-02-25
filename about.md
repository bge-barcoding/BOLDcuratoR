A shiny app to check and curate data on BOLD based on input taxonomy, geography, datasets and projects. Uses BOLDconnectR (https://github.com/boldsystems-central/BOLDconnectR)

### Features
Process multiple species level taxa with their synonyms
Process multiple higher level taxa (e.g. families)
Process multiple BOLD datasets and projects
Filter based on multiple countries
Fetch all specimens based on search parameters
Fetch all specimens within each downloaded BIN (e.g if they didn't match the original search terms)
Analyze BIN content and concordance
Rank all specimens
Grade all species using BAGS (https://doi.org/10.1111/1755-0998.13262)
Select representative specimens for curated datasets based on semi-automated workflow
Download all records in spreadsheet and sequences in fasta format
Download all flagged/annotated records for update at BOLD

### Usage
Enter your BOLD API key, or use the test API key
Enter your name and email
Resume a previous session or start a new one
Input taxa (one per line, synonyms after valid names, separated by commas)
Include any datasets and/or projects and/or countries
Click 'Get Data' to start analysis
