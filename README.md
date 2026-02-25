A shiny app to check and curate data on BOLD based on input taxonomy, geography, datasets and projects. 
Uses BOLDconnectR (https://github.com/boldsystems-central/BOLDconnectR)

## Features
- Process multiple species level taxa with their synonyms
- Process multiple higher level taxa (e.g. families)
- Process multiple BOLD datasets and projects
- Filter based on multiple countries
- Fetch all specimens based on search parameters
- Fetch all specimens within each downloaded BIN (e.g if they didn't match the original search terms)
- Analyze BIN content and concordance
- Rank all specimens
- Grade all species using BAGS (https://doi.org/10.1111/1755-0998.13262)
- Select representative specimens for curated datasets based on semi-automated workflow
- Download all resulting data in spreadsheet and fasta format

## Usage
1. Enter your BOLD API key or use the test key provided
2. Enter your name and email
3. Input taxa (one per line, synonyms after valid names, separated by commas)
4. Include any datasets, projects, and/or countries
5. Click 'Get Data' to start analysis
