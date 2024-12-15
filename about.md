# BOLDcuratoR

## Overview
A shiny app to check and curate data on BOLD based on input taxonomy, geography, datasets and projects. Uses BOLDconnectR (https://github.com/boldsystems-central/BOLDconnectR/tree/main)

## Features
Process multiple species level taxa with their synonyms
Process multiple higher level taxa (e.g. families)
Process multiple BOLD datasets and projects
Filter based on multiple countries or continents
Fetch all specimens based on search parameters
Fetch all specimens within each downloaded BIN (e.g if they didn't match the original search terms)
Analyze BIN content and concordance
Rank all specimens
Grade all species using BAGS (https://doi.org/10.1111/1755-0998.13262)
Select representative specimens for curated datasets based on semi-automated workflow
Download all resulting data in spreadsheet and fasta format

## Usage
Enter your BOLD API key
Enter your name, email and ORCID (for subsequent publications)
Input taxa (one per line, synonyms after valid names, separated by commas)
Include any datasets and/or projects
Include any countries and/or continents
Click 'Get Data' to start analysis
