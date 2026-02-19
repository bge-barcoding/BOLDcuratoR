
R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"
Copyright (C) 2024 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from C:/GitHub/BOLDcurator/.RData]

> shiny::runApp()
Loading required package: shiny

Attaching package: ‘DT’

The following objects are masked from ‘package:shiny’:

    dataTableOutput, renderDataTable


Attaching package: ‘shinydashboard’

The following object is masked from ‘package:graphics’:

    box

Don't forget that shinyjs can also be used in Rmd documents!

Attaching package: ‘shinyjs’

The following object is masked from ‘package:shiny’:

    runExample

The following objects are masked from ‘package:methods’:

    removeClass, show


Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union


Attaching package: ‘devtools’

The following objects are masked from ‘package:remotes’:

    dev_package_deps, install_bioc, install_bitbucket, install_cran, install_deps, install_dev,
    install_git, install_github, install_gitlab, install_local, install_svn, install_url,
    install_version, update_packages


Attaching package: ‘jsonlite’

The following object is masked from ‘package:purrr’:

    flatten

The following object is masked from ‘package:shiny’:

    validate

Warning: package ‘RSQLite’ was built under R version 4.4.2
Loading required package: BOLDconnectR
Warning: package ‘BOLDconnectR’ was built under R version 4.4.3

Listening on http://127.0.0.1:5076
[2026-02-19 20:37:00] INFO: Starting main data observer for grade A
[2026-02-19 20:37:00] WARNING: Missing required data
  Details: {"has_specimens":false,"has_grades":false} 
[2026-02-19 20:37:00] INFO: Starting main data observer for grade B
[2026-02-19 20:37:00] WARNING: Missing required data
  Details: {"has_specimens":false,"has_grades":false} 
[2026-02-19 20:37:00] INFO: Starting main data observer for grade C
[2026-02-19 20:37:00] WARNING: Missing required data
  Details: {"has_specimens":false,"has_grades":false} 
[2026-02-19 20:37:00] INFO: Starting main data observer for grade D
[2026-02-19 20:37:00] WARNING: Missing required data
  Details: {"has_specimens":false,"has_grades":false} 
[2026-02-19 20:37:00] INFO: Starting main data observer for grade E
[2026-02-19 20:37:00] WARNING: Missing required data
  Details: {"has_specimens":false,"has_grades":false} 
[2026-02-19 20:37:12] INFO: State updated successfully: user_info
[2026-02-19 20:37:12] INFO: User information updated
  Details: {"email":"","name":"Ben","bold_api_key":"SET"} 
[2026-02-19 20:37:16] INFO: State updated successfully: processing
[2026-02-19 20:37:16] INFO: Starting BOLD search
  Details: {"taxa":"sialis","datasets":"","projects":"","countries":0} 
[2026-02-19 20:37:16] INFO: State updated successfully: error
[2026-02-19 20:37:16] INFO: Processing taxonomy search
  Details: "sialis" 
[2026-02-19 20:37:16] INFO: State updated successfully: processing
 Downloading ids. 
Download complete.
[2026-02-19 20:37:19] INFO: Found 366 records for 'Sialis'
Initiating download
Downloading data in a single batch
Download complete & BCDM dataframe generated
[2026-02-19 20:37:23] INFO: Starting BIN expansion
  Details: {"initial_specimens":366,"bins_to_expand":37} 
[2026-02-19 20:37:23] INFO: State updated successfully: processing
[2026-02-19 20:37:23] INFO: State updated successfully: processing
Initiating download
Downloading data in a single batch
Download complete & BCDM dataframe generated
[2026-02-19 20:37:25] INFO: BIN expansion complete
  Details: {"bins_queried":37,"total_specimens":379} 
[2026-02-19 20:37:25] INFO: State updated successfully: processing
[2026-02-19 20:37:25] INFO: Columns before processing
  Details: {"num_columns":72,"column_names":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end"]} 
[2026-02-19 20:37:25] INFO: Columns after data.frame conversion
  Details: {"num_columns":72,"column_names":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end"],"missing_columns":[]} 
[2026-02-19 20:37:25] INFO: Columns after final processing
  Details: {"num_columns":74,"column_names":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date"],"missing_columns":[]} 
[2026-02-19 20:37:25] INFO: State updated successfully: specimen_data
[2026-02-19 20:37:25] INFO: State updated successfully: search_taxa
[2026-02-19 20:37:25] INFO: State updated successfully: bin_analysis
[2026-02-19 20:37:25] INFO: State updated successfully: bags_grades
[2026-02-19 20:37:25] INFO: State updated successfully: selected_specimens
[2026-02-19 20:37:25] INFO: State updated successfully: processing
[2026-02-19 20:37:25] INFO: Search completed successfully
  Details: {"total_specimens":369,"unique_species":18,"unique_bins":38} 
[2026-02-19 20:37:25] INFO: Processing 369 specimens
[2026-02-19 20:37:25] INFO: Validating 369 specimens
[2026-02-19 20:37:25] INFO: Validation complete: 220/369 valid species names, 220/369 species level IDs, 340/369 valid BINs
[2026-02-19 20:37:25] INFO: Scoring specimens...
[2026-02-19 20:37:25] INFO: Starting specimen scoring
[2026-02-19 20:37:26] INFO: Ranking specimens...
[2026-02-19 20:37:26] INFO: Successfully processed 369 specimens
[2026-02-19 20:37:26] INFO: State updated successfully: specimen_data
[2026-02-19 20:37:26] INFO: State updated successfully: specimen_metrics
[2026-02-19 20:37:26] INFO: Specimen processing complete
  Details: {"total_specimens":369,"metrics":{"total_specimens":369,"avg_quality_score":8.561,"median_quality_score":9,"rank_distribution":{"Var1":["2","3","4","5","7"],"Freq":[21,3,140,46,159]},"species_count":18,"criteria_coverage":{"total_criteria":14,"criteria_counts":{"criterion":["SPECIES_ID","TYPE_SPECIMEN","SEQ_QUALITY","PUBLIC_VOUCHER","HAS_IMAGE","IDENTIFIER","ID_METHOD","COLLECTORS","COLLECTION_DATE","COUNTRY","SITE","COORD","INSTITUTION","MUSEUM_ID"],"count":[220,0,335,77,369,222,101,279,290,314,187,287,295,183]},"avg_criteria_per_specimen":8.561}}} 
[2026-02-19 20:37:26] INFO: Starting data filtering - Initial columns
  Details: {"initial_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"]} 
[2026-02-19 20:37:26] INFO: State updated successfully: metrics
[2026-02-19 20:37:26] INFO: Complete filtered data info
  Details: {"total_records":369,"filtered_records":369,"initial_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"final_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coor ... <truncated>
[2026-02-19 20:37:26] INFO: Starting BIN analysis
  Details: {"total_records":369,"records_with_bins":340} 
[2026-02-19 20:37:26] INFO: Starting BIN analysis
[2026-02-19 20:37:26] INFO: State updated successfully: bin_analysis
[2026-02-19 20:37:26] INFO: BIN analysis completed
  Details: {"total_bins":{},"concordant_bins":{},"discordant_bins":{}} 
[2026-02-19 20:37:26] INFO: Building species checklist
  Details: {"specimens":369,"has_grades":false} 
[2026-02-19 20:37:26] INFO: Performing gap analysis
  Details: {"input_taxa_count":1} 
[2026-02-19 20:37:26] INFO: Species analysis complete
  Details: {"species_count":17,"gap_analysis":true} 
[2026-02-19 20:37:26] INFO: Starting main data observer for grade A
[2026-02-19 20:37:26] WARNING: Missing required data
  Details: {"has_specimens":true,"has_grades":false} 
[2026-02-19 20:37:26] INFO: Starting main data observer for grade B
[2026-02-19 20:37:26] WARNING: Missing required data
  Details: {"has_specimens":true,"has_grades":false} 
[2026-02-19 20:37:26] INFO: Starting main data observer for grade C
[2026-02-19 20:37:26] WARNING: Missing required data
  Details: {"has_specimens":true,"has_grades":false} 
[2026-02-19 20:37:26] INFO: Starting main data observer for grade D
[2026-02-19 20:37:26] WARNING: Missing required data
  Details: {"has_specimens":true,"has_grades":false} 
[2026-02-19 20:37:26] INFO: Starting main data observer for grade E
[2026-02-19 20:37:26] WARNING: Missing required data
  Details: {"has_specimens":true,"has_grades":false} 
[2026-02-19 20:37:26] INFO: Processing 369 specimens
[2026-02-19 20:37:26] INFO: Validating 369 specimens
[2026-02-19 20:37:26] INFO: Validation complete: 220/369 valid species names, 220/369 species level IDs, 340/369 valid BINs
[2026-02-19 20:37:26] INFO: Scoring specimens...
[2026-02-19 20:37:26] INFO: Starting specimen scoring
[2026-02-19 20:37:27] INFO: Ranking specimens...
[2026-02-19 20:37:27] INFO: Successfully processed 369 specimens
[2026-02-19 20:37:27] INFO: State updated successfully: bags_grades
[2026-02-19 20:37:27] INFO: State updated successfully: specimen_data
[2026-02-19 20:37:27] INFO: Processing 369 specimens
[2026-02-19 20:37:27] INFO: Validating 369 specimens
[2026-02-19 20:37:27] INFO: Validation complete: 220/369 valid species names, 220/369 species level IDs, 340/369 valid BINs
[2026-02-19 20:37:27] INFO: Scoring specimens...
[2026-02-19 20:37:27] INFO: Starting specimen scoring
[2026-02-19 20:37:27] INFO: Ranking specimens...
[2026-02-19 20:37:27] INFO: Successfully processed 369 specimens
[2026-02-19 20:37:27] INFO: State updated successfully: specimen_data
[2026-02-19 20:37:27] INFO: State updated successfully: specimen_metrics
[2026-02-19 20:37:27] INFO: Specimen processing complete
  Details: {"total_specimens":369,"metrics":{"total_specimens":369,"avg_quality_score":8.561,"median_quality_score":9,"rank_distribution":{"Var1":["2","3","4","5","7"],"Freq":[21,3,140,46,159]},"species_count":18,"criteria_coverage":{"total_criteria":14,"criteria_counts":{"criterion":["SPECIES_ID","TYPE_SPECIMEN","SEQ_QUALITY","PUBLIC_VOUCHER","HAS_IMAGE","IDENTIFIER","ID_METHOD","COLLECTORS","COLLECTION_DATE","COUNTRY","SITE","COORD","INSTITUTION","MUSEUM_ID"],"count":[220,0,335,77,369,222,101,279,290,314,187,287,295,183]},"avg_criteria_per_specimen":8.561}}} 
[2026-02-19 20:37:27] INFO: Pre-format specimen table data
  Details: {"rows":369,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["ANEU003-25","ANEU011-25","ANEU023-25","ANEU181-25","ANEU182-25","ANEU183-25"]} 
[2026-02-19 20:37:27] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:37:27] INFO: Specimen table refreshed with updated annotations
[2026-02-19 20:37:27] INFO: Building species checklist
  Details: {"specimens":369,"has_grades":true} 
[2026-02-19 20:37:27] INFO: Performing gap analysis
  Details: {"input_taxa_count":1} 
[2026-02-19 20:37:27] INFO: Species analysis complete
  Details: {"species_count":17,"gap_analysis":true} 
[2026-02-19 20:37:27] INFO: Starting main data observer for grade A
[2026-02-19 20:37:27] INFO: Processing BAGS grade A data
  Details: {"total_records":369,"grade":"A"} 
[2026-02-19 20:37:27] INFO: Found 3 species for grade A
[2026-02-19 20:37:27] INFO: Filtered to 54 specimens for grade A
[2026-02-19 20:37:27] INFO: Grade A data processing complete
  Details: {"specimen_count":54,"species_count":3} 
[2026-02-19 20:37:27] INFO: Starting main data observer for grade B
[2026-02-19 20:37:27] INFO: Processing BAGS grade B data
  Details: {"total_records":369,"grade":"B"} 
[2026-02-19 20:37:27] INFO: Found 2 species for grade B
[2026-02-19 20:37:27] INFO: Filtered to 9 specimens for grade B
[2026-02-19 20:37:27] INFO: Grade B data processing complete
  Details: {"specimen_count":9,"species_count":2} 
[2026-02-19 20:37:27] INFO: Starting main data observer for grade C
[2026-02-19 20:37:27] INFO: Processing BAGS grade C data
  Details: {"total_records":369,"grade":"C"} 
[2026-02-19 20:37:27] INFO: Found 5 species for grade C
[2026-02-19 20:37:27] INFO: Filtered to 69 specimens for grade C
[2026-02-19 20:37:27] INFO: Grade C data processing complete
  Details: {"specimen_count":69,"species_count":5} 
[2026-02-19 20:37:27] INFO: Starting main data observer for grade D
[2026-02-19 20:37:27] INFO: Processing BAGS grade D data
  Details: {"total_records":369,"grade":"D"} 
[2026-02-19 20:37:27] INFO: Found 3 species for grade D
[2026-02-19 20:37:27] INFO: Filtered to 4 specimens for grade D
[2026-02-19 20:37:27] INFO: Grade D data processing complete
  Details: {"specimen_count":4,"species_count":3} 
[2026-02-19 20:37:27] INFO: Starting main data observer for grade E
[2026-02-19 20:37:27] INFO: Processing BAGS grade E data
  Details: {"total_records":369,"grade":"E"} 
[2026-02-19 20:37:27] INFO: Found 4 species for grade E
[2026-02-19 20:37:27] INFO: Filtered to 77 specimens for grade E
[2026-02-19 20:37:27] INFO: Grade E data processing complete
  Details: {"specimen_count":77,"species_count":4} 
[2026-02-19 20:38:22] INFO: Rendering specimen table
  Details: {"rows":369} 
[2026-02-19 20:38:22] INFO: Pre-format specimen table data
  Details: {"rows":369,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["ANEU003-25","ANEU011-25","ANEU023-25","ANEU181-25","ANEU182-25","ANEU183-25"]} 
[2026-02-19 20:38:22] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:38:32] INFO: Rendering specimen tables for grade A
[2026-02-19 20:38:32] INFO: Creating tables for 54 specimens
[2026-02-19 20:38:32] INFO: Organized specimens into 3 groups
[2026-02-19 20:38:32] INFO: Creating table 1 for grade A
[2026-02-19 20:38:32] INFO: Pre-format specimen table data
  Details: {"rows":18,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["RRINV2792-15","TTSOW676-12","RARBB1179-17","RARBB1180-17","RARBB1242-17","RARBB1243-17"]} 
[2026-02-19 20:38:32] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:38:32] INFO: Creating table 2 for grade A
[2026-02-19 20:38:32] INFO: Pre-format specimen table data
  Details: {"rows":21,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["KMDS002-14","KMDS003-14","KMDS004-14","KMDS012-14","KMDS013-14","KMDS014-14"]} 
[2026-02-19 20:38:32] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:38:32] INFO: Creating table 3 for grade A
[2026-02-19 20:38:32] INFO: Pre-format specimen table data
  Details: {"rows":15,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["KMDS005-14","KMDS023-14","KMDS024-14","KMDS025-14","KMDS029-14","KMDS030-14"]} 
[2026-02-19 20:38:32] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:38:32] INFO: Successfully created 3 tables
[2026-02-19 20:39:02] INFO: Rendering specimen tables for grade B
[2026-02-19 20:39:02] INFO: Creating tables for 9 specimens
[2026-02-19 20:39:02] INFO: Organized specimens into 2 groups
[2026-02-19 20:39:02] INFO: Creating table 1 for grade B
[2026-02-19 20:39:02] INFO: Pre-format specimen table data
  Details: {"rows":8,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["KMDS008-14","KMDS009-14","KMDS010-14","KMDS011-14","GBA22413-15","GBA22414-15"]} 
[2026-02-19 20:39:02] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:39:02] INFO: Creating table 2 for grade B
[2026-02-19 20:39:02] INFO: Pre-format specimen table data
  Details: {"rows":1,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":"GBMH6944-09"} 
[2026-02-19 20:39:02] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:39:02] INFO: Successfully created 2 tables
[2026-02-19 20:39:46] INFO: Rendering specimen tables for grade E
[2026-02-19 20:39:46] INFO: Creating tables for 77 specimens
[2026-02-19 20:39:46] INFO: Organized specimens into 3 groups
[2026-02-19 20:39:46] INFO: Creating table 1 for grade E
[2026-02-19 20:39:46] INFO: Pre-format specimen table data
  Details: {"rows":44,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["DDNEU113-17","DDNEU114-17","DDNEU115-17","DDNEU116-17","DTNHM7949-23","DTNHM7958-23"]} 
[2026-02-19 20:39:46] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:39:46] INFO: Creating table 2 for grade E
[2026-02-19 20:39:46] INFO: Pre-format specimen table data
  Details: {"rows":4,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["ROMAC1605-22","GBCOU4540-14","PLSW105-20","GBMNE23296-21"]} 
[2026-02-19 20:39:46] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:39:46] INFO: Creating table 3 for grade E
[2026-02-19 20:39:46] INFO: Pre-format specimen table data
  Details: {"rows":21,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["BACZP1093-16","BACZP1151-16","BACZP1152-16","BACZP1408-16","BACZP1590-16","BACZP1591-16"]} 
[2026-02-19 20:39:46] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:39:46] INFO: Successfully created 3 tables
[2026-02-19 20:40:56] INFO: Starting data filtering - Initial columns
  Details: {"initial_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"]} 
[2026-02-19 20:40:56] INFO: After criteria filter
  Details: {"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"filter_criteria":"TYPE_SPECIMEN","rows_before":369,"rows_after":0} 
[2026-02-19 20:40:56] INFO: State updated successfully: metrics
[2026-02-19 20:40:56] INFO: Complete filtered data info
  Details: {"total_records":369,"filtered_records":0,"initial_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"final_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_ ... <truncated>
[2026-02-19 20:40:56] INFO: Rendering specimen table
  Details: {"rows":0} 
[2026-02-19 20:40:56] WARNING: Empty input data to prepare_module_data
[2026-02-19 20:40:56] WARNING: Empty input data
[2026-02-19 20:40:58] INFO: Starting data filtering - Initial columns
  Details: {"initial_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"]} 
[2026-02-19 20:40:58] INFO: State updated successfully: metrics
[2026-02-19 20:40:58] INFO: Complete filtered data info
  Details: {"total_records":369,"filtered_records":369,"initial_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"final_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coor ... <truncated>
[2026-02-19 20:40:58] INFO: Pre-format specimen table data
  Details: {"rows":369,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["ANEU003-25","ANEU011-25","ANEU023-25","ANEU181-25","ANEU182-25","ANEU183-25"]} 
[2026-02-19 20:40:58] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:40:58] INFO: Specimen table refreshed with updated annotations
[2026-02-19 20:40:58] INFO: Rendering specimen table
  Details: {"rows":369} 
[2026-02-19 20:40:58] INFO: Pre-format specimen table data
  Details: {"rows":369,"columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank"],"sample_processids":["ANEU003-25","ANEU011-25","ANEU023-25","ANEU181-25","ANEU182-25","ANEU183-25"]} 
[2026-02-19 20:40:58] INFO: Post-format specimen table
  Details: {"table_class":"data.frame","table_columns":["processid","record_id","insdc_acs","sampleid","specimenid","taxid","short_note","identification_method","museumid","fieldid","collection_code","processid_minted_date","inst","funding_src","sex","life_stage","reproduction","habitat","collectors","site_code","specimen_linkout","collection_event_id","sampling_protocol","tissue_type","collection_date_start","collection_time","associated_taxa","associated_specimens","voucher_type","notes","taxonomy_notes","collection_notes","geoid","marker_code","kingdom","phylum","class","order","family","subfamily","tribe","genus","species","subspecies","identification","identification_rank","species_reference","identified_by","sequence_run_site","nuc","nuc_basecount","sequence_upload_date","bin_uri","bin_created_date","elev","depth","coord","coord_source","coord_accuracy","elev_accuracy","depth_accuracy","realm","biome","ecoregion","region","sector","site","country_iso","country.ocean","province.state","bold_recordset_code_arr","collection_date_end","data_source","import_date","valid_species","species_id","valid_bin","quality_score","criteria_met","rank","selected","flag","curator_notes"]} 
[2026-02-19 20:41:57] INFO: Downloading search results
  Details: {"format":"csv"} 
[2026-02-19 20:43:06] INFO: Downloaded filtered specimens
  Details: {"count":369} 
[2026-02-19 20:47:12] INFO: Starting main data observer for grade A
[2026-02-19 20:47:12] WARNING: Missing required data
  Details: {"has_specimens":false,"has_grades":false} 
[2026-02-19 20:47:12] INFO: Starting main data observer for grade B
[2026-02-19 20:47:12] WARNING: Missing required data
  Details: {"has_specimens":false,"has_grades":false} 
[2026-02-19 20:47:12] INFO: Starting main data observer for grade C
[2026-02-19 20:47:12] WARNING: Missing required data
  Details: {"has_specimens":false,"has_grades":false} 
[2026-02-19 20:47:12] INFO: Starting main data observer for grade D
[2026-02-19 20:47:12] WARNING: Missing required data
  Details: {"has_specimens":false,"has_grades":false} 
[2026-02-19 20:47:12] INFO: Starting main data observer for grade E
[2026-02-19 20:47:12] WARNING: Missing required data
  Details: {"has_specimens":false,"has_grades":false}