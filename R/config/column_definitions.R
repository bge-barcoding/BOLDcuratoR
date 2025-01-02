# R/config/column_definitions.R

# Column display names and configurations for specimen tables
COLUMN_DEFINITIONS <- list(
  # Core specimen fields
  processid = "Process_ID",
  record_id = "Record_ID",
  insdc_acs = "GenBank",
  sampleid = "Sample_ID",
  specimenid = "Specimen_ID",
  taxid = "NCBI_TaxID",
  bin_uri = "BIN",

  # Collection info
  collection_code = "Collection_Code",
  collection_date_start = "Date",
  collection_date_end = "End_Date",
  collection_time = "Collection_Time",
  collection_event_id = "Collection_Event_ID",
  collection_notes = "Collection_Notes",

  # Location
  country.ocean = "Country",
  province.state = "State",
  region = "Region",
  site = "Site",
  coord = "Coordinates",
  coord_source = "Coord_Source",
  coord_accuracy = "Coord_Accuracy",
  elev = "Elevation",
  elev_accuracy = "Elev_Accuracy",
  depth = "Depth",
  depth_accuracy = "Depth_Accuracy",

  # Taxonomy
  kingdom = "Kingdom",
  phylum = "Phylum",
  class = "Class",
  order = "Order",
  family = "Family",
  tribe = "Tribe",
  genus = "Genus",
  species = "Species",
  subspecies = "Subspecies",
  identification = "Identification",
  identified_by = "Identified_By",
  identification_method = "ID_Method",
  identification_rank = "ID_Rank",
  taxonomy_notes = "Taxonomy_Notes",

  # Specimen details
  voucher_type = "Voucher_Type",
  sex = "Sex",
  life_stage = "Life_Stage",
  reproduction = "Reproduction",
  habitat = "Habitat",
  associated_taxa = "Associated_Taxa",
  associated_specimens = "Associated_Specimens",
  tissue_type = "Tissue_Type",
  notes = "Notes",

  # Institution
  inst = "Institution",
  funding_src = "Funding_Source",
  museumid = "Museum_ID",

  # Sequence
  marker_code = "Marker",
  nuc = "Sequence",
  nuc_basecount = "Base_Count",
  sequence_upload_date = "Sequence_Upload_Date",
  sequence_run_site = "Sequencing_Site",

  # Quality metrics
  quality_score = "Score",
  criteria_met = "Criteria_Met",
  rank = "Rank",

  # Curator fields
  flag = "Issue",
  curator_notes = "Curator_Notes",
  selected = "Selected",

  # Others Claude forgot
  bin_created_date = "BIN_created",
  biome = "Biome",
  bold_recordset_code_arr = "Recordset_Code",
  collectors = "Collectors",
  country_iso = "Country_ISO",
  data_source = "Data_Source",
  ecoregion = "Ecoregion",
  fieldid = "Field_ID",
  geoid = "Geo_ID",
  import_date = "Import_Date",
  processid_minted_date = "Process_ID_Date",
  realm = "Realm",
  sampling_protocol = "Sampling_Protocol",
  sector = "Sector",
  short_note = "Short_Note",
  site_code = "Site_Code",
  species_id = "Species_ID",
  species_reference = "Species_Ref",
  specimen_linkout = "Specimen_Link",
  subfamily = "Subfamily",
  valid_bin = "Valid_BIN",
  valid_species = "Valid_Species"
)

# Export the definitions
list2env(list(COLUMN_DEFINITIONS = COLUMN_DEFINITIONS), envir = .GlobalEnv)
