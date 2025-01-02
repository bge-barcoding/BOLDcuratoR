# Application constants

# Geographic constants
CONTINENT_COUNTRIES <- list(
  Africa = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso",
             "Burundi", "Cameroon", "Cape Verde", "Central African Republic",
             "Chad", "Comoros", "Congo", "Democratic Republic of the Congo",
             "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia",
             "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
             "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya",
             "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius",
             "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria",
             "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles",
             "Sierra Leone", "Somalia", "South Africa", "South Sudan",
             "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia",
             "Uganda", "Zambia", "Zimbabwe"),
  Asia = c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh",
           "Bhutan", "Brunei", "Cambodia", "China", "Cyprus", "Georgia",
           "India", "Indonesia", "Iran", "Iraq", "Israel", "Japan", "Jordan",
           "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", "Lebanon", "Malaysia",
           "Maldives", "Mongolia", "Myanmar", "Nepal", "North Korea", "Oman",
           "Pakistan", "Palestine", "Philippines", "Qatar", "Saudi Arabia",
           "Singapore", "South Korea", "Sri Lanka", "Syria", "Taiwan",
           "Tajikistan", "Thailand", "Timor-Leste", "Turkey", "Turkmenistan",
           "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen"),
  Europe = c("Albania", "Andorra", "Austria", "Belarus", "Belgium",
             "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czech Republic",
             "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
             "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia",
             "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova",
             "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway",
             "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia",
             "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine",
             "United Kingdom", "Vatican City"),
  Oceania = c("Australia", "Fiji", "Kiribati", "Marshall Islands",
              "Micronesia", "Nauru", "New Zealand", "Palau",
              "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga",
              "Tuvalu", "Vanuatu"),
  "North America" = c("Canada", "United States", "Mexico", "Greenland",
                      "Bermuda", "Saint Pierre and Miquelon"),
  "Central America" = c("Belize", "Costa Rica", "El Salvador", "Guatemala",
                        "Honduras", "Nicaragua", "Panama"),
  "South America" = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                      "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname",
                      "Uruguay", "Venezuela", "French Guiana")
)

# Specimen scoring criteria
SPECIMEN_SCORING_CRITERIA <- list(
  SPECIES_ID = list(
    fields = c("species"),
    negative_pattern = "sp\\.|spp\\.|[0-9]|^sp$|aff\\.|cf\\."
  ),

  TYPE_SPECIMEN = list(
    fields = c("taxonomy_notes", "short_note", "collection_notes", "voucher_type", "notes"),
    positive_pattern = "holotype|lectotype|isotype|syntype|paratype|neotype|allotype|paralectotype|hapantotype|cotype"
  ),

  SEQ_QUALITY = list(
    fields = c("nuc_basecount", "bin_uri"),
    min_length = 500
  ),

  PUBLIC_VOUCHER = list(
    fields = c("voucher_type"),
    negative_pattern = "DNA|e-vouch|privat|no voucher|unvouchered|destr|lost|missing|no specimen|none|not vouchered|person|Photo Voucher Only|not registered",
    positive_pattern = "herb|museum|registered|type|national|CBG|INHS|deposit|harbarium|hebarium|holot"
  ),

  HAS_IMAGE = list(
    fields = c("processid") # update this once proper image checking available in api
  ),

  IDENTIFIER = list(
    fields = c("identified_by"),
    negative_pattern = "Kate Perez|Angela Telfer|BOLD"
  ),

  ID_METHOD = list(
    fields = c("identification_method"),
    positive_pattern = "descr|det|diss|exam|expert|genit|identifier|key|label|literature|micros|mor|taxonomic|type|vou|guide|flora|specimen|traditional|visual|wing|logical|knowledge|photo|verified"
  ),

  COLLECTORS = list(
    fields = c("collectors")
  ),

  COLLECTION_DATE = list(
    fields = c("collection_date_start")
  ),

  COUNTRY = list(
    fields = c("country.ocean")
  ),

  SITE = list(
    fields = c("site")
  ),

  COORD = list(
    fields = c("coord")
  ),

  INSTITUTION = list(
    fields = c("inst"),
    negative_pattern = "genbank|no voucher|personal|private|research collection of|unknown|unvouchered"
  ),

  MUSEUM_ID = list(
    fields = c("museumid")
  )
)

# Analysis constants
ANALYSIS_CONSTANTS <- list(
  HAPLOTYPE = list(
    MIN_OVERLAP = 100,
    MATCH_SCORE = 1,
    MISMATCH_PENALTY = -1,
    GAP_PENALTY = -2,
    MIN_COVERAGE = 0.8,
    MIN_QUALITY = 0.6,
    MIN_SEQUENCE_LENGTH = 500
  ),

  BIN = list(
    MIN_SPECIMENS = 3,
    MIN_COVERAGE = 0.8,
    CONCORDANCE_THRESHOLD = 0.95,
    MAX_SPECIES_PER_BIN = 5
  ),

  SEQUENCE = list(
    MIN_LENGTH = 500,
    MAX_NS = 0.01,
    QUAL_THRESHOLD = 30
  ),

  QUALITY = list(
    MINIMUM_SEQUENCE_LENGTH = 500,
    MAXIMUM_QUALITY_SCORE = 14,
    PASSING_QUALITY_SCORE = 7
  )
)

# Specimen ranking constants
SPECIMEN_RANK_CRITERIA <- list(
  RANK_1 = c("SPECIES_ID", "TYPE_SPECIMEN"),
  RANK_2 = c("SPECIES_ID", "SEQ_QUALITY", "HAS_IMAGE", "PUBLIC_VOUCHER",
             "IDENTIFIER", "SITE", "COLLECTION_DATE", "COUNTRY", "COORD", "COLLECTORS"),
  RANK_3 = c("SPECIES_ID", "SEQ_QUALITY", "HAS_IMAGE", "PUBLIC_VOUCHER",
             "IDENTIFIER", "COUNTRY"),
  RANK_4 = c("SPECIES_ID", "SEQ_QUALITY", "HAS_IMAGE", "COUNTRY"),
  RANK_5 = c("SPECIES_ID", "SEQ_QUALITY", "HAS_IMAGE"),
  RANK_6 = c("SPECIES_ID", "SEQ_QUALITY"),
  RANK_7 = c()  # Default rank if no other criteria met
)

# BAGS grading constants
BAGS_GRADE_CRITERIA <- list(
  A = list(min_specimens = 10, max_bins = 1, shared_bins = FALSE),
  B = list(min_specimens = 3, max_bins = 1, shared_bins = FALSE),
  C = list(min_specimens = 1, max_bins = Inf, shared_bins = FALSE),
  D = list(min_specimens = 1, max_bins = 1, shared_bins = FALSE),
  E = list(shared_bins = TRUE)
)

# Validation constants
VALIDATION_CONSTANTS <- list(
  REQUIRED_FIELDS = c("processid", "bin_uri"),
  MAX_MISSING = 0.1,
  MIN_QUALITY_SCORE = 0,
  MAX_QUALITY_SCORE = 14,
  VALID_RANKS = 1:7
)

# API and request constants
API_CONSTANTS <- list(
  MAX_REQUEST_SIZE = 30 * 1024^2,  # 30MB
  TIMEOUT = 300,                    # 5 minutes
  RATE_LIMIT_INTERVAL = 0.06,      # 60ms between requests
  MAX_RETRIES = 3,
  RETRY_DELAY = 1                  # 1 second
)

# UI constants
UI_CONSTANTS <- list(
  TABLE_PAGE_LENGTHS = c(10, 25, 50, 100),
  MAX_PLOT_POINTS = 10000,
  TIMEOUT_WARNING = 270,           # Show warning at 4.5 minutes
  MAX_FILE_SIZE = 30 * 1024^2     # 30MB
)

# Column configurations for data display
PREFERRED_COLUMNS <- function(data) {
  custom_cols <- c(
    "selected", "flag", "curator_notes",
    "rank","quality_score", "processid", "bin_uri",
    "identification", "identified_by",
    "identification_method", "country.ocean",
    "collection_date_start", "collectors", "inst",
    "criteria_met"
  )
  bold_cols <- setdiff(names(data), custom_cols)
  c(custom_cols, bold_cols)
}

# Export all constants to global environment
list2env(list(
  CONTINENT_COUNTRIES = CONTINENT_COUNTRIES,
  SPECIMEN_SCORING_CRITERIA = SPECIMEN_SCORING_CRITERIA,
  ANALYSIS_CONSTANTS = ANALYSIS_CONSTANTS,
  SPECIMEN_RANK_CRITERIA = SPECIMEN_RANK_CRITERIA,
  BAGS_GRADE_CRITERIA = BAGS_GRADE_CRITERIA,
  VALIDATION_CONSTANTS = VALIDATION_CONSTANTS,
  API_CONSTANTS = API_CONSTANTS,
  UI_CONSTANTS = UI_CONSTANTS,
  PREFERRED_COLUMNS = PREFERRED_COLUMNS
), envir = .GlobalEnv)
