"""Scoring criteria, the rank ladder, geography and limits.

Ported from ``R/config/constants.R``.  Two blocks in that file are **deliberately
not ported**:

``ANALYSIS_CONSTANTS``
    Dead config.  Nothing in the R analysis code reads ``HAPLOTYPE``,
    ``BIN$CONCORDANCE_THRESHOLD`` or ``MAX_SPECIES_PER_BIN``; the real BIN
    concordance rule is presence/absence of conflicting names, not a 0.95
    threshold.  Porting them would imply a scoring model that does not exist.

``BAGS_GRADE_CRITERIA``
    Dead config that **contradicts the implementation** -- it says grade A at 10
    specimens, ``determine_bags_grade`` (R/utils/bags_grading.R:123-151) and the
    R tests both say 11.  The implementation is ported instead, in
    ``boldcurator.core.bags``.

One deliberate change from R: ``HAS_IMAGE`` is gone.  It required the CAOS image
API, which an offline app cannot reach.  The image requirement is also dropped
from ``RANK_2`` so that rank stays reachable -- without that change every former
rank-2 specimen would silently fall to rank 3.  Maximum quality score is
therefore **15, not 16**, and scores are not numerically comparable with the
Shiny app's.  That is stamped into every export.
"""

from __future__ import annotations

import re
from dataclasses import dataclass

# --------------------------------------------------------------------------
# Scoring
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class Criterion:
    """One scoring criterion.  Every criterion is worth exactly 1 point.

    There are no weights anywhere in the R codebase: ``quality_score`` is a
    plain count of criteria met, and ``criteria_met`` lists their names in
    declaration order.
    """

    name: str
    fields: tuple[str, ...]
    negative_pattern: str | None = None
    positive_pattern: str | None = None
    min_length: int | None = None

    @property
    def negative_re(self) -> re.Pattern[str] | None:
        return _compile(self.negative_pattern)

    @property
    def positive_re(self) -> re.Pattern[str] | None:
        return _compile(self.positive_pattern)


_CACHE: dict[str, re.Pattern[str]] = {}


def _compile(pattern: str | None) -> re.Pattern[str] | None:
    """Compile an R ``grepl`` pattern.

    Every ``grepl`` call in the R scorer passes ``ignore.case = TRUE``, so every
    pattern is case-insensitive here too.  R's ``\\b`` and ``|`` mean the same
    thing in Python, so the pattern strings transfer unchanged.
    """
    if pattern is None:
        return None
    if pattern not in _CACHE:
        _CACHE[pattern] = re.compile(pattern, re.IGNORECASE)
    return _CACHE[pattern]


#: Declaration order is significant: it is the order names appear in
#: ``criteria_met``.
SPECIMEN_SCORING_CRITERIA: tuple[Criterion, ...] = (
    Criterion(
        "SPECIES_ID",
        ("species",),
        negative_pattern=r"sp\.|spp\.|[0-9]|^sp$|aff\.|cf\.| nr ",
    ),
    Criterion(
        "TYPE_SPECIMEN",
        ("taxonomy_notes", "short_note", "collection_notes", "voucher_type", "notes"),
        positive_pattern=(
            "holotype|lectotype|isotype|syntype|paratype|neotype|allotype|"
            "paralectotype|hapantotype|cotype"
        ),
    ),
    Criterion("SEQ_QUALITY", ("nuc_basecount", "bin_uri"), min_length=500),
    Criterion(
        "PUBLIC_VOUCHER",
        ("voucher_type",),
        negative_pattern=(
            "DNA|e-vouch|privat|no voucher|unvouchered|destr|lost|missing|"
            "no specimen|none|not vouchered|person|Photo Voucher Only|not registered"
        ),
        positive_pattern=(
            "herb|museum|registered|type|national|CBG|INHS|deposit|harbarium|"
            "hebarium|holot"
        ),
    ),
    # HAS_IMAGE removed -- see the module docstring.
    Criterion(
        "IDENTIFIER",
        ("identified_by",),
        negative_pattern=(
            r"\bKate Perez\b|\bAngela Telfer\b|\bBOLD\b|\bBLAST\b|\bBIN\b|\bNone\b"
        ),
    ),
    Criterion(
        "ID_METHOD",
        ("identification_method",),
        negative_pattern=(
            "barco|BOLD|mBRAVE|SINTAX|CO1|COI|COX|DNA|mole|phylo|sequ|tree|image|"
            "bin|silva|ncbi|ncbl|engine|blast|genbank|genetic|unspecified|its|"
            "^None$|^NA$|^$"
        ),
    ),
    Criterion("COLLECTORS", ("collectors",)),
    Criterion("COLLECTION_DATE", ("collection_date_start", "collection_date_end")),
    Criterion("COUNTRY", ("country.ocean",)),
    Criterion("SITE", ("site",)),
    Criterion("SECTOR", ("sector",)),
    Criterion("REGION", ("region",)),
    Criterion("COORD", ("coord",)),
    Criterion(
        "INSTITUTION",
        ("inst",),
        negative_pattern=(
            "genbank|no voucher|personal|private|research collection of|unknown|"
            "unvouchered"
        ),
    ),
    Criterion("MUSEUM_ID", ("museumid",)),
)

CRITERIA_BY_NAME: dict[str, Criterion] = {c.name: c for c in SPECIMEN_SCORING_CRITERIA}

#: 15, not the R app's 16.  ``HAS_IMAGE`` is gone.
MAX_QUALITY_SCORE: int = len(SPECIMEN_SCORING_CRITERIA)

#: Separator used to join criterion names into ``criteria_met``.
CRITERIA_SEPARATOR = "; "

# --------------------------------------------------------------------------
# Ranks
# --------------------------------------------------------------------------

#: Each rank is a sequence of requirements, ALL of which must hold.  A
#: requirement is a tuple of alternatives, ANY of which satisfies it -- so a
#: one-element tuple is a plain AND condition.  This mirrors the R convention
#: where a scalar is an AND and a vector of length > 1 is an OR group.
#:
#: Evaluated in order 1 -> 6, **first match wins**; no match is rank 7.  Rank 1
#: is therefore not a superset of rank 2: a type specimen with a clean name is
#: rank 1 even if it lacks everything else.
#:
#: RANK_2 has lost ``HAS_IMAGE`` relative to R -- see the module docstring.
SPECIMEN_RANK_CRITERIA: tuple[tuple[tuple[str, ...], ...], ...] = (
    # RANK 1
    (("SPECIES_ID",), ("TYPE_SPECIMEN",)),
    # RANK 2
    (
        ("SPECIES_ID",),
        ("SEQ_QUALITY",),
        ("COLLECTORS",),
        ("COLLECTION_DATE",),
        ("COUNTRY",),
        ("SITE", "SECTOR", "REGION", "COORD"),
        ("IDENTIFIER",),
        ("ID_METHOD",),
        ("INSTITUTION", "PUBLIC_VOUCHER", "MUSEUM_ID"),
    ),
    # RANK 3
    (
        ("SPECIES_ID",),
        ("SEQ_QUALITY",),
        ("COUNTRY",),
        ("IDENTIFIER",),
        ("ID_METHOD",),
        ("INSTITUTION", "PUBLIC_VOUCHER", "MUSEUM_ID"),
    ),
    # RANK 4
    (("SPECIES_ID",), ("SEQ_QUALITY",), ("COUNTRY",)),
    # RANK 5
    (("SPECIES_ID",), ("SEQ_QUALITY",)),
    # RANK 6
    (("SPECIES_ID",),),
)

#: Assigned when no rank definition is satisfied.
DEFAULT_RANK = 7
VALID_RANKS = tuple(range(1, DEFAULT_RANK + 1))

# --------------------------------------------------------------------------
# Column ordering
# --------------------------------------------------------------------------

#: Columns pulled to the front of any specimen table, in this order; everything
#: else follows in its original order.  Ported from ``PREFERRED_COLUMNS()``
#: (R/config/constants.R:210-221), which emits these names even when the
#: columns are absent.
PREFERRED_COLUMN_ORDER: tuple[str, ...] = (
    "selected",
    "flag",
    "updated_id",
    "curator_notes",
    "rank",
    "quality_score",
    "processid",
    "bin_uri",
    "identification",
    "identified_by",
    "identification_method",
    "country.ocean",
    "collection_date_start",
    "collectors",
    "inst",
    "criteria_met",
)

# --------------------------------------------------------------------------
# Annotation vocabulary
# --------------------------------------------------------------------------

#: ``get_flag_options()`` (R/utils/annotation_utils.R:43).  Keys are stored,
#: values displayed.
FLAG_OPTIONS: dict[str, str] = {
    "": "None",
    "misidentification": "Misidentification",
    "synonym": "Synonym",
    "id_uncertain": "ID Uncertain",
    "data_issue": "Data Issue",
    "other_issue": "Other Issue",
}

# --------------------------------------------------------------------------
# Download limits
# --------------------------------------------------------------------------

#: ``R/config/download_limits.R:9-22``.  With the BOLD API gone these are no
#: longer shared-key rate guards -- they are purely memory guards, since the
#: binding constraint is now the cost of materialising the frame in this
#: process rather than anything on BOLD's side.
DOWNLOAD_LIMITS: dict[str, int] = {
    "WARN_RECORDS": 10_000,
    "WARN_BINS": 1_000,
    "MAX_RECORDS": 250_000,
    "MAX_BINS": 25_000,
}
# --------------------------------------------------------------------------
# Geography
# --------------------------------------------------------------------------

#: Extracted verbatim from ``R/config/constants.R:4-46`` rather than retyped.
#: These are **exact literal strings** matched against ``country.ocean``, so the
#: historic and unusual spellings are load-bearing: "Ivory Coast", "Swaziland",
#: "Cape Verde", and "United States" (not "United States of America").
#: Transcontinental countries are assigned to Asia only.
#:
#: Known gap, preserved deliberately: there are **no Caribbean entries at all**
#: -- Cuba, Jamaica, Trinidad and the rest appear under no continent, so a
#: continent filter silently excludes them. Fixing that changes results in both
#: apps and is tracked separately rather than done silently here.
CONTINENT_COUNTRIES: dict[str, list[str]] = {'Africa': ['Algeria',
            'Angola',
            'Benin',
            'Botswana',
            'Burkina Faso',
            'Burundi',
            'Cameroon',
            'Cape Verde',
            'Central African Republic',
            'Chad',
            'Comoros',
            'Congo',
            'Democratic Republic of the Congo',
            'Djibouti',
            'Egypt',
            'Equatorial Guinea',
            'Eritrea',
            'Ethiopia',
            'Gabon',
            'Gambia',
            'Ghana',
            'Guinea',
            'Guinea-Bissau',
            'Ivory Coast',
            'Kenya',
            'Lesotho',
            'Liberia',
            'Libya',
            'Madagascar',
            'Malawi',
            'Mali',
            'Mauritania',
            'Mauritius',
            'Morocco',
            'Mozambique',
            'Namibia',
            'Niger',
            'Nigeria',
            'Rwanda',
            'Sao Tome and Principe',
            'Senegal',
            'Seychelles',
            'Sierra Leone',
            'Somalia',
            'South Africa',
            'South Sudan',
            'Sudan',
            'Swaziland',
            'Tanzania',
            'Togo',
            'Tunisia',
            'Uganda',
            'Zambia',
            'Zimbabwe'],
 'Asia': ['Afghanistan',
          'Armenia',
          'Azerbaijan',
          'Bahrain',
          'Bangladesh',
          'Bhutan',
          'Brunei',
          'Cambodia',
          'China',
          'Cyprus',
          'Georgia',
          'India',
          'Indonesia',
          'Iran',
          'Iraq',
          'Israel',
          'Japan',
          'Jordan',
          'Kazakhstan',
          'Kuwait',
          'Kyrgyzstan',
          'Laos',
          'Lebanon',
          'Malaysia',
          'Maldives',
          'Mongolia',
          'Myanmar',
          'Nepal',
          'North Korea',
          'Oman',
          'Pakistan',
          'Palestine',
          'Philippines',
          'Qatar',
          'Saudi Arabia',
          'Singapore',
          'South Korea',
          'Sri Lanka',
          'Syria',
          'Taiwan',
          'Tajikistan',
          'Thailand',
          'Timor-Leste',
          'Turkey',
          'Turkmenistan',
          'United Arab Emirates',
          'Uzbekistan',
          'Vietnam',
          'Yemen'],
 'Europe': ['Albania',
            'Andorra',
            'Austria',
            'Belarus',
            'Belgium',
            'Bosnia and Herzegovina',
            'Bulgaria',
            'Croatia',
            'Czech Republic',
            'Denmark',
            'Estonia',
            'Finland',
            'France',
            'Germany',
            'Greece',
            'Hungary',
            'Iceland',
            'Ireland',
            'Italy',
            'Kosovo',
            'Latvia',
            'Liechtenstein',
            'Lithuania',
            'Luxembourg',
            'Malta',
            'Moldova',
            'Monaco',
            'Montenegro',
            'Netherlands',
            'North Macedonia',
            'Norway',
            'Poland',
            'Portugal',
            'Romania',
            'Russia',
            'San Marino',
            'Serbia',
            'Slovakia',
            'Slovenia',
            'Spain',
            'Sweden',
            'Switzerland',
            'Ukraine',
            'United Kingdom',
            'Vatican City'],
 'Oceania': ['Australia',
             'Fiji',
             'Kiribati',
             'Marshall Islands',
             'Micronesia',
             'Nauru',
             'New Zealand',
             'Palau',
             'Papua New Guinea',
             'Samoa',
             'Solomon Islands',
             'Tonga',
             'Tuvalu',
             'Vanuatu'],
 'North America': ['Canada',
                   'United States',
                   'Mexico',
                   'Greenland',
                   'Bermuda',
                   'Saint Pierre and Miquelon'],
 'Central America': ['Belize',
                     'Costa Rica',
                     'El Salvador',
                     'Guatemala',
                     'Honduras',
                     'Nicaragua',
                     'Panama'],
 'South America': ['Argentina',
                   'Bolivia',
                   'Brazil',
                   'Chile',
                   'Colombia',
                   'Ecuador',
                   'Guyana',
                   'Paraguay',
                   'Peru',
                   'Suriname',
                   'Uruguay',
                   'Venezuela',
                   'French Guiana']}
