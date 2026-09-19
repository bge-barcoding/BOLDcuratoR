import pandas as pd

from boldcurator.core.ranking import score_and_rank


def _rank(**fields) -> int:
    return int(score_and_rank(pd.DataFrame([fields]))["rank"].iloc[0])


GOOD = dict(
    species="Danaus plexippus",
    bin_uri="BOLD:AAA1",
    nuc_basecount="658",
    collectors="A. Smith",
    collection_date_start="2015-07",
    site="Pen Ponds",
    identified_by="A. Smith",
    identification_method="Morphology",
    inst="Natural History Museum",
    museumid="NHMUK1",
)
GOOD["country.ocean"] = "United Kingdom"


def test_rank_1_is_not_a_superset_of_rank_2():
    """A type specimen with a clean name is rank 1 even lacking everything else."""
    assert _rank(species="Danaus plexippus", notes="holotype") == 1


def test_rank_2_is_reachable_without_images():
    """The point of dropping HAS_IMAGE from RANK_2.

    In R this record would be rank 3, because the image criterion is a hard AND
    and an offline app can never satisfy it.
    """
    assert _rank(**GOOD) == 2


def test_rank_3_when_locality_and_collection_details_are_missing():
    fields = {k: v for k, v in GOOD.items()
              if k not in ("collectors", "collection_date_start", "site")}
    assert _rank(**fields) == 3


def test_rank_4_5_6_ladder():
    assert _rank(species="Danaus plexippus", bin_uri="BOLD:AAA1",
                 nuc_basecount="658", **{"country.ocean": "United Kingdom"}) == 4
    assert _rank(species="Danaus plexippus", bin_uri="BOLD:AAA1",
                 nuc_basecount="658") == 5
    assert _rank(species="Danaus plexippus") == 6


def test_default_rank_is_7():
    assert _rank(species="Danaus sp.") == 7
    assert _rank(species="") == 7


def test_or_group_any_locality_field_satisfies_rank_2():
    for field in ("site", "sector", "region", "coord"):
        fields = {k: v for k, v in GOOD.items() if k != "site"}
        fields[field] = "something"
        assert _rank(**fields) == 2, field
