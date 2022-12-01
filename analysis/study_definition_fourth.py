######################################
#
# This script provides the formal specification of the study data that will be extracted from
# the OpenSAFELY database.
#
# STUDY PURPOSE: to understand cumulative uptake 
#   of fourth dose (second booster) COVID-19 vaccine
#   by age, before and after 50+ became eligible
#   on October 14, 2022
#
######################################


# IMPORT STATEMENTS ----

# Import code building blocks from cohort extractor package
from cohortextractor import (
    StudyDefinition,
    patients,
    Measure,
    codelist,
)

# Import codelists from codelist.py (which pulls them from the codelist folder)
# from codelists import *

# Specifiy study defeinition
study = StudyDefinition(

    # Configure the expectations framework
    default_expectations = {
        "date": {"earliest": "2020-12-08", "latest": "2022-12-01"},
        "rate": "uniform",
        "incidence": 0.2,
    },
        
    # Set index date
    index_date = "2022-09-01",

    # This line defines the study population
    population=patients.satisfying(
        """
        registered
        AND
        (age >= 40 AND age < 60)
        AND
        (sex = "M" OR sex = "F")
        AND
        NOT has_died
        """,
        registered = patients.registered_as_of(
            "index_date",
        ),
        has_died = patients.died_from_any_cause(
            on_or_before = "index_date",
            returning = "binary_flag",
        ),
    ),

    ### Age (continuous)
    age = patients.age_as_of(
        "2022-10-01", # People 50+ years became vaccine eligible on October 14
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
            "incidence" : 0.001
        },
    ),

    ### Age categories
    age_cat = patients.categorised_as(
    {"0": "DEFAULT",
      "1": """age >= 40 AND age < 45""",
      "2": """age >= 45 AND age < 50""",
      "3": """age >= 50 AND age < 55""",
      "4": """age >= 55 AND age < 60""",
    },

    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "0": 0.01,
          "1": 0.24,
          "2": 0.25,
          "3": 0.25,
          "4": 0.25,
        }},
    },
    ),
    
    ### Sex
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),

    ###############################################################################
    # COVID VACCINATION
    ###############################################################################

    # any COVID vaccination (first dose)
    covid_vax_1_date=patients.with_tpp_vaccination_record(
        target_disease_matches = "SARS-2 CORONAVIRUS",
        on_or_after = "2020-12-08",  
        find_first_match_in_period = True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {
                "earliest": "2020-12-08",  # first vaccine administered on the 8/12
                "latest": "2022-12-01",
            }
        },
    ),
    # SECOND DOSE COVID VACCINATION - any type
    covid_vax_2_date=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after = "covid_vax_1_date + 1 days",
        find_first_match_in_period = True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {
                "earliest": "2020-12-08", 
                "latest": "2022-12-01",
            }
        },
    ),
    # THIRD DOSE COVID VACCINATION - any type
    covid_vax_3_date=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="covid_vax_2_date + 1 day",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {
                "earliest": "2020-12-08",  
                "latest": "2022-12-01",
            }
        },
    ),
    # 4th DOSE COVID VACCINATION - any type
    covid_vax_4_date = patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="covid_vax_3_date + 1 day",
        find_first_match_in_period=True,
        returning = "date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {
                "earliest": "2020-12-08",  
                "latest": "2022-12-01",
            }
        },
    ),
)

