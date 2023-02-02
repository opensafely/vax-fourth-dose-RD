##############################################################################
#
# This script provides the formal specification of the study data
# that will be extracted from
# the OpenSAFELY database.
#
# STUDY PURPOSE: to understand cumulative uptake 
#   of fourth dose (second booster) COVID-19 vaccine
#   by age, before and after 50+ became eligible
#   on October 14, 2022
#
##############################################################################


# IMPORT STATEMENTS ----

# Import code building blocks from cohort extractor package
from cohortextractor import (
    StudyDefinition,
    patients,
    Measure,
    codelist,
)

# Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

# Specifiy study defeinition
study = StudyDefinition(

    # Configure the expectations framework
    default_expectations = {
        "date": {"earliest": "2020-12-08", "latest": "2023-01-01"},
        "rate": "uniform",
        "incidence": 0.5,
    },
        
    # Set index date
    index_date = "2022-10-01",

    # This line defines the study population
    population = patients.satisfying(
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

    # Age (continuous)
    age = patients.age_as_of(
        "2022-10-01", # People 50+ years became vaccine eligible on October 14
        return_expectations = {
            "rate": "universal",
            "int": {"distribution": "population_ages"},
            "incidence" : 0.001
        },
    ),

    # Age categories
    age_cat = patients.categorised_as(
        {"Missing": "DEFAULT",
            "40-44 y": """age >= 40 AND age < 45""",
            "45-49 y": """age >= 45 AND age < 50""",
            "50-54 y": """age >= 50 AND age < 55""",
            "55-59 y": """age >= 55 AND age < 60""",
        },
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "Missing": 0.01,
                    "40-44 y": 0.24,
                    "45-49 y": 0.25,
                    "50-54 y": 0.25,
                    "55-59 y": 0.25,
                }
            },
        },
    ),
    
    # Sex
    sex = patients.sex(
         return_expectations={
             "rate": "universal",
             "category": {"ratios": {"M": 0.49, "F": 0.51}},
         }
    ),

    ###########################################################
    # Demographics - for confirming that population is 
    #   consistent over time (before/after discontinuity)
    ###########################################################
       
    # IMD - quintile
    imd = patients.categorised_as(
        {
            "0": "DEFAULT",
            "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
            "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
            "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
            "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
            "5": """index_of_multiple_deprivation >= 32844*4/5 AND index_of_multiple_deprivation < 32844""",
        },
        index_of_multiple_deprivation=patients.address_as_of(
            "index_date",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0.01,
                    "1": 0.20,
                    "2": 0.20,
                    "3": 0.20,
                    "4": 0.20,
                    "5": 0.19,
                }
            },
        },
    ),

    ### Region
    region = patients.registered_practice_as_of(
        "index_date",
        returning = "nuts1_region_name",
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.1,
                    "Yorkshire and The Humber": 0.1,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East": 0.1,
                    "London": 0.2,
                    "South East": 0.1,
                    "South West": 0.1,
                },
            },
         },
    ),

    ethnicity = patients.categorised_as(
        {
            "Unknown": "DEFAULT",
            "White": "eth6='1'",
            "Mixed": "eth6='2'",
            "Asian or Asian British": "eth6='3'",
            "Black or Black British": "eth6='4'",
            "Other": "eth6='5'",
        },
        eth6 = patients.with_these_clinical_events(
            ethnicity_codes_6,
            returning = "category",
            find_last_match_in_period = True,
            include_date_of_match = False,
            return_expectations = {
                "incidence": 0.75,
                "category": {
                    "ratios": {
                        "1": 0.30,
                        "2": 0.20,
                        "3": 0.20,
                        "4": 0.20,
                        "5": 0.05,
                        "6": 0.05,
                    },
                },
            },
        ),
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "White": 0.30,
                    "Mixed": 0.20,
                    "Asian or Asian British": 0.20,
                    "Black or Black British": 0.20,
                    "Other": 0.05,
                    "Unknown": 0.05,
                },
            },
        },
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
                "latest": "2023-01-01",
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
                "latest": "2023-01-01",
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
                "latest": "2023-01-01",
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
                "latest": "2023-01-01",
            }
        },
    ),

    ###############################################################################
    # FLU VACCINATION in 2022-23
    ###############################################################################

    flu_vax_tpp_date=patients.with_tpp_vaccination_record(
        target_disease_matches="INFLUENZA",
        on_or_after="2022-07-01",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={           
            "date": {
                "earliest": "2022-07-01",  
                "latest": "2023-01-01",
            },
        },
    ),
        
    flu_vax_med_date=patients.with_these_medications(
        flu_med_codes,
        on_or_after="2022-07-01",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={           
            "date": {
                "earliest": "2022-07-01",  
                "latest": "2023-01-01",
            },
        },
    ),

    flu_vax_clinical_date=patients.with_these_clinical_events(
        flu_clinical_given_codes,
        ignore_days_where_these_codes_occur=flu_clinical_not_given_codes,
        on_or_after="2022-07-01",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={           
            "date": {
                "earliest": "2022-07-01",  
                "latest": "2023-01-01",
            },
        },
    ),

  ############################################################
  ## OUTCOMES
  ############################################################
  
    # Unplanned hospital admission (all cause)
    admitted_unplanned_date=patients.admitted_to_hospital(
        returning="date_admitted",
        on_or_after="2022-10-01",
        #   see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
        #   see https://docs.opensafely.org/study-def-variables/#sus for more info
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_patient_classification = ["1"], # ordinary admissions only
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),
  
    # COVID unplanned admission
    covidadmitted_date=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_diagnoses=covid_codes,
        on_or_after="2022-10-01",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),

    # COVID-related death
    coviddeath_date=patients.with_these_codes_on_death_certificate(
        covid_codes,
        returning="date_of_death",
        date_format="YYYY-MM-DD",
    ),
  
    # All-cause death
    any_death_date=patients.died_from_any_cause(
        returning="date_of_death",
        date_format="YYYY-MM-DD",
    ),

      # emergency attendance for covid
    covidemergency_date=patients.attended_emergency_care(
        returning="date_arrived",
        on_or_after="2022-10-01",
        with_these_diagnoses = covid_emergency,
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),

      # any emergency attendance
    emergency_date=patients.attended_emergency_care(
        returning="date_arrived",
        on_or_after="2022-10-01",
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),

  )