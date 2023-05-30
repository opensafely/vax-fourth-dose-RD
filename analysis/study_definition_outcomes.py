##############################################################################
#
# This script provides the formal specification of the study data
# that will be extracted from
# the OpenSAFELY database.
#
# STUDY PURPOSE: to perform regression discontinuity of 2022/23 
#   autumn booster COVID-19 vaccine, before and after 50+ became eligible
#   on October 15, 2022
#
# This study definition extracts information on outcomes from 42 days
# of each index date (Sep 03 (pre-campaign),  Oct 15 (start of campaign), 
# and Nov 26 onwards. Only one outcome 
# per person is extracted.
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

COHORT = "output/cohort/cohort_final_sep.csv"

# Specify study definition
study = StudyDefinition(

    # Configure the expectations framework
    default_expectations = {
        "date": {"earliest": "2020-12-08", "latest": "2023-02-01"},
        "rate": "uniform",
        "incidence": 0.5,
    },
        
    # Set index date
    index_date = "2022-09-03",

    population=patients.which_exist_in_file(COHORT),

    # Date of birth month/year
    dob=patients.with_value_from_file(
        COHORT,
        returning="dob",
        returning_type="date",
    ),

    # Date of death
    dod=patients.with_value_from_file(
        COHORT,
        returning="dod",
        returning_type="date",
    ),

    # Flu vax
    flu_vax_date=patients.with_value_from_file(
        COHORT,
        returning="flu_vax_date",
        returning_type="date",
    ),

    # Booster vax
    boost_date=patients.with_value_from_file(
        COHORT,
        returning="boost_date",
        returning_type="date",
    ),

    ############################################################
    ## OUTCOMES
    ############################################################
  
   # All-cause death
    anydeath=patients.died_from_any_cause(
        returning="binary_flag",
        between=["index_date","index_date + 41 days"],        
        return_expectations = {"incidence": 0.4},
    ),
    
    # COVID death
    coviddeath=patients.with_these_codes_on_death_certificate(
        covid_codes,
        returning="binary_flag",
        between=["index_date","index_date + 41 days"],        
        return_expectations = {"incidence": 0.4},
        ),
    
    # COVID unplanned admission
    covidadmitted=patients.admitted_to_hospital(
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_diagnoses=covid_codes,
        between=["index_date","index_date + 41 days"],        
        returning="binary_flag",
        return_expectations = {"incidence": 0.4},
        ),   
    
    # COVID emergency attendance 
    covidemergency=patients.attended_emergency_care(
        between=["index_date","index_date + 41 days"],        
        with_these_diagnoses = covid_emergency,
        returning="binary_flag",
        return_expectations = {"incidence": 0.4},
        ),

    # COVID composite
    covidcomposite=patients.categorised_as(
        {
            0: "DEFAULT",
            1: """
                coviddeath = 1 OR
                covidadmitted = 1 OR
                covidemergency = 1
                 """,
            },
            return_expectations = {"incidence": 0.4},
        ),

    # Respiratory death (underlying cause only)
    respdeath=patients.with_these_codes_on_death_certificate(
        resp_codes,
        match_only_underlying_cause=True,
        returning="binary_flag",
        between=["index_date","index_date + 41 days"],        
        return_expectations = {"incidence": 0.4},
        ),
    
    # Respiratory unplanned admission (primary diagnosis only)
    respadmitted=patients.admitted_to_hospital(
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_primary_diagnoses=resp_codes,
        between=["index_date","index_date + 41 days"],        
        returning="binary_flag",
        return_expectations = {"incidence": 0.4},
        ),

   # Respiratory composite
   respcomposite=patients.categorised_as(
       {
           0: "DEFAULT",
           1: """
            respdeath = 1 OR
            respadmitted = 1
            """
       },
    return_expectations = {"incidence": 0.4},
    ),
   
    # Unplanned hospital admission (all cause)
    anyadmitted=patients.admitted_to_hospital(
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_patient_classification = ["1"], # ordinary admissions only
        between=["index_date","index_date + 41 days"],        
        returning="binary_flag",
        return_expectations = {"incidence": 0.4},
    ),
)


