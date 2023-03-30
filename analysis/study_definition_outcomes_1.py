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
# This study definition extracts information on outcomes for the control
# periods - i.e. 42 days after Sep 03 (pre-campaign), 
# and 42 days after Oct 15 (start of campaign). Only one outcome (first)
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

    ############################################################
    ## OUTCOMES
    ############################################################
  
    ## Deaths ##

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
    
    # Respiratory death (underlying cause only)
    respdeath_date=patients.with_these_codes_on_death_certificate(
        resp_codes,
        match_only_underlying_cause=True,
        returning="date_of_death",
        date_format="YYYY-MM-DD",
    ),

    ## Hospitalisations ##
    
    # Unplanned hospital admission (all cause)
    admitted_unplanned_date=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_patient_classification = ["1"], # ordinary admissions only
        between=["index_date","index_date + 42 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),
    # COVID unplanned admission
    covidadmitted_date=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_diagnoses=covid_codes,
        between=["index_date","index_date + 42 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),   
    # Respiratory unplanned admission (primary diagnosis only)
    respadmitted_date=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_primary_diagnoses=resp_codes,
        between=["index_date","index_date + 42 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),

    # COVID emergency attendance 
    covidemergency_date=patients.attended_emergency_care(
        returning="date_arrived",
        between=["index_date","index_date + 42 days"],
        with_these_diagnoses = covid_emergency,
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),   
)