##############################################################################
#
# This script provides the formal specification of the study data
# that will be extracted from
# the OpenSAFELY database.
#
# STUDY PURPOSE: to understand cumulative uptake 
#   of fourth dose (second booster) COVID-19 vaccine
#   by age, before and after 50+ became eligible
#   on October 15, 2022
#
# This study definition extracts information on outcomes. There are multiple
# index dates (starting on 3 Sep through latest available) to capture all 
# outcomes over the stud period.
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
  
    ## Hospitalisations ##
    
    # Unplanned hospital admission (all cause)
    admitted_unplanned_date_1=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_patient_classification = ["1"], # ordinary admissions only
        between=["index_date","index_date + 6 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),
    admitted_unplanned_date_2=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_patient_classification = ["1"], # ordinary admissions only
        between=["admitted_unplanned_date_1 + 1","index_date + 6 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),
    admitted_unplanned_date_3=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_patient_classification = ["1"], # ordinary admissions only
        between=["admitted_unplanned_date_2 + 1","index_date + 6 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),
<<<<<<< Updated upstream
=======
    admitted_unplanned_date_4=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_patient_classification = ["1"], # ordinary admissions only
        between=["admitted_unplanned_date_3 + 1","index_date + 6 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),
    admitted_unplanned_date_5=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_patient_classification = ["1"], # ordinary admissions only
        between=["admitted_unplanned_date_4 + 1","index_date + 6 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),
>>>>>>> Stashed changes
    # Check max number of admissions per week
    admitted_unplanned_num=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_patient_classification = ["1"], # ordinary admissions only
        between=["index_date","index_date + 6 days"],
        return_expectations={"int" : {"distribution": "normal", "mean": 5, "stddev": 5}, "incidence" : 0.5},
    ),
  
    # COVID unplanned admission
    covidadmitted_date_1=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_diagnoses=covid_codes,
        between=["index_date","index_date + 6 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),   
    covidadmitted_date_2=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_diagnoses=covid_codes,
        between=["covidadmitted_date_1 + 1","index_date + 6 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),   
    covidadmitted_date_3=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_diagnoses=covid_codes,
        between=["covidadmitted_date_2 + 1","index_date + 6 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
<<<<<<< Updated upstream
    ),    
=======
    ),  
    # Check max number of admissions per week
    covidadmitted_num=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",      
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_diagnoses=covid_codes,
        between=["index_date + 1","index_date + 6 days"],
        return_expectations={"int" : {"distribution": "normal", "mean": 5, "stddev": 5}, "incidence" : 0.5},
    ),   
>>>>>>> Stashed changes

    # Respiratory unplanned admission (primary diagnosis only)
    respadmitted_date_1=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_primary_diagnoses=resp_codes,
        between=["index_date + 1","index_date + 6 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),
    respadmitted_date_2=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_primary_diagnoses=resp_codes,
        between=["respadmitted_date_1 + 1","index_date + 6 days"],
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),
    respadmitted_date_3=patients.admitted_to_hospital(
        returning="date_admitted",
        with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
        with_these_primary_diagnoses=resp_codes,
        between=["respadmitted_date_2 + 1","index_date + 6 days"],
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
    
    # Respiratory death (underlying cause only)
    respdeath_date=patients.with_these_codes_on_death_certificate(
        resp_codes,
        match_only_underlying_cause=True,
        returning="date_of_death",
        date_format="YYYY-MM-DD",
    ),

    ## A&E attendances ##

    # COVID emergency attendance 
    covidemergency_date_1=patients.attended_emergency_care(
        returning="date_arrived",
        between=["index_date","index_date + 6 days"],
        with_these_diagnoses = covid_emergency,
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),   
    covidemergency_date_2=patients.attended_emergency_care(
        returning="date_arrived",
        between=["covidemergency_date_1 + 1","index_date + 6 days"],
        with_these_diagnoses = covid_emergency,
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),    
    covidemergency_date_3=patients.attended_emergency_care(
        returning="date_arrived",
        between=["covidemergency_date_2 + 1","index_date + 6 days"],
        with_these_diagnoses = covid_emergency,
        date_format="YYYY-MM-DD",
        find_first_match_in_period=True,
    ),    
)