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
# This study definition defines the baseline characteristics (e.g. demographics, vax dates).
# The baseline date is Sep 3, 2022 (6 weeks prior to start of vaccination campaign)
#
#################################################################################


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
        "date": {"earliest": "2020-12-08", "latest": "2023-02-01"},
        "rate": "uniform",
        "incidence": 0.5,
    },
        
    # Set index date
    index_date = "2022-09-03",

    # This line defines the study population
    population = patients.satisfying(
        """
        registered
        AND
        (age >= 44 AND age < 55)
        AND
        (sex = "M" OR sex = "F")
        AND
        NOT has_died
        AND
        has_follow_up
        """,
        registered = patients.registered_as_of(
            "index_date",
        ),
        has_died = patients.died_from_any_cause(
            on_or_before = "index_date",
            returning = "binary_flag",
        ),
        has_follow_up=patients.registered_with_one_practice_between(
            "index_date - 90 days", "index_date"
        ),
    ),

    # Age (continuous)
    age = patients.age_as_of(
        "index_date", 
        return_expectations = {
            "rate": "universal",
            "int": {"distribution": "population_ages"},
            "incidence" : 0.001
        },
    ),
    
    # Date of birth month/year
    dob = patients.date_of_birth(
        "YYYY-MM",
        return_expectations={
            "date": {"earliest": "1962-01-01", "latest": "1982-01-01"},
            "rate": "uniform",
            "incidence" : .999
        },
    ),

    # Sex
    sex = patients.sex(
         return_expectations={
             "rate": "universal",
             "category": {"ratios": {"M": 0.49, "F": 0.51}},
         }
    ),

    # Date of death
    dod = patients.died_from_any_cause(
        returning="date_of_death",
        date_format="YYYY-MM-DD",
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

    ### Ethnicity (6 categories)
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
    # Risk groups prioritised for earlier vaccination
    ###############################################################################

    # Healthcare worker at time of vaccination
    hscworker = patients.with_healthcare_worker_flag_on_covid_vaccine_record(returning="binary_flag"),

    ## From PRIMIS ##
    # Asthma
    asthma = patients.satisfying(
        """
        astadm OR
        (ast AND astrxm1 AND astrxm2 AND astrxm3)
        """,
        # Asthma Admission codes
        astadm=patients.with_these_clinical_events(
            astadm,
            returning="binary_flag",
            on_or_before="index_date - 1 day",
            return_expectations = {"incidence": 0.01},
            ),
        # Asthma Diagnosis code
        ast = patients.with_these_clinical_events(
            ast,
            returning="binary_flag",
            on_or_before="index_date - 1 day",
            return_expectations = {"incidence": 0.01},
            ),
        # Asthma systemic steroid prescription code in month 1
        astrxm1=patients.with_these_medications(
            astrx,
            returning="binary_flag",
            between=["index_date - 30 days", "index_date - 1 day"],
            return_expectations = {"incidence": 0.01},
            ),
        # Asthma systemic steroid prescription code in month 2
        astrxm2=patients.with_these_medications(
            astrx,
            returning="binary_flag",
            between=["index_date - 60 days", "index_date - 31 days"],
            return_expectations = {"incidence": 0.01},
            ),
        # Asthma systemic steroid prescription code in month 3
        astrxm3=patients.with_these_medications(
            astrx,
            returning="binary_flag",
            between=["index_date - 90 days", "index_date - 61 days"],
            return_expectations = {"incidence": 0.01},
        ),
    return_expectations = {"incidence": 0.01},
    ),

    # Chronic Respiratory Disease
    chronic_resp_disease = patients.satisfying(
        "asthma OR resp_cov",
        resp_cov=patients.with_these_clinical_events(
            resp_cov,
            returning="binary_flag",
            on_or_before="index_date - 1 day",
            return_expectations = {"incidence": 0.01},
            ),
    ),

    # Chronic Neurological Disease including Learning Disorder
    chronic_neuro_disease=patients.satisfying(
        """cns_cov OR learndis """,
        # Chronic neurological disease
        cns_cov=patients.with_these_clinical_events(
            cns_cov,
            returning="binary_flag",
            on_or_before="index_date - 1 day",
        ),
        # Wider Learning Disability
        learndis=patients.with_these_clinical_events(
            learndis,
            returning="binary_flag",
            on_or_before="index_date - 1 day",
        ),
    return_expectations = {"incidence": 0.01},
    ),

    # Obesity
    sev_obesity = patients.satisfying(
        """
        sev_obesity_date > bmi_date OR
        bmi_value1 >= 40
        """,
        bmi_stage_date=patients.with_these_clinical_events(
            bmi_stage,
            returning="date",
            find_last_match_in_period=True,
            on_or_before="index_date - 1 day",
            date_format="YYYY-MM-DD",
            return_expectations = {"incidence": 0.01},
        ),
        sev_obesity_date=patients.with_these_clinical_events(
            sev_obesity,
            returning="date",
            find_last_match_in_period=True,
            ignore_missing_values=True,
            between= ["bmi_stage_date", "index_date - 1 day"],
            date_format="YYYY-MM-DD",
            return_expectations = {"incidence": 0.01},
        ),
        bmi_date=patients.with_these_clinical_events(
            bmi,
            returning="date",
            ignore_missing_values=True,
            find_last_match_in_period=True,
            on_or_before="index_date - 1 day",
            date_format="YYYY-MM-DD",
            return_expectations = {"incidence": 0.01},
        ),
        bmi_value1=patients.with_these_clinical_events(
            bmi,
            returning="numeric_value",
            ignore_missing_values=True,
            find_last_match_in_period=True,
            on_or_before="index_date - 1 day",
            return_expectations = {"incidence": 0.01},
        ),
        return_expectations = {"incidence": 0.01},
    ),

    # Diabetes
    diabetes = patients.satisfying(
        """
        (dmres_date < diab_date) OR 
        (diab_date AND (NOT dmres_date))
        """,
        diab_date=patients.with_these_clinical_events(
            diab,
            returning="date",
            find_last_match_in_period=True,
            on_or_before="index_date - 1 day",
            date_format="YYYY-MM-DD",
            return_expectations = {"incidence": 0.01},
        ),
        # Diabetes resolved
        dmres_date=patients.with_these_clinical_events(
            dmres,
            returning="date",
            find_last_match_in_period=True,
            on_or_before="index_date - 1 day",
            date_format="YYYY-MM-DD",
            return_expectations = {"incidence": 0.01},
        ),
        return_expectations = {"incidence": 0.01},
    ),

    # Severe mental illness
    sev_mental=patients.satisfying(
        """
        (smhres_date < sev_mental_date) OR 
        (sev_mental_date AND (NOT smhres_date))
        """,
        # Severe Mental Illness codes
        sev_mental_date=patients.with_these_clinical_events(
            sev_mental,
            returning="date",
            find_last_match_in_period=True,
            on_or_before="index_date - 1 day",
            date_format="YYYY-MM-DD",
            return_expectations = {"incidence": 0.01},
        ),
        # Remission codes relating to Severe Mental Illness
        smhres_date=patients.with_these_clinical_events(
            smhres,
            returning="date",
            find_last_match_in_period=True,
            on_or_before="index_date - 1 day",
            date_format="YYYY-MM-DD",
            return_expectations = {"incidence": 0.01},
        ),
        return_expectations = {"incidence": 0.01},
    ),

    # Chronic heart disease codes
    chronic_heart_disease=patients.with_these_clinical_events(
        chd_cov,
        returning="binary_flag",
        on_or_before="index_date - 1 day",
        return_expectations = {"incidence": 0.01},
        ),

    # Chronic kidney disease
    chronic_kidney_disease=patients.satisfying(
        """
        ckd OR
        (ckd15_date AND ckd35_date >= ckd15_date)
        """,
        # Chronic kidney disease codes - all stages
        ckd15_date=patients.with_these_clinical_events(
            ckd15,
            returning="date",
            find_last_match_in_period=True,
            on_or_before="index_date - 1 day",
            date_format="YYYY-MM-DD",
            return_expectations = {"incidence": 0.01},
        ),
        # Chronic kidney disease codes-stages 3 - 5
        ckd35_date=patients.with_these_clinical_events(
            ckd35,
            returning="date",
            find_last_match_in_period=True,
            on_or_before="index_date - 1 day",
            date_format="YYYY-MM-DD",
            return_expectations = {"incidence": 0.01},
        ),
        # Chronic kidney disease diagnostic codes
        ckd=patients.with_these_clinical_events(
            ckd_cov,
            returning="binary_flag",
            on_or_before="index_date - 1 day",
            return_expectations = {"incidence": 0.01},
            ),
        return_expectations = {"incidence": 0.01},
    ),

    # Chronic Liver disease codes
    chronic_liver_disease=patients.with_these_clinical_events(
        cld,
        returning="binary_flag",
        on_or_before="index_date - 1 day",
        return_expectations = {"incidence": 0.01},
    ),

    # Immunosupressive conditions / medications #
    immunosuppressed=patients.satisfying(
        """
        immrx OR 
        immdx OR 
        hiv_aids OR 
        solid_organ_transplant OR
        cancer_haem OR
        cancer_nonhaem
        """,
        # Immunosuppression diagnosis codes
        immdx=patients.with_these_clinical_events(
            immdx_cov,
            returning="binary_flag",
            on_or_before="index_date - 1 day",
        ),
        # Immunosuppression medication codes
        immrx=patients.with_these_medications(
            immrx,
            returning="binary_flag",
            between=["index_date - 182 days", "index_date - 1 day"],
        ),       
        # HIV / AIDS
        hiv_aids = patients.with_these_clinical_events( 
            hiv_aids,
            on_or_before="index_date - 1 day",
            returning="binary_flag",
        ), 
        # Solid organ transplant
        solid_organ_transplant = patients.with_these_clinical_events( 
            solid_organ_transplant,
            on_or_before="index_date - 1 day",
            returning="binary_flag",
        ), 
        # Haematological cancer
        cancer_haem = patients.with_these_clinical_events(
            cancer_haem_snomed,
            returning="binary_flag",
            between=["index_date - 3 years", "index_date - 1 day"],
        ),
        # Solid cancer
        cancer_nonhaem = patients.with_these_clinical_events( 
            cancer_nonhaem_snomed,
            between=["index_date - 3 years", "index_date - 1 day"],
            returning="binary_flag",
        ), 
    return_expectations = {"incidence": 0.01},
    ),
  
    # Asplenia or Dysfunction of the Spleen 
    asplenia=patients.with_these_clinical_events(
        spln_cov,
        returning="binary_flag",
        on_or_before="index_date - 1 day",
        return_expectations = {"incidence": 0.01},
    ),
  
    # End of life
    endoflife = patients.satisfying(
        """
        midazolam OR
        endoflife_coding
        """,
        midazolam = patients.with_these_medications(
            midazolam,
            returning="binary_flag",
            on_or_before = "index_date - 1 day",
        ),
        endoflife_coding = patients.with_these_clinical_events(
            eol,
            returning="binary_flag",
            on_or_before = "index_date - 1 day",
            find_last_match_in_period = True,
        ),
    ),
    
    # Housebound
    housebound = patients.satisfying(
        """housebound_date
        AND NOT no_longer_housebound
        AND NOT moved_into_care_home
        """,
        housebound_date=patients.with_these_clinical_events( 
            housebound, 
            on_or_before="index_date - 1 day",
            find_last_match_in_period = True,
            returning="date",
            date_format="YYYY-MM-DD",
            return_expectations = {"incidence": 0.01},
        ), 
        no_longer_housebound=patients.with_these_clinical_events( 
            no_longer_housebound, 
            between=["housebound_date", "index_date - 1 day"],
            return_expectations = {"incidence": 0.01},
            ),
        moved_into_care_home=patients.with_these_clinical_events(
            carehome,
            between=["housebound_date", "index_date - 1 day"],
            return_expectations = {"incidence": 0.01},
        ),
        return_expectations = {"incidence": 0.01},
    ),

    # Care home
    carehome = patients.satisfying(
        """
        carehome_codes
        OR
        tpp_care_home_type
        """,
        # Care home from codelists
        carehome_codes = patients.with_these_clinical_events(
            carehome,
            on_or_before = "index_date",
            returning = "binary_flag",
            return_expectations = {"incidence": 0.01},
            ),
        ## Care home from TPP address list (as binary variable)
        tpp_care_home_type=patients.care_home_status_as_of(
            "index_date",
            categorised_as={
            1: """
            IsPotentialCareHome
            """,
            0: "DEFAULT",
            },
        ),
        return_expectations = {"incidence": 0.01},
    ),

    ###############################################################################
    # COVID VACCINATION (ANY TYPE)
    ###############################################################################

    # First dose COVID vaccination
    covid_vax_1_date=patients.with_tpp_vaccination_record(
        target_disease_matches = "SARS-2 CORONAVIRUS",
        on_or_after = "2020-12-08",  
        find_first_match_in_period = True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {
                "earliest": "2020-12-08",  # first vaccine administered on the 8/12
                "latest": "2023-02-01",
            }
        },
    ),

    # Second dose COVID vaccination
    covid_vax_2_date=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after = "covid_vax_1_date + 1 days",
        find_first_match_in_period = True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {
                "earliest": "2020-12-08", 
                "latest": "2023-02-01",
            }
        },
    ),

    # Third dose (first booster) COVID vaccination
    covid_vax_3_date=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="covid_vax_2_date + 1 day",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {
                "earliest": "2021-11-01",  
                "latest": "2023-02-01",
            }
        },
    ),

    # Fourth dose (second booster) COVID vaccination
    covid_vax_4_date = patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after="covid_vax_3_date + 1 day",
        find_first_match_in_period=True,
        returning = "date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {
                "earliest": "2022-07-01",  
                "latest": "2023-02-01",
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
                "latest": "2023-02-01",
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
                "latest": "2023-02-01",
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
                "latest": "2023-02-01",
            },
        },
    ),
)