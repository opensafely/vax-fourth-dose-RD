
## Import code building blocks from cohort extractor package
from cohortextractor import (
    codelist,
    codelist_from_csv,
)

covid_codes = codelist(
    ["U071", "U072", "U099", "U109"], 
    system="icd10",
)

covid_emergency = codelist(
    ["1240751000000100","1325171000000109","1325181000000106","1325161000000102"],
    system="snomed",
)

ethnicity_codes_6 = codelist_from_csv(
    "codelists/opensafely-ethnicity-snomed-0removed.csv",
    system="snomed",
    column="snomedcode",
    category_column="Grouping_6",
)

flu_med_codes = codelist_from_csv(
    "codelists/opensafely-influenza-vaccination.csv",  
    system="snomed",  
    column="snomed_id",
)

flu_clinical_given_codes = codelist_from_csv(
    "codelists/opensafely-influenza-vaccination-clinical-codes-given.csv",  
    system="ctv3", 
    column="CTV3ID",
)

flu_clinical_not_given_codes = codelist_from_csv(
    "codelists/opensafely-influenza-vaccination-clinical-codes-not-given.csv",  
    system="ctv3", 
    column="CTV3ID",
)
