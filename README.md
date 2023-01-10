# COVID-19 second booster (fourth dose) regression discontinuity analysis (feasibility)

[View on OpenSAFELY](https://jobs.opensafely.org/repo/https%253A%252F%252Fgithub.com%252Fopensafely%252Fvax-fourth-dose-RD)

Details of the purpose and any published outputs from this project can be found at the link above.

This study is determining the feasibility of estimating the effectiveness of the second booster (fourth dose) COVID-19 vaccine using a regression discontinuity design. 

The contents of this repository MUST NOT be considered an accurate or valid representation of the study or its purpose. 
This repository may reflect an incomplete or incorrect analysis with no further ongoing work.
The content has ONLY been made public to support the OpenSAFELY [open science and transparency principles](https://www.opensafely.org/about/#contributing-to-best-practice-around-open-science) and to support the sharing of re-usable code for other subsequent users.
No clinical, policy or safety conclusions must be drawn from the contents of this repository.


## Repository navigation

-   If you are interested in how we defined our codelists, look in the [`codelists/`](./codelists/) directory.

-   Analysis scripts are in the [`analysis/`](./analysis) directory.

    -   The instructions used to extract data from the OpensAFELY-TPP database is specified in the [study definition](./analysis/study_definition.py); this is written in Python, but non-programmers should be able to understand what is going on there
    -   The [`lib/`](./lib) directory contains preliminary (pre data extract) scripts, useful functions, and dummy data.
    -   The remaining folders mostly contain the R scripts that process, describe, and analyse the extracted database data.

-   Non-disclosive model outputs, including tables, figures, etc, are in the [`released_outputs/`](./released_outputs) directory.

-   The [`project.yaml`](./project.yaml) defines run-order and dependencies for all the analysis scripts. **This file should *not* be edited directly**. To make changes to the yaml, edit and run the [`create-project.R`](./create-project.R) script instead.

-   You can run this project via [Gitpod](https://gitpod.io) in a web browser by clicking on this badge: [![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-908a85?logo=gitpod)](https://gitpod.io/#https://github.com/opensafely/comparative-booster)


## Licences

As standard, research projects have a MIT license. 


# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).
