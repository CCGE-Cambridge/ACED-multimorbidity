# ACED-multimorbidity 
Dr Hannah Harrison

This repository provides the codelists and analysis code needed to reproduce the analysis for the paper "Implementation and External Validation of the Cambridge Multimorbidity Score in the UK Biobank cohort" (published in BMC Medical Research Methodology). 

Full analysis code can be run from main.R
Paths and settings can be modified in 0_config.R
Required libraies are also given in 0_config.R

Scripts are named by analysis stage

0 - configuration,
1 - select analysis cohort,
2 - identify outcomes,
3 - implement mm score (multiple versions),
4 - results: characteristics of cohort,
5 - results: calculate and plot discrimination,
6 - results: caluclate and plot calibration,
7 - results: KM plots,

Auxillary scripts "save_cohorts.R" and "load_cohorts.R" can be used to save and load intermediate dataframes during analysis.

The directory dependencies includes all the in-house functions called during the analysis. They are grouped into files by type and placement within the analysis (e.g. baseline_var_functions.R is a suit of functions for manipulating UKB baseline data). This directory also includes some set-up information for the mm score implementation and a theme for plotting with ggplot2. 

Codelists for the 37 conditions are given in the codelists directory in the files "cam_mm_clinical.csv" and "cam_mm_scripts" (codes for GP clinical records and prescriptions respectively). This directory also gives a list of variables for working with UKB baseline data, ICD codes for common cancer types, and the coefficients and data dictionary for the mm score. 

The code was developed using R version 4.1.0