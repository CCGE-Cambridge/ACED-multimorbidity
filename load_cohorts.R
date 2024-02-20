##File name: save_cohorts.R
##Author: Hannah Harrison
##Last Edit: 06/10/2022
##Description:loads all four analysis cohorts

load(file.path(fpath_save_data, "EHR_cohort_main.Rdata"))#used for main analysis - has 1 year lb and at least 6months follow-up
load(file.path(fpath_save_data, "EHR_cohort_lb_only.Rdata"))#sensitivity analysis (mortality and cancer diagnosis only)
load(file.path(fpath_save_data, "EHR_cohort_1yr.Rdata"))#sensitivity analysis (all inc. consulation rates)
load(file.path(fpath_save_data, "EHR_cohort_5yr.Rdata"))#sensitivity analysis (all inc. consulation rates)