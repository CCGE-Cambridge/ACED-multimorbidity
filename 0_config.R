##File name: config.R
##Author: Hannah Harrison
##Last Edit: 07/10/2022
##Description:set-up fro mm_score_project, install libraries, sets paths and loads functions, sets global variables

##LIBRARIES
library(MASS) #for boxcox function
library(ggpubr) # for qq plots
library(tidyverse) 
library(readxl) 
library(dplyr)
library(data.table)
library(lubridate)
library(remotes)
library(ggplot2)
library(RColorBrewer) #access to a range of colourpalettes
library(viridis) #access to viridis within ggplot2
library(rms) #survival analysis
library(survAUC) # discrimination - survival/cox models
library(ggsurvfit)

##PATHS TO UKB DATA
fpath_baseline <- "~/ukb_baseline.csv" 
fpath_gp_reg <- "~/health_records/gp_registrations.txt"
fpath_gp <- "~/health_records/gp_clinical.txt"
fpath_gp_med <- "~/health_records/gp_scripts.txt"


##PATHS TO LOCAL DATA/CODELISTS
fpath_baseline_vars <- "~/ACED-multimorbidity/codelists/baseline_vars_inc_HES_list.csv"
fpath_ICD10_codes <- "~/ACED-multimorbidity/codelists/ICD10_cancer.csv"
fpath_ICD9_codes <- "~/ACED-multimorbidity/codelists/ICD9_cancer.csv"
fpath_save_data <- "~/private_data/mm_score"
my_conditions <- "~/ACED-multimorbidity/codelists/cam_mm_score.csv"
cam_multi_morb_file <- "~/ACED-multimorbidity/codelists/cam_mm_score_coeffs.csv"
mm_score_clinical <- "~/ACED-multimorbidity/codelists/cam_mm_clinical.csv" #codes for clincial records
mm_score_scripts <- "~/ACED-multimorbidity/codelists/cam_mm_scripts.csv" #codes for prescriptions

##LOAD FUNCTION FILES
source("~/ACED-multimorbidity/dependencies/graph_theme.r") #defines custon theme for ggplot2
source("~/ACED-multimorbidity/dependencies/gen_data_functions.r") #contains date of birth function
source("~/ACED-multimorbidity/dependencies/wide_baseline_functions.R") 
source("~/ACED-multimorbidity/dependencies/gp_data_functions.r") ##extracting things from gp clinical records
source("~/ACED-multimorbidity/dependencies/multimorb_score_info_and_functions.r") ##mm_score specific code
source("~/ACED-multimorbidity/dependencies/baseline_vars_functions.r") ##functions for cleaning and processing simple baseline data
source("~/ACED-multimorbidity/dependencies/biomarker_cleaning_functions.r") #defines functions that clean biomarker variables


##CONSTANT VARIABLES
study_end_date <- as.Date("2016-07-01") #last date with valid primary care data for data provider 3 (English TPP)
gp_study_end <-  as.Date("2016-07-01") #last date with valid primary care data for data provider 3 (English TPP)
cancer_study_end  <- as.Date("2016-07-01") #used the same censor data for everything for simplicity (in practise we have cancer data until 2022)
death_study_end  <- as.Date("2016-07-01") #used the same censor data for everything for simplicity (in practise we have death registry linkage up to 2022)
