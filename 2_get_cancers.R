##File name: get_cancers.r
##Author: Hannah Harrison
##Last Edit: 18/08/2022
##Description:identifies cancer diagnoses in follow-up periods (1 yr and 5 yr) relative to baseline assessment

all_baseline_fields <- fread(fpath_baseline_vars)
cancer_fields <- all_baseline_fields %>% filter(type == "cancer")
ukb_fields <- all_baseline_fields %>% filter(type == "ukb")

##section A: Load and format cancer info
df_baseline_cancer <- fread(fpath_baseline, select = c("eid", cancer_fields$field, ukb_fields$field)) #force to read ICD9 codes as strings?
df_baseline_cancer[sapply(df_baseline_cancer, `%in%`, c("", "NA"))] <- NA_character_
colnames(df_baseline_cancer) <- gsub(x = names(df_baseline_cancer), pattern = "-|\\.", replacement = "_") 
colnames(df_baseline_cancer) <- gsub(x = names(df_baseline_cancer), pattern = "^([0-9])", replacement = "v\\1") 
colnames(df_baseline_cancer) <- gsub(x = names(df_baseline_cancer), pattern = "_0$", replacement = "") 
df_baseline_cancer <- df_baseline_cancer %>% rename(baseline_date = v53_0) 
df_baseline_cancer$baseline_date <- as.Date(df_baseline_cancer$baseline_date)
df_baseline_cancer <- df_baseline_cancer %>% mutate(cancer_follow_up_length = interval(baseline_date, cancer_study_end) / years(1))#follow-up time as numeric value in years


patt_var_cancer_ICD10 <- "v40006_" #16
patt_var_cancer_ICD9 <- "v40013_" #14
patt_var_cancer_date <- "v40005_" #16

##Section B: get codes for NM skin cancer (so we can exclude)
ICD10_cancer_codes <- fread(fpath_ICD10_codes) %>% select(!Oral)
ICD10_cancer_codes[sapply(ICD10_cancer_codes, `%in%`, c("", "NA"))] <- NA_character_
ICD9_cancer_codes <- fread(fpath_ICD9_codes)
ICD9_cancer_codes[sapply(ICD9_cancer_codes, `%in%`, c("", "NA"))] <- NA_character_

patt_ICD10_Skin_NM_codes <- (ICD10_cancer_codes %>% select(Skin_NM) %>% filter(!is.na(Skin_NM)) %>% summarise(Skin_NM = paste(Skin_NM, collapse = "|")))$Skin_NM[1]
patt_ICD9_Skin_NM_codes <- (ICD9_cancer_codes %>% select(Skin_NM) %>% filter(!is.na(Skin_NM)) %>%summarise(Skin_NM = paste(Skin_NM, collapse = "|")))$Skin_NM[1]

##Section C: identify all people with at least one cancer diagnosis (except nm skin) in follow-up periods
#1 year
df_baseline_cancer$min_date <- df_baseline_cancer$baseline_date
df_baseline_cancer <- UKB_wide_event_any_except_in_range(df_baseline_cancer, patt_var_cancer_ICD10, patt_var_cancer_date, 16, patt_ICD10_Skin_NM_codes, "ICD10", "min_date", 1)
df_baseline_cancer <- UKB_wide_event_any_except_in_range(df_baseline_cancer, patt_var_cancer_ICD9, patt_var_cancer_date, 14, patt_ICD9_Skin_NM_codes, "ICD9", "min_date", 1)
df_baseline_cancer <- df_baseline_cancer %>% mutate(any_cancer_except_1yr = case_when((ICD10_any_except_1yr == 1 | ICD9_any_except_1yr == 1) ~ 1, TRUE ~ 0))

df_baseline_cancer <- UKB_wide_event_any_except_in_range(df_baseline_cancer, patt_var_cancer_ICD10, patt_var_cancer_date, 16, patt_ICD10_Skin_NM_codes, "ICD10", "min_date", 5)
df_baseline_cancer <- UKB_wide_event_any_except_in_range(df_baseline_cancer, patt_var_cancer_ICD9, patt_var_cancer_date, 14, patt_ICD9_Skin_NM_codes, "ICD9", "min_date", 5)
df_baseline_cancer <- df_baseline_cancer %>% mutate(any_cancer_except_5yr = case_when((ICD10_any_except_5yr == 1 | ICD9_any_except_5yr == 1) ~ 1, TRUE ~ 0))

df_baseline_cancer <- UKB_wide_event_any_except_after(df_baseline_cancer, patt_var_cancer_ICD10, patt_var_cancer_date, 16, patt_ICD10_Skin_NM_codes, "ICD10", "min_date", cancer_study_end)
df_baseline_cancer <- UKB_wide_event_any_except_after(df_baseline_cancer, patt_var_cancer_ICD9, patt_var_cancer_date, 14, patt_ICD9_Skin_NM_codes, "ICD9", "min_date", cancer_study_end)
df_baseline_cancer <- df_baseline_cancer %>% mutate(any_cancer_except_after = case_when((ICD10_any_except_after == 1 | ICD9_any_except_after == 1) ~ 1, TRUE ~ 0))


##Section D: find the code and date of the first cancer diagnosis (except skin_nm) for whole cohort
df_baseline_cancer <- UKB_wide_event_fetch_first_except_in_range(df_baseline_cancer, patt_var_cancer_ICD10, patt_var_cancer_date, 16, patt_ICD10_Skin_NM_codes, "ICD10", "min_date", 1)
df_baseline_cancer <- UKB_wide_event_fetch_first_except_in_range(df_baseline_cancer, patt_var_cancer_ICD9, patt_var_cancer_date, 14, patt_ICD9_Skin_NM_codes, "ICD9", "min_date", 1)
df_baseline_cancer <- UKB_wide_event_fetch_first_except_in_range(df_baseline_cancer, patt_var_cancer_ICD10, patt_var_cancer_date, 16, patt_ICD10_Skin_NM_codes, "ICD10", "min_date", 5)
df_baseline_cancer <- UKB_wide_event_fetch_first_except_in_range(df_baseline_cancer, patt_var_cancer_ICD9, patt_var_cancer_date, 14, patt_ICD9_Skin_NM_codes, "ICD9", "min_date", 5)
df_baseline_cancer <- UKB_wide_event_fetch_first_except_after(df_baseline_cancer, patt_var_cancer_ICD10, patt_var_cancer_date, 16, patt_ICD10_Skin_NM_codes, "ICD10", "min_date", cancer_study_end)
df_baseline_cancer <- UKB_wide_event_fetch_first_except_after(df_baseline_cancer, patt_var_cancer_ICD9, patt_var_cancer_date, 14, patt_ICD9_Skin_NM_codes, "ICD9", "min_date", cancer_study_end)

df_baseline_cancer <- df_baseline_cancer %>% mutate(first_cancer_date_1yr = 
                                              case_when(ICD9_date_first_1yr < ICD10_date_first_1yr ~ ICD9_date_first_1yr,
                                              is.na(ICD10_code_first_1yr) & !is.na(ICD9_code_first_1yr) ~ ICD9_date_first_1yr,
                                                  TRUE ~ ICD10_date_first_1yr))  

df_baseline_cancer <- df_baseline_cancer %>% mutate(first_cancer_date_5yr = 
                                              case_when(ICD9_date_first_5yr < ICD10_date_first_5yr ~ ICD9_date_first_5yr,
                                              is.na(ICD10_code_first_5yr) & !is.na(ICD9_code_first_5yr) ~ ICD9_date_first_5yr,
                                                  TRUE ~ ICD10_date_first_5yr)) 

df_baseline_cancer <- df_baseline_cancer %>% mutate(first_cancer_date_after = 
                                              case_when(ICD9_date_first_after < ICD10_date_first_after ~ ICD9_date_first_after,
                                              is.na(ICD10_code_first_after) & !is.na(ICD9_code_first_after) ~ ICD9_date_first_after,
                                                  TRUE ~ ICD10_date_first_after)) 

##Section E: for sensitivity analysis - find people with a cancer diagnosis before baseline                                                
df_baseline_cancer <- UKB_wide_event_fetch_first_except(df_baseline_cancer, patt_var_cancer_ICD10, patt_var_cancer_date, 16, patt_ICD10_Skin_NM_codes, "ICD10")
df_baseline_cancer <- UKB_wide_event_fetch_first_except(df_baseline_cancer, patt_var_cancer_ICD9, patt_var_cancer_date, 14, patt_ICD9_Skin_NM_codes, "ICD9")

df_baseline_cancer <- df_baseline_cancer %>% mutate(first_cancer_date = 
                                              case_when(ICD9_date_first < ICD10_date_first ~ ICD9_date_first,
                                              is.na(ICD10_code_first) & !is.na(ICD9_code_first) ~ ICD9_date_first,
                                                  TRUE ~ ICD10_date_first)) 

df_baseline_cancer <- df_baseline_cancer %>% mutate(pre_baseline_cancer = case_when(first_cancer_date <= baseline_date ~ 1, TRUE ~ 0))


#Section F: Clean dataframe
df_baseline_cancer <- df_baseline_cancer %>% select(!c(starts_with("v"), starts_with("ICD"), "min_date"))

