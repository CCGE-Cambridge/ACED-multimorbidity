##File name: run_mm_scores.r
##Authors: Hannah Harrison
##Last Edit: 24/08/2023
##Description: compute mm_score at baseline, tidied up august 2023 to remove reference to unused model versions

#required codelists
df_comorbs <- fread(my_conditions) #list of included comorbidities

df_codes_clinical <- fread(mm_score_clinical, colClasses = "character")
df_codes_script <- fread(mm_score_scripts, colClasses = "character")

#load clinical data and query with corresponding codelist
df_gp_clinical <- fread(fpath_gp, colClasses = "character")
df_gp_clinical <- df_gp_clinical %>% filter(data_provider == 3) #reduce data size to speed up processing
df_gp_clinical <- df_gp_clinical %>% mutate(read_2 = na_if(read_2, ""), read_3 = na_if(read_3, ""))
df_gp_clinical$eid <- as.integer(df_gp_clinical$eid)
df_clinical_diags <- gp_clinical_query(df_gp_clinical, df_codes_clinical)
df_clinical_diags <- df_clinical_diags %>% select(!c("Desc"))

#load prescription data and query with corresponding codelist
df_gp_scripts <- fread(fpath_gp_med, colClasses = "character")
df_gp_scripts <- df_gp_scripts %>% filter(data_provider == 3) #reduce data size to speed up processing
df_gp_scripts <- df_gp_scripts %>% mutate(read_2 = na_if(read_2, ""),
                                          bnf_code = na_if(bnf_code, ""),
                                          dmd_code = na_if(dmd_code, "NA"))
df_gp_scripts$eid <- as.integer(df_gp_scripts$eid)
df_script_events <- gp_script_query(df_gp_scripts, df_codes_script)
df_script_events <- df_script_events %>% select(eid, event_dt, med_type, Type)

#load a version of baseline data - calculate dob
all_baseline_fields <- fread(fpath_baseline_vars)
basic_fields <- all_baseline_fields %>% filter(type == "ukb" | type == "age" | type == "sex")
df_baseline_basic <- fread(fpath_baseline, select = c("eid", basic_fields$field))
colnames(df_baseline_basic) <- c("eid", "sex", "genetic_sex", "yob", "mob", "baseline_date", "ukb_centre", "age_at_assessment") 
df_baseline_basic$dob <-  gen_birth_dates(df_baseline_basic$yob, df_baseline_basic$mob)
df_baseline_basic <- df_baseline_basic %>% select("eid", "sex", "dob", "baseline_date")
df_baseline_basic$baseline_date <- as.Date(df_baseline_basic$baseline_date)

# limit events to dates before baseline
df_clinical_diags_before <- events_relative_to_date(df_clinical_diags, df_baseline_basic, "baseline_date")
df_script_events_before <- events_relative_to_date(df_script_events, df_baseline_basic, "baseline_date")

##extract clinical events and pivot to wide format (see "multimorb_score_info_and_funcs.r" for some of these variable definitions)
df_ckd_only <- define_CKD(df_clinical_diags_before) #apply specific conditions for kidney disease
df_can_only <- define_CAN(df_clinical_diags_before) #apply specific conditions for cancer diagnoses
df_clinical_others <- df_clinical_diags_before %>% 
                        filter((Comorb %in% comorb_ever) | 
                                ((Comorb %in% comorb_in_last_year) & (event_dt >= my_date_minus_1_year))) %>%
                                distinct(eid, Comorb, .keep_all = FALSE) 
df_clinical_all <- rbind(df_clinical_others, df_ckd_only, df_can_only) %>%
                    mutate(dummy = 1) %>% 
                        pivot_wider(names_from = "Comorb", values_from = "dummy",  names_prefix = "clinic_", values_fill = 0)

#extract prescription events and pivot to wide format (see "multimorb_score_info_and_funcs.r" for some of these variable definitions)
df_script_all <- df_script_events_before %>% 
                    filter(event_dt >= my_date_minus_1_year | med_type == "SCZ") %>%
                        add_count(eid, med_type, name = "num_scripts") %>%
                            distinct(eid, med_type, .keep_all = TRUE) %>% 
                                select(eid, med_type, num_scripts)
df_script_all <- df_script_all %>% 
                    pivot_wider(names_from = "med_type", values_from = "num_scripts",  names_prefix = "script_", values_fill = 0)

##combine clinical and script with the full EHR database events -  !!what is best dataset to use here!! 
##could use df_EHR_cohort_lb_only/could use basic (people with no primary care records will drop out later)
df_EHR_events <- right_join(df_clinical_all, df_baseline_basic, by = "eid")
df_EHR_events <- right_join(df_script_all, df_EHR_events, by = "eid")
df_EHR_events[is.na(df_EHR_events)] <- 0

##calculate mm age categories (note I don't include the under 40 age categories)
df_EHR_events$age <- gen_event_age(df_EHR_events$dob, df_EHR_events$baseline_date)
df_EHR_events <- gen_mm_age_categories(df_EHR_events)

df_EHR_events <- df_EHR_events %>% rename(age_cont = age)

##define 37 comorbidites needed for the model
df_mm_covars <- define_37_mm_covariates(df_EHR_events)

##create and save all model versions
df_mm_a <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "cons_NB_37_RR") #test with one model
df_mm_b <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "cons_count_13_RR") #test with one model
df_mm_c <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "cons_bin_13_OR") #test with one model

df_mm_d <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "cons_NB_20_RR") #test with one model
df_mm_e <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "cons_count_5_RR") #test with one model
df_mm_f <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "cons_bin_5_OR") #test with one model

df_mm_g <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "hospital_37_HR") #test with one model
df_mm_h <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "hospital_20_HR") #test with one model

df_mm_i <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "mortality_37_HR") #test with one model
df_mm_j <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "mortality_20_HR") #test with one model

df_mm_k <- mm_score_HR(df_mm_covars, df_cam_multi_morb, "gen_37") # generalised outcome standarised weights

#get list of the 20 most significant conditions
df_mm <- df_cam_multi_morb %>% select(starts_with(c("conditions", "codes", "hospital", "mortality")))
df_mm_20 <- df_mm %>% filter(!is.na(hospital_20_HR)) %>% filter(!grepl('z:', conditions))
mm_codes_20 <- paste0(df_mm_20$codes, collapse = "|")

#create addtional outcome of "summed" conditions
df_mm_all <- df_mm_g %>% mutate(num_mm_37 = rowSums(across(starts_with("event")))) %>% 
                            mutate(num_mm_20 = rowSums(across(contains(df_mm_20$codes) & starts_with("event")))) %>% 
                                select(c("eid", "sex", "age_cont", "prognostic_index", "residual_fit", "num_mm_37", "num_mm_20")) %>%
                                rename(RF_hosp_37 = residual_fit, PI_hosp_37 = prognostic_index)

#combine all outcomes in one dataframe
df_mm_a <- df_mm_a %>% select(c("eid", "residual_fit", "prognostic_index")) %>%
                        rename(RF_cons_NB_37 = residual_fit, PI_cons_NB_37 = prognostic_index)
df_mm_b <- df_mm_b %>% select(c("eid", "residual_fit", "prognostic_index")) %>%
                        rename(RF_cons_count_13 = residual_fit, PI_cons_count_13 = prognostic_index)
df_mm_c <- df_mm_c %>% select(c("eid", "residual_fit", "prognostic_index")) %>%
                        rename(RF_cons_bin_13 = residual_fit, PI_cons_bin_13 = prognostic_index)
df_mm_d <- df_mm_d %>% select(c("eid", "residual_fit", "prognostic_index")) %>%
                        rename(RF_cons_NB_20 = residual_fit, PI_cons_NB_20 = prognostic_index)
df_mm_e <- df_mm_e %>% select(c("eid", "residual_fit", "prognostic_index")) %>%
                        rename(RF_cons_count_5 = residual_fit, PI_cons_count_5 = prognostic_index)
df_mm_f <- df_mm_f %>% select(c("eid", "residual_fit", "prognostic_index")) %>%
                        rename(RF_cons_bin_5 = residual_fit, PI_cons_bin_5 = prognostic_index)                                                 


df_mm_h <- df_mm_h %>% select(c("eid", "residual_fit", "prognostic_index")) %>%
                        rename(RF_hosp_20 = residual_fit, PI_hosp_20 = prognostic_index)

df_mm_i <- df_mm_i %>% select(c("eid", "residual_fit", "prognostic_index")) %>%
                        rename(RF_mort_37 = residual_fit, PI_mort_37 = prognostic_index)

df_mm_j <- df_mm_j %>% select(c("eid", "residual_fit", "prognostic_index")) %>%
                        rename(RF_mort_20 = residual_fit, PI_mort_20 = prognostic_index)

df_mm_k <- df_mm_k %>% select(c("eid", "residual_fit", "prognostic_index")) %>%
                    rename(RF_gen_37 = residual_fit, PI_gen_37 = prognostic_index)


df_mm_all_binary_outcome <- df_mm_all %>% merge(df_mm_h, by = "eid") %>% merge(df_mm_i, by = "eid") %>% merge(df_mm_j, by = "eid")

df_mm_all <- df_mm_all_binary_outcome %>% merge(df_mm_a, by = "eid") %>% merge(df_mm_b, by = "eid") %>% merge(df_mm_c, by = "eid") %>% 
                                merge(df_mm_d, by = "eid") %>% merge(df_mm_e, by = "eid") %>% merge(df_mm_f, by = "eid") %>% merge(df_mm_k, by = "eid")


