##File name: get_cancers.r
##Author: Hannah Harrison
##Last Edit: 18/08/2022
##Description: finds deaths in 1 and 5 year follow-up period, using baseline assessment date as index date

all_baseline_fields <- fread(fpath_baseline_vars)
death_fields <- all_baseline_fields %>% filter(type == "death") 
ukb_fields <- all_baseline_fields %>% filter(type == "ukb")  
df_baseline_death <- fread(fpath_baseline, select = c("eid", death_fields$field, ukb_fields$field))

df_baseline_death[sapply(df_baseline_death, `%in%`, c("", "NA"))] <- NA_character_
colnames(df_baseline_death) <- gsub(x = names(df_baseline_death), pattern = "-|\\.", replacement = "_") 
colnames(df_baseline_death) <- gsub(x = names(df_baseline_death), pattern = "^([0-9])", replacement = "v\\1") 
colnames(df_baseline_death) <- gsub(x = names(df_baseline_death), pattern = "_0$", replacement = "") 

#Section X: clean data
df_baseline_death <- df_baseline_death %>% rename(death_date_1 = v40000_0, death_date_2 = v40000_1, baseline_date = v53_0)
df_baseline_death <- df_baseline_death %>% mutate(death_date = case_when(!is.na(death_date_1) ~ as.Date(death_date_1), 
                                                                  !is.na(death_date_2) & is.na(death_date_1) ~ as.Date(death_date_2), TRUE ~ NA_Date_))
df_baseline_death$baseline_date <- as.Date(df_baseline_death$baseline_date)                                                                 
df_baseline_death <- df_baseline_death %>% select(c("eid",  "death_date", "baseline_date"))

#Find deaths in 1yr follow-up 
df_baseline_death <- df_baseline_death %>% 
        mutate(case_death_1yr = case_when(death_date > baseline_date & death_date <= baseline_date + years(1) ~ 1, TRUE ~ 0), 
              case_death_5yr = case_when(death_date > baseline_date & death_date <= baseline_date + years(5) ~ 1, TRUE ~ 0), 
              case_death_any = case_when(death_date > baseline_date & death_date <= death_study_end ~ 1, TRUE ~ 0))

