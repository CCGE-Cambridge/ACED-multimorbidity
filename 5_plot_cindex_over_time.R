##File name: plot_cindex_over_time_v2.r
##Authors: Hannah Harrison
##Last Edit: 23/01/2023
##Description: plot the c-index for mmscores (outcomes of death/cancer) over time

results_folder <- res_cstats
save_folder <- graph_cstats
model_refs <- c("PI_mort_37", "PI_hosp_37", "num_mm_37", "PI_gen_37") 

model_labels <- c("Mortality CMS", "Hospital CMS", "Count Score", "General CMS") 

custom_colors_model_types <- c("#540785", "#1e7a14", "#9c6a0b", "#424a6b")

df_cancer_res <- fread(file.path(results_folder, "cstat_cancer.csv")) %>% filter(model %in% model_refs)
df_cancer_res <- df_cancer_res %>% mutate(custom_shape = case_when(grepl("20", model) ~ "o", grepl("37", model) ~ "^", TRUE ~ "x"))
df_cancer_res$custom_colors <- rep(custom_colors_model_types, 10)
df_cancer_res$labels <- rep(model_labels, 10)

df_SA_cancer_res <- fread(file.path(results_folder, "cstat_SA_cancer.csv")) %>% filter(model %in% model_refs)
df_SA_cancer_res <- df_SA_cancer_res %>% mutate(custom_shape = case_when(grepl("20", model) ~ "o", grepl("37", model) ~ "^", TRUE ~ "x"))
df_SA_cancer_res$custom_colors <- rep(custom_colors_model_types, 10)
df_SA_cancer_res$labels <- rep(model_labels, 10)

df_death_res <- fread(file.path(results_folder, "cstat_death.csv")) %>% filter(model %in% model_refs)
df_death_res <- df_death_res %>% mutate(custom_shape = case_when(grepl("20", model) ~ "o", grepl("37", model) ~ "^", TRUE ~ "x"))
df_death_res$custom_colors <- rep(custom_colors_model_types, 10)
df_death_res$labels <- rep(model_labels, 10)


df_cons_res <- fread(file.path(results_folder, "cstat_rate.csv")) %>% filter(model %in% model_refs)
df_cons_res <- df_cons_res %>% mutate(custom_shape = case_when(grepl("20", model) ~ "o", grepl("37", model) ~ "^", TRUE ~ "x"))
df_cons_res$custom_colors <- rep(custom_colors_model_types, 8)
df_cons_res$labels <- rep(model_labels, 8)


run_custom_plot <- function(df_plot, plot_name) {
    col <- as.character(df_plot$custom_colors)
    names(col) <- as.character(df_plot$labels)

    df_plot %>% ggplot(aes(x=years, y=c_index, colour = labels, group = labels)) + 
        geom_errorbar(aes(ymin=L_CI, ymax=U_CI),  width = .2) +
        geom_line(linetype = 2) +
        geom_point(size=2) + 
        ylim(c(0.50, 0.75)) + 
        xlab("Follow-up (years)") + 
        ylab("c-statistic") +
        scale_color_manual(values=col) +
        theme_hh() + 
        guides(colour =  guide_legend("Model"))    

    ggsave(file.path(save_folder, plot_name),  dpi = 300, type = "cairo")
}
 

###PLOT AND SAVE GRAPHS#### 
run_custom_plot(df_death_res, "death_all_v3.png")
run_custom_plot(df_death_res %>% filter((grepl("mort", model) | grepl("gen", model) | grepl("num", model))), "death_figA_v3.png")
run_custom_plot(df_death_res %>% filter(((grepl("mort", model) | grepl("gen", model) | grepl("num", model)) & (grepl("37", model)))), "death_figB_v3.png")

run_custom_plot(df_cancer_res, "cancer_all_v3.png")
run_custom_plot(df_cancer_res %>% filter((grepl("mort", model) | grepl("gen", model) | grepl("num", model))), "cancer_figA_v3.png")
run_custom_plot(df_cancer_res %>% filter(((grepl("mort", model) | grepl("gen", model) | grepl("num", model)) & (grepl("37", model)))), "cancer_figB_v3.png")

run_custom_plot(df_cons_res, "cons_all_v3.png")
run_custom_plot(df_cons_res %>% filter((grepl("mort", model) | grepl("gen", model) | grepl("num", model))), "cons_figA_v3.png")
run_custom_plot(df_cons_res %>% filter(((grepl("mort", model) | grepl("gen", model) | grepl("num", model)) & (grepl("37", model)))), "cons_figB_v3.png")

run_custom_plot(df_SA_cancer_res, "SA_cancer_all_v3.png")
run_custom_plot(df_SA_cancer_res %>% filter((grepl("mort", model) | grepl("gen", model) | grepl("num", model))), "SA_cancer_figA_v3.png")
run_custom_plot(df_SA_cancer_res %>% filter(((grepl("mort", model) | grepl("gen", model) | grepl("num", model)) & (grepl("37", model)))), "SA_cancer.png")




