##File name: plot_cindex_over_time_v2.r
##Authors: Hannah Harrison
##Last Edit: 23/01/2023
##Description: plot the c-index for general outcome score, in subgroups

results_folder <- "~/ACED-RREDD-EHR/cam_mm_score_project/results/subgroups"
save_folder <- "~/ACED-RREDD-EHR/cam_mm_score_project/graphs/subgroups"

#create summary files for general outcome score, all subgroups
sg_file_names <- c("all", "men", "women", "forties", "fifties", "sixties", "forties_men", "fifties_men", "sixties_men", "forties_women", "fifties_women", "sixties_women")

df_death_sg <- data.frame(matrix(ncol = 10, nrow = 0))
for (i in 1:length(sg_file_names)){
    temp <- fread(file.path(results_folder, paste0("cstat_death_", sg_file_names[i], ".csv")))
    temp <- temp %>% mutate(subgroup = sg_file_names[i]) %>% mutate(subgroup = case_when(subgroup == "v2" ~ "all", TRUE ~ subgroup))
    df_death_sg = rbind(df_death_sg, temp)
}
df_death_sg_gen <- df_death_sg %>% filter(model == "PI_gen_37")

df_rate_sg <- data.frame(matrix(ncol = 9, nrow = 0))
for (i in 1:length(sg_file_names)){
    temp <- fread(file.path(results_folder, paste0("cstat_rate_", sg_file_names[i], ".csv")))
    temp <- temp %>% mutate(subgroup = sg_file_names[i]) %>% mutate(subgroup = case_when(subgroup == "v2" ~ "all", TRUE ~ subgroup))
    df_rate_sg = rbind(df_rate_sg, temp)
}
df_rate_sg_gen <- df_rate_sg %>% filter(model == "PI_gen_37")

df_cancer_sg <- data.frame(matrix(ncol = 10, nrow = 0))
for (i in 1:length(sg_file_names)){
    temp <- fread(file.path(results_folder, paste0("cstat_cancer_", sg_file_names[i], ".csv")))
    temp <- temp %>% mutate(subgroup = sg_file_names[i]) %>% mutate(subgroup = case_when(subgroup == "v2" ~ "all", TRUE ~ subgroup))
    df_cancer_sg = rbind(df_cancer_sg, temp)
}
df_cancer_sg_gen <- df_cancer_sg %>% filter(model == "PI_gen_37")

#graphing set-up
subgroup_refs <- sg_file_names
num_to_shape <- c("All Ages", "All Ages", "All Ages", "In 40's", "In 50's", "In 60's", "In 40's", "In 50's", "In 60's", "In 40's", "In 50's", "In 60's")
num_to_size <- c(5, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3)
type_to_line <- c("Both", "Men", "Women", "Both", "Both", "Both", "Men", "Men", "Men", "Women", "Women", "Women")

model_labels <- c("Whole Cohort", "All Men", "All Women", "40-49 yrs", "50-59 yrs", "60-69 yrs",  
                "Men, 40-49 yrs", "Men, 50-59 yrs", "Men, 60-69 yrs", "Women, 40-49 yrs", "Women, 50-59 yrs", "Women, 60-69 yrs") 

#000000#2a0242#2e2e2e#bbbbbb#8a8#4d4d4d
custom_colors_model_types <- c("#000000", 
                                "#461a85", "#1d6817",
                                "#5c0e0e", "#941f1f", "#ca4545",
                                "#5e0a92", "#703396", "#a680be",
                                "#1a6811", "#4d9b44", "#79b473") 



df_death_sg_gen$custom_colors <- rep(custom_colors_model_types, each=10)
df_death_sg_gen$labels <- rep(model_labels, each = 10)
df_death_sg_gen$shape_info <-  rep(num_to_shape, each = 10)
df_death_sg_gen$line_info <- rep(type_to_line, each = 10)
df_death_sg_gen$size_info <- rep(num_to_size, each = 10)

df_cancer_sg_gen$custom_colors <- rep(custom_colors_model_types, each=10)
df_cancer_sg_gen$labels <- rep(model_labels, each = 10)
df_cancer_sg_gen$shape_info <-  rep(num_to_shape, each = 10)
df_cancer_sg_gen$line_info <- rep(type_to_line, each = 10)
df_cancer_sg_gen$size_info <- rep(num_to_size, each = 10)

df_rate_sg_gen$custom_colors <- rep(custom_colors_model_types, each=8)
df_rate_sg_gen$labels <- rep(model_labels, each = 8)
df_rate_sg_gen$shape_info <-  rep(num_to_shape, each = 8)
df_rate_sg_gen$line_info <- rep(type_to_line, each = 8)
df_rate_sg_gen$size_info <- rep(num_to_size, each = 8)

scale_shape_discrete = function(...) {
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 3))
}

run_custom_plot <- function(df_plot, plot_name) {
    col <- as.character(df_plot$custom_colors)
    names(col) <- as.character(df_plot$labels)

    df_plot %>% ggplot(aes(x=years, y=c_index, colour = labels, group = labels, shape = shape_info)) + 
        geom_errorbar(aes(ymin=L_CI, ymax=U_CI), width = .3) +
        geom_line(aes(linetype = line_info)) +
        geom_point(size = 2.5) + 
        ylim(c(0.50, 0.75)) + 
        xlab("Follow-up (years)") + 
        ylab("c-statistic") +
        scale_color_manual(values=col) +
        theme_hh() + 
        guides(linetype = guide_legend("Sex"),
              shape = guide_legend("Age Group"), 
              colour =  guide_legend("Subgroup"), 
              size = "none")    

    ggsave(file.path(save_folder, plot_name),  dpi = 300, type = "cairo")
}
 

###QUICK SUMMARY GRAPHS#### 
plot_sex <- c("all", "men", "women")
plot_age <- c("all", "forties", "fifties", "sixties")
plot_age_and_sex <- c("all", "forties_men", "fifties_men", "sixties_men", "forties_women", "fifties_women", "sixties_women")

run_custom_plot(df_death_sg_gen, "death_sg_all.png")
run_custom_plot(df_death_sg_gen %>% filter(subgroup %in% plot_sex), "death_sg_sex.png")
run_custom_plot(df_death_sg_gen %>% filter(subgroup %in% plot_age), "death_sg_age.png")
run_custom_plot(df_death_sg_gen %>% filter(subgroup %in% plot_age_and_sex), "death_sg_age_and_sex.png")

run_custom_plot(df_cancer_sg_gen, "cancer_sg_all.png")
run_custom_plot(df_cancer_sg_gen %>% filter(subgroup %in% plot_sex), "cancer_sg_sex.png")
run_custom_plot(df_cancer_sg_gen %>% filter(subgroup %in% plot_age), "cancer_sg_age.png")
run_custom_plot(df_cancer_sg_gen %>% filter(subgroup %in% plot_age_and_sex), "cancer_sg_age_and_sex.png")

run_custom_plot(df_rate_sg_gen, "rate_sg_all.png")
run_custom_plot(df_rate_sg_gen %>% filter(subgroup %in% plot_sex), "rate_sg_sex.png")
run_custom_plot(df_rate_sg_gen %>% filter(subgroup %in% plot_age), "rate_sg_age.png")
run_custom_plot(df_rate_sg_gen %>% filter(subgroup %in% plot_age_and_sex), "rate_sg_age_and_sex.png")

