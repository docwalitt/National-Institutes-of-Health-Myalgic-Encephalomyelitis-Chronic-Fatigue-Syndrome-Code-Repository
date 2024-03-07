# Sam LaMunion; samuel.lamunion@nih.gov

rm(list=ls()) # start with a clean environment
# list.of.packages <- c("ggplot2", "stats", "tidyr", "dplyr", "magrittr", "reshape2", "janitor", "broom", "ggpubr", "ggpmisc", "readxl", "patchwork")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages) #install/load required and supporting packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, stats, tidyr, dplyr, magrittr, reshape2, janitor, broom, ggpubr, ggpmisc, readxl, patchwork, showtext, openxlsx, purrr)
setwd("Z:/ME-CFS/Figures/final_figures/Sam - Revisions/Current Versions/Compiled Versions/Individual Figure Panels/Figures Source Data Files/Nature Comms MECFS/Data")

# List all .xlsx files in the directory
##xlsx_files_to_share <- list.files(pattern = "\\.xlsx$", full.names = TRUE)

# Read all .xlsx files into a named list
##xlsx_list <- map(xlsx_files_to_share, ~ read.xlsx(.x))

# Name the list elements based on file names
##names(xlsx_list) <- tools::file_path_sans_ext(basename(xlsx_files_to_share))

# Save the list as RData
##saveRDS(xlsx_list, file = "Nature Comms MECFS Source Data.RData")

##### Start Here #####
# Load the RData file containing the individual source data files
# source_data <- readRDS("Nature Comms MECFS Source Data.RData")

# once the source data are loaded you can save them as individual .xlsx files to a directory of your choosing.
# source_data is the list of data frames
# Create a directory to save the output files
#output_dir <- "output_directory" # set this to be where you want the files written to
#dir.create(output_dir, showWarnings = FALSE) # this creates that directory to save the .xlsx files to

# purrr::walk iterates over the source_data list of data frames and writes each data frame to a separate .xlsx file
#purrr::walk(names(source_data), ~ openxlsx::write.xlsx(source_data[[.x]], file.path(output_dir, paste0(.x, ".xlsx")), row.names = FALSE))

# If you want to just extract all files into separate data frames in the current global environment
# list2env(source_data, envir = .GlobalEnv)

##### set content so it is loaded for use where relevant
formula = y ~ x # sets the formula for regression
showtext_auto() # enables the use of custom fonts in ggplot2
female = intToUtf8(9792) # sets symbol for female
male = intToUtf8(9794) # sets symbol for male

##### Figure 1C - PROMIS Fatigue #####

data <- readxl::read_excel("Figure 1C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

bw = 1 # sets binwidth
n_obs_hv <- length(which(data$group == "HV")) # gets number of HV in sample
n_obs_cfs <- length(which(data$group == "PI-ME/CFS")) # gets number of CFS in sample

fig1c.1 <- ggplot2::ggplot(data, aes(x = promis_fatigue_short_form, fill = group)) + # 
  geom_histogram(binwidth = bw, position = "dodge", alpha = 1) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "HV")$promis_fatigue_short_form), sd = sd(subset(data, group == "HV")$promis_fatigue_short_form)) * bw * n_obs_hv,
    aes(color = "HV"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "PI-ME/CFS")$promis_fatigue_short_form), sd = sd(subset(data, group == "PI-ME/CFS")$promis_fatigue_short_form)) * bw * n_obs_cfs,
    aes(color = "PI-ME/CFS"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  scale_color_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  scale_fill_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  labs(x = "PROMIS Fatigue", y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 0.2e-5"))


fig1c.1

ggsave(fig1c.1, file = "Figure_1C - PROMIS Fatigue.png")  
ggsave(fig1c.1, file = "Figure_1C - PROMIS Fatigue.eps", device = "eps")

##### Figure 1C - PROMIS Emotional Distress #####

fig1c.2 <- ggplot2::ggplot(data, aes(x = promis_emotional_distress_depression_t_score, fill = group)) +  
  geom_histogram(binwidth = bw, position = "dodge", alpha = 1) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "HV")$promis_emotional_distress_depression_t_score), sd = sd(subset(data, group == "HV")$promis_emotional_distress_depression_t_score)) * bw * n_obs_hv,
    aes(color = "HV"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "PI-ME/CFS")$promis_emotional_distress_depression_t_score), sd = sd(subset(data, group == "PI-ME/CFS")$promis_emotional_distress_depression_t_score)) * bw * n_obs_cfs,
    aes(color = "PI-ME/CFS"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  scale_color_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  scale_fill_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  labs(x = "PROMIS Emotional Distress", y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 0.03"))

fig1c.2

ggsave(fig1c.2, file = "Figure_1C - PROMIS Emotional Distress.png")  
ggsave(fig1c.2, file = "Figure_1C - PROMIS Emotional Distress.eps", device = "eps")

##### Figure 1C - PROMIS Sleep Disturbance #####

fig1c.3 <- ggplot2::ggplot(data, aes(x = promis_sleep_disturbance_short_form, fill = group)) +  
  geom_histogram(binwidth = bw, position = "dodge", alpha = 1) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "HV")$promis_sleep_disturbance_short_form), sd = sd(subset(data, group == "HV")$promis_sleep_disturbance_short_form)) * bw * n_obs_hv,
    aes(color = "HV"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "PI-ME/CFS")$promis_sleep_disturbance_short_form), sd = sd(subset(data, group == "PI-ME/CFS")$promis_sleep_disturbance_short_form)) * bw * n_obs_cfs,
    aes(color = "PI-ME/CFS"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  scale_color_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  scale_fill_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  labs(x = "PROMIS Sleep Disturbance", y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 0.2e-4"))

fig1c.3

ggsave(fig1c.3, file = "Figure_1C - PROMIS Sleep Disturbance.png")  
ggsave(fig1c.3, file = "Figure_1C - PROMIS Sleep Disturbance.eps", device = "eps")

##### Figure 1D - PROMIS Anxiety #####

fig1c.4 <- ggplot2::ggplot(data, aes(x = promis_emotional_distress_anxiety_t_score, fill = group)) +  
  geom_histogram(binwidth = bw, position = "dodge", alpha = 1) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "HV")$promis_emotional_distress_anxiety_t_score), sd = sd(subset(data, group == "HV")$promis_emotional_distress_anxiety_t_score)) * bw * n_obs_hv,
    aes(color = "HV"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "PI-ME/CFS")$promis_emotional_distress_anxiety_t_score), sd = sd(subset(data, group == "PI-ME/CFS")$promis_emotional_distress_anxiety_t_score)) * bw * n_obs_cfs,
    aes(color = "PI-ME/CFS"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  scale_color_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  scale_fill_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  labs(x = "PROMIS Anxiety", y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 0.007"))

fig1c.4

ggsave(fig1c.4, file = "Figure_1C - PROMIS Anxiety.png")  
ggsave(fig1c.4, file = "Figure_1C - PROMIS Anxiety.eps", device = "eps")

##### Figure 1C - PHQ15 #####

fig1c.5 <- ggplot2::ggplot(data, aes(x = physical_symptoms_phq_15, fill = group)) +  
  geom_histogram(binwidth = bw, position = "dodge", alpha = 1) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "HV")$physical_symptoms_phq_15), sd = sd(subset(data, group == "HV")$physical_symptoms_phq_15)) * bw * n_obs_hv,
    aes(color = "HV"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "PI-ME/CFS")$physical_symptoms_phq_15), sd = sd(subset(data, group == "PI-ME/CFS")$physical_symptoms_phq_15)) * bw * n_obs_cfs,
    aes(color = "PI-ME/CFS"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  scale_color_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  scale_fill_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  labs(x = "PHQ 15", y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 0.7e-6"))

fig1c.5

ggsave(fig1c.5, file = "Figure_1C - PHQ 15.png")  
ggsave(fig1c.5, file = "Figure_1C - PHQ 15.eps", device = "eps")

##### Figure 1C - MASQ Language #####

fig1c.6 <- ggplot2::ggplot(data, aes(x = multiple_ability_self_report_qnr_masq_language, fill = group)) +  
  geom_histogram(binwidth = bw, position = "dodge", alpha = 1) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "HV")$multiple_ability_self_report_qnr_masq_language), sd = sd(subset(data, group == "HV")$multiple_ability_self_report_qnr_masq_language)) * bw * n_obs_hv,
    aes(color = "HV"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "PI-ME/CFS")$multiple_ability_self_report_qnr_masq_language), sd = sd(subset(data, group == "PI-ME/CFS")$multiple_ability_self_report_qnr_masq_language)) * bw * n_obs_cfs,
    aes(color = "PI-ME/CFS"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  scale_color_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  scale_fill_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  labs(x = "MASQ Lanugage", y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 0.4e-4"))

fig1c.6

ggsave(fig1c.6, file = "Figure_1C - MASQ Language.png")
ggsave(fig1c.6, file = "Figure_1C - MASQ Language.eps", device = "eps")

##### Figure 1C - MASQ Attention #####

fig1c.7 <- ggplot2::ggplot(data, aes(x = multiple_ability_self_report_qnr_masq_attention, fill = group)) +  
  geom_histogram(binwidth = bw, position = "dodge", alpha = 1) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "HV")$multiple_ability_self_report_qnr_masq_attention), sd = sd(subset(data, group == "HV")$multiple_ability_self_report_qnr_masq_attention)) * bw * n_obs_hv,
    aes(color = "HV"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "PI-ME/CFS")$multiple_ability_self_report_qnr_masq_attention), sd = sd(subset(data, group == "PI-ME/CFS")$multiple_ability_self_report_qnr_masq_attention)) * bw * n_obs_cfs,
    aes(color = "PI-ME/CFS"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  scale_color_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  scale_fill_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  labs(x = "MASQ Attention", y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 0.5e-3"))

fig1c.7

ggsave(fig1c.7, file = "Figure_1C - MASQ Attention.png")  
ggsave(fig1c.7, file = "Figure_1C - MASQ Attention.eps", device = "eps")

##### Figure 1C - MASQ Verbal Memory #####

fig1c.8 <- ggplot2::ggplot(data, aes(x = multiple_ability_self_report_qnr_masq_verbal_memory, fill = group)) +  
  geom_histogram(binwidth = bw, position = "dodge", alpha = 1) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "HV")$multiple_ability_self_report_qnr_masq_verbal_memory), sd = sd(subset(data, group == "HV")$multiple_ability_self_report_qnr_masq_verbal_memory)) * bw * n_obs_hv,
    aes(color = "HV"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "PI-ME/CFS")$multiple_ability_self_report_qnr_masq_verbal_memory), sd = sd(subset(data, group == "PI-ME/CFS")$multiple_ability_self_report_qnr_masq_verbal_memory)) * bw * n_obs_cfs,
    aes(color = "PI-ME/CFS"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  scale_color_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  scale_fill_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  labs(x = "MASQ Verbal Memory", y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 0.2e-3"))

fig1c.8

ggsave(fig1c.8, file = "Figure_1C - MASQ Verbal Memory.png")  
ggsave(fig1c.8, file = "Figure_1C - MASQ Verbal Memory.eps", device = "eps")

##### Figure 1C - MASQ Visual Memory #####

fig1c.9 <- ggplot2::ggplot(data, aes(x = multiple_ability_self_report_qnr_masq_visual_memory, fill = group)) +  
  geom_histogram(binwidth = bw, position = "dodge", alpha = 1) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "HV")$multiple_ability_self_report_qnr_masq_visual_memory), sd = sd(subset(data, group == "HV")$multiple_ability_self_report_qnr_masq_visual_memory)) * bw * n_obs_hv,
    aes(color = "HV"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "PI-ME/CFS")$multiple_ability_self_report_qnr_masq_visual_memory), sd = sd(subset(data, group == "PI-ME/CFS")$multiple_ability_self_report_qnr_masq_visual_memory)) * bw * n_obs_cfs,
    aes(color = "PI-ME/CFS"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  scale_color_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  scale_fill_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  labs(x = "MASQ Visual Memory", y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 0.09"))

fig1c.9

ggsave(fig1c.9, file = "Figure_1C - MASQ Visual Memory.png")  
ggsave(fig1c.9, file = "Figure_1C - MASQ Visual Memory.eps", device = "eps")

##### Figure 1C - MASQ Visual Perception #####

fig1c.10 <- ggplot2::ggplot(data, aes(x = multiple_ability_self_report_qnr_masq_visuoperceptual, fill = group)) +  
  geom_histogram(binwidth = bw, position = "dodge", alpha = 1) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "HV")$multiple_ability_self_report_qnr_masq_visuoperceptual), sd = sd(subset(data, group == "HV")$multiple_ability_self_report_qnr_masq_visuoperceptual)) * bw * n_obs_hv,
    aes(color = "HV"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  stat_function(
    fun = function(x) dnorm(x, mean = mean(subset(data, group == "PI-ME/CFS")$multiple_ability_self_report_qnr_masq_visuoperceptual), sd = sd(subset(data, group == "PI-ME/CFS")$multiple_ability_self_report_qnr_masq_visuoperceptual)) * bw * n_obs_cfs,
    aes(color = "PI-ME/CFS"),
    linewidth = 1,
    geom = "line",
    alpha = 0.1,
    show.legend = F
  ) +
  scale_color_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  scale_fill_manual(values = c("HV" = "blue", "PI-ME/CFS" = "red")) +
  labs(x = "MASQ Visual Perception", y = "Frequency") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 13),
        axis.text.y = element_text(color = "black", size = 13)) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 0.02"))

fig1c.10

ggsave(fig1c.10, file = "Figure_1C - MASQ Visual Perception.png")  
ggsave(fig1c.10, file = "Figure_1C - MASQ Visual Perception.eps", device = "eps")

##### Figure 1C #####

fig1c <- ((fig1c.1 / fig1c.2 / fig1c.3 / fig1c.4 / fig1c.5) | (fig1c.6 / fig1c.7 / fig1c.8 / fig1c.9 / fig1c.10)) + plot_layout(guides = "collect")

ggsave(fig1c, file = "Figure 1C.png", width = 10, height = 16, dpi = 300)
ggsave(fig1c, file = "Figure 1C.eps", width = 10, height = 16, dpi = 300, device = "eps")

##### Figure 2A #####
# please see the line by line comments in this figure for guidance on the structure 
# this assumes you have the .xlsx files downloaded

data <- readxl::read_excel("Figure 2A.xlsx", col_names = TRUE) %>% # reads in the .xlsx file of the formatted data for the specified figure number and panel
  janitor::clean_names() %>% # cleans the column names
  na.omit() # removes any rows with missing data, occasional spacing between groups in source data files, otherwise source data are complete

colnames(data) <- c("group", "var") # renames the columns in the data frame for plotting

fig2A <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + # creates the ggplot object
  geom_boxplot(outlier.shape = NA) + # creates the boxplot
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) + # adds the error bars
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) + # adds the individual data points
  theme_classic() + # sets the theme
  scale_color_manual(values = c("blue", "red")) + # sets the colors
  labs(y = "24 Hr Heart Rate (bpm)") + # sets the y-axis label
  theme(legend.position = "none", # removes the legend
        axis.title.x = element_blank(), # removes the x-axis label
        axis.title.y = element_text(face = "bold", color = "black", size = 28), # sets the y-axis label
        axis.text.x = element_text(face = "bold", color = "black", size = 22), # sets the x-axis text
        axis.text.y = element_text(color = "black", size = 22)) #+ # sets the y-axis text
# ggsignif::geom_signif(test = "wilcox.test", # adds the significance test where relevant
#                       comparisons = list(c("HV", "PI-ME/CFS")), # sets the comparisons to test
#                       map_signif_level = function(p) sprintf("p = %.1g", p), # sets the significance level
#                       color = "black", # sets the color of the p-value
#                       textsize = 8) # sets the size of the p-value

ggsave(fig2A, file = "Figure_2A.png", width = 500, height = 700, dpi = 300, units = "px")  # saves the figure as a .png
ggsave(fig2A, file = "Figure_2A.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px") # saves the figure as a .eps

##### Figure 2B #####

data <- readxl::read_excel("Figure 2B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>% 
  na.omit() 

colnames(data) <- c("group", "var") 

fig2b <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA) + 
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) + 
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) + 
  theme_classic() + 
  scale_color_manual(values = c("blue", "red")) + 
  labs(y = "SDNN Index (msec)") + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(face = "bold", color = "black", size = 28),
        axis.text.x = element_text(face = "bold", color = "black", size = 22), 
        axis.text.y = element_text(color = "black", size = 22)) + 
  ggsignif::geom_signif(test = "wilcox.test", 
                        comparisons = list(c("HV", "PI-ME/CFS")), 
                        map_signif_level = function(p) sprintf("p = %.1g", p), 
                        color = "black",
                        textsize = 8) 

fig2b 

ggsave(fig2b, file = "Figure_2B.png", width = 500, height = 700, dpi = 300, units = "px")  # saves the figure as a .png
ggsave(fig2b, file = "Figure_2B.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px") # saves the figure as a .eps


##### Figure 2C #####

data <- readxl::read_excel("Figure 2C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig2c <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "rMSSD (msec)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig2c

ggsave(fig2c, file = "Figure_2C.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig2c, file = "Figure_2C.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")


##### Figure 2D #####

data <- readxl::read_excel("Figure 2D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig2d <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "pNN50 (%)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig2d

ggsave(fig2d, file = "Figure_2D.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig2d, file = "Figure_2D.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 2 HF #####
# Analysis only for text, no figure

data <- readxl::read_excel("Figure 2 HF-collapsed-group.xlsx", col_names = TRUE) %>% # reads in the .xlsx file
  janitor::clean_names() %>% # cleans the column names
  na.omit() # removes any rows with missing data

colnames(data) <- c("group", "var") # renames the columns in the data frame

# fig2_HF <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + # creates the ggplot object
  # geom_violin() + # creates the boxplot
  # stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) + # adds the error bars
  # geom_point(shape=19, position=position_jitter(width = 0.1, height = 0.25), size = 1) + # adds the individual data points
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.25) +
  # theme_classic() + # sets the theme
  # scale_color_manual(values = c("blue", "red")) + # sets the colors
  # labs(y = "HF power") + # sets the y-axis label
  # theme(legend.position = "none", # removes the legend
  #       axis.title.x = element_blank(), # removes the x-axis label
  #       axis.title.y = element_text(face = "bold", color = "black", size = 28), # sets the y-axis label
  #       axis.text.x = element_text(face = "bold", color = "black", size = 22), # sets the x-axis text
  #       axis.text.y = element_text(color = "black", size = 22)) + # sets the y-axis text
  # ggsignif::geom_signif(test = "wilcox.test", # adds the significance test
  #                       comparisons = list(c("HV", "PI-ME/CFS")), # sets the comparisons
  #                       map_signif_level = function(p) sprintf("p = %.1g", p), # sets the significance level
  #                       color = "black",
  #                       textsize = 8) + # sets the color of the significance test 
#   ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 9.9E-85", size = 10))
# 
# fig2_HF
# 
# ggsave(fig2_HF, file = "Figure_2_HF.png", width = 500, height = 700, dpi = 300, units = "px")  # saves the figure as a .png
# ggsave(fig2_HF, file = "Figure_2_HF.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px") # saves the figure as a .eps

data <- data %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    bin = row_number(),
    bin = as.factor(bin))

summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group))
summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group)$contrasts)$p.value

##### Figure 2 LF #####
# Analysis only for text, no figure

data <- readxl::read_excel("Figure 2 LF-collapsed-group.xlsx", col_names = TRUE) %>% # reads in the .xlsx file
  janitor::clean_names() %>% # cleans the column names
  na.omit() # removes any rows with missing data

colnames(data) <- c("group", "var") # renames the columns in the data frame

# fig2_LF <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + # creates the ggplot object
#   geom_violin() + # creates the boxplot
#   geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1) +
#   theme_classic() + # sets the theme
#   scale_color_manual(values = c("blue", "red")) + # sets the colors
#   labs(y = "LF power") + # sets the y-axis label
#   theme(legend.position = "none", # removes the legend
#         axis.title.x = element_blank(), # removes the x-axis label
#         axis.title.y = element_text(face = "bold", color = "black", size = 28), # sets the y-axis label
#         axis.text.x = element_text(face = "bold", color = "black", size = 22), # sets the x-axis text
#         axis.text.y = element_text(color = "black", size = 22)) + # sets the y-axis text
  # ggsignif::geom_signif(test = "wilcox.test", # adds the significance test
  #                       comparisons = list(c("HV", "PI-ME/CFS")), # sets the comparisons
  #                       map_signif_level = function(p) sprintf("p = %.1g", p), # sets the significance level
  #                       color = "black",
  #                       textsize = 8) + # sets the color of the significance test 
#   ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 9.3E-78", size = 10))
# 
# fig2_LF
# 
# ggsave(fig2_LF, file = "Figure_2_LF.png", width = 500, height = 700, dpi = 300, units = "px")  # saves the figure as a .png
# ggsave(fig2_LF, file = "Figure_2_LF.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px") # saves the figure as a .eps

data <- data %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    bin = row_number(),
    bin = as.factor(bin))

summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group))
summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group)$contrasts)$p.value

##### Figure 2 SD1 #####
# Analysis only for text, no figure

data <- readxl::read_excel("Figure 2 SD1-collapsed-group.xlsx", col_names = TRUE) %>% # reads in the .xlsx file
  janitor::clean_names() %>% # cleans the column names
  na.omit() # removes any rows with missing data

colnames(data) <- c("group", "var") # renames the columns in the data frame

# fig2_SD1 <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + # creates the ggplot object
#   geom_violin() + # creates the boxplot
#   geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
#   theme_classic() + # sets the theme
#   scale_color_manual(values = c("blue", "red")) + # sets the colors
#   labs(y = "SD1") + # sets the y-axis label
#   theme(legend.position = "none", # removes the legend
#         axis.title.x = element_blank(), # removes the x-axis label
#         axis.title.y = element_text(face = "bold", color = "black", size = 28), # sets the y-axis label
#         axis.text.x = element_text(face = "bold", color = "black", size = 22), # sets the x-axis text
#         axis.text.y = element_text(color = "black", size = 22)) + # sets the y-axis text
  # ggsignif::geom_signif(test = "wilcox.test", # adds the significance test
  #                       comparisons = list(c("HV", "PI-ME/CFS")), # sets the comparisons
  #                       map_signif_level = function(p) sprintf("p = %.1g", p), # sets the significance level
  #                       color = "black",
  #                       textsize = 8) + # sets the color of the significance test 
  # ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 1.1E-12", size = 10))
# 
# fig2_SD1
# 
# ggsave(fig2_SD1, file = "Figure_2_SD1.png", width = 500, height = 700, dpi = 300, units = "px")  # saves the figure as a .png
# ggsave(fig2_SD1, file = "Figure_2_SD1.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px") # saves the figure as a .eps

data <- data %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    bin = row_number(),
    bin = as.factor(bin))

summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group))
summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group)$contrasts)$p.value

##### Figure 2 SD2 #####
# Analysis only for text, no figure

data <- readxl::read_excel("Figure 2 SD2-collapsed-group.xlsx", col_names = TRUE) %>% # reads in the .xlsx file
  janitor::clean_names() %>% # cleans the column names
  na.omit() # removes any rows with missing data

colnames(data) <- c("group", "var") # renames the columns in the data frame

# fig2_SD2 <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + # creates the ggplot object
#   geom_violin() + # creates the boxplot
#   geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
#   theme_classic() + # sets the theme
#   scale_color_manual(values = c("blue", "red")) + # sets the colors
#   labs(y = "SD2") + # sets the y-axis label
#   theme(legend.position = "none", # removes the legend
#         axis.title.x = element_blank(), # removes the x-axis label
#         axis.title.y = element_text(face = "bold", color = "black", size = 28), # sets the y-axis label
#         axis.text.x = element_text(face = "bold", color = "black", size = 22), # sets the x-axis text
#         axis.text.y = element_text(color = "black", size = 22)) + # sets the y-axis text
  # ggsignif::geom_signif(test = "wilcox.test", # adds the significance test
  #                       comparisons = list(c("HV", "PI-ME/CFS")), # sets the comparisons
  #                       map_signif_level = function(p) sprintf("p = %.1g", p), # sets the significance level
  #                       color = "black",
  #                       textsize = 8) + # sets the color of the significance test 
#   ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 2.1E-05", size = 10))
# 
# fig2_SD2
# 
# ggsave(fig2_SD1, file = "Figure_2_SD1.png", width = 500, height = 700, dpi = 300, units = "px")  # saves the figure as a .png
# ggsave(fig2_SD1, file = "Figure_2_SD1.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px") # saves the figure as a .eps


data <- data %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    bin = row_number(),
    bin = as.factor(bin))

summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group))
summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group)$contrasts)$p.value

##### Figure 2 SD1/SD2 #####
# Analysis only for text, no figure

data <- readxl::read_excel("Figure 2 SD1-SD2-collapsed-group.xlsx", col_names = TRUE) %>% # reads in the .xlsx file
  janitor::clean_names() %>% # cleans the column names
  na.omit() # removes any rows with missing data

colnames(data) <- c("group", "var") # renames the columns in the data frame

# fig2_SD1SD2 <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + # creates the ggplot object
#   geom_violin() + # creates the boxplot
#   geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
#   theme_classic() + # sets the theme
#   scale_color_manual(values = c("blue", "red")) + # sets the colors
#   labs(y = "SD1/SD2") + # sets the y-axis label
#   theme(legend.position = "none", # removes the legend
#         axis.title.x = element_blank(), # removes the x-axis label
#         axis.title.y = element_text(face = "bold", color = "black", size = 28), # sets the y-axis label
#         axis.text.x = element_text(face = "bold", color = "black", size = 22), # sets the x-axis text
#         axis.text.y = element_text(color = "black", size = 22)) + # sets the y-axis text
  # ggsignif::geom_signif(test = "wilcox.test", # adds the significance test
  #                       comparisons = list(c("HV", "PI-ME/CFS")), # sets the comparisons
  #                       map_signif_level = function(p) sprintf("p = %.1g", p), # sets the significance level
  #                       color = "black",
  #                       textsize = 8) + # sets the color of the significance test 
  # ggpp::geom_text_npc(aes(npcx = "right", npcy = "top", label = "p = 2.1E-05", size = 10))
# 
# fig2_SD1SD2
# 
# ggsave(fig2_SD1, file = "Figure_2_SD1.png", width = 500, height = 700, dpi = 300, units = "px")  # saves the figure as a .png
# ggsave(fig2_SD1, file = "Figure_2_SD1.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px") # saves the figure as a .eps

data <- data %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    bin = row_number(),
    bin = as.factor(bin))

summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group))
summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group)$contrasts)$p.value

##### Figure 2E #####

data <- readxl::read_excel("Figure 2E-collapsed-group.xlsx", col_names = TRUE) %>% # reads in the .xlsx file
  janitor::clean_names() %>% # cleans the column names
  na.omit() # removes any rows with missing data

colnames(data) <- c("group", "var") # renames the columns in the data frame

fig2e <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + # creates the ggplot object
  geom_violin() + # creates the boxplot
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.25) +
  theme_classic() + # sets the theme
  scale_color_manual(values = c("blue", "red")) + # sets the colors
  labs(y = "lnHF power") + # sets the y-axis label
  theme(legend.position = "none", # removes the legend
        axis.title.x = element_blank(), # removes the x-axis label
        axis.title.y = element_text(face = "bold", color = "black", size = 28), # sets the y-axis label
        axis.text.x = element_text(face = "bold", color = "black", size = 22), # sets the x-axis text
        axis.text.y = element_text(color = "black", size = 22)) + # sets the y-axis text
  # ggsignif::geom_signif(test = "wilcox.test", # adds the significance test
  #                       comparisons = list(c("HV", "PI-ME/CFS")), # sets the comparisons
  #                       map_signif_level = function(p) sprintf("p = %.1g", p), # sets the significance level
  #                       color = "black",
  #                       textsize = 8) + # sets the color of the significance test 
  ggpp::geom_text_npc(aes(npcx = "center", npcy = "top", label = "p = 8.2E-127", size = 10))

fig2e

ggsave(fig2e, file = "Figure_2E.png", width = 500, height = 700, dpi = 300, units = "px")  # saves the figure as a .png
ggsave(fig2e, file = "Figure_2E.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px") # saves the figure as a .eps

data <- data %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    bin = row_number(),
    bin = as.factor(bin))

summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group))
summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group)$contrasts)$p.value

##### Figure 2F #####

data <- readxl::read_excel("Figure 2F-collapsed-group.xlsx", col_names = TRUE) %>% # reads in the .xlsx file
  janitor::clean_names() %>% # cleans the column names
  na.omit() # removes any rows with missing data

colnames(data) <- c("group", "var") # renames the columns in the data frame

fig2f <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + # creates the ggplot object
  geom_violin() + # creates the boxplot
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.25) +
  theme_classic() + # sets the theme
  scale_color_manual(values = c("blue", "red")) + # sets the colors
  labs(y = "lnLF power") + # sets the y-axis label
  theme(legend.position = "none", # removes the legend
        axis.title.x = element_blank(), # removes the x-axis label
        axis.title.y = element_text(face = "bold", color = "black", size = 28), # sets the y-axis label
        axis.text.x = element_text(face = "bold", color = "black", size = 22), # sets the x-axis text
        axis.text.y = element_text(color = "black", size = 22)) + # sets the y-axis text
  # ggsignif::geom_signif(test = "wilcox.test", # adds the significance test
  #                       comparisons = list(c("HV", "PI-ME/CFS")), # sets the comparisons
  #                       map_signif_level = function(p) sprintf("p = %.1g", p), # sets the significance level
  #                       color = "black",
  #                       textsize = 8) + # sets the color of the significance test 
  ggpp::geom_text_npc(aes(npcx = "center", npcy = "top", label = "p = 1.1E-87", size = 10))

fig2f

ggsave(fig2f, file = "Figure_2F.png", width = 500, height = 700, dpi = 300, units = "px")  # saves the figure as a .png
ggsave(fig2f, file = "Figure_2F.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px") # saves the figure as a .eps


data <- data %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    bin = row_number(),
    bin = as.factor(bin))

summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group))
summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group)$contrasts)$p.value

##### Figure 2G #####
# Analysis only, no figure regeneration

data <- readxl::read_excel("Figure 2G-collapsed-group.xlsx", col_names = TRUE) %>% # reads in the .xlsx file
  janitor::clean_names() %>% # cleans the column names
  na.omit() # removes any rows with missing data

colnames(data) <- c("group", "bin", "var") # renames the columns in the data frame

summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group))
summary(lsmeans::lsmeans(lm(var ~ group + bin, data = data), pairwise ~ group)$contrasts)$p.value

##### Figure 2H #####

data <- readxl::read_excel("Figure 2H.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig2h <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Baroslope (ms/Hg)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig2h

ggsave(fig2h, file = "Figure_2H.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig2h, file = "Figure_2H.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")


##### Figure 3C #####

data <- readxl::read_excel("Figure 3C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig3c <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Maximum Grip Force (kg)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.1g", p),
#                       color = "black",                         textsize = 8)

fig3c

ggsave(fig3c, file = "Figure_3C.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig3c, file = "Figure_3C.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 3D #####

data <- readxl::read_excel("Figure 3D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig3d <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Time to Failure (seconds)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig3d

ggsave(fig3d, file = "Figure_3D.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig3d, file = "Figure_3D.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 3E #####

data <- readxl::read_excel("Figure 3E.xlsx", col_names = TRUE) %>% # read in data
  janitor::clean_names() %>% # clean column names
  na.omit() # remove NA values

colnames(data) <- c("group", "xvar", "yvar") # rename columns

fig3e <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) + # create plot
  geom_point(size = 1) + # add points
  geom_smooth(method= "lm", se = FALSE) + # add regression line
  ggpmisc::stat_correlation(exact = TRUE, # add correlation
                            label.x = "right", # set label position
                            label.y = "bottom", # set label position
                            r.digits = 2, # set digits
                            p.digits = 2, # set digits
                            small.p = TRUE, # set small p,
                            size = 6,
                            use_label(c("R", "P", "n"))) + # set label
  theme_classic() + # set theme
  scale_color_manual(values = c("blue", "red")) + # set colors
  labs(y = "Proportion of Hard-Task Choices (%)", # set y label
       x = "Time to Failure (seconds)") + # set x label
  theme(legend.title = element_blank(), legend.position = "none", # remove legend title
        axis.title.x = element_text(face = "bold", color = "black", size = 18), # set x axis title
        axis.title.y = element_text(face = "bold", color = "black", size = 18), # set y axis title
        axis.text.x = element_text(color = "black", size = 15), # set x axis text
        axis.text.y = element_text(color = "black", size = 15),
        legend.text=element_text(size=12)) # set y axis text

fig3e

ggsave(fig3e, file = "Figure_3E.png", width = 1600, height = 1600, dpi = 300, units = "px") # save figure as .png
ggsave(fig3e, file = "Figure_3E.eps", width = 1600, height = 1600, dpi = 300, units = "px", device = "eps") # save figure as .eps

##### Figure 5C #####

data <- readxl::read_excel("Figure 5C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig5c <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Peak Heart Rate (bpm)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.1g", p),
#                       color = "black",                         textsize = 8)

fig5c

ggsave(fig5c, file = "Figure_5C.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig5c, file = "Figure_5C.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 5D #####

data <- readxl::read_excel("Figure 5D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig5d <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Peak VO2 (mL/kg/min)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig5d

ggsave(fig5d, file = "Figure_5D.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig5d, file = "Figure_5D.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 5E #####

data <- readxl::read_excel("Figure 5E.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig5e <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Percent of Predicted VO2peak (%)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig5e

ggsave(fig5e, file = "Figure_5E.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig5e, file = "Figure_5E.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 5F #####

data <- readxl::read_excel("Figure 5F.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig5f <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Heart Rate Reserve (peak-resting; bpm)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 24),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig5f

ggsave(fig5f, file = "Figure_5F.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig5f, file = "Figure_5F.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 5H #####

data <- readxl::read_excel("Figure 5H.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig5h <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "VO2 at Anerobic Threshold \n (mL/kg/min)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26, lineheight = 0.35),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig5h

ggsave(fig5h, file = "Figure_5H.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig5h, file = "Figure_5H.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 6A #####

data <- readxl::read_excel("Figure 6A.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig6a <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Cerebrospinal Fluid DOPA (pg/mL)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig6a

ggsave(fig6a, file = "Figure_6A.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig6a, file = "Figure_6A.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 6B #####

data <- readxl::read_excel("Figure 6B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig6b <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Cerebrospinal Fluid DOPAC (pg/mL)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig6b

ggsave(fig6b, file = "Figure_6B.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig6b, file = "Figure_6B.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 6C #####

data <- readxl::read_excel("Figure 6C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig6c <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Cerebrospinal Fluid DHPG (pg/mL)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig6c

ggsave(fig6c, file = "Figure_6C.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig6c, file = "Figure_6C.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 6D #####

data <- readxl::read_excel("Figure 6D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "xvar", "yvar")

fig6d <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Time to Failure (seconds)",
       x = "Cerebrospinal fluid NE (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

fig6d

ggsave(fig6d, file = "Figure_6D.png", width = 800, height = 800, dpi = 300, units = "px")  
ggsave(fig6d, file = "Figure_6D.eps", width = 800, height = 800, dpi = 300, units = "px", device = "eps")

##### Figure 6E #####

data <- readxl::read_excel("Figure 6E.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "xvar", "yvar")

fig6e <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Proportion Hard Trials (%)",
       x = "Cerebrospinal fluid NE (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",


fig6e

ggsave(fig6e, file = "Figure_6E.png", width = 800, height = 800, dpi = 300, units = "px")  
ggsave(fig6e, file = "Figure_6E.eps", width = 800, height = 800, dpi = 300, units = "px", device = "eps")

##### Figure 6F #####

data <- readxl::read_excel("Figure 6F.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "xvar", "yvar")

fig6f <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Time to Failure (seconds)",
       x = "Cerebrospinal fluid DA (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",


fig6f

ggsave(fig6f, file = "Figure_6F.png", width = 800, height = 800, dpi = 300, units = "px")  
ggsave(fig6f, file = "Figure_6F.eps", width = 800, height = 800, dpi = 300, units = "px", device = "eps")

##### Figure 6G #####

data <- readxl::read_excel("Figure 6G.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "xvar", "yvar")

fig6g <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Proportion Hard Trials (%)",
       x = "Cerebrospinal fluid DHPG (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",


fig6g

ggsave(fig6g, file = "Figure_6G.png", width = 800, height = 800, dpi = 300, units = "px")  
ggsave(fig6g, file = "Figure_6G.eps", width = 800, height = 800, dpi = 300, units = "px", device = "eps")

##### Figure 7A #####

data <- readxl::read_excel("Figure 7A.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig7a <- ggplot2::ggplot(data, aes(x = group, y = var, color = group, fill = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = alpha(c("blue", "red"), .35)) +
  labs(y = "B cell subset Naive (%)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig7a

ggsave(fig7a, file = "Figure_7A.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig7a, file = "Figure_7A.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 7B #####

data <- readxl::read_excel("Figure 7B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig7b <- ggplot2::ggplot(data, aes(x = group, y = var, color = group, fill = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = alpha(c("blue", "red"), .35)) +
  labs(y = "B cell switched memory (%)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig7b

ggsave(fig7b, file = "Figure_7B.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig7b, file = "Figure_7B.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 7C #####

data <- readxl::read_excel("Figure 7C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig7c <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "CD8+ T cell subset PD-1 (%)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)
fig7c

ggsave(fig7c, file = "Figure_7C.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig7c, file = "Figure_7C.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 7D #####

data <- readxl::read_excel("Figure 7D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig7d <- ggplot2::ggplot(data, aes(x = group, y = var, color = group, fill = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = alpha(c("blue", "red"), .35)) +
  labs(y = "CD8+ T cell subset CD226 (%)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

fig7d

ggsave(fig7d, file = "Figure_7D.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig7d, file = "Figure_7D.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 7E #####

data <- readxl::read_excel("Figure 7E.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig7e <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "CD8+ T cell CXCR5 (%)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "bottom", label = male), size = 10, angle = -45)

fig7e

ggsave(fig7e, file = "Figure_7E.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig7e, file = "Figure_7E.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure 7f #####

data <- readxl::read_excel("Figure 7f.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

fig7f <- ggplot2::ggplot(data, aes(x = group, y = var, color = group, fill = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = alpha(c("blue", "red"), .35)) +
  labs(y = "CD8+ T cell Naive (%)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8) +
  ggpp::geom_text_npc(aes(npcx = "right", npcy = "bottom", label = female), size = 10, angle = 0)

fig7f

ggsave(fig7f, file = "Figure_7F.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(fig7f, file = "Figure_7F.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

###### SUPPLEMENTARY FIGURES #####

##### Figure S1A #####

data <- readxl::read_excel("Figure S1A.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1a <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "IENFD Distal Leg (fibers/mm)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1a

ggsave(figS1a, file = "Figure_S1A.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1a, file = "Figure_S1A.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1B #####

data <- readxl::read_excel("Figure S1B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1b <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "IENFD Distal Thigh (fibers/mm)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1b

ggsave(figS1b, file = "Figure_S1B.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1b, file = "Figure_S1B.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1C #####

data <- readxl::read_excel("Figure S1C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1c <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "SGNFD Distal Leg (fibers/mm)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1c

ggsave(figS1c, file = "Figure_S1C.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1c, file = "Figure_S1C.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1D #####

data <- readxl::read_excel("Figure S1D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1d <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "SGNFD Distal Thigh (fibers/mm)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1d

ggsave(figS1c, file = "Figure_S1C.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1c, file = "Figure_S1C.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1E #####

data <- readxl::read_excel("Figure S1E.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1e <- ggplot2::ggplot(data, aes(x = group, y = var, color = group, fill = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = alpha(c("blue", "red"), .35)) +
  labs(y = "Tau (pg/mL)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1e

ggsave(figS1e, file = "Figure_S1E.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1e, file = "Figure_S1E.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1F #####

data <- readxl::read_excel("Figure S1F.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1f <- ggplot2::ggplot(data, aes(x = group, y = var, color = group, fill = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = alpha(c("blue", "red"), .35)) +
  labs(y = "NfL (pg/mL)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1f

ggsave(figS1f, file = "Figure_S1F.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1f, file = "Figure_S1F.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1G #####

data <- readxl::read_excel("Figure S1G.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1g <- ggplot2::ggplot(data, aes(x = group, y = var, color = group, fill = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = alpha(c("blue", "red"), .35)) +
  labs(y = "GFAP (pg/mL)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1g

ggsave(figS1g, file = "Figure_S1G.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1g, file = "Figure_S1G.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1H #####

data <- readxl::read_excel("Figure S1H.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1h <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Tau (pg/mL)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1h

ggsave(figS1h, file = "Figure_S1H.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1h, file = "Figure_S1H.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1I #####

data <- readxl::read_excel("Figure S1I.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1i <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "NfL (pg/mL)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1i

ggsave(figS1i, file = "Figure_S1I.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1i, file = "Figure_S1I.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1J #####

data <- readxl::read_excel("Figure S1J.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1j <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "GFAP (pg/mL)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1j

ggsave(figS1i, file = "Figure_S1I.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1i, file = "Figure_S1I.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1K #####

data <- readxl::read_excel("Figure S1K.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS1k <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "UCHL1 (pg/mL)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) # +
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS1k

ggsave(figS1k, file = "Figure_S1K.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS1k, file = "Figure_S1K.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S1 #####

figS1 <- (figS1a + figS1b + figS1c + figS1d + figS1e + figS1f + figS1g + figS1h + figS1i + figS1j + figS1k) + 
  plot_layout(guides = "collect", ncol = 4) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 36, face = 'bold')) 

ggsave(figS1, file = "Figure_S1.png", width = 7, height = 7, dpi = 300, units = "in")  
ggsave(figS1, file = "Figure_S1.eps", width = 7, height = 7, dpi = 300, device = "eps", units= "in")

##### Figure S2A #####

data <- readxl::read_excel("Figure S2A.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS2a <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Lean Body Mass (kg)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS2a

ggsave(figS2a, file = "Figure_S2A.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS2a, file = "Figure_S2A.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S2B #####

data <- readxl::read_excel("Figure S2B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS2b <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Fat Mass (kg)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS2b

ggsave(figS2b, file = "Figure_S2B.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS2b, file = "Figure_S2B.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S2C #####

data <- readxl::read_excel("Figure S2C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS2c <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Bone Mineral Content (kg)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS2c

ggsave(figS2c, file = "Figure_S2C.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS2c, file = "Figure_S2C.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S2D #####

data <- readxl::read_excel("Figure S2D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS2d <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Body Fat (%)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS2d

ggsave(figS2d, file = "Figure_S2D.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS2d, file = "Figure_S2D.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S2E #####

data <- readxl::read_excel("Figure S2E.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS2e <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Visceral Fat Mass (g)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS2e

ggsave(figS2e, file = "Figure_S2E.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS2e, file = "Figure_S2E.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")


##### Figure S2F #####

data <- readxl::read_excel("Figure S2F.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS2f <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Dominant Arm Lean Mass (kg)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS2f

ggsave(figS2f, file = "Figure_S2F.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS2f, file = "Figure_S2F.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S2G #####

data <- readxl::read_excel("Figure S2G.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS2g <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Legs Lean Mass (kg)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS2g

ggsave(figS2g, file = "Figure_S2G.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS2g, file = "Figure_S2G.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")


##### Figure S2 #####

figS2 <- (figS2a + figS2b + figS2c + figS2d + figS2e + figS2f + figS2g) +
  plot_layout(guides = "collect", ncol = 4) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 36, face = 'bold')) 

ggsave(figS2, file = "Figure_S2.png", width = 6, height = 6, dpi = 300, units = "in")  
ggsave(figS2, file = "Figure_S2.eps", width = 6, height = 6, dpi = 300, device = "eps", units= "in")

##### Figure S3A #####

data <- readxl::read_excel("Figure S3A.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS3a <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Basal respiration (pmol/min)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS3a

ggsave(figS3a, file = "Figure_S3A.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS3a, file = "Figure_S3A.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")


##### Figure S3B #####

data <- readxl::read_excel("Figure S3B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS3b <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Maximal respiration (pmol/min)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS3b

ggsave(figS3b, file = "Figure_S3B.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS3b, file = "Figure_S3B.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")


##### Figure S3C #####

data <- readxl::read_excel("Figure S3C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS3c <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Non-mitochondrial \n respiration (pmol/min)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26, lineheight = 0.35),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS3c

ggsave(figS3c, file = "Figure_S3C.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS3c, file = "Figure_S3C.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")


##### Figure S3D #####

data <- readxl::read_excel("Figure S3D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS3d <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Type 2:Type 1 muscle fiber \n median Feret diameter ratio") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26, lineheight = 0.35),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)

figS3d

ggsave(figS3d, file = "Figure_S3D.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS3d, file = "Figure_S3D.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S3 #####

figS3 <- (figS3a + figS3b + figS3c + figS3d) +
  plot_layout(guides = "collect", ncol = 2) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 36, face = 'bold')) 

ggsave(figS3, file = "Figure_S3.png", width = 5, height = 5, dpi = 300, units = "in")  
ggsave(figS3, file = "Figure_S3.eps", width = 5, height = 5, dpi = 300, device = "eps", units= "in")

##### Figure S6A #####

data <- readxl::read_excel("Figure S6A.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS6a <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "left",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 6,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Maximum Grip Force (kg)",
       x = "Dominant Arm Lean Mass (kg)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 26),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(color = "black", size = 22),
        axis.text.y = element_text(color = "black", size = 22)) #legend.position = "none",

figS6a

ggsave(figS6a, file = "Figure_S6A.png", width = 1600, height = 1600, dpi = 300, units = "px")  
ggsave(figS6a, file = "Figure_S6A.eps", width = 1600, height = 1600, dpi = 300, units = "px", device = "eps")

##### Figure S6B #####

data <- readxl::read_excel("Figure S6B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS6b <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Maximum Grip Force (kg)",
       x = "Proportion Hard-Task Choices (%)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 26),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(color = "black", size = 22),
        axis.text.y = element_text(color = "black", size = 22)) #legend.position = "non

figS6b

ggsave(figS6b, file = "Figure_S6B.png", width = 1600, height = 1600, dpi = 300, units = "px")  
ggsave(figS6b, file = "Figure_S6B.eps", width = 1600, height = 1600, dpi = 300, units = "px", device = "eps")

##### Figure S6C #####

data <- readxl::read_excel("Figure S6C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS6c <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Maximum Grip Force (kg)",
       x = "Type2:Type1 muscle fiber \n median Feret diameter ratio") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 26, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(color = "black", size = 22),
        axis.text.y = element_text(color = "black", size = 22)) #legend.position = "non

figS6c

ggsave(figS6c, file = "Figure_S6C.png", width = 1600, height = 1600, dpi = 300, units = "px")  
ggsave(figS6c, file = "Figure_S6C.eps", width = 1600, height = 1600, dpi = 300, units = "px", device = "eps")

##### Figure S6D #####

data <- readxl::read_excel("Figure S6D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS6d <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "left",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Time to Failure (seconds)",
       x = "Dominant Arm Lean Mass (kg)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 26),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(color = "black", size = 22),
        axis.text.y = element_text(color = "black", size = 22)) #legend.position = "non

figS6d

ggsave(figS6d, file = "Figure_S6D.png", width = 1600, height = 1600, dpi = 300, units = "px")  
ggsave(figS6d, file = "Figure_S6D.eps", width = 1600, height = 1600, dpi = 300, units = "px", device = "eps")

##### Figure S6E #####

data <- readxl::read_excel("Figure S6E.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS6e <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Time to Failure (seconds)",
       x = "Proportion Hard-Task Choices (%)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 26),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(color = "black", size = 22),
        axis.text.y = element_text(color = "black", size = 22)) #legend.position = "non

figS6e

ggsave(figS6e, file = "Figure_S6E.png", width = 1600, height = 1600, dpi = 300, units = "px")  
ggsave(figS6e, file = "Figure_S6E.eps", width = 1600, height = 1600, dpi = 300, units = "px", device = "eps")

##### Figure S6F #####

data <- readxl::read_excel("Figure S6F.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS6f <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Time to Failure (seconds)",
       x = "Type2:Type1 muscle fiber \n median Feret diameter ratio") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 26, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(color = "black", size = 22),
        axis.text.y = element_text(color = "black", size = 22)) #legend.position = "non

figS6f

ggsave(figS6f, file = "Figure_S6F.png", width = 1600, height = 1600, dpi = 300, units = "px")  
ggsave(figS6f, file = "Figure_S6F.eps", width = 1600, height = 1600, dpi = 300, units = "px", device = "eps")

##### Figure S6 #####

figS6 <- ((figS6a | figS6b | figS6c) / (figS6d | figS6e | figS6f)) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 36, face = 'bold')) 

ggsave(figS6, file = "Figure S6.png", width = 8, height = 8, dpi = 300, units = "in")
ggsave(figS6, file = "Figure S6.eps", width = 8, height = 8, dpi = 300, device = "eps", units = "in")

##### Figure S7B #####

data <- readxl::read_excel("Figure S7B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS7b <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size= 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = expression(paste("Total Motor Output  ", 10^{5})),
       x = "Proportion Hard-Task Choices (%)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

figS7b

ggsave(figS7b, file = "Figure_S7B.png", width = 700, height = 700, dpi = 300, units = "px")  
ggsave(figS7b, file = "Figure_S7B.eps", width = 700, height = 700, dpi = 300, units = "px", device = "eps")

##### Figure S7C #####

data <- readxl::read_excel("Figure S7C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS7c <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            size = 6,
                            small.p = TRUE,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = expression(paste("Total Motor Output  ", 10^{5})),
       x = "Dominant Arm Lean Mass (kg)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

figS7c

ggsave(figS7c, file = "Figure_S7C.png", width = 700, height = 700, dpi = 300, units = "px")  
ggsave(figS7c, file = "Figure_S7C.eps", width = 700, height = 700, dpi = 300, units = "px", device = "eps")

##### Figure S7D #####

data <- readxl::read_excel("Figure S7D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS7d <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = expression(paste("Total Motor Output  ", 10^{5})),
       x = "Type2:Type1 muscle fiber \n median Feret diameter ratio") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

figS7d

ggsave(figS7d, file = "Figure_S7D.png", width = 700, height = 700, dpi = 300, units = "px")  
ggsave(figS7d, file = "Figure_S7D.eps", width = 700, height = 700, dpi = 300, units = "px", device = "eps")

##### Figure S8B #####

data <- readxl::read_excel("Figure S8B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS8b <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "middle",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 5,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Peak Power (W)",
       x = "Proportion Hard-Task Choices (%)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

figS8b

ggsave(figS8b, file = "Figure_S8B.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS8b, file = "Figure_S8B.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S8F #####

data <- readxl::read_excel("Figure S8F.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS8f <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) + 
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Peak Heart Rate \n (% age-predicted max heart rate)") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26, lineheight = 0.35),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.2g", p),
                        color = "black",                         textsize = 8)

figS8f

ggsave(figS8f, file = "Figure_S8F.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS8f, file = "Figure_S8F.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")

##### Figure S8G #####

data <- readxl::read_excel("Figure S8G.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS8g <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 3,
                            small.p = TRUE,
                            size = 5,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Peak Power (W)",
       x = "Legs Lean Mass (kg)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

figS8g

ggsave(figS8g, file = "Figure_S8G.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS8g, file = "Figure_S8G.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S8H #####

data <- readxl::read_excel("Figure S8H.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS8h <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 5,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Peak Power (W)",
       x = "Type2:Type1 muscle fiber \n median Feret diameter ratio") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

figS8h

ggsave(figS8h, file = "Figure_S8H.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS8h, file = "Figure_S8H.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S8I #####

data <- readxl::read_excel("Figure S8I.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS8i <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 5,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Peak VO2 (mL/kg/min)",
       x = "Type2:Type1 muscle fiber \n median Feret diameter ratio") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

figS8i

ggsave(figS8i, file = "Figure_S8I.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS8i, file = "Figure_S8I.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S8J #####

data <- readxl::read_excel("Figure S8J.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS8j <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 5,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "VO2 (mL/kg/min) \n at Anerobic Threshold (W)",
       x = "Type2:Type1 muscle fiber \n median Feret diameter ratio") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

figS8j

ggsave(figS8j, file = "Figure_S8J.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS8j, file = "Figure_S8J.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S8K #####

data <- readxl::read_excel("Figure S8K.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS8k <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 5,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Peak Power \n at Anerobic Threshold (W)",
       x = "Type2:Type1 muscle fiber \n median Feret diameter ratio") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",",

figS8k

ggsave(figS8k, file = "Figure_S8K.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS8k, file = "Figure_S8K.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S10A #####

data <- readxl::read_excel("Figure S10A.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "xvar", "yvar")

data2 <- data %>% dplyr::group_by(group, xvar) %>%
  dplyr::summarise(mean = mean(yvar),
                   sd = sd(yvar))

figS10a <- ggplot2::ggplot(data2, aes(x = xvar, y = mean, color = group, group = group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Mental Fatigue",
       x = "Time Points") +
  theme(legend.title = element_blank(),
        # legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

figS10a

ggsave(figS10a, file = "Figure_S10A.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS10a, file = "Figure_S10A.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S10B #####

data <- readxl::read_excel("Figure S10B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "xvar", "yvar")

data2 <- data %>% dplyr::group_by(group, xvar) %>%
  dplyr::summarise(mean = mean(yvar),
                   sd = sd(yvar))

figS10b <- ggplot2::ggplot(data2, aes(x = xvar, y = mean, color = group, group = group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Physical Fatigue",
       x = "Time Points") +
  theme(legend.title = element_blank(),
        # legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

figS10b

ggsave(figS10b, file = "Figure_S10B.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS10b, file = "Figure_S10B.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S10C #####

data <- readxl::read_excel("Figure S10C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "xvar", "yvar")

data2 <- data %>% dplyr::group_by(group, xvar) %>%
  dplyr::summarise(mean = mean(yvar),
                   sd = sd(yvar))

figS10c <- ggplot2::ggplot(data2, aes(x = xvar, y = mean, color = group, group = group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Effort",
       x = "Time Points") +
  theme(legend.title = element_blank(),
        # legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

figS10c

ggsave(figS10c, file = "Figure_S10C.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS10c, file = "Figure_S10C.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S10D #####

data <- readxl::read_excel("Figure S10D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "xvar", "yvar")

data2 <- data %>% dplyr::group_by(group, xvar) %>%
  dplyr::summarise(mean = mean(yvar),
                   sd = sd(yvar))

figS10d <- ggplot2::ggplot(data2, aes(x = xvar, y = mean, color = group, group = group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Performance",
       x = "Time Points") +
  theme(legend.title = element_blank(),
        # legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

figS10d

ggsave(figS10d, file = "Figure_S10D.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS10d, file = "Figure_S10D.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")


##### Figure S10 #####

figS10 <- (figS10a + figS10b + figS10c + figS10d) + 
  plot_layout(guides = "collect", ncol = 2) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 36, face = 'bold')) 

ggsave(figS10, file = "Figure_S10.png", width = 11, height = 10, dpi = 300, units = "in")  
ggsave(figS10, file = "Figure_S10.eps", width = 11, height = 10, dpi = 300, device = "eps", units= "in")

##### Figure S11B #####

data <- readxl::read_excel("Figure S11B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS11b <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Proportion Hard-Task Choices (%)",
       x = "Cerebrospinal fluid \n DA (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

figS11b

ggsave(figS11b, file = "Figure_S11B.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS11b, file = "Figure_S11B.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S11C #####

data <- readxl::read_excel("Figure S11C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS11c <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Time to Failure (seconds)",
       x = "Cerebrospinal fluid \n DHPG (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 22, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 22),
        axis.text.x = element_text(color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18)) #legend.position = "none",

figS11c

ggsave(figS11c, file = "Figure_S11C.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS11c, file = "Figure_S11C.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12A #####

data <- readxl::read_excel("Figure S12A.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12a <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            method = "spearman",
                            label.x = "left",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "MMPI Cognitive \n Complaints Score",
       x = "Cerebrospinal fluid \n NE (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12a

ggsave(figS12a, file = "Figure_S12A.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12a, file = "Figure_S12A.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12B #####

data <- readxl::read_excel("Figure S12B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12b <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            method = "spearman",
                            label.x = "right",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "MASQ Visual-Spatial \n Memory Score",
       x = "Cerebrospinal fluid \n DA (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12b

ggsave(figS12b, file = "Figure_S12B.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12b, file = "Figure_S12B.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12C #####

data <- readxl::read_excel("Figure S12C.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12c <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "left",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 3,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "MMPI Cognitive \n Complaints Score",
       x = "Cerebrospinal fluid \n cys-DOPA (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12c

ggsave(figS12c, file = "Figure_S12C.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12c, file = "Figure_S12C.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12D #####

data <- readxl::read_excel("Figure S12D.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12d <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "left",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "MASQ Language Score",
       x = "Cerebrospinal fluid \n cys-DOPA (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12d

ggsave(figS12d, file = "Figure_S12D.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12d, file = "Figure_S12D.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12E #####

data <- readxl::read_excel("Figure S12E.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12e <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "left",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "MASQ Visuoperception Score",
       x = "Cerebrospinal fluid \n cys-DOPA (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12e

ggsave(figS12e, file = "Figure_S12E.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12e, file = "Figure_S12E.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12F #####

data <- readxl::read_excel("Figure S12F.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12f <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            method = "spearman",
                            label.x = "right",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "MMPI Cognitive \n Complaints Score",
       x = "Cerebrospinal fluid \n DOPAC (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12f

ggsave(figS12f, file = "Figure_S12F.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12f, file = "Figure_S12F.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12G #####

data <- readxl::read_excel("Figure S12G.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12g <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            method = "spearman",
                            label.x = "right",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "MASQ Attention/Concentration Score",
       x = "Cerebrospinal fluid \n DOPAC (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12g

ggsave(figS12g, file = "Figure_S12G.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12g, file = "Figure_S12G.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12H #####

data <- readxl::read_excel("Figure S12H.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12h <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "MMPI Cognitive \n Complaints Score",
       x = "Cerebrospinal fluid \n DHPG (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12h

ggsave(figS12h, file = "Figure_S12H.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12h, file = "Figure_S12H.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")


##### Figure S12I #####

data <- readxl::read_excel("Figure S12I.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12i <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "MASQ Visuoperception Score",
       x = "Cerebrospinal fluid \n DHPG (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12i

ggsave(figS12i, file = "Figure_S12I.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12i, file = "Figure_S12I.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12J #####

data <- readxl::read_excel("Figure S12J.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12j <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "left",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Wisconsin Card Sort \n Test Score",
       x = "Cerebrospinal fluid \n DOPA (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12j

ggsave(figS12j, file = "Figure_S12J.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12j, file = "Figure_S12J.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12K #####

data <- readxl::read_excel("Figure S12K.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12k <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "top",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "WAIS Symbol Search Scale",
       x = "Cerebrospinal fluid \n NE (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12k

ggsave(figS12k, file = "Figure_S12K.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12k, file = "Figure_S12K.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12L #####

data <- readxl::read_excel("Figure S12L.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12l <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Brief Visual Memory Test \n Revised Recall Score",
       x = "Cerebrospinal fluid \n DOPA (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",


figS12l

ggsave(figS12l, file = "Figure_S12L.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12l, file = "Figure_S12L.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12M #####

data <- readxl::read_excel("Figure S12M.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12m <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "HVLT - Revised Total Score",
       x = "Cerebrospinal fluid \n DOPA (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",


figS12m

ggsave(figS12m, file = "Figure_S12M.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12m, file = "Figure_S12M.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12N #####

data <- readxl::read_excel("Figure S12N.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12n <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "WAIS Coding Scale",
       x = "Cerebrospinal fluid \n cys-DA (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",


figS12n

ggsave(figS12n, file = "Figure_S12N.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12n, file = "Figure_S12N.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12O #####

data <- readxl::read_excel("Figure S12O.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "yvar", "xvar")

figS12o <- ggplot2::ggplot(data, aes(x = xvar, y = yvar, color = group)) +
  geom_point(size = 1) +
  geom_smooth(method= "lm", se = FALSE) +
  ggpmisc::stat_correlation(exact = TRUE,
                            method = "spearman",
                            label.x = "right",
                            label.y = "bottom",
                            r.digits = 2,
                            p.digits = 2,
                            small.p = TRUE,
                            size = 6,
                            use_label(c("R", "P", "n"))) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "WAIS Coding Scale",
       x = "Cerebrospinal fluid \n DOPAC (pg/mL)") +
  theme(legend.title = element_blank(), legend.position = "none",
        axis.title.x = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.title.y = element_text(face = "bold", color = "black", size = 24, lineheight = 0.35),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #legend.position = "none",

figS12o

ggsave(figS12o, file = "Figure_S12O.png", width = 600, height = 600, dpi = 300, units = "px")  
ggsave(figS12o, file = "Figure_S12O.eps", width = 600, height = 600, dpi = 300, units = "px", device = "eps")

##### Figure S12 #####

figS12 <- (figS12a + figS12b + figS12c + figS12d + figS12e + figS12f + figS12g + figS12h + figS12i + figS12j + figS12k + figS12l + figS12m + figS12n + figS12o) + 
  plot_layout(guides = "collect", ncol = 3) +
  plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 36, face = 'bold')) 

ggsave(figS12, file = "Figure_S12.png", width = 8, height = 11, dpi = 300)  
ggsave(figS12, file = "Figure_S12.eps", width = 8, height = 11, dpi = 300, device = "eps")

##### Male Plasma - S15 B ##### 

VIPmerge <- readxl::read_excel("Figure S15B_VIP_plasma_M.xlsx", col_names = TRUE) %>%
  janitor::clean_names()
VIPmerge <- VIPmerge[!is.na(VIPmerge$target),]

VIPres_names <- VIPmerge[order(VIPmerge$vip, decreasing = T),]
VIPres_names <- VIPres_names[order(VIPres_names$vip, decreasing = T),]
Data <- readxl::read_excel("Figure S15B_plasma_M.xlsx", col_names = TRUE) %>%
  janitor::clean_names()

Data$Groups <- as.factor(Data$grp)
levels(Data$Groups) <- c("HV", "PI-ME/CFS")

top4 <- VIPres_names[1:4,] 
top4$row_names[1]

p1 <- ggplot(Data, aes(x = Groups, y = x3580_25_5, color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[1])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[2]

p2 <- ggplot(Data, aes(x = Groups, y = x5934_1_3,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[2])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[3]

p3 <- ggplot(Data, aes(x = Groups, y = x2700_56_2,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[3])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[4]

p4 <- ggplot(Data, aes(x = Groups, y = x4991_12_1,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[4])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))


S15B <- (p1 | p2 | p3 | p4)

ggsave(S15B, file = "Figure_S15B.png", width = 7, height = 3, dpi = 300)  
ggsave(S15B, file = "Figure_S15B.eps", width = 7, height = 3, dpi = 300, device = "eps")

##### S15D Male CSF ##### 

VIPmerge<-readxl::read_excel("Figure S15D_VIP_CSF_M.xlsx", col_names = TRUE) %>%
  janitor::clean_names()
VIPmerge<-VIPmerge[!is.na(VIPmerge$target),]

VIPres_names <- VIPmerge[order(VIPmerge$vip, decreasing = T),]
VIPres_names <- VIPres_names[order(VIPres_names$vip, decreasing = T),]
Data <- readxl::read_excel("Figure S15D_CSF_M.xlsx", col_names = TRUE) %>% 
  janitor::clean_names()

Data$Groups <- as.factor(Data$grp)
levels(Data$Groups) <- c("HV", "PI-ME/CFS")

top4 <- VIPres_names[1:4,] 
top4$row_names[1]

p1 <- ggplot(Data, aes(x = Groups, y = x4134_4_2,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[1])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[2]

p2 <- ggplot(Data, aes(x = Groups, y = x5066_134_3,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[2])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[3]

p3 <- ggplot(Data, aes(x = Groups, y = x4278_14_3,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[3])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[4]

p4 <- ggplot(Data, aes(x = Groups, y = x5250_53_3,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[4])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

S15D <- (p1 | p2 | p3 | p4)

ggsave(S15D, file = "Figure_S15D.png", width = 7, height = 3, dpi = 300)  
ggsave(S15D, file = "Figure_S15D.eps", width = 7, height = 3, dpi = 300, device = "eps")

##### S16B Female Plasma ##### 

VIPmerge <- readxl::read_excel("Figure S16B_VIP_plasma_F.xlsx", col_names = TRUE) %>%
  janitor::clean_names()
VIPmerge <- VIPmerge[!is.na(VIPmerge$target),]

VIPres_names <- VIPmerge[order(VIPmerge$vip, decreasing = T),]
VIPres_names <- VIPres_names[order(VIPres_names$vip, decreasing = T),]
Data <- readxl::read_excel("Figure S16B_plasma_F.xlsx", col_names = TRUE) %>%
  janitor::clean_names()

Data$Groups <- as.factor(Data$grp)
levels(Data$Groups) <- c("HV", "PI-ME/CFS")

top4 <- VIPres_names[1:4,] 
top4$row_names[1]

p1 <- ggplot(Data, aes(x = Groups, y = x4151_6_2,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[1])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[2]

p2 <- ggplot(Data, aes(x = Groups, y = x2982_82_2,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[2])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[3]

p3 <- ggplot(Data, aes(x = Groups, y = x3506_49_1,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[3])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[4]

p4 <- ggplot(Data, aes(x = Groups, y = x2683_1_2,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[4])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))


S16B <- (p1 | p2 | p3 | p4)

ggsave(S16B, file = "Figure_S16B.png", width = 7, height = 3, dpi = 300)  
ggsave(S16B, file = "Figure_S16B.eps", width = 7, height = 3, dpi = 300, device = "eps")

##### S16D Female CSF ##### 

VIPmerge <- readxl::read_excel("Figure S16D_VIP_CSF_F.xlsx", col_names = TRUE) %>%
  janitor::clean_names()
VIPmerge <- VIPmerge[!is.na(VIPmerge$target),]

VIPres_names <- VIPmerge[order(VIPmerge$vip, decreasing = T),]
VIPres_names <- VIPres_names[order(VIPres_names$vip, decreasing = T),]
Data <- readxl::read_excel("Figure S16D_CSF_F.xlsx", col_names = TRUE) %>%
  janitor::clean_names()

Data$Groups <- as.factor(Data$grp)
levels(Data$Groups) <- c("HV", "PI-ME/CFS")

top4 <- VIPres_names[1:4,] 
top4$row_names[1]

p1 <- ggplot(Data, aes(x = Groups, y = x2841_13_2,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[1])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[2]

p2 <- ggplot(Data, aes(x = Groups, y = x3045_72_2,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[2])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[3]

p3 <- ggplot(Data, aes(x = Groups, y = x8469_41_3,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[3])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

top4$row_names[4]

p4 <- ggplot(Data, aes(x = Groups, y = x2685_21_2,  color=Groups, fill=Groups))+
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  scale_fill_manual(values = alpha(c("blue","red"), 0.35)) +
  scale_color_manual(values=c("blue","red")) +
  theme_classic() +
  ylab(paste(gsub("_"," ", VIPres_names$target[4])))+
  geom_signif(comparisons = list(c("HV", "PI-ME/CFS")),
              map_signif_level = function(p) sprintf("p = %.1g", p),
              color = "black",
              test = "wilcox.test",
              textsize = 8)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 18),
        axis.text.y = element_text(color = "black", size = 18))

S16D <- (p1 | p2 | p3 | p4)

ggsave(S16D, file = "Figure_S16D.png", width = 7, height = 3, dpi = 300)  
ggsave(S16D, file = "Figure_S16D.eps", width = 7, height = 3, dpi = 300, device = "eps")

##### Figure S20A #####

data <- readxl::read_excel("Figure S20A.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS20a <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Observed Features") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) +
  ggsignif::geom_signif(test = "wilcox.test",
                        comparisons = list(c("HV", "PI-ME/CFS")),
                        map_signif_level = function(p) sprintf("p = %.1g", p),
                        color = "black",                         textsize = 8)

figS20a

ggsave(figS20a, file = "Figure_S20A.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS20a, file = "Figure_S20A.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")


##### Figure S20B #####

data <- readxl::read_excel("Figure S20B.xlsx", col_names = TRUE) %>%
  janitor::clean_names() %>%
  na.omit()

colnames(data) <- c("group", "var")

figS20b <- ggplot2::ggplot(data, aes(x = group, y = var, color = group)) +
  geom_boxplot(outlier.shape = NA, coef = NULL) +
  stat_boxplot(geom = 'errorbar', width = 0.5, coef = NULL) +
  geom_point(shape=19, position=position_jitter(width = 0.25, height = 0.25), size = 1) +
  theme_classic() +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Inverse Simpson") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black", size = 26),
        axis.text.x = element_text(face = "bold", color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20)) #+
# ggsignif::geom_signif(test = "wilcox.test",
#                       comparisons = list(c("HV", "PI-ME/CFS")),
#                       map_signif_level = function(p) sprintf("p = %.2g", p),
#                       color = "black",                         textsize = 8)#,
# annotation = c("*"))

figS20b

ggsave(figS20b, file = "Figure_S20B.png", width = 500, height = 700, dpi = 300, units = "px")  
ggsave(figS20b, file = "Figure_S20B.eps", width = 500, height = 700, dpi = 300, device = "eps", units= "px")
