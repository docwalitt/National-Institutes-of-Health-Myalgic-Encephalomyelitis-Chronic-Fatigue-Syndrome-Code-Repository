rm(list=ls())
pacman::p_load(ggplot2, stats, tidyverse, tidyr, dplyr, magrittr, reshape2, janitor, broom)
#setwd() set the working directory to the where the source data files are available 

##### This script was used to verify statistical tests for the following panels. These formatted data files are available within the "Nature Comms MECFS Source Data.RData" object

# [1] "Figure 2A.xlsx"  "Figure 2B.xlsx"  "Figure 2C.xlsx"  "Figure 2D.xlsx"  "Figure 2H.xlsx"  "Figure 3C.xlsx"  "Figure 3D.xlsx"  "Figure 5A.xlsx"  "Figure 5C.xlsx"  "Figure 5D.xlsx" 
# [11] "Figure 5E.xlsx"  "Figure 5F.xlsx"  "Figure 5H.xlsx"  "Figure 6A.xlsx"  "Figure 6B.xlsx"  "Figure 6C.xlsx"  "Figure 7A.xlsx"  "Figure 7B.xlsx"  "Figure 7C.xlsx"  "Figure 7D.xlsx" 
# [21] "Figure 7E.xlsx"  "Figure 7F.xlsx"  "Figure 9A.xlsx"  "Figure 9B.xlsx"  "Figure S1A.xlsx" "Figure S1B.xlsx" "Figure S1C.xlsx" "Figure S1D.xlsx" "Figure S1E.xlsx" "Figure S1F.xlsx"
# [31] "Figure S1G.xlsx" "Figure S1H.xlsx" "Figure S1I.xlsx" "Figure S1J.xlsx" "Figure S1K.xlsx" "Figure S2A.xlsx" "Figure S2B.xlsx" "Figure S2C.xlsx" "Figure S2D.xlsx" "Figure S2E.xlsx"
# [41] "Figure S2F.xlsx" "Figure S2G.xlsx" "Figure S3A.xlsx" "Figure S3B.xlsx" "Figure S3C.xlsx" "Figure S3D.xlsx" "Figure S8F.xlsx"

files <- list.files(pattern = ".xlsx") #gets a list of the .xlsx files in the dir, please note this only works for the files listed above

for(i in 1:length(files)) { # initiatea a for loop to run through the list of files
# Load data

data <- readxl::read_excel(files[i]) %>% na.omit() %>% janitor::clean_names() # reads in the source data
outcome <- colnames(data)[2] # gets the outcome for this source data file from the col 2 header
colnames(data) <- c("group", "var") # changes the col names for processing
samp_sizes <- data %>% dplyr::group_by(group) %>% dplyr::summarise(n = n()) # obtains sample size by group

# stats 
     
shap_wilk_test_results <- data %>% # shapiro wilk testing for normality
  group_by(group)  %>% # by group
  do(tidy(shapiro.test(.$var))) %>% # by outcome 
  ungroup() %>% 
  select(-method)

output <- data %>%
  dplyr::mutate(
    figure_panel = files[i],
    outcome = outcome,
    n_HV = samp_sizes$n[1],
    n_MECFS = samp_sizes$n[2],
    shap_wilk_p = min(shap_wilk_test_results$p.value),
    distribution = dplyr::case_when(
      min(shap_wilk_p) < 0.05 ~ "non-normal",
      TRUE ~ "normal"
    ),
    var_p = var.test(var ~ group, data = data)$p.value,
    variance = dplyr::case_when(
      var_p > 0.05 ~ "equal",
      TRUE ~ "unequal"
    ),
    test_stat = dplyr::case_when(
      distribution == "normal" & variance == "equal" ~ "unpaired two-sample t-test with equal variance",
      distribution == "normal" & variance == "unequal" ~ "unpaired two-sample t-test with unequal variance",
      TRUE ~ "unpaired two-sample Wilcoxon Rank Sum/Mann Whitney U test",
    ),
    p_val = dplyr::case_when(
      distribution == "normal" & variance == "equal" ~ t.test(var ~ group, data = ., paired = FALSE, alternative = "two.sided", var.equal = TRUE)$p.value,
      distribution == "normal" & variance == "unequal" ~ t.test(var ~ group, data = ., paired = FALSE, alternative = "two.sided", var.equal = FALSE)$p.value,
      TRUE ~ wilcox.test(var ~ group, data = ., paired = FALSE, alternative = "two.sided", exact = TRUE)$p.value
    ),
    unadj_mwu_p_val = rstatix::wilcox_test(var ~ group, data = ., paired = FALSE, alternative = "two.sided", exact = TRUE, p.adjust.method = "none")$p, # cals an unadjusted mann whitney pvalue
    ) %>%
  dplyr::select(-group, -var) %>%
  dplyr::slice(1)


data.table::fwrite(output, paste0("MECFS Figures Stats Output ", Sys.Date(), ".csv"), append = TRUE)

}


