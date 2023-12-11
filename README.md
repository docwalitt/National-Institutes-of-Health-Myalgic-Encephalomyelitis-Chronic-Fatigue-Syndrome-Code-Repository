# Nature_Comms_MECFS
Source data and code for analysis and figure creation associated with the Walitt et al. Nature Comms paper on MECFS

This repo contains an R script file that contains the source code to generate the specified figure panels for the main figures and supplemental figures. 

This R script calls source data stored in the "Nature Comms MECFS Source Data.RData" object. 

To access the source data, please see the following steps below. One option is to extract the individual data files from the RData object and write them as .xlsx files to a directory of your choosing and then read them in as specified for plotting when needed. The second option allows you to extract them all into individual data frames in the current global environment. Doing this will allow the individual data frames to be called but will require a slight code modification for each section to use that instead of reading in a .xlsx with those data. 

##### Start Here #####
# Load the RData file containing the individual source data files
# source_data <- readRDS("Nature Comms MECFS Source Data.RData")

# once the source data are loaded you can save them as individual .xlsx files to a directory of your choosing.
# source_data is the list of data frames
# Create a directory to save the output files
#output_dir <- "output_directory" # set this to be where you want the files written to
#dir.create(output_dir, showWarnings = FALSE) # this creates that directory to save the .xlsx files to

# purrr::walk iterates over the source_data list of data frames and writes each data frame to a separate .xlsx file
#purrr::walk(names(xlsx_list), ~ openxlsx::write.xlsx(xlsx_list[[.x]], file.path(output_dir, paste0(.x, ".xlsx")), row.names = FALSE))

# If you want to just extract all files into separate data frames in the current global environment
# list2env(xlsx_list, envir = .GlobalEnv)
