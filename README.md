# Nature Communications - Walitt et al. source code and data
Source data and code for analysis and figure creation associated with the Walitt et al. Nature Comms paper on MECFS

All of the DNA sequencing data has been deposited to the [Sequence Read Archive (SRA)](https://www.ncbi.nlm.nih.gov/sra) of the National Library of Medicine (NLM) at the [National Institutes of Health (NIH)](https://www.nih.gov/), under BioProject accession number [PRJNA954397](https://www.ncbi.nlm.nih.gov/sra/PRJNA954397).


Click below for more information on the code and approach used for MICROBIOME sequencing analyis used on the paper:

## [Analysis of MICROBIOME shotgun sequencing data](docs/WGS.md)

For other data, see below:

This repo contains an R script file that contains the source code to generate the specified figure panels for the main figures and supplemental figures.

This R script calls source data stored in the "Nature Comms MECFS Source Data.RData" object.

To access the source data, please see the following steps below. One option is to extract the individual data files from the RData object and write them as .xlsx files to a directory of your choosing and then read them in as specified for plotting when needed. The second option allows you to extract them all into individual data frames in the current global environment. Doing this will allow the individual data frames to be called but will require a slight code modification for each section to use that instead of reading in a .xlsx with those data.

## Transcriptomics code comes from NHLBI-BCB/COVID-19_Transcriptomics (https://github.com/NHLBI-BCB/COVID-19_Transcriptomics.git)

## Start here for replication analysis and figure creation using source data where available

Option #1

source_data <- readRDS("Nature Comms MECFS Source Data.RData") # Load the RData file containing the individual source data files

Once the source data are loaded you can save them as individual .xlsx files to a directory of your choosing.

output_dir <- "output_directory" # Create a directory to save the output files, set this to be where you want the files written to
dir.create(output_dir, showWarnings = FALSE) # this creates that directory to save the .xlsx files to

purrr::walk(names(source_data), ~ openxlsx::write.xlsx(source_data[[.x]], file.path(output_dir, paste0(.x, ".xlsx")), row.names = FALSE)) # purrr::walk iterates over the source_data list of data frames and writes each data frame to a separate .xlsx file

Option #2
If you want to just extract all files into separate data frames in the current global environment
list2env(source_data, envir = .GlobalEnv)
