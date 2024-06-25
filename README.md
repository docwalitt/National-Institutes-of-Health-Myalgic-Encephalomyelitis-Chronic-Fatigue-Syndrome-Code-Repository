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

## EEfRT Data and Supporting Materials

The "EEfRT" folder contains the data used to make the statistical models and generate the graphs for the paper. It is included as an excel file and as a tab delimited text file.
 
Additionally, the SAS scripts used to model the dataset and generate our graphs are included. The Hard Task Choice script runs the analysis and creates the graph used for Figure 3a, whereas the Button Press Rate script runs the analysis and creates the graph used for Figure 3b. These scripts are attached as SAS files and as text files. If one has a copy of SAS and the attached dataset, replicating our results should be as simple as importing the dataset into SAS and then running the included scripts.
 
All elements of the scripts and models were constructed closely following the official SAS user’s guide for the PROC GEE procedure (see the attached gee.pdf file) and the UCLA Statistical Methods and Data Analytics seminar on analyzing and visualizing interactions in SAS (which can be found here: https://stats.oarc.ucla.edu/sas/seminars/analyzing-and-visualizing-interactions/). 
 
The first half of both scripts takes the variables in the dataset and retitles them to make their names shorter and more manageable and to make the outputs easier to read and understand. The “Trial” variable is also adjusted to begin at 0 instead of at 1, which by my understanding is best practice, and a copy of the variable is also generated, which is necessary for the GEE procedure.
 
The second half of both scripts controls the construction and testing of the model itself and the creation of the graphs. The only element of these which I believe requires significant extra explanation is the “where” statement, which removes some data from the analysis. This statement does three things:
 
1). Data is only included if the participant is adjudicated AND if their data is valid
 
2). Data from the practice trials (indicated by a negative value for Trial) are dropped
 
3). Trials in which the participant failed to make a choice about trial difficulty within the allotted 5 seconds (thus requiring the computer to make a random choice for the participant) were also excluded from the analysis, since the participant did not actually make their own difficulty choice that trial
 
For the graphs, default settings were used so that they were drawn for the condition in which the model covariates (e.g., reward value, reward probability) are set to their average values. The only place where this was adjusted was for the Sex covariate, since the default behavior for the graph function when working with binary covariates is to create the graphs for only one of the two conditions (i.e., only men or only women). It was specified that the graphs should instead be made for the “average” participant (which is an approximately 6:4 sex ratio).

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
