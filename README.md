# About

This repo is designed to make the storm hazard warning data gathering and plotting reproducible.

R scripts are housed in the `R` directory; `prepare_data.R` is a script that pulls, cleans and joins the data,
and one that contains functions for assigning Saffir-Simpson like categories to storms 
and for generating bar plots displaying the categories. 

The cleaned primary dataset is housed in the `data` directory. Note that 
it is possible to use this dataset directly without recreating it by running `prepare_data.R`. 

For details on the data sources and cleaning, see the README_data file  in the data subfolder.

# How to Download the code and data: 

## Command Line git
From the command line (with git installed):
`git clone https://github.com/gmellison/StormHazard.git`

Then, update if necessary with 
`git pull` from any location within the `StormHazard` directory

## R Studio

1. Create a New Project.
2. Select "Version Control" 
3. Select "Git"
4. Enter "https://github.com/gmellison/StormHazard.git" in Repository URL
5. Choose the local file location and directory name

The data and scripts will be downloaded in the new R project. 

Then, update by using the GIT dropdown menu in R Studio, and selecting "pull branches".
Alternatively, navigate to the "Git" pane, and click the blue down arrow. 

# How to use the categorization and  plotting functions:

The `main.R` script has a small demonstration of how to use the code to generate a plot. 
