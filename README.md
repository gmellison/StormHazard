# About

This repo is designed to make the storm hazard warning data gathering and plotting reproducible.

R scripts are housed in the `R` directory, and one python script is in `python`.
The HURDAT data should be downloaded first, as the other scripts 
download data based on the locations and times in the resulting data. 

Note that each script loads packages necessary to run the code, these 
packages will need to be installed before running the scripts.

# How to Download the code: 

## Command Line git
From the command line (with git installed):
`git clone https://github.com/gmellison/StormHazard.git`

## R Studio

1. Create a New Project.
2. Select "Version Control" 
3. Select "Git"
4. Enter "https://github.com/gmellison/StormHazard.git" in Repository URL
5. Choose the local file location and directory name

The scripts will be downloaded in the new R project. 

Then, update by using the GIT dropdown menu in R Studio, and selecting "pull branches".
Alternatively, navigate to the "Git" pane, and click the blue down arrow. 


