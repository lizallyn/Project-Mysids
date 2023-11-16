# Project Mysids

## Gray whale prey analysis 2023
 
 This repo has all the files associated with the manuscript accepted by PeerJ in November 2023 and the many analysis attempts leading up to that. Data files (.csv) called by R scripts are in the "Data Files" folder. The R Scripts used to create the final figures, tables, and in-text numbers are in the folder "R Scripts". Modeling attempts from spring/summer 2023 are in the folder "Modeling". "Recycle Bin" holds any files that are not called by a script in the Modeling or R Scripts files or any interim coding files that were not completed. 
 
## R Scripts

#### Mysid Figures Second Revision.R

This file is the script used to produce the final versions of the figures in the published manuscript in PeerJ.

## Modeling

#### Mysid Analysis 4 sites.R

This file assigns each whale sighting to a mysid tow site if within a certain distance of that site and then constructs models based on whales within the vicinity during mysid sampling. Has diagnostics.

#### Mysid Analysis 4 daily.R

This file summarizes whales and mysids on a daily basis (only including sightings on days when tows happened) and then constructs models based on that.

#### Mysid Analysis 5 monthly v2.R

This file summarizes the whale and mysids on a monthly basis and then uses lm and lmer with visualization and diagnostics.

#### Mysid Analysis 6 monthly zi.R.

This file summarizes the data monthly during the study and then constructs zi-negbinom models and incorporates size. Not a lot of diagnostics here, see file 5 for that.

## Data Files

All data files called by a script are in this folder as .csv format. The whale icon .png from the map figures is also here.

