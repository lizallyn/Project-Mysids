# Project Mysids

## Gray whale prey analysis 2023
 
 This repo has all the files associated with the manuscript accepted by PeerJ in November 2023 (published in January 2024 and available open access) and the many analysis attempts leading up to that. Data files (.csv) called by R scripts are in the "Data Files" folder. The R Scripts used to create the final figures, tables, and in-text numbers are in the folder "R Scripts". Modeling attempts from spring/summer 2023 are in the folder "Modeling". "Recycle Bin" holds any files that are not called by a script in the Modeling or R Scripts files or any interim coding files that were not completed. 
 
## R Scripts

#### Mysid Figures Second Revision.R

This file is the script used to produce the final versions of the figures in the published manuscript in PeerJ. Available online: https://peerj.com/articles/16587/

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

#### All obs for R.csv

This file has all tow sample data - one row for each organism recorded.

#### CRC IDs per sighting June - Nov 2019 2020 mysid survey area only all behaviors.csv

This file has a row for each CRC ID confirmed per whale sighting during 2019 and 2020, but only includes sightings that occurred within the mysid tow sampling area (south to Sand Point, east to just past Chito). Also lists the behavior and location for each sighting.

#### Er prey analysis for R fixed whale presence.csv

This file has a row for each tow sample and lists the location, mysid count, species breakdown, average length, information about the non-mysids in the sample, and whale information (though the whale data may not be updated and should largely be ignored in favor of the whale information presented in separate files).

#### Sample site coords.csv

This file has the coordinates of each tow sample site.

#### Whale at Seal and Sail and Bullman 2019 2020.csv

This file contains all the sightings within the area around Seal and Sail Rocks, Sail River, and Bullman beach as clipped by Adrianne in GIS. Each row is a sighting and all the sighting query information is here.

#### Whales in full survey area 2019 2020.csv

This file contains all the sightings within the mysid tow sampling area (south to Sand Point, east to just past Chito) as clipped by Adrianne in GIS. Each row is a sighting and all the sighting query information is here.

#### whales per day for R.csv

An old file called by one of the version 4 analysis scripts, use with caution. Summarizes whales sighted per day.

#### Tow data for R.csv

This is an older version of "Er prey analysis for E fixed whale presence.csv" that is called by one of the version 4 scripts. Use with caution.

#### icons8-whale-66.png

Whale icon used in the map figures.

