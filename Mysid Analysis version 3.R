### Mysid Analysis, Take 3
### Including corrections from QERM 514 feedback
### July 25, 2023

### Read In Data

# from file
# data <- data <- read.csv("Er prey analysis for R fixed whale presence.csv")
# from GitHub repo
data <- read.csv("https://raw.githubusercontent.com/lizallyn/Project-Mysids/main/Er%20prey%20analysis%20for%20R%20fixed%20whale%20presence.csv?token=GHSAT0AAAAAACFRBFAJSQ6NKD2A3Y6VTLAYZGAJMRA")

### Data Manipulation/Cleaning

data$Site <- factor(data$Site, levels = c("Chito Beach", "Bullman Beach", "Seal and Sail", "Sail River", "First Beach", "Koitlah", "Slant Rock", "Skagway Rocks", "Anderson Rocks", "Portage Head", "Duk Point", "North Bodelteh", "South Bodelteh", "Ozette Island"))

