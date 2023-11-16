# Environmental stuff

library(tidyr)
library(dplyr)

# Read in and clean chlorophyll data

chlo19.5 <- read.csv("MY1DMM_CHLORA_2019-05-01_rgb_3600x1800.SS.csv")
chlo19.6 <- read.csv("MY1DMM_CHLORA_2019-06-01_rgb_3600x1800.SS.csv")
chlo19.7 <- read.csv("MY1DMM_CHLORA_2019-07-01_rgb_3600x1800.SS.csv")
chlo19.8 <- read.csv("MY1DMM_CHLORA_2019-08-01_rgb_3600x1800.SS.csv")
chlo19.9 <- read.csv("MY1DMM_CHLORA_2019-09-01_rgb_3600x1800.SS.csv")
chlo19.10 <- read.csv("MY1DMM_CHLORA_2019-10-01_rgb_3600x1800.SS.csv")
chlo19.11 <- read.csv("MY1DMM_CHLORA_2019-11-01_rgb_3600x1800.SS.csv")
chlo20.5 <- read.csv("MY1DMM_CHLORA_2020-05-01_rgb_3600x1800.SS.csv")
chlo20.6 <- read.csv("MY1DMM_CHLORA_2020-06-01_rgb_3600x1800.SS.csv")
chlo20.7 <- read.csv("MY1DMM_CHLORA_2020-07-01_rgb_3600x1800.SS.csv")
chlo20.8 <- read.csv("MY1DMM_CHLORA_2020-08-01_rgb_3600x1800.SS.csv")
chlo20.9 <- read.csv("MY1DMM_CHLORA_2020-09-01_rgb_3600x1800.SS.csv")
chlo20.10 <- read.csv("MY1DMM_CHLORA_2020-10-01_rgb_3600x1800.SS.csv")
chlo20.11 <- read.csv("MY1DMM_CHLORA_2020-11-01_rgb_3600x1800.SS.csv")


long <- data.frame(gather(chlo19.5, Long, Chlora, X.179.95:X179.95))
long$Long <- sub(x = long$Long, "X", "")
long <- long[-(long$Long >0)]
long$Long <- sub(x = long$Long, ".", "-")
as.numeric(long$Long)
region <- filter(long$Long %in% (-125:-124))
