###############################################################
## Download and process the EPI data from Unicef             ##
## https://data.unicef.org/topic/child-health/immunization/  ##
###############################################################

# Load libraries
library(countrycode)
library(tidyverse)
library(readxl)

# Download the EPI data
epi_url <- "https://data.unicef.org/wp-content/uploads/2023/07/wuenic2022rev_web-update.xlsx"
setwd("./data")
system(paste("wget", epi_url))

# Read the EPI data
file_name <- "./data/wuenic2022rev_web-update.xlsx"
sheet_names <- excel_sheets(file_name)[!(excel_sheets(file_name) %in% c("regional_global"))]

# Read the first sheet (BCG)
epiDF <- read_excel("data/wuenic2022rev_web-update.xlsx")

# Loop through the remaining sheets and append them
for(i in sheet_names[2:14]){
  epiDF <- rbind(epiDF,
                 read_excel("data/wuenic2022rev_web-update.xlsx", sheet = i))
}

### Clean up
# Delete everything that is not a dataframe
rm(list = ls()[sapply(ls(), function(x) !is.data.frame(get(x)))])

# Save the mortality data and the country data
save.image("fetched.RData")
