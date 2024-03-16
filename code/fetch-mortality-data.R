#########################################################
## Download and process the country profiles data from ##
## childmortality.org                                  ##
#########################################################

# Load libraries
library(countrycode)
library(tidyverse)
library(readxl)


#### Download country profiles from childmortality.org
# encode all iso3c country names
ccodes <- read_excel("data/ccodes.xlsx")

# Template URL for the country profiles
url_template <- "https://childmortality.org/profileDownload/Country_profile_data_XYZ.xlsx"
# Save the data here
setwd("./data")
# Loop through all the possible country codes and download the profiles
for(i in ccodes$iso3c){
  url <- gsub("XYZ", i, url_template)
  # Download the file using wget
  system(paste("wget", url))
}
# Remove the ccodes dataframe
rm(ccodes)


### Create a dataframe of the countries with downloaded profiles
# List all the country profiles that were saved
file_list <- list.files(path = "data") 
file_list <- file_list[grep("^Country_profile_data_", file_list)]

# Extract the iso3c country codes for the available files
country_codes <- sub("^Country_profile_data_(.{3})\\.xlsx$", "\\1", file_list)
country_names <- countrycode(country_codes, "iso3c", destination = "country.name" )
country_region <- countrycode(country_codes, "iso3c", destination = "region" )
# Save the country codes, names, and UN region in a dataframe
countryDF <- data.frame(code = country_codes, name = country_names, region = country_region)


### Loop through all the country profiles and extract the relevant data into a dataframe

# Create empty data frame to hold the country profiles
dataDF <- data.frame(year = integer(), ccode = character(), u5_pop = integer(), 
                     u5mr_median = numeric(), u5mr_lb = numeric(), u5mr_ub = numeric(),
                     u5mr_q1 = numeric(),  u5mr_q2 = numeric(),  u5mr_q3 = numeric(),  
                     u5mr_q4 = numeric(),  u5mr_q5 = numeric(),
                     u5_count_median = integer(), u5_count_lb = integer(), u5_count_ub = integer(),
                     source_survey = integer(), source_census = integer(), source_vr = integer(), 
                     source_other = integer())

# Loop through all the country profiles
for(i in country_codes){
  file_name <- paste0("data/", gsub("XYZ", i, "Country_profile_data_XYZ.xlsx"))
  if(!(i %in% c("AIA", "COK", "MSR", "NIU", "VEN"))){  # Exclude these countries -- odd data sheets
  # Under 5 population count
  u5_pop <- unlist(read_excel(file_name, 
                              range = "B7:AH7", col_names = FALSE)[1,])
  # U5MR
  u5mr_median <-  unlist(read_excel(file_name, 
                                    range = "B37:AH37", col_names = FALSE)[1,])
  u5mr_lb <-  unlist(read_excel(file_name, 
                                range = "B38:AH38", col_names = FALSE)[1,])
  u5mr_ub <-  unlist(read_excel(file_name, 
                                range = "B39:AH39", col_names = FALSE)[1,])
  # U5 Count
  u5_count_median <-  unlist(read_excel(file_name, 
                                        range = "B45:AH45", col_names = FALSE)[1,])
  u5_count_lb <-  unlist(read_excel(file_name, 
                                    range = "B46:AH46", col_names = FALSE)[1,])
  u5_count_ub <-  unlist(read_excel(file_name, 
                                    range = "B47:AH47", col_names = FALSE)[1,])
  
  # Month 1-59 mortality rate
  mo1_59_mr <-  unlist(read_excel(file_name, 
                                  range = "B81:AH81", col_names = FALSE)[1,])
  
  # Month 1-59 mortality count
  mo1_59_count <-  unlist(read_excel(file_name, 
                                     range = "B92:AH92", col_names = FALSE)[1,])
  
  # U5MR by wealth quintile
  if(unlist(read_excel(file_name, range = "B154:B154", col_names = FALSE)[1,]) !=
     "There is no data available"){
  u5mr_q1 <-  unlist(read_excel(file_name, 
                                range = "B154:AH154", col_names = FALSE)[1,])
  u5mr_q2 <-  unlist(read_excel(file_name, 
                                range = "B155:AH155", col_names = FALSE)[1,])
  u5mr_q3 <-  unlist(read_excel(file_name, 
                                range = "B156:AH156", col_names = FALSE)[1,])
  u5mr_q4 <-  unlist(read_excel(file_name, 
                                range = "B157:AH157", col_names = FALSE)[1,])
  u5mr_q5 <-  unlist(read_excel(file_name, 
                                range = "B158:AH158", col_names = FALSE)[1,])
  } else {
    u5mr_q1 <-  rep(NA, length(1990:2022))
    u5mr_q2 <-  rep(NA, length(1990:2022))
    u5mr_q3 <-  rep(NA, length(1990:2022))
    u5mr_q4 <-  rep(NA, length(1990:2022))
    u5mr_q5 <-  rep(NA, length(1990:2022))
  }
  
  # Data Sources
  source_survey <- unlist(read_excel(file_name, 
                                     range = "B233:AH233", col_names = FALSE)[1,])
  source_census <- unlist(read_excel(file_name, 
                                     range = "B234:AH234", col_names = FALSE)[1,])
  source_vr <- unlist(read_excel(file_name, 
                                 range = "B235:AH235", col_names = FALSE)[1,])
  source_other <- unlist(read_excel(file_name, 
                                    range = "B236:AH236", col_names = FALSE)[1,])
  }
  
  if(i %in% c("VEN")){  # Catch Venezuala here
    # Under 5 population count
    u5_pop <- unlist(read_excel(file_name, 
                                range = "B6:AH6", col_names = FALSE)[1,])
    # U5MR
    u5mr_median <-  unlist(read_excel(file_name, 
                                      range = "B34:AH34", col_names = FALSE)[1,])
    u5mr_lb <-  unlist(read_excel(file_name, 
                                  range = "B35:AH35", col_names = FALSE)[1,])
    u5mr_ub <-  unlist(read_excel(file_name, 
                                  range = "B36:AH36", col_names = FALSE)[1,])
    # U5 Count
    u5_count_median <-  unlist(read_excel(file_name, 
                                          range = "B41:AH41", col_names = FALSE)[1,])
    u5_count_lb <-  unlist(read_excel(file_name, 
                                      range = "B42:AH42", col_names = FALSE)[1,])
    u5_count_ub <-  unlist(read_excel(file_name, 
                                      range = "B43:AH43", col_names = FALSE)[1,])
    
    # Month 1-59 mortality rate
    mo1_59_mr <-  unlist(read_excel(file_name, 
                                    range = "B72:AH72", col_names = FALSE)[1,])
    
    # Month 1-59 mortality count
    mo1_59_count <-  unlist(read_excel(file_name, 
                                       range = "B83:AH83", col_names = FALSE)[1,])
    
    # U5MR by wealth quintile
    u5mr_q1 <-  rep(NA, length(1990:2022))
    u5mr_q2 <-  rep(NA, length(1990:2022))
    u5mr_q3 <-  rep(NA, length(1990:2022))
    u5mr_q4 <-  rep(NA, length(1990:2022))
    u5mr_q5 <-  rep(NA, length(1990:2022))
    
    # Data Sources
    source_survey <- unlist(read_excel(file_name, 
                                       range = "B224:AH224", col_names = FALSE)[1,])
    source_census <- unlist(read_excel(file_name, 
                                       range = "B225:AH225", col_names = FALSE)[1,])
    source_vr <- unlist(read_excel(file_name, 
                                   range = "B226:AH226", col_names = FALSE)[1,])
    source_other <- unlist(read_excel(file_name, 
                                      range = "B227:AH227", col_names = FALSE)[1,])
  }
  
  
  dataDF <- rbind(dataDF,
                  data.frame(year = 1990:2022, ccode = i, u5_pop, u5mr_median, u5mr_lb, u5mr_ub,
                             u5mr_q1,  u5mr_q2,  u5mr_q3,  u5mr_q4,  u5mr_q5,
                             u5_count_median, u5_count_lb, u5_count_ub,
                             source_survey, source_census, source_vr, source_other))
}

# Delete everything that is not a dataframe
rm(list = ls()[sapply(ls(), function(x) !is.data.frame(get(x)))])

# Save the mortality data and the country data
save.image("fetched.RData")

