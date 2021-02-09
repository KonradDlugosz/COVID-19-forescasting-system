# This file contains population per country
library(readr)
# Read data set
iso_look_up <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv")

country <- c()
population <- c()

# Extract countries
for(i in 1:nrow(iso_look_up)){
  if(is.na(iso_look_up$Province_State[i])){
    country[i] <- iso_look_up$Country_Region[i]
    population[i] <- iso_look_up$Population[i]
  }
}

# Make final dataframe of all coutries 
pop <- data.frame(country,population)
pop <- na.omit(pop)
rownames(pop) <- NULL
