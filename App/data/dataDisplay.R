source("data/population.R")
source("data/covid19Data.R")
library(dplyr)

c <- casesDataSet
d <- deathsDataSet 
r <- recoveredDataSet

names(c)[ncol(c)] <- "lastDate"
names(d)[ncol(d)] <- "lastDate"
names(r)[ncol(r)] <- "lastDate"


c <- c %>% group_by(`Country/Region`) %>% summarise(Cases = sum(lastDate))
d <- d %>% group_by(`Country/Region`) %>% summarise(Deaths = sum(lastDate))
r <- r %>% group_by(`Country/Region`) %>% summarise(Recovered = sum(lastDate))

popList <- pop[order(pop$country),]
popList <- popList[-c(132,189),]

c <- c[-c(49,118),]
d <- d[-c(49,118),]
r <- r[-c(49,118),]


cases1MPop <- c()
for(i in 1:length(c$Cases)){
  cases1MPop[i]  <- ceiling(c$Cases[i]/popList$population[i] * 1000000)
}
deaths1MPop <- c()
for(i in 1:length(d$Deaths)){
  deaths1MPop[i]  <- ceiling(d$Deaths[i]/popList$population[i] * 1000000)
}
caseFatalityRate <- c()
for(i in 1:length(d$Deaths)){
  holder <- d$Deaths[i] / c$Cases[i] * 100
  caseFatalityRate[i] <- paste(format(round(holder, 2), nsmall = 2),"%")
}

#c1pop <- data.frame(c$`Country/Region`, cases1MPop)
#names(c1pop)[1] <-"Country/Region"
#d1pop <- data.frame(c$`Country/Region`, deaths1MPop)
#names(c1pop)[1] <-"Country/Region"



df <- data.frame(c,r$Recovered,d$Deaths,cases1MPop, deaths1MPop, caseFatalityRate,popList$population)

names(df)[1] <- "Country"
names(df)[3] <- "Recovered"
names(df)[4] <- "Deaths"
names(df)[5] <- "Cases/1M Population"
names(df)[6] <- "Deaths/1M Population"
names(df)[7] <- "Case Fatality Rate"
names(df)[8] <- "Population"
