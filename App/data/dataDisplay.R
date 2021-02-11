source("data/population.R")
source("data/covid19Data.R")
c <- casesDataSet
d <- deathsDataSet 
r <- recoveredDataSet

names(c)[ncol(c)] <- "lastDate"
names(d)[ncol(d)] <- "lastDate"
names(r)[ncol(r)] <- "lastDate"


c <- c %>% group_by(`Country/Region`) %>% summarise(Cases = sum(lastDate))
d <- d %>% group_by(`Country/Region`) %>% summarise(Deaths = sum(lastDate))
r <- r %>% group_by(`Country/Region`) %>% summarise(Recovered = sum(lastDate))

population <- pop[order(pop$country),]
population <- population[-189,]

c <- c[-49,]
d <- d[-49,]
r <- r[-49,]

df <- data.frame(c,r$Recovered,d$Deaths,population$population)

names(df)[1] <- "Country"
names(df)[3] <- "Recovered"
names(df)[4] <- "Deaths"
names(df)[5] <- "Population"
