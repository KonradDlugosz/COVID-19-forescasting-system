library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(lubridate)

endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","newCases":"newCasesByPublishDate"}'

response = GET(endpoint)


if (response$status_code >= 400) {
  err_msg = httr::http_status(response)
  stop(err_msg)
}

## Data format
# Convert binary to JSON:
json_text <- content(response, "text")
data      <- jsonlite::fromJSON(json_text)

# Convert list to data-frame:
dailyCases <- as.data.frame(do.call(cbind, data$data))
first28Cases <- dailyCases[1:28,]

#correct date format:
dailyCases <- dailyCases %>% 
  mutate(date = ymd(dailyCases$date), newCases = as.numeric(newCases))

avgNumberOfCases <- mean(dailyCases$newCases)

sortCasesDesc <- dailyCases %>%
  arrange(desc(newCases))

# plot time Series NewCases:
dailyCases %>% 
  ggplot( aes(x=date, y=newCases)) +
  geom_line(color="#69b3a2") +
  geom_area(fill="#69b3a2", alpha=0.5) +
  ylim(0,40000) +
  annotate(geom="text", x=as.Date(sortCasesDesc[1,"date"]), y=sortCasesDesc[1,"newCases"] + 3000, 
           label="Higest number of cases" , size= 5 ) +
  annotate(geom="point", x=as.Date(sortCasesDesc[1,"date"]), y=sortCasesDesc[1,"newCases"], size=5, shape=21, fill="transparent") +
  geom_hline(yintercept=avgNumberOfCases, color="orange", size=.5) +
  theme_ipsum()
