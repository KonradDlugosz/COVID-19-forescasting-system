library(httr)
library(jsonlite)
library(ggplot2)

endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","newCases":"newCasesByPublishDate"}'

response = GET(endpoint)


if (response$status_code >= 400) {
  err_msg = httr::http_status(response)
  stop(err_msg)
}

# Convert response from binary to JSON:
json_text <- content(response, "text")
data      <- jsonlite::fromJSON(json_text)

# Convert list into data frame
dailyCases <- as.data.frame(do.call(cbind, data$data))
first28Cases <- dailyCases[1:28,]

ggplot(first28Cases, aes(x = date, y = newCases)) +
  geom_bar(fill = "#0073C2FF", stat = "identity")

