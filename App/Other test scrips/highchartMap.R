data(GNI2014, package = "treemap")

hcmap(
  "custom/world-robinson-lowres", 
  data = GNI2014,
  name = "Gross national income per capita", 
  value = "GNI",
  borderWidth = 0,
  nullColor = "#d3d3d3",
  joinBy = c("iso-a3", "iso3")
) %>%
  hc_colorAxis(
    stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1)),
    type = "logarithmic"
  ) 
