library(tidyverse)
library(jsonlite)
library(readr)

CTL_wax <- read_csv("CTL_wax.csv")
CTL_wax$fieldsite<- ifelse(grepl("BZ", CTL_wax$fieldsite, fixed=TRUE), "Brazil", CTL_wax$fieldsite)
CTL_wax$fieldsite<- ifelse(grepl("CH", CTL_wax$fieldsite, fixed=TRUE), "China", CTL_wax$fieldsite)
CTL_wax$fieldsite<- ifelse(grepl("GM", CTL_wax$fieldsite, fixed=TRUE), "Germany", CTL_wax$fieldsite)
CTL_wax$fieldsite<- ifelse(grepl("IN", CTL_wax$fieldsite, fixed=TRUE), "India", CTL_wax$fieldsite)
CTL_wax$fieldsite<- ifelse(grepl("IS", CTL_wax$fieldsite, fixed=TRUE), "Israel", CTL_wax$fieldsite)
CTL_wax$fieldsite<- ifelse(grepl("JP", CTL_wax$fieldsite, fixed=TRUE), "Japan", CTL_wax$fieldsite)
CTL_wax$fieldsite<- ifelse(grepl("MX", CTL_wax$fieldsite, fixed=TRUE), "Mexico", CTL_wax$fieldsite)
CTL_wax$fieldsite<- ifelse(grepl("RU", CTL_wax$fieldsite, fixed=TRUE), "Russia", CTL_wax$fieldsite)
CTL_wax$fieldsite<- ifelse(grepl("SA", CTL_wax$fieldsite, fixed=TRUE), "South Africa", CTL_wax$fieldsite)
CTL_wax$fieldsite<- ifelse(grepl("UK", CTL_wax$fieldsite, fixed=TRUE), "United Kingdom", CTL_wax$fieldsite)
CTL_wax$fieldsite<- ifelse(grepl("US", CTL_wax$fieldsite, fixed=TRUE), "United States of America", CTL_wax$fieldsite)

data <- CTL_wax %>% 
  group_by(fieldsite) %>%
  summarise_all(funs(mean(., na.rm=TRUE))) %>%
  mutate(fieldsite = factor(fieldsite, levels(as.factor(fieldsite)))) %>% 
  dplyr::select(fieldsite, HandWash, HandDisinfect, StayHome, Antibact, MaskFreq, AvoidPublic, Stockup, AltRem, CoverCough, SocDist)


geojson <- readLines("ctl_countries.json", warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)
geojson$style = list(
  weight=1,
  color="#555555",
  opacity=1,
  fillOpacity=0.8
)

map <- function(hbehav) {
  data <- data[, c("fieldsite", hbehav)]
  # reformat json data according to selected variable
  for(i in 1:length(geojson$features)) {
    props <- geojson$features[[i]]$properties
    geojson$features[[i]]$properties <- unclass(data[data$fieldsite==props$sovereignt,])
  }
  # Gather var estimates
  temp <- sapply(geojson$features, function(feat) {
    feat$properties[[hbehav]]
  })
  pallete <- colorQuantile("Blues", temp)
  geojson$features <- lapply(geojson$features, function(feat) {
    feat$properties$style <- list(
      fillColor = pallete(
        feat$properties[[hbehav]]
      )
    )
    feat
  })
  leaflet(options = leafletOptions(minZoom=2)) %>% 
    addGeoJSON(geojson) %>% addProviderTiles(providers$OpenStreetMap) %>%
    addLegend(position="bottomright", pal = pallete, values=temp)
}

