library(rvest)
library(tidyverse)
library(sf)
library(leaflet)

# Manually downloaded from my city bike site
# https://kaupunkipyorat.hsl.fi/fi/activity
page <- xml2::read_html("Omat matkasi   Kaupunkipyörät.html", encoding = "UTF-8")

items <- page %>% 
  html_nodes(xpath = "//*[@class='activity-feed-item']") 

stations <- items %>% 
  html_nodes(xpath = "*[@class = 'station-info']")

depts <- stations %>% 
  html_nodes(xpath = "*[@class = 'departure-station']") %>% 
  html_text()

arrs <- stations %>% 
  html_nodes(xpath = "*[@class = 'return-station']") %>% 
  html_text()

dates <- items %>% 
  html_nodes(xpath = "*[@class = 'date-info']")

depttime <- dates %>% 
  html_nodes(xpath = "*[@class = 'departure-date']") %>% 
  html_text()

arrtime <- dates %>% 
  html_nodes(xpath = "*[@class = 'return-date']") %>% 
  html_text()

info <- items %>% 
  html_nodes(xpath = "*[@class = 'trip-info']")

duration <- info %>% 
  html_nodes(xpath = "*[@class = 'duration']") %>% 
  html_text()

dist <- info %>% 
  html_nodes(xpath = "*[@class = 'covered-distance']") %>% 
  html_text()

df <- data.frame(depts, arrs, stringsAsFactors = FALSE)

data <- df %>%
  # Dept
  mutate(deptstation = str_replace(depts, "\\s", "|")) %>% 
  separate(deptstation, into = c("first", "rest"), sep = "\\|") %>% 
  rename(deptstationcode = first) %>% 
  mutate(deptstationcode = as.numeric(deptstationcode)) %>% 
  select(-rest) %>% 
  # Arr
  mutate(arrstation = str_replace(arrs, "\\s", "|")) %>% 
  separate(arrstation, into = c("first", "rest"), sep = "\\|") %>% 
  rename(arrstationcode = first) %>% 
  mutate(arrstationcode =as.numeric(arrstationcode)) %>% 
  select(-rest, -depts, -arrs) 

bikestations <- "https://opendata.arcgis.com/datasets/1b492e1a580e4563907a6c7214698720_0.csv"

temp <- tempfile()
download.file(bikestations, temp)
stations <- read.csv(temp, encoding = "UTF-8")
unlink(temp)

stations_cleaned <- stations %>% 
  rename(x = X.U.FEFF.X,
         y = Y) %>%
  select(x, y, name, id)

joindept <- left_join(data, stations_cleaned, by = c("deptstationcode"="id"))
joinarr <- left_join(joindept, stations_cleaned, by = c("arrstationcode"="id"))

bikedata <- joinarr %>% 
  rename(deptx = x.x,
         depty = y.x,
         arrx = x.y,
         arry = y.y,
         depstationname= name.x,
         arrstationname = name.y)

bikedata <- rowid_to_column(bikedata, "ID")

# https://gis.stackexchange.com/a/294399
bikedata_sf <- bikedata %>%
  unite(start, deptx, depty) %>% 
  unite(end, arrx, arry) %>%
  gather(start_end, coords, start, end) %>% 
  separate(coords, c("LONG", "LAT"), sep = "_") %>% 
  mutate_at(vars(LONG, LAT), as.numeric) %>%
  st_as_sf(coords = c("LONG", "LAT")) %>% 
  group_by(ID) %>% 
  summarize(geometry = st_combine(geometry)) %>% 
  st_cast("LINESTRING") %>%
  st_set_crs(4326) 

# How often have I biked the same route?
bikedata_sf$n = sapply(st_equals(bikedata_sf), function(x)length(unique(x)))

uniqstations <- bikedata %>% 
  distinct_at(vars(deptstationcode, arrstationcode), .keep_all = TRUE)

icon.bike <- awesomeIcons(icon = "bicycle",
                          markerColor = "lightgreen",
                          library = "fa",
                          iconColor = "black",
                          squareMarker = FALSE)

leaflet() %>% 
  addProviderTiles("Stamen.Watercolor") %>% 
  addTiles(urlTemplate = "", 
           attribution = paste0('kaupunkipyorat.hsl.fi ', Sys.Date())) %>%   
  setView(lng = 25.00657,la = 60.18895, zoom = 11) %>% # Kulosaari metro station
  addPolylines(
    data = bikedata_sf,
    color = "black",
    weight = ~n * 2
  ) %>% 
  addAwesomeMarkers(
    data = uniqstations,
    lng = ~deptx,
    lat = ~depty,
    icon = icon.bike,
    label = uniqstations$depstationname
  ) %>% 
  addAwesomeMarkers(
    data = uniqstations,
    lng = ~arrx,
    lat = ~arry,
    icon = icon.bike,
    label = uniqstations$arrstationname
  ) 
