library(rvest)
library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)

#--------------------------------------------
# Manually downloaded from my city bike site
# https://kaupunkipyorat.hsl.fi/fi/activity
#
# Parsing XML
#-------------------------

page <- xml2::read_html("Omat matkasi Kaupunkipyörät.html", encoding = "UTF-8")

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

statistics <- page %>% 
  html_nodes(xpath = "//*[@class='statistics']") %>% 
  html_text() %>% 
  gsub("\\s\\(.*", "", .)

df <- data.frame(depts, arrs, stringsAsFactors = FALSE)

#-------------------
# Biking start date
#-------------------
start_date <- "2020-03-26"

#-----------------
# Bike station data
#------------------
bikestations <- "https://opendata.arcgis.com/datasets/1b492e1a580e4563907a6c7214698720_0.csv"

temp <- tempfile()
download.file(bikestations, temp)
stations <- read.csv(temp, encoding = "UTF-8")
unlink(temp)

#-------------------
# Filter by dept/arr
#-------------------

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

#----------------------
# Join with stations
#----------------------

joindept <- data %>% 
  left_join(stations, by = c("deptstationcode"="id")) %>% 
  rename(coordinate_dept = coordinate,
         name_dept = name)

bikedata <- joindept %>% 
  left_join(stations, by = c("arrstationcode"="id")) %>% 
  rename(coordinate_arr = coordinate,
         name_arr = name) %>% 
  select(deptstationcode, name_dept, coordinate_dept, arrstationcode, name_arr, coordinate_arr)

bikedata <- rowid_to_column(bikedata, "ID")

bikedata_sf <- bikedata %>%
  group_by(ID) %>% 
  separate(coordinate_dept, c("LAT_DEPT", "LONG_DEPT"), sep = ",") %>% 
  separate(coordinate_arr, c("LAT_ARR", "LONG_ARR"), sep = ",") %>% 
  mutate_at(vars(LONG_DEPT, LONG_ARR, LAT_DEPT, LAT_ARR), as.numeric) %>% 
  unite(start, LONG_DEPT, LAT_DEPT) %>% # collect coords into one column for reshaping
  unite(end, LONG_ARR, LAT_ARR) %>%
  filter(end != "NA_NA") %>% # 
  gather(start_end, coords, start, end) %>% 
  separate(coords, c("LONG", "LAT"), sep = "_") %>% 
  mutate_at(vars(LONG, LAT), as.numeric) %>%
  st_as_sf(coords = c("LONG", "LAT")) %>% 
  summarize(geometry = st_combine(geometry)) %>% 
  st_cast("LINESTRING") %>% 
  st_set_crs(4326)

#-------------------------------------------  
# How often have I biked the same route?
#-------------------------------------------

bikedata_sf$n = sapply(st_equals(bikedata_sf), function(x)length(unique(x)))

uniqstations <- bikedata %>% 
  distinct_at(vars(deptstationcode, arrstationcode), .keep_all = TRUE) %>% 
  separate(coordinate_dept, c("LAT_DEPT", "LONG_DEPT"), sep = ",") %>% 
  separate(coordinate_arr, c("LAT_ARR", "LONG_ARR"), sep = ",") %>% 
  mutate_at(vars(LONG_DEPT, LONG_ARR, LAT_DEPT, LAT_ARR), as.numeric) 
  

#--------------
# Mapping
#--------------

icon.bike <- awesomeIcons(icon = "bicycle",
                          markerColor = "lightgreen",
                          library = "fa",
                          iconColor = "black",
                          squareMarker = FALSE)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 40%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    font-weight: bold;
    font-family: verdana;
    color: black;
    font-size: 13px
  }
"))


title <- tags$div(
  tag.map.title, HTML(paste0("From ", start_date, " to ", Sys.Date(), " I've been biking ", statistics))
)  

m <- leaflet() %>% 
  addProviderTiles("Stamen.Watercolor") %>% 
  addTiles(urlTemplate = "", 
           attribution = 'Data by kaupunkipyorat.hsl.fi') %>% 
  setView(lng = 25.00657,la = 60.18895, zoom = 12) %>% 
  addControl(title, position = "topleft", className = "map-title") %>% 
  addPolylines(
    data = bikedata_sf,
    color = "black",
    weight = ~n
  ) %>%
  addAwesomeMarkers(
    data = uniqstations,
    lng = ~LONG_DEPT,
    lat = ~LAT_DEPT,
    icon = icon.bike,
    label = uniqstations$name_dept
    ) %>% 
  addAwesomeMarkers(
    data = uniqstations,
    lng = ~LONG_ARR,
    lat = ~LAT_ARR,
    icon = icon.bike,
    label = uniqstations$name_arr
  ) 

mapview::mapshot(m, "myrides.html")
