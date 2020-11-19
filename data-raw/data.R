
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(sf)
library(tigris)
library(mapdeck)
library(fs)
library(zip)
library(ggmap)
library(usethis)
library(readxl)
library(covdata)
library(tidycensus)


# Oregon and Siskiyou Stuff ------------------------------------------

oregon_siskiyou <- counties(cb = TRUE, class="sf") %>%
  clean_names() %>%
  filter(statefp == "41" | statefp == "06") %>%
  filter(statefp == "41" | name == "Siskiyou") %>%
  st_union() %>%
  st_as_sf() %>%
  st_transform(crs = "WGS84")

use_data(oregon_siskiyou,
         overwrite = TRUE)

oregon_siskiyou_population <- get_acs(geography = "county",
                                      variables = "B01003_001",
                                      state = c("OR", "CA")) %>%
  clean_names() %>%
  rename(county = name,
         population = estimate) %>%
  filter(str_detect(county, "Oregon") | str_detect(county, "Siskiyou")) %>%
  separate(county, into = c("county", "state"), sep = ", ") %>%
  mutate(county = str_remove(county, " County")) %>%
  select(county, population)


oregon_siskiyou_counties <- counties(cb = TRUE, class="sf") %>%
  clean_names() %>%
  filter(statefp == "41" | statefp == "06") %>%
  filter(statefp == "41" | name == "Siskiyou") %>%
  select(name) %>%
  rename(county = name)



# Communities Destroyed ---------------------------------------------------

# https://www.usnews.com/news/top-news/articles/2020-09-09/explosive-western-us-wildfires-threaten-oregon-towns

communities_destroyed <- c("Detroit",
                           "Blue River",
                           "Vida",
                           "Phoenix",
                           "Talent") %>%
  tibble() %>%
  set_names("community") %>%
  mutate(state = "Oregon") %>%
  mutate(address = str_glue("{community}, {state}")) %>%
  mutate_geocode(address) %>%
  mutate(lat = case_when(
    community == "Phoenix" ~ 42.29,
    TRUE ~ lat
  ))

use_data(communities_destroyed,
         overwrite = TRUE)

# Evacuation Orders -------------------------------------------------------

# Oregon data
# https://oregon-oem-geo.hub.arcgis.com/datasets/fire-evacuation-areas-public?geometry=-137.839%2C41.215%2C-107.429%2C46.743


evacuation_orders_oregon <- st_read("https://opendata.arcgis.com/datasets/dd2bee51e3004dc4b1004df34fbd9e29_0.geojson") %>%
  clean_names() %>%
  st_transform(crs = "WGS84") %>%
  st_filter(oregon_siskiyou, join = st_intersects)

use_data(evacuation_orders_oregon,
         overwrite = TRUE)



# Roads -------------------------------------------------------------------

oregon_roads <- primary_secondary_roads(state = "OR") %>%
  clean_names()

california_roads <- primary_secondary_roads(state = "CA") %>%
  clean_names()

oregon_siskiyou_roads <- bind_rows(oregon_roads, california_roads) %>%
  st_transform(crs = "WGS84") %>%
  st_filter(oregon_siskiyou, join = st_intersects)


use_data(oregon_siskiyou_roads,
         overwrite = TRUE)

# Current Wildfires -------------------------------------------------------

# Definitions of variables here:
# https://www.nwcg.gov/sites/default/files/data-standards/pdf/NWCGWildlandFireEventPolygon.pdf

wildfires_current <- st_read("https://opendata.arcgis.com/datasets/5da472c6d27b4b67970acc7b5044c862_0.geojson") %>%
  clean_names() %>%
  st_transform(crs = "WGS84") %>%
  st_filter(oregon_siskiyou, join = st_intersects)

use_data(wildfires_current,
         overwrite = TRUE)


# 2020 Wildfires Perimeter Data -------------------------------------------

wildfires_2020 <- st_read("https://opendata.arcgis.com/datasets/bf373b4ff85e4f0299036ecc31a1bcbb_0.geojson") %>%
  clean_names() %>%
  st_transform(crs = "WGS84") %>%
  st_filter(oregon_siskiyou, join = st_intersects)

use_data(wildfires_2020,
         overwrite = TRUE)


# All-Time Wildfires Data -------------------------------------------------

# https://data-nifc.opendata.arcgis.com/datasets/interagency-fire-perimeter-history-all-years

# download.file(url = "https://opendata.arcgis.com/datasets/4454e5d8e8c44b0280258b51bcf24794_0.geojson",
#               destfile = "data-raw/all-time-wildfire-perimeter-data.geojson")

wildfires_all_time <- st_read("https://opendata.arcgis.com/datasets/4454e5d8e8c44b0280258b51bcf24794_0.geojson") %>%
  clean_names() %>%
  st_transform(crs = "WGS84") %>%
  st_filter(oregon_siskiyou, join = st_intersects)

use_data(wildfires_all_time,
         overwrite = TRUE)

# Zip Code Data -----------------------------------------------------------

oregon_siskiyou_zips <- zctas(cb = TRUE,
                              class="sf") %>%
  clean_names() %>%
  st_transform(crs = "WGS84") %>%
  select(zcta5ce10) %>%
  rename(zip = zcta5ce10) %>%
  st_filter(oregon_siskiyou, join = st_intersects)

use_data(oregon_siskiyou_zips,
         overwrite = TRUE)



# COVID -------------------------------------------------------------------

covid_data <- nytcovcounty %>%
  filter(state == "Oregon" | county == "Siskiyou") %>%
  select(-fips) %>%
  arrange(county, state) %>%
  left_join(oregon_siskiyou_population, by = "county") %>%
  mutate(cases_per_1000 = cases / (population / 1000)) %>%
  mutate(cases_seven_day_avg = slider::slide_dbl(cases_per_1000, mean,
                                                 .before = 7,
                                                 .after = 0,
                                                 na.rm = TRUE)) %>%
  left_join(oregon_siskiyou_counties, by = "county") %>%
  st_as_sf() %>%
  st_transform(crs = "WGS84")

use_data(covid_data,
         overwrite = TRUE)


# Hospitals ---------------------------------------------------------------

hospitals <- st_read("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.geojson") %>%
  clean_names() %>%
  st_transform(crs = "WGS84") %>%
  st_filter(oregon_siskiyou, join = st_intersects) %>%
  mutate(beds = na_if(beds, "-999"))


hospitals %>%
  st_drop_geometry() %>%
  view()



# Clinics -----------------------------------------------------------------

# Source: https://data.chhs.ca.gov/dataset/primary-care-clinic-annual-utilization-data

# download.file(url = "https://data.chhs.ca.gov/dataset/445db6df-3987-4145-af34-0d7cc3c0e5eb/resource/8bf05bac-5862-4cb6-a5d8-08b3cf7914f8/download/pcc19_util_data_final.xlsx",
#               destfile = "data-raw/pcc19_util_data_final.xlsx")

var_names <- read_excel("data-raw/pcc19_util_data_final.xlsx",
                        sheet = "Page 1-8") %>%
  clean_names() %>%
  names()

clinics_responders_california <- read_excel("data-raw/pcc19_util_data_final.xlsx",
                                            sheet = "Page 1-8",
                                            skip = 5,
                                            range = "A6:VK50000",
                                            col_names = var_names) %>%
  clean_names()


clinics_non_responders_california <- read_excel("data-raw/pcc19_util_data_final.xlsx",
                                                sheet = "NonResp 1-8",
                                                skip = 5,
                                                range = "A6:VK50000",
                                                col_names = var_names) %>%
  clean_names()


clinics_california <- bind_rows(clinics_responders_california,
                                clinics_non_responders_california) %>%
  set_names(var_names) %>%
  filter(county == "Siskiyou") %>%
  select(fac_name:county) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "WGS84")

use_data(clinics_california,
         overwrite = TRUE)


