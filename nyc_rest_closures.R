
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(broom)
library(httr)
library(tigris)
library(rgdal)
library(googleway)


key1 <- "AIzaSyCpI4_qbUAzmxW8XdMzdKSzH3kOZKk9Q_s"

 nyc_zip_names <- c("Inwood",
   "Washington Heights",
   "Harlem",
   "Morningside Heights",
   "East Harlem",
   "Upper West Side",
   "Upper East Side",
   "Hell's Kitchen",
   "Theater District",
   "Midtown",
   "Murray Hill",
   "Chelsea",
   "Flatiron District",
   "Kips Bay",
   "Gramercy",
   "Stuyvesant Town",
   "West Village",
   "Greenwich Village",
   "NoHo",
   "East Village",
   "SoHo",
   "Nolita",
   "Lower East Side",
   "Little Italy",
   "Tribeca",
   "Chinatown",
   "Civic Center",
   "Two Bridges",
   "Battery Park City",
   "Financial District")


nyc_restaurants <- shell %>% 
  mutate(types = as.character(types),
         pull_date = Sys.Date(),
         pull_datetime = Sys.time(), 
         pull_week = lubridate::week(pull_date))

date_pulled <- nyc_restaurants %>% slice(1) %>% mutate(pull_date = str_replace_all(pull_date, "-", "_")) %>% pull(pull_date)

write_csv(nyc_restaurants, paste0("./data/",date_pulled, "_nyc_restaurant_closures.csv"))

### GRAPH
# 
# pal <- colorFactor(c("red","blue"), domain = c(TRUE, FALSE))
# 
# shell %>% 
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(lat = ~lat, lng = ~lng, color = ~pal(open), stroke = FALSE, fillOpacity = .5,
#                    popup = ~name)


r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# nyc_restaurants <- shell
# coordinates(nyc_restaurants) <- ~lng + lat
# proj4string(nyc_restaurants) <- proj4string(nyc_neighborhoods)
# matches <- over(nyc_restaurants, nyc_neighborhoods)
# points <- cbind(shell, matches) %>% as_tibble()




avgs_by_neighborhood <- nyc_restaurants %>% 
  group_by(neighborhood) %>% 
  summarize(open_rate = mean(open),
            open_rate = 100 * open_rate,
            open_rate = 100 - open_rate,
            n = n())

# points <- points %>% 
#   group_by(neighborhood) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   filter(n >= 10)


map_data <- geo_join(nyc_neighborhoods, avgs_by_neighborhood, "neighborhood", "neighborhood")

nyc_neighborhoods@data <- nyc_neighborhoods@data %>% as_tibble() %>%
  inner_join(points %>% select(name, business_status, neighborhood)) %>%
  select(name, neighborhood, everything()) %>%
  mutate(closed = business_status == "CLOSED_TEMPORARILY") %>%
  group_by(neighborhood) %>%
  mutate(closure_rate = mean(closed),
         sample_size = n()) %>%
  ungroup()


nyc_pal <- colorNumeric(c("Reds"), domain  = nyc_neighborhoods@data$median_score)


nyc_neighborhoods %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = map_data, weight = 2, fillOpacity = .4,
              color = ~nyc_pal(open_rate),
              group = "Neighborhood",
              popup = ~paste0("<b>", neighborhood, "</b>", "<br>",
                            "Closure Rate: ", paste0(round(open_rate, 2), "%"), "<br>")) %>% 
  setView(-73.9840, 40.7549, zoom = 11)



nyc_neighborhoods
