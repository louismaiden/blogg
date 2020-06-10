
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(broom)
library(httr)
library(tigris)
library(rgdal)
library(googleway)
library(taskscheduleR)

# myscript <- system.file("extdata", "helloworld.R", package = "taskscheduleR")
# 
# ## run script once within 62 seconds
# taskscheduler_create(taskname = "myfancyscript", rscript = myscript, 
#                      schedule = "ONCE", starttime = format(Sys.time() + 62, "%H:%M"))
# 
# myscript <- system.file()


key1 <- "AIzaSyCpI4_qbUAzmxW8XdMzdKSzH3kOZKk9Q_s"

 
 # GET ZIPCODE NAMES
 r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
 nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
 
 
time_begin <- Sys.time()

 nyc_zip_names <- nyc_neighborhoods@data %>%
   as_tibble() %>% 
   mutate(zip = paste0(neighborhood, ", ", borough)) %>% 
   unique() %>% 
   pull(zip)
 

shell <- tibble()  

for(zip_name in nyc_zip_names) {
  
  #zip_name <- gsub(" ", "_", zip_name)
  
  zip_name <- print(zip_name)
  
  # PULL DATA 1
  api_result1 <- google_places(search_string = paste0("Restaurants in ", zip_name, ", ", ",New York"), key = key1)
  print("FIRST API PULLED")
  print(paste0(api_result1$results %>% names(), "ARE THE NAMES IN DF1"))  
  
  
  temp1 <- api_result1 %>% 
    .$results %>% 
    as_tibble() %>% 
    janitor::clean_names() %>%
    mutate(opening_hours = as.character(opening_hours)) %>% 
    select(name, business_status, price_level, place_id, starts_with("rat"), types) %>%
    mutate(lat = api_result1$results$geometry$location$lat,
           lng = api_result1$results$geometry$location$lng,
           open = business_status == "OPERATIONAL",
           neighborhood = zip_name)
  print("FIRST DF SAVED")
  
  token1 <- api_result1$next_page_token
  print("FIRST TOKEN SAVED")
  print(paste0(token1))
  
  Sys.sleep(3)

  # PULL DATA 2
api_result2 <- google_places(search_string = paste0("Restaurants in ", zip_name, ", ", ",New York"), page_token = token1, key = key1)
print("SECOND API PULLED")
print(paste0(api_result2$results %>% names(), "ARE THE NAMES IN DF2"))

  
  temp2 <- api_result2 %>% 
    .$results %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    select(name, business_status, price_level, place_id, starts_with("rat"), types) %>%
    mutate(lat = api_result2$results$geometry$location$lat,
           lng = api_result2$results$geometry$location$lng,
           open = business_status == "OPERATIONAL",
           neighborhood = zip_name)
  print("SECOND DF SAVED")
  
  token2 <- api_result2$next_page_token
  print("SECOND TOKEN SAVED")
  
  Sys.sleep(3)
  
  # PULL DATA 2
  api_result3 <- google_places(search_string = paste0("Restaurants in ", zip_name, ", ", ",New York"), page_token = token2, key = key1)
  print("THIRD API PULLED")
  print(paste0(api_result3$results %>% names(), "ARE THE NAMES IN DF3"))
  
  
  temp3 <- api_result3 %>% 
    .$results %>% 
    as_tibble() %>% 
    janitor::clean_names() %>%
    select(name, business_status, price_level, place_id, starts_with("rat"), types) %>%
    mutate(lat = api_result3$results$geometry$location$lat,
           lng = api_result3$results$geometry$location$lng,
           open = business_status == "OPERATIONAL",
           neighborhood = zip_name)
  print("THIRD DF SAVED")
  
  #token2 <- api_result2$next_page_token
  #print("FIRST TOKEN SAVED")
  
  Sys.sleep(3)
  
  neighborhood_df <- bind_rows(temp1, temp2, temp3)
  
  shell <- bind_rows(shell, neighborhood_df)
  
}

time_end <- Sys.time()

time_begin - time_end

nyc_restaurants <- shell %>% 
  mutate(types = as.character(types),
         pull_date = Sys.Date() - 1,
         pull_datetime = Sys.time() - 86400, 
         pull_week = lubridate::week(pull_date)) %>% 
    as_tibble() %>% 
  rename(old_neighborhood = neighborhood) %>% 
  separate(old_neighborhood, c("neighborhood", "borough"), sep = ",", remove = FALSE)

date_pulled <- nyc_restaurants %>% slice(1) %>% mutate(pull_date = str_replace_all(pull_date, "-", "_")) %>% pull(pull_date)

write_csv(nyc_restaurants, paste0("./data/nyc_restaurant_closures/",date_pulled, "_nyc_full.csv"))


#write_csv(nyc_restaurants, paste0("./data/nyc_restaurant_closures/",date_pulled, "_nyc_restaurant_closures.csv"))


# BIND INDIVIDUAL FILES
files <- list.files("C:/Users/nyulo/Documents/R/blogg/data/nyc_restaurant_closures/", full.names = TRUE) %>% 
          as_tibble() %>% 
          filter(str_detect(value, "_full")) %>% 
          filter(!str_detect(value, "_bound")) %>% 
    pull()

bound <- files %>%
  map_df(~read_csv(.) %>% 
           map(as.character)) %>% 
  mutate(price_level = price_level %>% as.integer,
         rating = rating %>% as.numeric,
         open = open %>% as.logical(),
         pull_date = pull_date %>% lubridate::ymd()
         ) %>% 
  mutate(old_neighborhood = paste0(neighborhood, ", ", borough)) %>% 
  mutate(old_neighborhood = case_when(old_neighborhood == "Bay Terrace, Staten Island, Staten Island" ~ "Bay Terrace, Staten Island",
                                      old_neighborhood == "Chelsea, Staten Island, Staten Island" ~ "Chelsea, Staten Island",
                                      TRUE ~ old_neighborhood)) 

write_csv(bound, paste0("./data/nyc_restaurant_closures/",date_pulled, "_nyc_full_bound.csv"))


bound %>% count(pull_date)
  
# GRAPH
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)


nyc_zip_names <- nyc_neighborhoods@data %>% as_tibble() %>% 
  mutate(zip = paste0("Restaurants in ",neighborhood, ", ", borough, ", New York")) %>% 
  pull(zip) %>% 
  unique()



# nyc_restaurants <- shell
# coordinates(nyc_restaurants) <- ~lng + lat
# proj4string(nyc_restaurants) <- proj4string(nyc_neighborhoods)
# matches <- over(nyc_restaurants, nyc_neighborhoods)
# points <- cbind(shell, matches) %>% as_tibble()


oldest_pull <- bound %>%
  filter(pull_date == min(pull_date)) %>%
  mutate(closed = business_status == "CLOSED_TEMPORARILY") %>% 
  group_by(neighborhood) %>%
  summarize(closure_rate = mean(closed),
            pull_date = max(pull_date),
            n = n())


changes <- bound %>% 
  filter(pull_date == max(pull_date)) %>% 
  mutate(closed = business_status == "CLOSED_TEMPORARILY") %>%
  group_by(neighborhood) %>% 
  summarize(closure_rate = mean(closed),
            pull_date = max(pull_date),
            n = n()) %>% 
  ungroup() %>% 
  bind_rows(oldest_pull) %>% 
  arrange(neighborhood, pull_date) %>%
  group_by(neighborhood) %>% 
  mutate(change = closure_rate - lag(closure_rate),
         change = change * 100) %>%
  ungroup() %>% 
  filter(!is.na(change)) %>% 
  select(neighborhood, max_date = pull_date, change)


# points <- points %>%
#   group_by(neighborhood) %>%
#   mutate(n = n()) %>%
#   ungroup() %>%
#   filter(n >= 10)


map_data <- geo_join(nyc_neighborhoods, oldest_pull, "neighborhood", "neighborhood")
map_data_change <- geo_join(nyc_neighborhoods, changes, "neighborhood", "neighborhood")

# nyc_neighborhoods@data <- nyc_neighborhoods@data %>% as_tibble() %>%
#   inner_join(points %>% select(name, business_status, neighborhood)) %>%
#   select(name, neighborhood, everything()) %>%
#   mutate(closed = business_status == "CLOSED_TEMPORARILY") %>%
#   group_by(neighborhood) %>%
#   mutate(closure_rate = mean(closed),
#          sample_size = n()) %>%
#   ungroup()

closure_pal <- colorNumeric(c("Reds"), domain  = oldest_pull$closure_rate)
closure_pal_change <- colorNumeric(c("RdBu"), domain  = changes$change + 1.5 %>% sqrt())


covid_map <- nyc_neighborhoods %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = map_data, weight = 2, fillOpacity = .4,
              group = "Base",
              color = ~closure_pal(closure_rate),
              popup = ~paste0("<b>", neighborhood, "</b>", "<br>",
                              "Closure Rate: ", paste0(round(closure_rate, 2), "%"), "<br>",
                              "Neighborhood Sample Size: ", n)) %>%
  addPolygons(data = map_data_change, weight = 2, fillOpacity = .4,
              group = "Change",
              color = ~closure_pal_change(change),
              popup = ~paste0("<b>", neighborhood, "</b>", "<br>",
                              "Change in Closure Rate: ", paste0(round(change, 2), "%"), "<br>")) %>%
  addLayersControl(overlayGroups = c("Base", "Change")) %>% 
  setView(-73.9840, 40.7549, zoom = 11)


htmlwidgets::saveWidget(covid_map, file = "covid_map.html")
