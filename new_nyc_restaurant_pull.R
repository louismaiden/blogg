
  
  
library(tidyverse)
library(leaflet)
library(tigris)
library(googleway)
# #1

key1 <- "AIzaSyCpI4_qbUAzmxW8XdMzdKSzH3kOZKk9Q_s"

#nyc_zip_names <- c("Financial District", "Battery Park City", "Chelsea")

nyc_zip_names <- c("Financial District")

shell <- tibble()

for(zip_name in nyc_zip_names) {
  
  print(zip_name)

# PULL DATA 1
  api_result1 <- google_places(search_string = paste0("Restaurants in ", zip_name, ",New York"), key = key1)
  print("FIRST API PULLED")
  
  temp1 <- api_result1 %>% 
    .$results %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    select(name, business_status, price_level, place_id, rating, types) %>% 
    mutate(lat = api_result1$results$geometry$location$lat,
           lng = api_result1$results$geometry$location$lng,
           open = business_status == "OPERATIONAL")
  print("FIRST DF SAVED")

  token1 <- api_result1$next_page_token
  print("FIRST TOKEN SAVED")



  # PULL DATA 2
google_places(search_string = paste0("Restaurants in ", zip_name, ",New York"),
                               page_token = token1,
                               key = key1) %>% 
    .$results %>%  
}

api_result1
api_result2


  print("SECOND API PULLED")

  temp2 <- api_result2 %>% 
    .$results %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    select(name, business_status, price_level, place_id, rating, types) %>% 
    mutate(lat = api_result2$results$geometry$location$lat,
           lng = api_result2$results$geometry$location$lng,
           open = business_status == "OPERATIONAL")
  
  print("SECOND DF SAVED")
  

}
  
temp2


api_result2 %>% 
  .$results %>% 
  as_tibble()




  token2 <- api_result2$next_page_token
  print("THIRD TOKEN SAVED")
  
  
  
  # # PULL DATA 3
  # api_result3 <- google_places(search_string = paste0("Restaurants in ", zip_name, ",New York"),
  #                              page_token = token2,
  #                              key = key1)
  # temp3 <- api_result3 %>% 
  #   .$results %>% 
  #   as_tibble() %>% 
  #   janitor::clean_names() %>% 
  #   select(name, business_status, price_level, place_id, rating, types) %>% 
  #   mutate(lat = res_base1$results$geometry$location$lat,
  #          lng = res_base1$results$geometry$location$lng,
  #          open = business_status == "OPERATIONAL")
  


shell <- bind_rows(shell, temp1, temp2, temp3)

}

shell

google_places(search_string = paste0("Restaurants in", "Financial District", "New York"), key = key1)

temp
# CONVERT TO DF
base_df1 <- res_base1$results %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(name, business_status, price_level, place_id, rating, rating, types) %>% 
  mutate(lat = res_base1$results$geometry$location$lat,
         lng = res_base1$results$geometry$location$lng,
         open = business_status == "OPERATIONAL")

}

# SAVE NEXT TOKEN
token1 <- res_base1$next_page_token

# #2

# PULL DATA 2 USING TOKEN #1
res_base2 <- google_places(search_string = "Restaurants in Manhattan, New York",
                           page_token = token1,
                           key = key1)

# CONVERT DATA 2 TO DF
base_df2 <- res_base2$results %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(name, business_status, price_level, place_id, rating, rating, types) %>% 
  mutate(lat = res_base2$results$geometry$location$lat,
         lng = res_base2$results$geometry$location$lng,
         open = business_status == "OPERATIONAL")

# SAVE NEXT TOKEN
token2 <- res_base2$next_page_token

base_df2

```

```{r}
# PULL DATA 3  USINF TOKEN # 2
res_base3 <- google_places(search_string = "Restaurants in Manhattan, New York",
                           page_token = token2,
                           key = key1)

base_df3 <- res_base3$results %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(name, business_status, price_level, place_id, rating, rating, types) %>% 
  mutate(lat = res_base3$results$geometry$location$lat,
         lng = res_base3$results$geometry$location$lng,
         open = business_status == "OPERATIONAL")

token3 <- res_base3$next_page_token

base_df3

restaurants <- bind_rows(base_df1, base_df2, base_df3)

restaurants %>% 
  ggplot(aes(rating, fill = open)) +
  geom_density(alpha = .5)

pal <- colorFactor(c("red","blue"), domain = c(TRUE, FALSE))

# restaurants %>% 
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(lat = ~lat, lng = ~lng, color = ~pal(open), stroke = FALSE, fillOpacity = .5,
#                     popup = ~name)


```


```{r}
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

nyc_restaurants <- restaurants
coordinates(nyc_restaurants) <- ~lng + lat
proj4string(nyc_restaurants) <- proj4string(nyc_neighborhoods)
matches <- over(nyc_restaurants, nyc_neighborhoods)
points <- cbind(restaurants, matches) %>% as_tibble()

avgs_by_neighborhood <- points %>% 
  mutate(closed = business_status == "CLOSED_TEMPORARILY") %>% 
  group_by(neighborhood) %>% 
  summarize(closure_rate = mean(closed),
            n = n())



map_data <- geo_join(nyc_neighborhoods, avgs_by_neighborhood, "neighborhood", "neighborhood")

nyc_neighborhoods@data <- nyc_neighborhoods@data %>% as_tibble() %>% 
  inner_join(points %>% select(name, business_status, neighborhood)) %>% 
  select(name, neighborhood, everything()) %>% 
  mutate(closed = business_status == "CLOSED_TEMPORARILY") %>% 
  group_by(neighborhood) %>% 
  mutate(closure_rate = mean(closed),
         sample_size = n()) %>% 
  ungroup()

closure_pal <- colorNumeric(c("Reds"), domain  = nyc_neighborhoods@data$closure_rate)


closure_map <- nyc_neighborhoods %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = map_data, weight = 2, fillOpacity = .4, 
              group = "neighborhood",
              color = ~closure_pal(closure_rate),
              popup = ~paste0("<b>", neighborhood, "</b>", "<br>",
                              "Closure Rate: ", closure_rate, "<br>",
                              "Neighborhood Sample Size: ", n))


closure_map

