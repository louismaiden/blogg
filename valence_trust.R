library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(highcharter)
library(moderndive)
library(readxl)
library(tidyverse)
library(stargazer)

load("C:/Users/nyulo/Documents/R/blogg/tracks_subset")
party <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/political_party_by_year.xlsx")
gdp <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/gdp_pc_growth.xlsx")
trust <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/trust_by_year.xlsx")
trust_dom <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/trust_by_year.xlsx", sheet = "dom")
media <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/trust_by_year.xlsx", sheet = "media")
politicians <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/trust_by_year.xlsx", sheet = "politicians")
americans <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/trust_by_year.xlsx", sheet = "americans")
wash_do_right <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/trust_by_year.xlsx", sheet = "wash_do_right")
crooked <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/trust_by_year.xlsx", sheet = "crooked")
state_gov <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/trust_by_year.xlsx", sheet = "state_gov")
local_gov <- read_xlsx("C:/Users/nyulo/Documents/R/blogg/trust_by_year.xlsx", sheet = "local_gov")
#--------------------international problems -------------------------------
trust_by_year <- trust %>% 
  mutate(release_year = as.numeric(substr(`International problems`,1,4))) %>%
  select(-`International problems`)


tracks_subset %>% 
  inner_join(trust_by_year) %>% 
  group_by(release_year) %>% 
  summarize(valence = mean(valence),
            trust = mean(trust)) %>%
  mutate(trust2 = (trust-min(trust))/(max(trust)-min(trust)),
         trust3 = trust / 100) %>% 
  lm(trust3 ~ valence, data = .) %>% 
  stargazer(type = 'html') %>% View



#---------------------media------------------------------------------------

trust_by_year <- media %>% 
  mutate(release_year = as.numeric(substr(media,1,4))) %>%
  select(-media)

tracks_subset %>% 
  inner_join(trust_by_year) %>% 
  group_by(release_year) %>% 
  summarize(valence = mean(valence),
            trust = mean(trust)) %>%
  mutate(trust2 = (trust-min(trust))/(max(trust)-min(trust)),
         trust3 = trust / 100) %>% 
  lm(trust2 ~ valence, data = .) %>% 
  summary

#---------------------american people---------------------------------------------

trust_by_year <- americans %>% 
  mutate(release_year = as.numeric(substr(americans,1,4))) %>%
  select(-americans)

tracks_subset %>% 
  inner_join(trust_by_year) %>% 
  group_by(release_year) %>% 
  summarize(valence = mean(valence),
            trust = mean(trust)) %>%
  mutate(trust2 = (trust-min(trust))/(max(trust)-min(trust)),
         trust3 = trust / 100) %>% 
  lm(trust3 ~ valence, data = .) %>% 
  summary

#---------------------wash_do_right---------------------------------------------

trust_by_year <- wash_do_right %>% 
  mutate(release_year = as.numeric(substr(wash_do_right,1,4))) %>%
  select(-wash_do_right)

tracks_subset %>% 
  inner_join(trust_by_year) %>% 
  group_by(release_year) %>% 
  summarize(valence = mean(valence),
            trust = mean(trust)) %>%
  mutate(trust2 = (trust-min(trust))/(max(trust)-min(trust)),
         trust3 = trust / 100) %>% 
  lm(trust3 ~ valence, data = .) %>% 
  summary

#---------------------crooked---------------------------------------------

trust_by_year <- crooked %>% 
  mutate(release_year = as.numeric(substr(crooked,1,4))) %>%
  select(-crooked)

tracks_subset %>% 
  inner_join(trust_by_year) %>% 
  group_by(release_year) %>% 
  summarize(valence = mean(valence),
            trust = mean(trust)) %>%
  mutate(trust2 = (trust-min(trust))/(max(trust)-min(trust)),
         trust3 = trust / 100) %>% 
  lm(trust3 ~ valence, data = .) %>% 
  summary


