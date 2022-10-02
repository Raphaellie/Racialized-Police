rm(list = ls())

# packages
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(zoo)
library(lubridate)
library(readstata13)
library(tidycensus)
library(viridisLite)
library(wesanderson)
library(zipcodeR)
library(broom)

# Police Violence ----

pv.df <- read_csv("input/fatal shootings/Mapping Police Violence.csv") %>% 
  select(name,age,gender,race,date,city,state2 = state,zip,county,
         agency = agency_responsible) %>% 
  mutate(date = (as.Date(date,format = "%m/%d/%Y")),
         year = year(date),
         state = state.name[match(state2,state.abb)])

## exclude those that happened after the start of CCES 2020
pv.df <- pv.df %>% filter(date < as.Date('2020-9-29'))
  
## get PV level from 2015 to 2019 ----
pv.df<- pv.df %>%
  filter(between(year,2020,2020)) %>% 
  group_by(state,city) %>% 
  summarise(pv = n(), 
            pv_white = sum(race == 'White'),
            pv_black = sum(race == 'Black') ) %>% 
  mutate(pv_white_p = pv_white / pv,
         pv_black_p = pv_black / pv,
         pv_white_d = (pv_white > 0),
         pv_black_d = (pv_black > 0),)

## descriptive viz
pv.df %>% 
  ggplot(aes(x = pv_black)) + 
  geom_histogram()

# Local Democragphics ----

local.acs <- 
  get_acs(geography = "place", year = 2019, output = 'tidy',survey = 'acs5',
          variables = c("B01003_001","B03002_003","B02008_001",'B02009_001',"B03001_003"))

## collect geo info ----
local.race <- 
  local.acs %>% 
  mutate(state = str_extract(NAME,', .*') %>% str_sub(start = 3) ,
         city = str_extract(NAME,'(.*),') %>% str_sub(end = -2)) %>% 
  mutate(state2 = state.abb[match(state,state.name)] ,
         city = str_replace(city,"\\s*\\([^\\)]+\\)", ""),
         city = word(city,1,-2))

## change variable name to races
local.race <- local.race %>% 
  rename(var = variable) %>% 
  mutate(var = ifelse(var == "B01003_001",'all',var),
         var = ifelse(var == "B03002_003",'white_pop',var), 
         var = ifelse(var == "B02008_001",'white2_pop',var), 
         # non-hispanic white: B03002_003 ; all white: B02008_001
         var = ifelse(var == 'B02009_001','black_pop',var),
         var = ifelse(var == "B03001_003",'hispanic_pop',var),)

write_csv(local.race,'data/local ACS races.csv')

local.race <- read_csv('data/local ACS races.csv')
## merge fragmented cities ----
local.race <- 
  local.race %>% 
  group_by(state,city,var) %>% 
  mutate(estimate = sum(estimate)) %>% 
  select(-NAME,-moe,-GEOID) %>% 
  distinct()

## reshape data from long to wide ----
local.race <- 
  local.race %>% 
  group_by(city,state) %>% 
  mutate(pop = first(estimate),
         estimate = estimate / first(estimate)) %>% 
  pivot_wider(names_from = var,values_from = estimate) 



# Racialized Police ---- 
rp.df <- read_csv('input/LEMAS2016/LEMAS2016-cleaned.csv')

rp.df <- rp.df %>% 
  rename_all(.,.funs = tolower) %>% 
  select(agency = agencyname, city, zipcode, state2 = state, county, fips, popserved, 
         sworn = ftsworn, sworn2 = totftemp,
         contains(c('black_','white_','hisp_','asian_'))) %>% 
  mutate(black_sworn = pers_black_male + pers_black_fem,
         white_sworn = pers_white_male + pers_white_fem,
         hisp_sworn = pers_hisp_male + pers_hisp_fem)

## adjust geo info ----
rp.df <- rp.df %>% 
  mutate(state = state.name[match(state2,state.abb)],
         city = str_to_title(city)  )  
  
rp.cities <- rp.df %>%
  mutate(zipcode = as.character(zipcode)) %>% 
  distinct(zipcode) %>%  
  mutate(city2 = reverse_zipcode(zipcode)$post_office_city)

rp.cities <- reverse_zipcode(rp.df$zipcode) %>% 
  select(zipcode, city2 = major_city,
         lat,lon = lng,county_ = county,state3 = state)

rp.df <- rp.df %>% left_join(rp.cities,by = 'zipcode')

## calculate racial shares ---- 
rp.df <- rp.df %>% 
  mutate(black_in_sworn = black_sworn / sworn, 
         white_in_sworn = white_sworn / sworn, 
         hisp_in_sworn = hisp_sworn / sworn) %>% 
  filter(black_in_sworn < 2, white_in_sworn < 2) %>% 
  mutate(across(contains('_in_sworn'),.fns = ~ifelse(.x >= 1,1,.x) )) 

rp.df <- rp.df %>% 
  select(agency,zipcode,popserved,city,state,state2,county,lat,lon,fips,
         sworn,ends_with('_sworn'))

## descriptive viz
rp.df %>% 
  ggplot() + 
  geom_density(aes(white_in_sworn))

# JOIN: local pop. & racialized police ----

rp.df <- 
  rp.df %>% 
  left_join(local.race,by = c('city','state2','state'))

## get racial imageries ----

rp.df <- 
  rp.df %>% 
  mutate(white_rep = white_in_sworn - white_pop, 
         black_rep = black_in_sworn - black_pop,
         hisp_rep = hisp_in_sworn - hispanic_pop)

## descriptive viz ----

rp.df.long <- rp.df %>% 
  select(contains('_rep'),agency,city,state,popserved,lat,lon)%>% 
  pivot_longer(cols = contains('_rep'), values_to = 'imagery',names_to = 'race') %>% 
  mutate(race = case_when(race == 'black_rep' ~ 'Black',
                          race == 'white_rep' ~ 'White',
                          race == 'hisp_rep' ~ 'Hispanic')) %>% 
  filter(!is.na(imagery)) 

### bar plot ---- 
rp.df.long %>%
  ggplot() + 
  geom_bar(aes(imagery >= 0,fill = imagery >= 0),alpha = 0.7,color = 'gray12') + 
  facet_grid(~race) + theme_bw() +
  scale_fill_viridis_d(option = 'mako', end = 0.65, direction = 1) + 
  scale_x_discrete(labels = c('Under-Rep.','Over-Rep.'))  +
  theme(legend.position  ='none',
        aspect.ratio = 1,
        axis.title = element_blank(),
        axis.text = element_text(color = 'gray10')) 

### density plot ---- 
rp.df.long %>%
  ggplot(aes(x = imagery)) + 
  geom_density(aes(imagery,fill = race == 'White'),alpha = 0.7,color = 'gray12') + 
  geom_vline(xintercept = 0, lty = 2, color = 'black') + 
  facet_wrap(~race,scales = 'free_x') + 
  scale_fill_viridis_d(option = 'mako', end = 0.65, direction = 1) +
  xlim(c(-0.5,0.5)) + theme_bw() + 
  theme(legend.position  ='none',
        aspect.ratio = 1,
        axis.title = element_blank(),
        axis.text = element_text(color = 'gray10')) 

# Police Attitudes ----

cces <- read_csv('input/CCES20 attitudes/cces attitudes.csv')

# JOIN: cces & local police ---- 

cces.cities <- cces %>% distinct(zipcode)
cces.cities <- reverse_zipcode(cces.cities$zipcode)
cces.cities <- cces.cities %>%
  select(zipcode,city = major_city)

## get R's city by zip code ----
cces <- 
  cces %>% 
  left_join(cces.cities,by = 'zipcode')

## linking racialized police ----
cces <- 
  cces %>% 
  left_join(rp.df,by = c('city','state'))

## linking police violence ----
cces <- 
  cces %>% 
  left_join(pv.df,by = c('city','state')) %>% 
  mutate(pv = ifelse(is.na(pv), 0, pv),
         pv_white = ifelse(is.na(pv_white), 0, pv_white),
         pv_black = ifelse(is.na(pv_black), 0, pv_black),) 
# set pv level to 0 by default if no related police killings were reported.

## polish variables ---- 
cces <- cces %>% 
  filter(!is.na(sworn)) %>% 
  mutate(police_safe = (5 - police_safe) / 2,
         police_increase = 3 - police_increase,
         police_decrease = 3 - police_decrease,
         pvd = as.numeric(pv > 0),# measure police violence on a binary basis
         white = factor(white) )


