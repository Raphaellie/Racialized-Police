# Police Attitudes ----

cces.22 <- read_csv('input/CCES22 attitudes/cces22 attitudes.csv') %>% 
  mutate(year = 2022)

# Join: cces & local police ---- 

cces.22.cities <- cces.22 %>% distinct(zipcode)
cces.22.cities <- reverse_zipcode(cces.22.cities$zipcode)
cces.22.cities <- cces.22.cities %>%
  select(zipcode,city = major_city)

## get R's city by zip code ----
cces.22 <-
  cces.22 %>%
  left_join(cces.22.cities,by = 'zipcode')

## linking racialized police ----
cces.22 <-
  cces.22 %>%
  left_join(rp.df,by = c('city','state'))

# ## linking police violence ----
cces.22 <-
  cces.22 %>%
  left_join(pv.df,by = c('city','state','year')) %>%
  mutate(pv = ifelse(is.na(pv), 0, pv),
         pv_white = ifelse(is.na(pv_white), 0, pv_white),
         pv_black = ifelse(is.na(pv_black), 0, pv_black),)
# set pv level to 0 by default if no related police killings were reported.

## polish variables ---- 
cces.22 <- cces.22 %>% 
  # filter(!is.na(sworn)) %>% 
  mutate(police_safe = (4 - police_safe)  / 3,
         police_increase = 2 - police_increase,
         police_decrease = 2 - police_decrease,
         police_banchoke = 2 - police_banchoke,
         police_camera = 2 - police_camera,
         police_endarms = 2 - police_endarms,
         police_investigate = 2 - police_investigate,
         police_sue = 2 - police_sue,
         pvd = as.numeric(pv > 0),# measure police violence on a binary basis
         white = factor(white) )

# Compare ----

## by race & educ ---- 
cces.compare <- 
bind_rows(cces %>% mutate(year = '2020'),
          cces.22 %>% mutate(year = '2022')) %>%
  distinct(caseid, .keep_all = TRUE) %>% 
  select(-police_spending) %>%
  mutate(race = case_when(race == 1 ~ 'White',
                          race == 2 ~ 'Black',
                          race == 3 ~ 'Hispanic',
                          TRUE ~ 'Other' ),
         college = ifelse(educ > 3,'Yes','No')  )%>% 
  mutate(race = factor(race, levels = c('White','Black','Hispanic','Other'))) %>% 
  group_by(year,race,college) %>% 
  summarise(across(contains('police'),
                   .fns = list(mean = ~mean(.,na.rm = T),
                               sd = ~sd(.,na.rm = T),
                               n = ~sum(!is.na(.)))
                   )) %>% 
  pivot_longer(cols = contains('police'),names_to = c('var','stat'), 
               names_prefix = 'police_',
               names_sep = '_') %>% 
  pivot_wider(names_from = stat)

cces.compare %>% 
  mutate(se = sd/sqrt(n), 
         conf.low = mean - qnorm(.975)*se, 
         conf.high = mean + qnorm(.975)*se) %>% 
  filter(var %in% c('safe','increase','decrease'),
         # race %in% c('White','Black'),
         ) %>% 
  ggplot(aes(x = year, y = mean,ymin = conf.low, ymax = conf.high,
             fill = college)) + 
  # geom_pointrange(position = position_dodge2(width = 0.5)) +
  geom_hline(yintercept = 0.5, lty = 'dashed',alpha = 1/3) + 
  geom_bar(aes(),position = position_dodge2(),
           stat = 'identity',color = 'gray15',alpha = 0.8,width = 0.6) +
  # geom_errorbar(width = 0.2) + 
  facet_grid(race~var,scales = 'free') + 
  scale_fill_viridis_d(option = 'viridis',end = 0.65) + 
  theme_minimal() + 
  theme(strip.background = element_rect(color = 'gray30',size = 0.8),
        aspect.ratio = 1)

## by state & politics ----

cces.compare <- 
  bind_rows(cces %>% mutate(year = '2020'),
            cces.22 %>% mutate(year = '2022')) %>% 
  distinct(caseid, .keep_all = TRUE) %>% 
  select(-police_spending) %>% 
  mutate(pid3 = case_when(pid7 < 4 ~ 'Democrat',
                          pid7 == 4 ~ 'Independent',
                          pid7 > 4 ~ 'Republican',
                          is.na(pid7) ~ NA_character_ )) %>% 
  filter(race <= 3) %>% 
  group_by(year,group = pid3, race) %>% 
  summarise(across(contains('police'),
                   .fns = list(mean = ~mean(.,na.rm = T),
                               sd = ~sd(.,na.rm = T),
                               n = ~sum(!is.na(.)))
  )) %>% 
  pivot_longer(cols = contains('police'),names_to = c('var','stat'), 
               names_prefix = 'police_',
               names_sep = '_') %>% 
  pivot_wider(names_from = stat)

cces.compare %>% 
  mutate(se = sd/sqrt(n), 
         conf.low = mean - qnorm(.975)*se, 
         conf.high = mean + qnorm(.975)*se) %>% 
  filter(var %in% c('safe','increase','decrease'),
         !is.na(group),
         # year == 2022,
         # race %in% c('White','Black'),
  ) %>% 
  ggplot(aes(x = str_sub(race,1,1), y = mean,ymin = conf.low, ymax = conf.high)) + 
  # geom_pointrange(position = position_dodge2(width = 0.5)) +
  geom_hline(yintercept = 0.5, lty = 'dashed',alpha = 1.5/3) + 
  geom_bar(aes(fill = factor(race)),position = position_dodge2(),
           stat = 'identity',color = 'gray15',alpha = 0.8,width = 0.6) +
  geom_smooth(se = FALSE, method = lm, color = 'gray70') +
  scale_fill_viridis_d(option = 'viridis',end = 0.65) + 
  facet_grid(rows = vars(var), cols = vars(year, group),scales = 'free') + 
  theme_minimal() + 
  theme(strip.background = element_rect(color = 'gray30',size = 0.6, fill = 0),
        legend.position = 'bottom',
        aspect.ratio = 1)


# Effects of Police Violence ----

pv.df <- read_csv("input/fatal shootings/Mapping Police Violence.csv") %>% 
  select(name,age,gender,race,date,city,state2 = state,zip,county,
         agency = agency_responsible) %>% 
  mutate(date = (as.Date(date,format = "%m/%d/%Y")),
         year = year(date),
         state = state.name[match(state2,state.abb)])

## exclude those that happened after the start of CCES 2020 & 2022

pv.df <- pv.df %>% filter(date < as.Date('2020-9-29'))


## get PV level ----
pv.df<- pv.df %>%
  filter(between(year,2020,2022)) %>% 
  group_by(year,state,city) %>% 
  summarise(pv = n(), 
            pv_white = sum(race == 'White'),
            pv_black = sum(race == 'Black') ) %>% 
  mutate(pv_white_p = pv_white / pv,
         pv_black_p = pv_black / pv,
         pv_white_d = (pv_white > 0),
         pv_black_d = (pv_black > 0),)


cces.panel <- bind_rows(cces %>% distinct(caseid,.keep_all = T),
                        cces.22 %>% distinct(caseid,.keep_all = T)) %>% 
  filter(gender <= 2)


interval.fe <-  function(left, right) {
  fe.results <- 
    felm(cbind(police_safe,police_increase,police_decrease) ~ 
           pvd  | year + state ,
         data = cces.panel %>% filter(race == 1, between(white_rep,left, right))) %>% 
    tidy(conf.int = TRUE) %>% 
    filter(term %in% c('pv','pvd')) %>% 
    mutate(interval = paste(left,'-',right),
           response = str_extract(response,'\\.(.*)')
           )
  return(fe.results)
}



fe.results <- data.frame()

for (left in seq(-0.1,0.4,0.1)) {
  fe.results <- bind_rows(fe.results,interval.fe(left,left + 0.1))
}

fe.results %>% 
  ggplot(aes(x = interval, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_pointrange() + 
  geom_hline(yintercept = 0, lty = 'dashed', alpha =0.6) + 
  facet_grid(~response) +
  theme_minimal()





