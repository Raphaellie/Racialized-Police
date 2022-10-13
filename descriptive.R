# packages
library(usmap)
library(wesanderson)

# Police Imagery in LEMAS2016 ----

rp.df.long <- rp.df %>% 
  select(contains('_rep'),agency,city,state,popserved)%>% 
  pivot_longer(cols = contains('_rep'), values_to = 'imagery',names_to = 'race') %>% 
  mutate(race = case_when(race == 'black_rep' ~ 'Black',
                          race == 'white_rep' ~ 'White',
                          race == 'hisp_rep' ~ 'Hispanic')) %>% 
  filter(!is.na(imagery)) 

## bar plot ---- 
rp.df.long %>%
  ggplot(aes(x = imagery >= 0)) + 
  geom_bar(aes(fill = imagery >= 0),alpha = 0.7,color = 'gray12') + 
  facet_grid(~factor(race,levels = c('White','Black','Hispanic'))) + 
  stat_count(geom = "text", colour = "white", size = 4, fontface = 'plain',
             aes(label = ..count..),position=position_stack(vjust=0.5)) + 
  scale_fill_viridis_d(option = 'mako', end = 0.65, direction = 1) + 
  scale_x_discrete(labels = c('Under-\nrepresented','Over-\nrepresented'))  +
  theme_bw() +
  theme(legend.position  ='none',
        aspect.ratio = 1,
        axis.title = element_blank(),
        axis.text = element_text(color = 'gray10',)) 

ggsave('figures/desc-lemas.pdf',width = 11,height = 11/3)

## OR density plot ---- 

fig.lemas.density <- 
rp.df.long %>%
  ggplot(aes(x = imagery)) + 
  geom_density(aes(imagery,fill = race == 'White'),alpha = 0.45,color = 'gray12') + 
  geom_vline(xintercept = 0, lty = 2, color = 'black') + 
  facet_wrap(~race,scales = 'free_x') + 
  scale_fill_viridis_d(option = 'cividis',end = 0.7, name = "Respondent's Race",labels = c('Others','Non-Hispanic White')) +
  # scale_fill_manual(values = c('seagreen','steelblue')) +
  xlim(c(-0.5,0.5)) + 
  ylab(NULL) + xlab('Racial Imagery of Police Departments by Racial Group') +
  theme_bw() + 
  theme(legend.position  ='none',
        aspect.ratio = 0.9,
        axis.text = element_text(color = 'gray10')) 
fig.lemas.density

ggsave('figures/desc-lemas2.pdf',width = 11,height = 11/3)


# Geography of LEMAS Sample ---- 

## city points ---- 

city.points <- rp.df %>% 
  filter(!is.na(lon), lon > -160) %>% 
  select(lon,lat,popserved) %>% 
  data.frame %>% usmap_transform()

plot_usmap("states",size = 0.2) + 
  geom_point(data = city.points, aes(x = x,y = y),position = 'jitter',
             color = "navyblue", size = 0.6,alpha = 1/3)
ggsave('figures/lemas-map.pdf')

## county blocks ---- 

ct.count <- rp.df %>% 
  group_by(state2,fips) %>%
  mutate(fips = str_pad(fips,5,pad= '0') ) %>% 
  summarise(n = n()) 

df <- countypop %>% 
  left_join(ct.count,by = 'fips') %>% 
  mutate(n = ifelse(is.na(n),0,n),
         sampled = (n >0)) 

fig.lemas.cover <- 
  plot_usmap(data = df,regions = 'counties',values = 'sampled',
           size = 0.05,color ='gray40')  + 
  geom_polygon(data = us_map(regions = "states"),aes(x, y, group = group), 
               size = 1/4, fill = NA,color = "black") + 
  scale_fill_manual(values = c('white','lightskyblue3'),na.value = 'white',
                    name = "Any Police Department Sampled",
                    labels = c('No','Yes'), breaks = c(FALSE,TRUE)) + 
  theme(legend.position = 'bottom',
        legend.justification = 1/2) 

fig.lemas.cover

ggsave('figures/lemas-map-ct.pdf',width = 12, height = 4)
  
# Policing Attitudes in 20 CES ---- 

attitudes <- cces %>% 
  select(white, safe = police_safe,increase = police_increase,decrease = police_decrease) %>%
  group_by(white) %>% 
  summarise(across(everything(), 
                   funs(mean = mean(., na.rm = T),sd = sd(., na.rm = T),n = sum(!is.na(.)))
                   )) %>%
  # tidying
  pivot_longer(cols = 2:10, names_to = c('var','stat'),names_sep = '_') %>% 
  pivot_wider(names_from ='stat') %>%
  mutate(se = sd/sqrt(n),
         ciup = mean + qnorm(.975)*se, 
         cilow = mean - qnorm(.975)*se)

fig.attitudes <- 
attitudes %>% 
  mutate(var = factor(var, levels = c('safe','increase','decrease'),
                      labels = c('Police Make R Feel Safe',
                                 'R Wants to Increase Police',
                                 'R Wants to Decrease Police'))) %>% 
  ggplot(aes(x = white, y = mean, ymin = cilow, ymax = ciup,fill = white)) + 
  geom_bar(color = 'black',stat = 'identity',alpha = 0.6,position = 'dodge2',width = 0.7) +
  geom_errorbar(position = position_dodge(width = 0.6),width = 0.2/2, size = 0.3) +
  facet_wrap(~var, scales = 'free_y') + 
  scale_fill_viridis_d(option = 'cividis',end = 0.7, name = "Respondent's Race",labels = c('Others','Non-Hispanic White')) +
  # scale_fill_manual(values = (wes_palette('Darjeeling2'))) + 
  scale_x_discrete(labels = c('Not White','White')) + 
  geom_text(aes(label = round(mean,2)),color = 'gray5',
             position=position_stack(vjust=0.5)) + 
  theme_bw() + 
  xlab("Respondent's Race") + ylab('Mean') + 
  theme(legend.position = 'none',
        # legend.background = element_rect(color = 'black',size = 1/3),
        aspect.ratio = 0.9)

fig.attitudes
 
## in table form
cces %>% 
  select(white,police_safe, police_increase,police_decrease) %>%
  mutate(white = ifelse(white == 1, 'White','Not White')) %>% 
  rename(`Police Make R Feel Safe` = police_safe,
         `R Wants to Increase Police` = police_increase,
         `R Wants to Decrease Police` = police_decrease,) %>% 
  datasummary_balance(~white,data = .,fmt = 3,
                      notes = 'Notes: All respondents in 2020 CES.')


