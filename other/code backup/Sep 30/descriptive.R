# packages
library(usmap)

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
  geom_density(aes(imagery,fill = race == 'White'),alpha = 0.6,color = 'gray12') + 
  geom_vline(xintercept = 0, lty = 2, color = 'black') + 
  facet_wrap(~race,scales = 'free_x') + 
  scale_fill_viridis_d(option = 'mako', end = 0.65, direction = 1) +
  xlim(c(-0.5,0.5)) + 
  ylab(NULL) + xlab('Racial Imagery of Police Departments by Racial Group') +
  theme_bw() + 
  theme(legend.position  ='none',
        aspect.ratio = 1,
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
           size = 0.1,color ='gray40')  + 
  geom_polygon(data = us_map(regions = "states"),aes(x, y, group = group), 
               size = 1/4, fill = NA,color = "black") + 
  scale_fill_manual(values = c('white','lightskyblue3'),na.value = 'white',
                    name = "Any Police Department Sampled",
                    labels = c('No','Yes'), breaks = c(FALSE,TRUE)) + 
  theme(legend.position = 'bottom',
        legend.justification = 1/2) 

fig.lemas.cover

ggsave('figures/lemas-map-ct.pdf',width = 9.6, height = 5)
  

