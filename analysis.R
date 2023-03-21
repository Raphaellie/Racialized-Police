# packages
library(tidyverse)
library(estimatr)
library(broom)
library(interactions)
library(patchwork)
library(modelsummary)
library(sjPlot)
library(ggeffects)
library(kableExtra)

# theme setting
theme_set(theme_light())
theme_update(axis.text = element_text(color = 'black'))
options(modelsummary_format_numeric_latex = "plain")

# Racialized Police & Police Attitudes ---- 

## by-group regressions ----
by.race <- function(model) {
  white.reg <-   lm_robust(model, data = subset(cces,race == 1), weights = NULL) %>% 
    tidy() %>% mutate(race = 'White Sample')
  # black.reg <-   lm_robust(model, data = subset(cces,race == 2), weights = NULL) %>% 
  #   tidy() %>% mutate(race = 'Black Sample')
  return(white.reg)
}

white.rep.impact <- by.race(cbind(police_safe,police_increase, police_decrease) ~ white_rep + white_pop )
black.rep.impact <- by.race(cbind(police_safe,police_increase, police_decrease) ~ black_rep + black_pop )
hisp.rep.impact <- by.race(cbind(police_safe,police_increase, police_decrease) ~ hisp_rep + white_pop )

results <- bind_rows( white.rep.impact, black.rep.impact,hisp.rep.impact) %>% 
  filter(grepl('_rep',term)) %>% 
  mutate(term = case_when(grepl("white_rep",term) ~ 'White',
                          grepl("black_rep",term) ~ 'Black',
                          grepl("hisp_rep",term) ~ 'Hispanic'),
         term = factor(term, levels = c('White','Black','Hispanic')),
         outcome = case_when(grepl("decrease",outcome) ~ 'Decrease Police',
                   grepl("increase",outcome) ~ 'Increase Police',
                   grepl("safe",outcome) ~ 'Police Make R Feel Safe') %>% 
           factor(levels = c('Police Make R Feel Safe',
                             'Increase Police','Decrease Police') ) )

## richer plot ---- 
results %>% 
  ggplot(aes(x = term, y = estimate, ymax = conf.high, ymin = conf.low, 
             color = term != 'White',
             )) + 
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6) + 
  geom_pointrange(position = position_dodge(width = 0.6)) + 
  geom_label(aes(label = round(estimate,2),
                 fontface = ifelse(p.value < 0.05,'bold','plain')),
             nudge_x = 0.35,size = 3) +
  facet_wrap(~outcome,) +
  ylab('Coefficient Estimate') + 
  xlab('Racialized Imagery of Local Police') +
  scale_colour_viridis_d(option = 'viridis',end = 0.45) + 
  theme_sjplot() + 
  theme(legend.position = 'none',
        aspect.ratio = 0.8,
        axis.text = element_text(color = 'black')) -> fig.baseline

fig.baseline

ggsave('figures/imagery & attitudes 2.pdf',width = 10.5,height = 10.5/3)

## simpler plot ---- 

results %>% 
  filter(race == 'White Sample',term == 'White') %>% 
  ggplot(aes(x = outcome, y = estimate, ymax = conf.high, ymin = conf.low,
             color = (p.value < 0.05))) + 
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6) + 
  geom_pointrange(size = 0.6,position = position_dodge(width = 1)) + 
  geom_label(aes(label = round(estimate,2), 
                 fontface = ifelse(p.value < 0.05,'bold','plain')),
             hjust = -0.3, vjust = 0.1) + 
  ylab('Coefficient Estimate') + 
  xlab('Outcome Attitude') + 
  scale_colour_viridis_d(option = 'viridis',end = 0.58) + 
  theme_bw() + # coord_flip() + 
  theme(legend.position = 'none',
        axis.text = element_text(color = 'black'),
        aspect.ratio = 0.4)

ggsave('figures/imagery & attitudes.pdf',width = 7,height = 3)

## table ---- 

models <- list(
  'White Sample' = lm_robust(police_safe ~ white_rep + black_pop , data = subset(cces,race == 1)),
  'White Sample' = lm_robust(police_safe ~ black_rep + white_pop, data = subset(cces,race == 1)),
  'White Sample' = lm_robust(police_safe ~ black_rep + white_pop, data = subset(cces,race == 1))
)

modelsummary(
  models,  stars = TRUE,
  gof_map = c("nobs", "r.squared"), 
  notes = 'This is the note of your regression table.'
  )


# Police Imagery & Racial Divides ---- 

## table ----

models.divides <- list(
  'Police Felt as Safe' = lm_robust(police_safe ~ white_rep*white + white_pop , data = cces, weights = NULL),
  'Increase Police' = lm_robust(police_increase ~ white_rep*white + white_pop, data = cces, weights = NULL),
  'Decrease Police' = lm_robust(police_decrease ~ white_rep*white + white_pop, data = cces, weights = NULL)
  )

modelsummary(
  models.divides,
  # output = 'tables/divides.tex',
  stars = TRUE,
  gof_map = c("nobs", "r.squared"), 
  coef_map = c(
    "white1" = "Racial Divide",
    "white_rep" = 'White Imagery of Police',
    "white_rep:white1" = 'Racial Divide × White Imagery'),
  title = 'Racial Imagery of Local Police Affects Racial Divide on Policing.',
  notes = list('This is the note of your regression table.'))

## marginal effect plot ----- 

divide.inter <- function(dv){
  
  inter <- lm_robust(paste(dv,"~ white*white_rep + white_pop") %>% formula
                           , data = cces %>% mutate(white = as.numeric(white)) ) 
  
  jn <- johnson_neyman(inter,pred = white, modx = white_rep, title = NULL)$plot

  plot <- 
    jn +
    geom_vline(xintercept = 0, lty = 2, size = 0.7,color = 'midnightblue') +
    xlab( "White Imagery of Local Police") + ylab("Racial Divide on Police") +
  theme_bw() + 
    theme(legend.position = 'none',
          aspect.ratio = 1,
          plot.subtitle = element_text(face = 'bold'))
  
  # plot.b <- 
  #   jn.black +
  #   geom_vline(xintercept = 0, lty = 2, size = 0.7,color = 'midnightblue') +
  #   xlab( "Black Imagery of Local Police") + ylab("Racial Divide on Police") +
  #   theme_bw() + theme(legend.position = 'none')
  
  return(plot )
}

safe <- divide.inter('police_safe') + 
  ggtitle(NULL,subtitle = 'Police Make R Feel Safe ')
increase <- divide.inter('police_increase') + 
  ggtitle(NULL,subtitle = 'Increase Police ')
decrease <- divide.inter('police_decrease') + 
  ggtitle(NULL,subtitle = 'Decrease Police ')

fig.divides <- safe + increase + decrease # + plot_annotation(tag_levels = 'A')

fig.divides

ggsave('figures/divide moderation.pdf',width = 12.5, height = 4.8) 



# Police Imagery & Attitudinal Reactions ----

## plot ---- 

 pvc.inter <- function(dv){
  
  reg <- lm_robust(paste(dv,"~ pv*white_rep + white_pop ") %>% formula
                       , data = subset(cces, race == 1) ,weights = NULL) 

  plot <-
    johnson_neyman(reg,pred = pv, modx = white_rep,
                   title = NULL)$plot +
    geom_vline(xintercept = 0, lty = 2, color = 'black') +
    xlab( "White Imagery of Local Police") +
    ylab("Impact of Police Violence on Attitudes") +
    theme_bw() +
    theme(legend.position = 'none',
          aspect.ratio = 1,
          plot.subtitle = element_text(face = 'bold'))
  
  return(plot) } # used only for generating johnson_neyman plot

pv.inter <- function(dv){
  
  fit <- lm_robust(paste(dv,"~ pvd*white_rep + white_pop ") %>% formula
                   , subset(cces, race == 1) %>%
                     mutate(pvd = as.factor(pvd))) 
  
  values <- ggpredict(fit, terms = c("pvd [0,1]", "white_rep [-0.1,0.2,0.5]"))
  
  plot <- 
    ggplot(values,
           aes(group,predicted,ymax = conf.high, ymin = conf.low,
                    shape = x, color = x)) +
    geom_pointrange(position = position_dodge(width = 0.8))  + 
    theme_light() + 
    ylab('Linear Predicted Value') + 
    xlab('White Imagery of Local Police') + 
    scale_shape_manual(name = 'Any Police Violence in 2020',
                       values = c(16,1),
                       labels = c('No','Yes')) + 
    scale_color_viridis_d(option = 'viridis',end = 0.6,
                          name = 'Any Police Violence in 2020',
                          labels = c('No','Yes')) +
    theme(legend.position = 'bottom',
          plot.subtitle = element_text(face= 'bold'),
          aspect.ratio = 1) 
  # plot <-
  #   plot_model(fit, type = 'pred',terms = c('pvd[0,1]','white_rep[-0.1,0.2,0.5]')) +
  #   scale_color_viridis_d(option = 'cividis',end = 0.7,
  #                         name = 'White Imagery of Local Police') +
  #   scale_fill_viridis_d(option = 'cividis',end = 0.7) +
  #   ylab('Linear Predicted Value') + 
  #   xlab('Any Police Violence in 2020') + 
  #   theme_bw() +
  #   theme(legend.position = 'bottom',
  #         aspect.ratio = 1)
  return(plot)
}

pv.safe <- pv.inter('police_safe') + 
  ggtitle(NULL,subtitle = 'Police Make R Feel Safe ')
pv.increase <- pv.inter('police_increase') + 
  ggtitle(NULL,subtitle = 'Increase Police ')
pv.decrease <- pv.inter('police_decrease') + 
  ggtitle(NULL,subtitle = 'Decrease Police ')

fig.reaction.mod <- 
pv.safe + pv.increase + pv.decrease + 
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

fig.reaction.mod

ggsave('figures/reaction moderation.pdf',
       width = 10, height = 12/3)  # try "scale = 1"

## table ----

models.reaction <- list(
  'Police Felt as Safe' = lm_robust(police_safe ~ white_rep*pvd + white_pop, subset(cces,race == 1)),
  'Increase Police' = lm_robust(police_increase ~ white_rep*pvd + white_pop, subset(cces,race == 1)) ,
  'Decrease Police' = lm_robust(police_decrease ~ white_rep*pvd + white_pop, subset(cces,race == 1))
)

modelsummary(
    models.reaction,
    output = 'tables/reaction moderation.tex',
    stars = TRUE,
    gof_map = c("nobs", "r.squared"),
    coef_map = c( 'white_rep' = 'White Imagery of Police',
                  'pvd' = "Any Police Violence in 2020",
                  'white_rep:pvd' = "Police Violence × White Imagery"),
    title = 'Racial Imagery of Local Police Moderates Attitudinal Reaction to Police Violence.',
    notes = list('This is the note of your regression table.') ) 

# Examine Racial Component ---- 

## races of PV victims ---- 

cces <- cces %>% 
  mutate(pv_race = case_when(pv == 0 ~ 'No PV',
                             pv > 0 & pv_white > 0 ~ 'PV Whites',
                             pv > 0 & pv_white == 0  ~ 'PV POC',
                             TRUE ~ NA_character_) %>% factor(levels = c('No PV','PV Whites','PV POC')))

## plot ---- 

pv.inter2 <- function(dv){
  
  fit <- lm_robust(paste(dv,"~ pv_race*white_rep + white_pop ") %>% formula
                   , subset(cces, race == 1) %>%
                     mutate(pvd = as.factor(pvd))) 

  values <- ggpredict(fit,terms = c("pv_race",'white_rep[0,0.5]')) %>% 
    mutate(group2 = ifelse(group == 0 ,0, 0.5))
  values.a <- values %>% filter(x != 'PV Whites') %>% mutate(compare = "PV involves no whites" )
  values.b <- values %>% filter(x != 'PV POC') %>% mutate(compare = "PV involves whites" )
  
  
  plot <- 
    ggplot(values,
           aes(group2,predicted,ymax = conf.high, ymin = conf.low,
               shape = x, color = x)) +
    annotate('rect',xmin = 0.3,xmax = 0.7, ymax = Inf, ymin = -Inf,
             alpha = 0.1) +
    geom_pointrange(position = position_dodge(width = 0.4))  + 
    theme_light() + 
    scale_x_continuous(breaks = c(0,0.5)) +
    ylab('Linear Predicted Value') + 
    xlab('White Imagery of Local Police') + 
    scale_shape_manual(values = c(15,16,18),
                       name = "Police Violence by Victim's Race") + 
    scale_color_viridis_d(option = 'viridis',end = 0.6,
                          name = "Police Violence by Victim's Race") +
    theme(legend.position = 'bottom',
          plot.subtitle = element_text(face= 'bold'),
          aspect.ratio = 1) 
  return(plot)
}

pv.safe <- pv.inter2('police_safe') + 
  ggtitle(NULL,subtitle = 'Police Make R Feel Safe ')
pv.increase <- pv.inter2('police_increase') + 
  ggtitle(NULL,subtitle = 'Increase Police ')
pv.decrease <- pv.inter2('police_decrease') + 
  ggtitle(NULL,subtitle = 'Decrease Police ')

fig.racial.component <- 
  pv.safe + pv.increase + pv.decrease + 
    plot_layout(guides = 'collect') &
    theme(legend.position = 'bottom')

fig.racial.component

ggsave('figures/racial component.pdf',
       width = 10, height = 12/3)  # try "scale = 1"

## table ---- 

models.racial.component <- list(
  'Police Felt as Safe' = lm_robust(police_safe ~ white_rep*pv_race  + white_pop, subset(cces,race == 1)),
  'Increase Police' = lm_robust(police_increase ~ white_rep*pv_race  + white_pop, subset(cces,race == 1)) ,
  'Decrease Police' = lm_robust(police_decrease ~ white_rep*pv_race  + white_pop, subset(cces,race == 1))
)

modelsummary(
  models.racial.component,
  output = 'tables/racial component.tex',
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),　
  coef_map = c( 'white_rep' = 'White Imagery of Police',
                'pv_racePV Whites' = "PV Whites",
                'pv_racePV POC' = "PV POC",
                'white_rep:pv_racePV Whites' = "White Imagery × PV Whites",
                'white_rep:pv_racePV POC' = "White Imagery × PV POC"),
  title = "Moderating Effect of Racial Imagery Depends upon Racial Groups Victimized by Police Violence",
  notes = list('This is the note of your regression table.'))

# Save Workplace ---- 
save.image("docs/final objects.RData")

