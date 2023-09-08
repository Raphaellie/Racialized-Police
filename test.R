list(
  'Police Felt as Safe' = lm_robust(police_safe ~ white_rep*pv_poc_d + white_rep*pv_white_d  + white_pop + age + educ + income + gender , subset(cces,race == 1 & type <= 3)),
  'Increase Police' = lm_robust(police_increase ~  white_rep*pv_poc_d + white_rep*pv_white_d  + white_pop + age + educ + income + gender, subset(cces,race == 1 & type <= 3)) ,
  'Decrease Police' = lm_robust(police_decrease ~  white_rep*pv_poc_d + white_rep*pv_white_d  + white_pop + age + educ + income + gender, subset(cces,race == 1 & type <= 3))
) %>% 
  modelsummary(
    .,
    stars = TRUE,
    gof_map = c("nobs", "r.squared"),　
    notes = list('This is the note of your regression table.')
    )

fit <- lm_robust(police_increase ~ white_rep*pv*pv_poc_p  + white_pop + age + educ + income + gender, 
                 data =  subset(cces,race == 1 & type <= 3) %>% 
                   mutate(pv_poc_p = 1 - pv_white_p))


plot_model(fit, type = 'pred',terms = c('pv_poc_p', 'white_rep[0,0.5]','pv[2]'))  + 
  ylim(c(0.2,0.8)) +
  theme_minimal() + 
  theme(aspect.ratio = 0.9)



## ======


pv2.inter <- function(dv){
  
  fit <- lm_robust(paste('police_decrease',"~ pv_poc_d*white_rep + white_pop + age + educ + income + gender" ) %>% formula
                   , subset(cces, race == 1 & type <= 3) %>%
                     mutate(pvd = as.factor(pvd))) 
  
  slopes(fit,conf_level = 0.9,
         newdata = datagrid(model = fit, white_rep = seq(0,0.4,0.1)),
         variables = c('pv_poc_d','pv_white_p')) %>% 
    mutate(term2 = ifelse(grepl(term, 'poc') ,'Non-White','White')) %>%
    ggplot(aes(x = white_rep, y = estimate, ymin = conf.low, ymax = conf.high)) + 
    geom_hline(yintercept = 0, lty = 2,color= 'gray40') +
    geom_pointrange(aes(color = term2, shape = term2)) + 
    facet_grid(rows = vars(term)) + 
    scale_color_viridis_d(option =  'viridis', end = 0.55) + 
    scale_shape_manual(values = c(15,16,17,18)) +
    ylab('Estimated Slope of Police Violence') + 
    xlab('White Representation in Local Police') + 
    theme_sjplot() + 
    theme(legend.position = 'none',
          plot.subtitle = element_text(face = 'bold'),
          aspect.ratio = 1/2,
          axis.text = element_text(color = 'black'),
    ) 
  
  predicted <-
    bind_rows(values.a, values.b) %>% 
    mutate(pv = ifelse(x == FALSE,NA,compare)) %>% 
    ggplot(aes(x,predicted, ymax = conf.high, ymin = conf.low, # shape = x
               color = compare, shape = compare)) +
    geom_line(aes(x,predicted,group = compare),lty = 'dashed') + 
    geom_pointrange(aes(),alpha = 1 ) + # position = position_dodge(width = 0.4)
    theme_sjplot() + 
    ylab('Linear Predicted Value') + 
    xlab('White Imagery of Local Police') + 
    scale_x_discrete(labels = c('No','Yes')) +
    scale_shape_manual(values = c(17,16,18),
                       name = "Victim of Fatal Police Violence") +
    scale_color_viridis_d(option = 'viridis',end = 0.6, direction = -1,
                          na.value = 'black',
                          name = "Victim of Fatal Police Violence") +
    facet_grid(rows = vars(compare), cols = vars(group)) +
    theme(legend.position = 'none',
          axis.text = element_text(color = 'black'),
          plot.subtitle = element_text(face= 'bold'),
          aspect.ratio = 0.8
    ) 
  
  return(slope)
}


pv2.inter('police_safe') 

pv2.safe <- pv2.inter('police_safe') + 
  ggtitle(NULL,subtitle = 'DV: Police Make R Feel Safe ')
pv2.increase <- pv2.inter('police_increase') + 
  ggtitle(NULL,subtitle = 'DV: R Wants to Increase Police ')
pv2.decrease <- pv2.inter('police_decrease') + 
  ggtitle(NULL,subtitle = 'DV: R Wants to Decrease Police ')

