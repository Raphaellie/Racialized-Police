# packages
library(tidyverse)
library(estimatr)
library(broom)
library(interactions)
# library(patchwork)
library(modelsummary)
library(marginaleffects)
library(sjPlot)
library(ggeffects)
library(kableExtra)
library(lemon)

# theme setting
theme_set(theme_sjplot())
theme_update(axis.text = element_text(color = "black"))
options(modelsummary_format_numeric_latex = "plain")

controls <- "factor(type) + white_pop + age + educ + income + gender"
cces.used <- cces %>% filter(race == 1, type <= 3)

# Racialized Police & Police Attitudes ----

## by-group regressions ----
by.race <- function(model) {
  # white.reg <-   lm_robust(model, data = subset(cces,race == 1 & type <= 3), weights = NULL) %>%
  #   tidy() %>% mutate(race = 'White Sample',)
  # results <- bind_rows(white.reg,nonwht.reg) %>%
  #   mutate(stars = gtools::stars.pval(p.value),
  #          stars = ifelse(stars == " " | stars == ".","",stars))
  results <-
    cces %>%
    filter(type <= 3) %>%
    mutate(race = case_when(
      race == 1 ~ "White Sample",
      race == 2 ~ "Black Sample",
      race == 3 ~ "Hispanic Sample",
      TRUE ~ "Other Sample"
    )) %>%
    nest(data = -race) %>%
    mutate(
      fit = map(data, ~ lm_robust(model, data = .x)),
      results = map(fit, tidy)
    ) %>%
    unnest(cols = results) %>%
    select(-data, -fit)

  results2 <- lm_robust(model, data = cces %>% filter(race != 1)) %>%
    tidy() %>%
    mutate(race = "All Non-White")

  return(bind_rows(results, results2))
}

white.rep.impact <- by.race(cbind(police_safe, police_increase, police_decrease) ~ white_rep + white_pop + factor(type) + age + educ + income + gender)
black.rep.impact <- by.race(cbind(police_safe, police_increase, police_decrease) ~ black_rep + black_pop + factor(type) + age + educ + income + gender)
hisp.rep.impact <- by.race(cbind(police_safe, police_increase, police_decrease) ~ hisp_rep + hispanic_pop + factor(type) + age + educ + income + gender)

results <- bind_rows(
  white.rep.impact,
  black.rep.impact,
  hisp.rep.impact
) %>%
  filter(grepl("_rep", term)) %>%
  mutate(race = factor(race, levels = c("White Sample", "Black Sample", "Hispanic Sample", "Other Sample", "All Non-White"))) %>%
  mutate(
    term = case_when(
      grepl("white_rep", term) ~ "White",
      grepl("black_rep", term) ~ "Black",
      grepl("hisp_rep", term) ~ "Hispanic"
    ),
    term = factor(term, levels = c("White", "Black", "Hispanic")),
    outcome = case_when(
      grepl("decrease", outcome) ~ "R Wants to Decrease Police",
      grepl("increase", outcome) ~ "R Wants to Increase Police",
      grepl("safe", outcome) ~ "Police Make R Feel Safe"
    ) %>%
      factor(levels = c(
        "Police Make R Feel Safe",
        "R Wants to Increase Police", "R Wants to Decrease Police"
      ))
  )

## richer plot ----

results %>%
  ggplot(aes(
    x = race, y = estimate, ymax = conf.high, ymin = conf.low,
    # color = term != 'White',
  )) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6) +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  # geom_text(aes(label = round(estimate,2), # paste0(round(estimate,2), stars)  ,
  #                fontface = ifelse(p.value < 0.05,'bold','plain')),
  #            nudge_x = 0.35,size = 3) +
  facet_grid(term ~ outcome, scales = "free") +
  ylab("Coefficient Estimate") +
  xlab("Share in Local Police Workforce - Share in Local Population") +
  scale_colour_viridis_d(option = "viridis", end = 0.45) +
  theme_sjplot() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1),
    aspect.ratio = 0.6,
    axis.text = element_text(color = "black")
  )

## All Rep. plot ----

results %>%
  # filter(race == 'White Sample',term == 'White') %>%
  ggplot(aes(
    x = race, y = estimate, ymax = conf.high, ymin = conf.low,
    color = (p.value < 0.05), identity = term
  )) +
  geom_hline(yintercept = 0, lty = "dotted", alpha = 0.6) +
  geom_pointrange(size = 0.3, position = position_dodge(width = 1 / 2)) +
  # geom_linerange(aes(x = outcome, y = estimate,
  #                     ymax = estimate + std.error * 1.65,
  #                     ymin = estimate - std.error * 1.65),
  #                size = 1.1 ) +
  # geom_text(aes(y = conf.high, label = round(estimate,2),
  #                fontface = ifelse(p.value < 0.05,'bold','plain')),nudge_y = 0.05, size = 2.5) +
  ylab("Coefficient Estimate") +
  xlab("Outcome Attitude") +
  scale_colour_viridis_d(option = "viridis", end = 0.5) +
  theme_minimal() +
  facet_grid(term ~ outcome, scales = "free") +
  coord_flip() +
  theme(
    legend.position = "none",
    strip.background = element_rect(color = "gray30", size = 1),
    axis.text = element_text(color = "black"),
    aspect.ratio = 0.4
  )

## White Rep. plot ----

fig.baseline <-
  results %>%
  filter(term == "White") %>%
  mutate(outcome2 = paste("DV:", outcome)) %>%
  ggplot(aes(
    x = race, y = estimate, ymax = conf.high, ymin = conf.low, identity = term,
    # color = term != 'White',
  )) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6) +
  # geom_linerange(aes(x = race, y = estimate,
  #                    ymax = estimate + std.error * 1.65,
  #                    ymin = estimate - std.error * 1.65),size = 0.8) +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  geom_text(aes(
    label = round(estimate, 3),
    fontface = ifelse(p.value < 0.05, "bold", "plain")
  ), nudge_x = -0.35, size = 2.8) +
  facet_grid(~outcome) +
  ylab("Coefficient Estimate") +
  xlab("") +
  scale_colour_viridis_d(option = "viridis", end = 0.45) +
  theme_sjplot() +
  coord_flip() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    aspect.ratio = 0.8,
    axis.text = element_text(color = "black")
  )

fig.baseline

ggsave("figures/imagery & attitudes.pdf", width = 7, height = 3)

## table ----

models <- list(
  "White Sample" = lm_robust(police_safe ~ white_rep + white_pop, data = subset(cces, race == 1)),
  "White Sample" = lm_robust(police_safe ~ black_rep + black_pop, data = subset(cces, race == 1)),
  "White Sample" = lm_robust(police_safe ~ hisp_rep + hispanic_pop, data = subset(cces, race == 1))
)

modelsummary(
  models,
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),
  notes = "This is the note of your regression table."
)


# Police Imagery & Racial Divides ----

## table ----

models.divides <- list(
  "Police Felt as Safe" = lm_robust(police_safe ~ white_rep * white + white_pop + age + educ + income + gender,
    data = cces %>% filter(type == 3)
  ),
  "Increase Police" = lm_robust(police_increase ~ white_rep * white + white_pop + age + educ + income + gender,
    data = cces %>% filter(type == 3)
  ),
  "Decrease Police" = lm_robust(police_decrease ~ white_rep * white + white_pop + age + educ + income + gender,
    data = cces %>% filter(type == 3)
  )
)

modelsummary(
  models.divides,
  # output = 'tables/divides.tex',
  # output = 'latex',
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),
  coef_map = c(
    "white1" = "Racial Divide",
    "white_rep" = "White Imagery of Police",
    "white_rep:white1" = "Racial Divide × White Imagery"
  ),
  title = "Racial Imagery of Local Police Affects Racial Divide on Policing.",
  notes = list("This is the note of your regression table.")
)


## try {clarify} ----
fit <- lm(police_safe ~ white_rep * white + white_pop + age + educ + income + gender,
  data = cces %>% filter(type <= 3)
)
s <- sim(fit, vcov = "HC2")

est <- sim_setx(s,
  x = list(
    white = c("0", "1"),
    white_rep = seq(0, 0.5, by = 0.1)
  )
)

plot(est)

plot(est, var = "white_rep", ci = FALSE) +
  facet_wrap(~white_rep, nrow = 2) +
  theme_minimal()

## marginal effect plot -----

divide.inter <- function(dv) {
  inter <- lm_robust(
    paste(dv, "~ white*white_rep + white_pop + age + educ + income + gender") %>% formula(),
    data = cces %>%
      mutate(
        race = case_when(
          race == 1 ~ "White",
          race == 2 ~ "Black",
          race == 3 ~ "Hispanic",
          TRUE ~ "Other"
        ),
        race = factor(race, levels = c("White", "Black", "Hispanic", "Other"))
      ) %>%
      filter(type == 3)
  )

  # jn <- johnson_neyman(inter,pred = white, modx = white_rep, title = NULL)$plot
  # plot <-
  #   jn + geom_vline(xintercept = 0, lty = 2, linewidth = 0.5,color = 'black') +
  #   xlab( "White Imagery of Local Police") + ylab("Racial Divide on Police") +
  #   theme_minimal() +
  #   theme(legend.position = 'none', aspect.ratio = 1,
  #   plot.subtitle = element_text(face = 'bold'))
  plot <-
    plot_model(inter,
      type = "pred",
      terms = c("white_rep", "white[0,1]")
    ) +
    aes(linetype = group) + # note how we define linetype by group var in plot_model()!
    ylab("Linear Predicted Value") +
    xlab("White Representation in Local Police") +
    ggtitle(NULL) +
    scale_x_continuous(limits = c(0, 0.5)) +
    scale_linetype_discrete(
      name = "Respondent's Race",
      labels = c("Non-White", "White")
    ) +
    scale_color_viridis_d(
      option = "viridis", end = 0.4,
      name = "Respondent's Race",
      labels = c("Non-White", "White")
    ) +
    scale_fill_viridis_d(option = "viridis", end = 0.4) +
    theme_minimal() + # theme_bw() +
    # facet_grid(~group) +
    theme(
      aspect.ratio = 1,
      plot.subtitle = element_text(face = "bold"),
      legend.position = "bottom"
    )

  return(plot)
}

safe <- divide.inter("police_safe") +
  ggtitle(NULL, subtitle = "DV: Police Make R Feel Safe ")
increase <- divide.inter("police_increase") +
  ggtitle(NULL, subtitle = "DV: R Wants to Increase Police ")
decrease <- divide.inter("police_decrease") +
  ggtitle(NULL, subtitle = "DV: R Wants to Decrease Police ")

fig.divides <- ggpubr::ggarrange(safe, increase, decrease,
  nrow = 1,
  common.legend = TRUE, legend = "bottom"
)

# fig.divides <- safe + increase + decrease +
#   plot_layout(guides = 'collect') &
#   theme(legend.position = 'bottom')

fig.divides

ggsave("figures/divide moderation.pdf", width = 12.5, height = 4.8)



# Police Imagery & Attitudinal Reactions ----

## plot ----

pv.inter <- function(dv) {
  fit <- lm_robust(
    paste(dv, "~ pvd*white_rep + white_pop + age + educ + income + gender ") %>% formula(),
    subset(cces, race == 1 & type <= 3) %>%
      mutate(pvd = as.factor(pvd))
  )

  values <- ggpredict(fit, terms = c("pvd [0,1]", "white_rep [-0.1,0.2,0.5]"))

  # plot <- interplot::interplot(fit,'pvd','white_rep',point = F) +
  #   geom_hline(yintercept = 0, lty = 'dashed') +
  #   theme_minimal()

  slopes <- slopes(fit,
    newdata = datagrid(model = fit, white_rep = seq(0, 0.5, 0.05)),
    variables = c("pvd")
  )

  slope <-
    slopes %>%
    mutate(term2 = "Police Violence") %>%
    ggplot() +
    geom_hline(yintercept = 0, lty = 2, color = "gray20") +
    geom_pointrange(aes(
      x = white_rep, y = estimate, ymin = conf.low, ymax = conf.high,
      color = term2
    )) +
    scale_color_viridis_d(end = 0.55) +
    ylab("Estimated Slope of Police Violence") +
    xlab("White Representation in Local Police") +
    scale_y_continuous(limits = c(-0.08, 0.08)) +
    theme_minimal() +
    # geom_rug(data = rp.df, aes(x = white_rep),alpha = 0.03,color = 'midnightblue') +
    xlim(c(0, 0.5)) +
    theme(
      legend.position = "none",
      plot.subtitle = element_text(face = "bold"),
      aspect.ratio = 1.1,
      axis.text = element_text(color = "black"),
    )

  predicted <-
    values %>%
    ggplot(aes(x, predicted, ymax = conf.high, ymin = conf.low, )) +
    geom_line(aes(x = x, y = predicted, group = NA), lty = "dashed", color = "gray60") +
    geom_pointrange(aes(shape = x, color = x),
      position = position_dodge(width = 0.8)
    ) +
    ylab("Linear Predicted Value") +
    xlab("Occurrence of Fatal Police Violence") +
    scale_shape_manual(
      name = "Any Police Violence in 2020",
      values = c(16, 17, 1)
    ) +
    scale_color_viridis_d(option = "viridis", end = 0.55, name = "Police Violence") +
    # scale_color_manual(values = c('black','gray60')) +
    scale_x_discrete(labels = c("No", "Yes")) +
    facet_wrap(vars(group)) +
    theme_sjplot() +
    theme(
      legend.position = "none",
      axis.text = element_text(color = "black"),
      plot.subtitle = element_text(face = "bold"),
      aspect.ratio = 1.8
    )

  return(slope)
}

pv.inter("police_decrease")

pv.safe <- pv.inter("police_safe") +
  ggtitle(NULL, subtitle = "DV: Police Make R Feel Safe ")
pv.increase <- pv.inter("police_increase") +
  ggtitle(NULL, subtitle = "DV: R Supports Increasing Police ")
pv.decrease <- pv.inter("police_decrease") +
  ggtitle(NULL, subtitle = "DV: R Supports Decreasing Police ")

fig.reaction.mod <- ggpubr::ggarrange(pv.safe, pv.increase, pv.decrease,
  nrow = 1,
  common.legend = TRUE, legend = "none"
)
# pv.safe + pv.increase + pv.decrease +
#   plot_layout(guides = 'collect') &
#   theme(legend.position = 'bottom')

fig.reaction.mod

ggsave("figures/reaction moderation.pdf",
  width = 10, height = 11 / 3
) # try "scale = 1"

## table ----

models.reaction <- list(
  "Feel Police as Safe" = lm_robust(police_safe ~ white_rep * pvd + factor(type) + white_pop + age + educ + income + gender, subset(cces, race == 1 & type <= 3)),
  "Increase Police" = lm_robust(police_increase ~ white_rep * pvd + factor(type) + white_pop + age + educ + income + gender, subset(cces, race == 1 & type <= 3)),
  "Decrease Police" = lm_robust(police_decrease ~ white_rep * pvd + factor(type) + white_pop + age + educ + income + gender, subset(cces, race == 1 & type <= 3))
)

modelsummary(
  models.reaction,
  # output = 'tables/reaction moderation.tex',
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),
  coef_map = c(
    "white_rep" = "White Imagery of Police",
    "pvd" = "Any Police Violence in 2020",
    "white_rep:pvd" = "Police Violence × White Imagery"
  ),
  title = "Racial Imagery of Local Police Moderates Attitudinal Reaction to Police Violence.",
  notes = list("This is the note of your regression table.")
)


# Examine Racial Component ----

## races of PV victims ----

cces <- cces %>%
  mutate(
    pv_race = case_when(
      pv == 0 ~ "No PV",
      pv > 0 & pv_white > 0 ~ "PV Whites",
      pv > 0 & pv_white == 0 ~ "PV POC",
      TRUE ~ NA_character_
    ),
    pv_race = factor(pv_race, levels = c("No PV", "PV Whites", "PV POC")),
    pv_white_p = pv_white / pv,
    pv_poc_p = 1 - pv_white_p,
    pv_poc_all = factor(pv_race == "PV POC"),
    pv_white_all = factor(pv_white_p == 1),
    pv_white_d = factor(pv_race == "PV Whites"),
    pv_poc_d = factor(pv - pv_white > 0)
  )

## plot ----

pv2.inter <- function(dv) {
  fit <- lm_robust(
    paste(dv, "~ pv_poc_all*white_rep + pv_white_d*white_rep + white_pop + age + educ + income + gender") %>% formula(),
    subset(cces, race == 1 & type <= 3) %>%
      mutate(pvd = as.factor(pvd))
  )

  # values <- ggpredict(fit,terms = c("pv_race",'white_rep[0,0.5]')) %>%
  #   mutate(pv = ifelse(x == 'No PV', 'No PV',"Yes PV"),
  #          pv_victim = case_when(x == 'PV Whites' ~ 'White',
  #                                x == 'PV POC'~ 'Non-White',
  #                                TRUE ~ NA_character_))

  values.a <- ggpredict(fit, terms = c("pv_poc_all", "white_rep[0,0.5]")) %>%
    mutate(compare = "All Non-White")
  values.b <- ggpredict(fit, terms = c("pv_white_d", "white_rep[0,0.5]")) %>%
    mutate(compare = "Some White")

  slopes <- slopes(fit,
    newdata = datagrid(model = fit, white_rep = seq(0, 0.4, 0.05)),
    variables = c("pv_poc_all", "pv_white_d")
  )

  slope <-
    slopes %>%
    mutate(term2 = ifelse(term == "pv_poc_all", "All Non-White", "Some White")) %>%
    ggplot(aes(x = white_rep, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0, lty = 2, color = "gray40") +
    geom_pointrange(aes(color = term2, shape = term2)) +
    facet_grid(rows = vars(term2)) +
    scale_color_viridis_d(option = "viridis", end = 0.55) +
    scale_shape_manual(values = c(15, 16, 17, 18)) +
    ylab("Estimated Slope of Police Violence") +
    xlab("White Representation in Local Police") +
    theme_sjplot() +
    theme(
      legend.position = "none",
      plot.subtitle = element_text(face = "bold"),
      aspect.ratio = 1 / 2,
      axis.text = element_text(color = "black"),
    )

  predicted <-
    bind_rows(values.a, values.b) %>%
    mutate(pv = ifelse(x == FALSE, NA, compare)) %>%
    ggplot(aes(x, predicted,
      ymax = conf.high, ymin = conf.low, # shape = x
      color = compare, shape = compare
    )) +
    geom_line(aes(x, predicted, group = compare), lty = "dashed") +
    geom_pointrange(aes(), alpha = 1) + # position = position_dodge(width = 0.4)
    theme_sjplot() +
    ylab("Linear Predicted Value") +
    xlab("White Imagery of Local Police") +
    scale_x_discrete(labels = c("No", "Yes")) +
    scale_shape_manual(
      values = c(17, 16, 18),
      name = "Victim of Fatal Police Violence"
    ) +
    scale_color_viridis_d(
      option = "viridis", end = 0.6, direction = -1,
      na.value = "black",
      name = "Victim of Fatal Police Violence"
    ) +
    facet_grid(rows = vars(compare), cols = vars(group)) +
    theme(
      legend.position = "none",
      axis.text = element_text(color = "black"),
      plot.subtitle = element_text(face = "bold"),
      aspect.ratio = 0.8
    )

  return(slope)
}

pv3.inter <- function(dv) {
  slope <-
    lm_robust(
      paste(dv, "~ pvd*white_rep + white_rep + white_pop + age + educ + income + gender") %>% formula(),
      subset(cces, race == 1 & type <= 3) %>%
        mutate(pvd = as.factor(pvd))
    ) %>%
    slopes(variables = "pvd", condition = "white_rep") %>%
    filter(white_rep >= 0, white_rep <= 0.5) %>%
    ggplot() +
    geom_hline(yintercept = 0, lty = "dashed", alpha = 0.8) +
    geom_ribbon(aes(x = white_rep, y = estimate, ymin = conf.low, ymax = conf.high),
      alpha = 0.2, fill = "steelblue4"
    ) +
    geom_line(aes(x = white_rep, y = estimate), size = 1, color = "slateblue4") +
    ylab("Estimated Slope of Police Violence") +
    xlab("White Representation in Local Police") +
    ylim(c(-0.08, 0.08)) +
    theme_minimal() +
    theme(aspect.ratio = 0.8)

  return(slope)
}

rm(fit, slope)
pv2.inter("police_safe")

pv2.safe <- pv2.inter("police_safe") +
  ggtitle(NULL, subtitle = "DV: Police Make R Feel Safe ")
pv2.increase <- pv2.inter("police_increase") +
  ggtitle(NULL, subtitle = "DV: R Wants to Increase Police ")
pv2.decrease <- pv2.inter("police_decrease") +
  ggtitle(NULL, subtitle = "DV: R Wants to Decrease Police ")

fig.racial.component <-
  ggpubr::ggarrange(pv2.safe, pv2.increase, pv2.decrease,
    nrow = 1,
    common.legend = TRUE, legend = "none"
  )
# pv.safe + pv.increase + pv.decrease +
#   plot_layout(guides = 'collect') &
#   theme(legend.position = 'bottom')

fig.racial.component

ggsave("figures/racial component.pdf",
  width = 11, height = 12 / 3
) # try "scale = 1"

## table ----

models.racial.component <- list(
  "Police Felt as Safe" = lm_robust(police_safe ~ white_rep * pv_race + white_pop + age + educ + income + gender, subset(cces, race == 1 & type <= 3)),
  "Increase Police" = lm_robust(police_increase ~ white_rep * pv_race + white_pop + age + educ + income + gender, subset(cces, race == 1 & type <= 3)),
  "Decrease Police" = lm_robust(police_decrease ~ white_rep * pv_race + white_pop + age + educ + income + gender, subset(cces, race == 1 & type <= 3))
)


modelsummary(
  models.racial.component,
  # output = 'tables/racial component.tex',
  stars = TRUE, threeparttable = TRUE,
  gof_map = c("nobs", "r.squared"),
  coef_map = c(
    "white_rep" = "White Imagery of Police",
    "pv_racePV Whites" = "PV Whites",
    "pv_racePV POC" = "PV POC",
    "white_rep:pv_racePV Whites" = "White Imagery × PV Whites",
    "white_rep:pv_racePV POC" = "White Imagery × PV POC"
  ),
  title = "Moderating Effect of Racial Imagery Depends upon Racial Groups Victimized by Police Violence",
  notes = list("This is the note of your regression table.")
)

# Save Workplace ----
save.image("docs/final objects.RData")
