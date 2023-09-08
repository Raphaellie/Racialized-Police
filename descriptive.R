# packages
library(usmap)
library(wesanderson)
library(sjPlot)

# Police Imagery in LEMAS2016 ----

rp.df.long <- rp.df %>%
  select(contains("_rep"), agency, city, state, popserved) %>%
  pivot_longer(cols = contains("_rep"), values_to = "imagery", names_to = "race") %>%
  mutate(race = case_when(
    race == "black_rep" ~ "Black",
    race == "white_rep" ~ "White",
    race == "hisp_rep" ~ "Hispanic"
  )) %>%
  filter(!is.na(imagery))

## bar plot ----
rp.df.long %>%
  ggplot(aes(x = imagery >= 0)) +
  geom_bar(aes(fill = imagery >= 0), alpha = 0.7, color = "gray12") +
  facet_grid(~ factor(race, levels = c("White", "Black", "Hispanic"))) +
  stat_count(
    geom = "text", colour = "white", size = 4, fontface = "plain",
    aes(label = ..count..), position = position_stack(vjust = 0.5)
  ) +
  scale_fill_viridis_d(option = "mako", end = 0.65, direction = 1) +
  scale_x_discrete(labels = c("Under-\nrepresented", "Over-\nrepresented")) +
  theme_bw() +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    axis.title = element_blank(),
    axis.text = element_text(color = "gray10", )
  )

ggsave("figures/desc-lemas.pdf", width = 11, height = 11 / 3)

## OR density plot ----

fig.lemas.density <- rp.df.long %>%
  ggplot(aes(x = imagery)) +
  geom_density(fill = "steelblue4", alpha = 0.45, color = "gray12") +
  geom_vline(xintercept = 0, lty = 2, color = "black") +
  facet_wrap(~race, scales = "free_x") +
  # scale_fill_viridis_d(option = 'viridis',end = 0.7, name = "Respondent's Race",labels = c('Others','Non-Hispanic White')) +
  # scale_fill_manual(values = c('seagreen','steelblue')) +
  xlim(c(-0.5, 0.5)) +
  ylab(NULL) +
  xlab("Racial Imagery of Police Departments by Racial Group") +
  theme_sjplot() +
  theme(
    legend.position = "none",
    aspect.ratio = 0.9,
    axis.text = element_text(color = "gray10")
  )

fig.lemas.density

ggsave("figures/desc-lemas2.pdf", width = 11, height = 11 / 3)


# Geography of LEMAS Sample ----

## city points ----

city.points <- rp.df %>%
  filter(!is.na(lon), lon > -160) %>%
  select(lon, lat, popserved) %>%
  data.frame() %>%
  usmap_transform()

fig.lemas.cover2 <-
  plot_usmap("states", size = 0.2) +
  geom_point(
    data = city.points, aes(x = x, y = y), position = "jitter",
    color = "navyblue", alpha = 0.35,
    # size = 0.9,
    stroke = 0, size = 1.5
  )

fig.lemas.cover2

ggsave("figures/lemas-map.pdf", width = 6, height = 3)

## county blocks ----

ct.count <- rp.df %>%
  group_by(state2, fips) %>%
  mutate(fips = str_pad(fips, 5, pad = "0")) %>%
  summarise(n = n())

df <- countypop %>%
  left_join(ct.count, by = "fips") %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    sampled = (n > 0)
  )

fig.lemas.cover <-
  plot_usmap(
    data = df, regions = "counties", values = "sampled",
    size = 0.05, color = "gray40"
  ) +
  geom_polygon(
    data = us_map(regions = "states"), aes(x, y, group = group),
    size = 1 / 4, fill = NA, color = "black"
  ) +
  scale_fill_manual(
    values = c("white", "lightskyblue3"), na.value = "white",
    name = "Any Police Department Sampled",
    labels = c("No", "Yes"), breaks = c(FALSE, TRUE)
  ) +
  theme(
    legend.position = "bottom",
    legend.justification = 1 / 2
  )

fig.lemas.cover

ggsave("figures/lemas-map-ct.pdf", width = 12, height = 4)

# Policing Attitudes in 20 CES ----

attitudes <- cces %>%
  distinct(caseid, .keep_all = TRUE) %>%
  select(race, safe = police_safe, increase = police_increase, decrease = police_decrease) %>%
  mutate(race = case_when(
    race == 1 ~ "White",
    race == 2 ~ "Black",
    race == 3 ~ "Hispanic",
    TRUE ~ "Other"
  )) %>%
  mutate(race = factor(race, levels = c("White", "Black", "Hispanic", "Other"))) %>%
  group_by(race) %>%
  summarise(across(
    everything(),
    funs(mean = mean(., na.rm = T), sd = sd(., na.rm = T), n = sum(!is.na(.)))
  )) %>%
  # tidying
  pivot_longer(cols = 2:10, names_to = c("var", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = "stat") %>%
  mutate(
    se = sd / sqrt(n),
    ciup = mean + qnorm(.975) * se,
    cilow = mean - qnorm(.975) * se
  )

attitudes %>%
  mutate(var = factor(var,
    levels = c("safe", "increase", "decrease"),
    labels = c(
      "Police Make R Feel Safe",
      "R Wants to Increase Police",
      "R Wants to Decrease Police"
    )
  )) %>%
  ggplot(aes(x = var, y = mean, ymin = cilow, ymax = ciup, color = factor(race))) +
  geom_errorbar(width = 0.2 / 2, size = 0.5, position = position_dodge(width = 0.4)) +
  # geom_point(position = position_dodge(width = 0.4)) +
  # geom_text(aes(label = round(mean,2)),color = 'gray5',nudge_x = 0.3, size = 3.5) +
  scale_color_viridis_d(
    option = "mako", end = 0.6, name = "Race",
    # labels = c('Non-White','White')
  ) +
  theme_sjplot() +
  xlab("Attitudes towards Police") +
  ylab("Mean") +
  coord_flip() +
  theme(
    legend.position = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.5), color = "gray"),
    legend.justification = c(1, 1),
    axis.text = element_text(color = "black"),
    # legend.background = element_rect(color = 'black',size = 1/3),
    aspect.ratio = 2 / 3
  )


fig.attitudes <-
  attitudes %>%
  mutate(var = factor(var,
    levels = c("safe", "increase", "decrease"),
    labels = c(
      "Police Make R Feel Safe",
      "R Wants to Increase Police",
      "R Wants to Decrease Police"
    )
  )) %>%
  ggplot(aes(x = race, y = mean, ymin = cilow, ymax = ciup)) +
  geom_bar(
    color = "black", stat = "identity", alpha = 0.4,
    position = "dodge2", width = 0.7, aes(fill = race == "White")
  ) +
  geom_text(aes(label = round(mean, 2)), color = "gray5", position = position_stack(vjust = 0.5)) +
  geom_errorbar(position = position_dodge(width = 0.6), width = 0.2 / 2, linewidth = 0.5) +
  # geom_point() +
  # geom_text(aes(label = round(mean,2)),color = 'gray5',nudge_x = 0.3, size = 3.5) +
  facet_wrap(~var) +
  # scale_fill_viridis_d(option = 'viridis', end = 0.6,
  #                      name = "Respondent's Race",
  #                      labels = c('Others','Non-Hispanic White')) +
  scale_fill_manual(values = c("white", "lightskyblue4")) +
  # scale_x_discrete(labels = c('Not White','White')) +
  theme_sjplot() +
  xlab("Respondent's Race") +
  ylab("Mean") +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black"),
    # legend.background = element_rect(color = 'black',size = 1/3),
    aspect.ratio = 0.9
  )

fig.attitudes

ggsave("figures/cces-attitudes.pdf", width = 10, height = 3.3)

## in table form
cces %>%
  distinct(caseid, .keep_all = TRUE) %>%
  select(white, police_safe, police_increase, police_decrease) %>%
  mutate(white = ifelse(white == 1, "White", "Not White")) %>%
  rename(
    `Police Make R Feel Safe` = police_safe,
    `R Wants to Increase Police` = police_increase,
    `R Wants to Decrease Police` = police_decrease,
  ) %>%
  datasummary_balance(~white,
    data = ., fmt = 3,
    notes = "Notes: All respondents in 2020 CES."
  )
