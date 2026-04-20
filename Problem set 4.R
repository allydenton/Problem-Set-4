#+ ------------------------
#+ Ally Denton
#+ SIS 750
#+ Problem Set 4
#+ ------------------------

# How does national wealth explain a country’s overall power?
# Which component of power is most closely related to wealth?

# SETUP --------------------

library(tidyverse)
library(knitr)
library(ggrepel)
library(patchwork)

df <- read_csv("global_power.csv")

# CLEANING THE DATA ----------------------

# variable selection

df1 <- df %>%
  select(
    country,
    region,
    gdp_per_capita,
    economic_power,
    tech_power,
    military_power,
    power_index,
    power_rank
  ) 

# BASIC DESCRIPTIVES -------------------

# gdp per capita and power by region (not really relevant)

table <- df1 %>%
  group_by(region) %>%
  summarize(
    mean.power = mean(power_index),
    mean.gdp = mean(gdp_per_capita)) %>%
  kable(digits = 2L)

table

# table of top 10 countries on power_rank and their power_index and gdp_per_capita

all <- df1 %>%
  arrange(power_rank) %>%
  select(country, power_rank, power_index, gdp_per_capita) %>%
  kable(digits = 1L,
        caption = "Top 10 Countries by Power Rank")

all

top10 <- df1 %>%
  arrange(power_rank) %>%
  select(country, power_rank, power_index, gdp_per_capita) %>%
  head(10) %>%
  kable(digits = 1L,
        caption = "Top 10 Countries by Power Rank")

top10

bottom10 <- df1 %>%
  arrange(desc(power_rank)) %>%
  head(10) %>%
  arrange(power_rank) %>%
  select(country, power_rank, power_index, gdp_per_capita) %>%
  kable(digits = 1L,
        caption = "Bottom 10 Countries by Power Rank")

bottom10




df1[df1$gdp_per_capita == max(df1$gdp_per_capita), 
    c("country", "gdp_per_capita", "power_index", "power_rank")]

df1 %>%
  summarize(
    mean.gdp = mean(gdp_per_capita),
    mean.power = mean(power_index)
    )

df1[df1$gdp_per_capita == min(df1$gdp_per_capita), 
    c("country", "gdp_per_capita", "power_index", "power_rank")]

# AI Index

#+ want to flip the output. right now it goes 250 at the top to 241. i'd like it 241 to 250 bottom10 <- df1 %>% arrange(desc(power_rank)) %>% select(country, power_rank, power_index, gdp_per_capita) %>% head(10) %>% kable(digits = 1L) bottom10bottom10 <- df1 %>% arrange(desc(power_rank)) %>% select(country, power_rank, power_index, gdp_per_capita) %>% head(10) %>% kable(digits = 1L) bottom10
#+ Ah! You want the bottom 10 to display in ascending order of rank (i.e., 241 → 250) instead of descending. You can do that by wrapping the result in arrange(power_rank) after taking the head(10):
#+ bottom10 <- df1 %>% arrange(desc(power_rank)) %>%     # get the bottom 10 ranks (largest ranks first) head(10) %>%     
#+ # take the first 10 = bottom 10 countries arrange(power_rank) %>% reorder ascending: 241 → 250select(country, power_rank, power_index, gdp_per_capita) %>%  kable(digits = 1L, caption = "Bottom 10 Countries by Power Rank")


  
# can combine in quarto once i get there so that the tables are side by side

#Simple Visualization

plot1 <- df1 %>%
  ggplot(aes(x = power_index)) +
  geom_density()

plot1

plot2 <- df1 %>%
  ggplot(aes(x = gdp_per_capita)) +
  geom_density()

plot2

plot3 <- df1 %>%
  ggplot(aes(economic_power)) +
  geom_density()

plot3

plot4 <- df1 %>%
  ggplot(aes(tech_power)) +
  geom_density()

plot4

plot5 <- df1 %>%
  ggplot(aes(military_power)) +
  geom_density()

plot5

# Linear Regression

m1 <- lm(power_index ~ gdp_per_capita, df1)

summary(m1)

m2 <- lm(economic_power ~ gdp_per_capita, df1)

summary(m2)

m3 <- lm(tech_power ~ gdp_per_capita, df1)

summary(m3)

m4 <- lm(military_power ~ gdp_per_capita, df1)

summary(m4)

# scatter plot with the different regression lines

df_scaled <- df1 %>%
  mutate(across(c(power_index, economic_power, tech_power, military_power), scale))



lin.reg <- df1 %>%
  ggplot(aes(gdp_per_capita, power_index)) +
  geom_point(alpha = .6) + 
  geom_smooth(method = lm, se = FALSE, 
              color = "magenta2", 
              linetype = "solid") +
  theme_classic() +
  labs(
    subtitle = "Overall Power Index and Wealth",
    y = NULL,
    x = NULL)


lin.reg1 <- df1 %>%
  ggplot(aes(gdp_per_capita, tech_power)) +
  geom_point(alpha = .6) + 
  geom_smooth(method = lm, se = FALSE, 
              color = "red4", 
              linetype = "solid") +
  theme_classic() +
  labs(
    subtitle = "Technological Power and Wealth",
    y = NULL,
    x = NULL)


lin.reg2 <- 
  df1 %>%
  ggplot(aes(gdp_per_capita, military_power)) +
  geom_point(alpha = .6) + 
  geom_smooth(method = lm, se = FALSE, 
              color = "steelblue1", 
              linetype = "solid") +
  labs(
    subtitle = "Military Power and Wealth",
    y = NULL,
    x = NULL
  ) +
  theme_classic() 

lin.reg3 <- 
  df1 %>%
  ggplot(aes(gdp_per_capita, economic_power)) +
  geom_point(alpha = .6) + 
  geom_smooth(method = lm, se = FALSE, 
              color = "coral", 
              linetype = "solid") +
  labs(
    subtitle = "Economic Power and Wealth",
    y = NULL,
    x = NULL
  ) +
  theme_classic() 

lin.reg.all <- (lin.reg + lin.reg1 + lin.reg2 + lin.reg3 ) +
  plot_annotation(
    title = "GDP per Capita and Different Dimensions of National Power") &
  theme(
    plot.title = element_text(size = 16, face = "bold")
  )

lin.reg.all 

chat <- df_scaled %>%
  pivot_longer(cols = c(power_index, economic_power, tech_power, military_power),
               names_to = "power_component",
               values_to = "value") %>%
  ggplot(aes(x = gdp_per_capita, y = value, color = power_component)) +
  geom_point(alpha = .8) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_x_log10() +
  labs(
    title = "GDP per Capita and Standardized Power Measures",
    x = "GDP per Capita",
    y = "Standardized Index Values",
    color = "Power Component"
  ) +
  scale_color_manual(values = c('coral', 'steelblue1', 'magenta2', 'red4')) +
  geom_label_repel(
    data = df_scaled %>% 
      filter(country %in% c("United States", "China", "Monaco")),
    aes(x = gdp_per_capita, y = power_index, label = country),
    inherit.aes = FALSE,
    size = 2.2
    ) +
  theme_minimal() +
  theme(legend.position = "right")

chat

scaled <- chat <- df_scaled %>%
  pivot_longer(cols = c(power_index, economic_power, tech_power, military_power),
               names_to = "power_component",
               values_to = "value") %>%
  ggplot(aes(x = gdp_per_capita, y = value, color = power_component)) +
  geom_point(alpha = .8) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  labs(
    title = "GDP per Capita and Standardized Power Measures",
    x = "GDP per Capita",
    y = "Standardized Index Values",
    color = "Power Component"
  ) +
  scale_color_manual(values = c('coral', 'steelblue1', 'magenta2', 'red4')) +
  geom_label_repel(
    data = df_scaled %>% 
      filter(country %in% c("United States", "China", "Monaco")),
    aes(x = gdp_per_capita, y = power_index, label = country),
    inherit.aes = FALSE,
    size = 2.2
  ) +
  theme_minimal() +
  theme(legend.position = "right")

scaled

df1 %>%
  pivot_longer(cols = c(economic_power, tech_power, military_power),
               names_to = "type",
               values_to = "value") %>%
  ggplot(aes(gdp_per_capita, value)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~type, scales = "free_y")

lin.reg1 <- df1 %>%
  ggplot(aes(gdp_per_capita, tech_power)) +
  geom_point(alpha = .6) + 
  geom_smooth(method = lm, se = FALSE, 
              color = "red4", 
              linetype = "solid") +
  theme_classic() +
  labs(
    subtitle = "Technological Power and Wealth",
    y = NULL,
    x = "GDP per capita") 


lin.reg2 <- 
  df1 %>%
  ggplot(aes(gdp_per_capita, military_power)) +
  geom_point(alpha = .6) + 
  geom_smooth(method = lm, se = FALSE, 
              color = "steelblue3", 
              linetype = "solid") +
  labs(
    subtitle = "Military Power and Wealth",
    y = NULL,
    x = "GDP per capita"
  ) +
  theme_classic() 

lin.reg3 <- 
  df1 %>%
  ggplot(aes(gdp_per_capita, economic_power)) +
  geom_point(alpha = .6) + 
  geom_smooth(method = lm, se = FALSE, 
              color = "coral", 
              linetype = "solid") +
  labs(
    subtitle = "Economic Power and Wealth",
    y = NULL,
    x = "GDP per capita"
  ) +
  theme_classic() 

lin.reg.components <- lin.reg1 + lin.reg2 + lin.reg3 

lin.reg.components
