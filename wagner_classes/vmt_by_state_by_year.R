# Calculating VMT by state by year over time

library(tidyverse)
library(ggplot2)
library(scales)

# Read VMT data and remove commas and convert to integers
vmt = read.csv("vmt_by_state_by_year.csv")
vmt$VMT = gsub(",", "", vmt$VMT)
vmt$VMT = as.numeric(vmt$VMT)

# Read population data
pop = read.csv("pop_by_year.csv") %>%
  filter(between(Year, 2015, 2025))

# Clean VMT
vmt_clean = vmt %>%
  group_by(State, Year) %>%
  summarise(total_vmt = sum(VMT)) %>%
  filter(State == "Virginia" | State == "North Carolina") %>%
  filter(between(Year, 2015, 2025)) %>%
  arrange(State, Year)

# Merge VMT with pop and calculate VMT per capita
vmt_clean_merged = vmt_clean %>%
  left_join(pop, by = c("State", "Year")) %>%
  mutate(vmt_percap = total_vmt/Population) %>%
  mutate(vmt_percap_idx = vmt_percap/first(vmt_percap))

# Plot
ggplot(vmt_clean_merged, aes(x=Year, y=vmt_percap_idx, group = State)) +
  geom_line(aes(colour = State)) +
  ggtitle("Indexed VMT per capita over time, North Carolina and Virginia") +
  scale_x_continuous(limits = c(2015, 2025), breaks = breaks_pretty(10)) +
  scale_y_continuous(limits = c(0, 1.5), breaks = breaks_pretty(10)) +
  ylab("VMT per capita (indexed to 2015)") +
  xlab("Year") +
  theme(panel.grid.minor = element_blank())
