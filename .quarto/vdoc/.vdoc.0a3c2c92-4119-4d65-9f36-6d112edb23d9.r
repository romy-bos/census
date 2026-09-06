#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(tidycensus)
library(sf)
#
#
#
#| message: false
income_tx <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "TX",
  year = 2020,
  geometry = TRUE
)
#
#
#
ggplot(income_tx) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c() +
  labs(
    title = "Median Household Income by County in Texas",
    fill = "Median household income",
    caption = "Source: U.S. Census Bureau, 2020 ACS 5-year estimates"
  ) +
  theme_void()
#
#
#
#| message: false
#| cache: true
edu_state <- get_acs(
  geography = "state",
  variables = c(
    "B15003_001",
    "B15003_022",
    "B15003_023",
    "B15003_024",
    "B15003_025"
  ),
  summary_var = "B15003_001",
  year = 2020
)
#
#
#
edu_state |>
  group_by(GEOID, NAME) |>
  summarize(
    total_adults = first(summary_est),
    bachelor_plus = sum(estimate[variable != "B15003_001"]),
    pct_bachelor_plus = 100 * bachelor_plus / total_adults,
    .groups = "drop"
  ) |>
  ggplot(aes(x = pct_bachelor_plus)) +
  geom_histogram()
#
#
#
#
#
