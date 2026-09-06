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
  arrange(pct_bachelor_plus) |>
  ggplot(aes(x = reorder(NAME, pct_bachelor_plus), y = pct_bachelor_plus)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Adults with a Bachelor's Degree or Higher by State",
    x = "State",
    y = "Adults with a bachelor's degree or higher",
    caption = "Source: U.S. Census Bureau, 2020 ACS 5-year estimates"
  ) +
  theme_minimal()
#
#
#
#| message: false
age_ca <- get_acs(
  geography = "county",
  variables = c(
    median_age = "B01002_001",
    population = "B01003_001"
  ),
  state = "CA",
  year = 2020,
  geometry = FALSE
)
#
#
#
age_ca |>
  select(GEOID, NAME, variable, estimate) |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  arrange(desc(population))
#
#
#
#
#
