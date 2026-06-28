

library(tidyverse)

crime <- read_csv(
  "https://raw.githubusercontent.com/SMUREU/Hackathon_2026/main/violent_crimes.csv"
)

crime_long <- crime %>%
  rename(Location = 1) %>%      # rename the first column
  pivot_longer(cols = -Location,
               names_to = "Year",
               values_to = "Violent_Crimes") %>%
  mutate(Year = as.numeric(Year),
         Violent_Crimes = as.numeric(Violent_Crimes))

cities <- c("Chicago", "Dallas", "New York City", "Los Angeles", "Houston")

crime_long %>%
  filter(Location %in% cities) %>%
  ggplot(aes(x = Year, y = Violent_Crimes, color = Location)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Violent Crimes by City (Past 10 Years)",
    x = "Year",
    y = "Number of Violent Crimes",
    color = "City"
  ) +
  theme_minimal(base_size = 14)
####################################################################

# reading parental education

reading_parents <- read_csv(
  "https://raw.githubusercontent.com/SMUREU/Hackathon_2026/main/reading_parental_education.csv"
)

###############

#Educational Attainment by City

library(tidyverse)
library(ggrepel)

df <- read_csv(
  "C:/Users/isabe/OneDrive - Southern Methodist University/hackathon_summer2026/educational_attainment_cleaned.csv"
)

# Most divergent metros — the story is in the disagreement
plot_df <- df %>%
  mutate(gap = abs(educational_attainment_rank -
                     quality_of_education_and_attainment_gap_rank)) %>%
  slice_max(gap, n = 15) %>%
  pivot_longer(
    c(educational_attainment_rank, quality_of_education_and_attainment_gap_rank),
    names_to = "measure", values_to = "rank_value"     # <- renamed
  ) %>%
  mutate(measure = recode(measure,
                          educational_attainment_rank = "Attainment",
                          quality_of_education_and_attainment_gap_rank = "Quality / Gap"))

# 
# plot_df <- df %>%
#   mutate(gap = abs(
#     educational_attainment_rank -
#       quality_of_education_and_attainment_gap_rank
#   )) %>%
#   slice_max(gap, n = 15) %>%
#   pivot_longer(
#     c(
#       educational_attainment_rank,
#       quality_of_education_and_attainment_gap_rank
#     ),
#     names_to = "measure",
#     values_to = "rank"
#   ) %>%
#   mutate(
#     measure = recode(
#       measure,
#       educational_attainment_rank = "Attainment",
#       quality_of_education_and_attainment_gap_rank = "Quality / Gap"
#     )
#   )

# ggplot(plot_df, aes(x = measure, y = rank, group = metro_name)) +
#   geom_line(aes(color = measure == "Attainment"),
#             linewidth = 0.9,
#             alpha = 0.7) +
#   geom_point(size = 2.5, color = "#20808D") +
#   geom_text_repel(
#     data = filter(plot_df, measure == "Attainment"),
#     aes(label = metro_name),
#     hjust = 1,
#     nudge_x = -0.05,
#     size = 3,
#     direction = "y",
#     segment.color = NA
#   ) +
#   geom_text_repel(
#     data = filter(plot_df, measure == "Quality / Gap"),
#     aes(label = rank),
#     hjust = 0,
#     nudge_x = 0.05,
#     size = 3
#   ) +
#   scale_y_reverse(breaks = c(1, 50, 100, 150)) +   # rank 1 on top
#   scale_color_manual(values = c("#A84B2F", "#20808D"), guide = "none") +
#   labs(
#     title = "Where a metro's two report cards disagree",
#     subtitle = "The 15 metros with the widest split between raw attainment and education quality/equity",
#     x = NULL,
#     y = "Rank (1 = best)"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(panel.grid.major.x = element_blank())


df <- df %>%
  mutate(divergence = quality_of_education_and_attainment_gap_rank -
           educational_attainment_rank)
# df <- df %>% filter(city %in% c("Dallas", "Houston", "Chicago", "Los Angeles", "New York"))

ggplot(
  df,
  aes(
    educational_attainment_rank,
    quality_of_education_and_attainment_gap_rank
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  geom_point(aes(size = total_score, color = divergence), alpha = 0.8) +
  geom_text_repel(
    data = df %>% slice_max(abs(divergence), n = 10),
    aes(label = metro_name),
    size = 3,
    max.overlaps = 20
  ) +
  scale_color_gradient2(
    low = "#20808D",
    mid = "grey80",
    high = "#A84B2F",
    midpoint = 0,
    name = "Gap rank −\nAttain. rank"
  ) +
  scale_size(range = c(1, 7), name = "Total score") +
  scale_x_reverse() + scale_y_reverse() +
  labs(
    title = "Attainment vs. quality: who's punching above (or below) their weight",
    subtitle = "Below the line = better educated than equitable; above = more equitable than educated",
    x = "Educational attainment rank (1 = best)",
    y = "Quality / gap rank (1 = best)"
  ) +
  theme_minimal(base_size = 12)
#################################


library(tidycensus)
library(tidyverse)

# Set your Census API key once
# census_api_key("0896836edef700a287006cf57435bc7015ca9c0a", install = TRUE)

vars <- c(income = "B19013_001", bachelors = "S1501_C02_015")
#
# vars <- c(
#   income = "B19013_001",      # Median household income
#   bachelors = "DP02_0067PE"   # % Bachelor's degree or higher
# )

dallas <- get_acs(
  geography = "tract",
  variables = vars,
  state = "TX",
  county = "Dallas",
  year = 2024,
  survey = "acs5",
  output = "wide"
)
median(dallas$bachelorsE, na.rm = TRUE)
mean(dallas$bachelorsE, na.rm = TRUE)

median(dallas$incomeE, na.rm = TRUE)
mean(dallas$incomeE, na.rm = TRUE)

ggplot(dallas, aes(incomeE, bachelorsE)) +
  geom_point(alpha = .7) +
  geom_vline(xintercept = median(dallas$incomeE, na.rm = TRUE)) +
  geom_hline(yintercept = median(dallas$bachelorsE, na.rm = TRUE)) +
  labs(title = "Dallas County Education Attainment and Median Income", x =
         "Median Household Income ($)", y = "% Bachelor's Degree or Higher")

median_income <- median(dallas$incomeE, na.rm = TRUE)
bachelors_pct <- median(dallas$bachelorsE, na.rm = TRUE)

ggplot(df, aes(median_income, bachelors_pct, color = poverty_rate)) +
  geom_point(size = 3) +
  geom_text(aes(label = neighborhood),
            check_overlap = TRUE,
            nudge_y = 1) +
  geom_vline(xintercept = median(df$median_income),
             linetype = "dashed") +
  geom_hline(yintercept = median(df$bachelors_pct),
             linetype = "dashed") +
  labs(x = "Median Household Income", y = "% Bachelor's Degree or Higher", color = "Poverty Rate")


library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)

# Download Dallas County tracts with geometry
dallas <- get_acs(
  geography = "tract",
  variables = c(
    income = "B19013_001",
    bachelors = "S1501_C02_015",
    poverty = "S1701_C03_001"
  ),
  state = "TX",
  county = "Dallas",
  geometry = TRUE,
  year = 2024,
  survey = "acs5",
  output = "wide"
)

# City boundary
cities <- places(state = "TX", year = 2024)

dallas_city <-
  cities %>%
  filter(NAME == "Dallas")

# Keep only tracts inside Dallas city
dallas_city_tracts <-
  st_filter(dallas, dallas_city)


# add quadrants
income_med <- median(dallas_city_tracts$incomeE, na.rm = TRUE)
educ_med   <- median(dallas_city_tracts$bachelorsE, na.rm = TRUE)

dallas_city_tracts <-
  dallas_city_tracts %>%
  mutate(
    quadrant = case_when(
      incomeE >= income_med &
        bachelorsE >= educ_med ~ "High Income\nHigh Education",
      incomeE >= income_med &
        bachelorsE < educ_med ~ "High Income\nLow Education",
      incomeE < income_med &
        bachelorsE >= educ_med ~ "Low Income\nHigh Education",
      TRUE ~ "Low Income\nLow Education"
    )
  )


quad_counts <-
  dallas_city_tracts %>%
  count(quadrant)

quad_counts


scale_color_viridis_c(option = "plasma",
                      direction = -1,
                      name = "Poverty (%)")

#408 dallas city tracts


library(ggrepel)

ggplot(dallas_city_tracts, aes(incomeE, bachelorsE, color = povertyE)) +
  
  geom_point(size = 2.8, alpha = .85) +
  
  geom_vline(xintercept = income_med, linetype = "dashed") +
  annotate("text", x = 200000, y = educ_med + 5,
           label = paste0("Overall Median Household Income= $", income_med), 
           angle = 0, vjust = -0.5, color = "red") +
  
  geom_hline(yintercept = educ_med, linetype = "dashed") +
  annotate("text", x = 200000, y = educ_med, 
           label = paste0("Median %Bachelor's Degree= ", educ_med, "%"),
           angle = 0, vjust = -0.5, color = "red")+
  
  scale_color_viridis_c(option = "plasma",
                        direction = -1,
                        name = "Poverty (%)") +
  
  labs(
    title = "Educational Attainment vs Household Income",
    subtitle = "Dallas Census Tracts",
    x = "Median Household Income ($)",
    y = "% Bachelor's Degree or Higher"
  ) +
  
  theme_minimal(base_size = 13) +
  
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste("High/High:", quad_counts$n[quad_counts$quadrant ==
                                                "High Income\nHigh Education"]),
    hjust = 1.1,
    vjust = 1.5
  ) +
  annotate(
    "text",
    x = Inf,
    y = -Inf,
    label = paste("High/Low:", quad_counts$n[quad_counts$quadrant ==
                                               "High Income\nLow Education"]),
    hjust = 1.1,
    vjust = -0.5
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = paste("Low/High:", quad_counts$n[quad_counts$quadrant ==
                                               "Low Income\nHigh Education"]),
    hjust = -0.1,
    vjust = 1.5
  ) +
  annotate(
    "text",
    x = -Inf,
    y = -Inf,
    label = paste("Low/Low:", quad_counts$n[quad_counts$quadrant ==
                                              "Low Income\nLow Education"]),
    hjust = -0.1,
    vjust = -0.5
  )






##############33
library(tidyverse)
library(ggrepel)

df <- read_csv("C:/Users/isabe/OneDrive - Southern Methodist University/hackathon_summer2026/educational_attainment_cleaned.csv")

# 
# plot_df <- df %>%
#   mutate(gap = abs(educational_attainment_rank -
#                      quality_of_education_and_attainment_gap_rank)) %>%
#   slice_max(gap, n = 15) %>%
#   pivot_longer(
#     c(educational_attainment_rank, quality_of_education_and_attainment_gap_rank),
#     names_to = "measure", values_to = "rank_value"
#   ) %>%
#   mutate(measure = recode(measure,
#                           educational_attainment_rank = "Attainment",
#                           quality_of_education_and_attainment_gap_rank = "Quality / Gap"))
# 
# ggplot(plot_df, aes(x = measure, y = rank_value, group = metro_name)) +
#   geom_line(aes(color = measure == "Attainment"), linewidth = 0.9, alpha = 0.7) +
#   geom_point(size = 2.5, color = "#20808D") +
#   geom_text_repel(
#     data = filter(plot_df, measure == "Attainment"),
#     aes(label = metro_name), hjust = 1, nudge_x = -0.05,
#     size = 3, direction = "y", segment.color = NA) +
#   geom_text_repel(
#     data = filter(plot_df, measure == "Quality / Gap"),
#     aes(label = rank_value), hjust = 0, nudge_x = 0.05, size = 3) +
#   scale_y_reverse(breaks = c(1, 50, 100, 150)) +
#   scale_color_manual(values = c("#A84B2F", "#20808D"), guide = "none") +
#   labs(
#     title = "Where a metro's two report cards disagree",
#     subtitle = "15 metros with the widest split between raw attainment and education quality/equity",
#     x = NULL, y = "Rank (1 = best)") +
#   theme_minimal(base_size = 12) +
#   theme(panel.grid.major.x = element_blank())





library(tidyverse)
library(ggrepel)


# Plain scatter: education on x, income on y
ggplot(df, aes(x = bachelorsE, y = incomeE)) +
  geom_point() +
  labs(
    title = "Education vs. Income across 150 cities",
    x = "Education",
    y = "Income"
  ) +
  theme_minimal()


# Most divergent metros — the story is in the disagreement
plot_df <- df %>%
  mutate(gap = abs(educational_attainment_rank -
                     quality_of_education_and_attainment_gap_rank)) %>%
  slice_max(gap, n = 15) %>%
  pivot_longer(
    c(educational_attainment_rank, quality_of_education_and_attainment_gap_rank),
    names_to = "measure", values_to = "rank"
  ) %>%
  mutate(measure = recode(measure,
                          educational_attainment_rank = "Attainment",
                          quality_of_education_and_attainment_gap_rank = "Quality / Gap"))

ggplot(plot_df, aes(x = measure, y = rank, group = metro_name)) +
  geom_line(aes(color = measure == "Attainment"), linewidth = 0.9, alpha = 0.7) +
  geom_point(size = 2.5, color = "#20808D") +
  geom_text_repel(
    data = filter(plot_df, measure == "Attainment"),
    aes(label = metro_name), hjust = 1, nudge_x = -0.05,
    size = 3, direction = "y", segment.color = NA) +
  geom_text_repel(
    data = filter(plot_df, measure == "Quality / Gap"),
    aes(label = rank), hjust = 0, nudge_x = 0.05, size = 3) +
  scale_y_reverse(breaks = c(1, 50, 100, 150)) +   # rank 1 on top
  scale_color_manual(values = c("#A84B2F", "#20808D"), guide = "none") +
  labs(
    title = "Where a metro's two report cards disagree",
    subtitle = "The 15 metros with the widest split between raw attainment and education quality/equity",
    x = NULL, y = "Rank (1 = best)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())


df %>%
  slice_min(total_score, n = 20) %>%
  mutate(metro_name = fct_reorder(metro_name, total_score)) %>%
  ggplot(aes(total_score, metro_name)) +
  geom_segment(aes(x = median(df$total_score), xend = total_score,
                   yend = metro_name), color = "grey80") +
  geom_point(aes(color = state), size = 3) +
  geom_vline(xintercept = median(df$total_score),
             linetype = "dashed", color = "grey50") +
  labs(title = "Bottom 20 metros by total score",
       subtitle = "Dashed line = national median",
       x = "Total score", y = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "none")



########################################
library(tidycensus)
library(tidyverse)
library(scales)

# One-time: get a free key at https://api.census.gov/data/key_signup.html
# census_api_key("YOUR_KEY_HERE", install = TRUE)  # run once, then restart R

# --- Pull data for ALL places nationwide (one row per city/town) ---
# B19013_001  = median household income ($)
# S1501_C02_015 = % of adults 25+ with a bachelor's degree or higher
vars <- c(
  median_income = "B19013_001",
  pct_bachelors = "S1501_C02_015",
  total_pop = "B01003_001"
)

acs <- get_acs(
  geography = "place",
  variables = vars,
  year      = 2023,
  survey    = "acs5",
  output    = "wide"     # one column per variable -> easy to plot
)

# --- Clean up ---
plot_df <- acs %>%
  
  transmute(
    place         = NAME,
    median_income = median_incomeE,   # 'E' = estimate column
    pct_bachelors = pct_bachelorsE,
    total_pop = total_popE
  ) %>%
  drop_na(median_income, pct_bachelors)
  

# population at least 100,000
plot_df <- plot_df %>%
filter(total_pop >= 100000)

# --- Plain scatter: education (x) vs income (y) ---
ggplot(plot_df, aes(x = pct_bachelors, y = median_income)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title    = "Education vs. income across U.S. cities",
    subtitle = "Each point is a Census 'place' (city/town), ACS 2019–2023 5-year estimates\n
    Filtered to places with a population over 100,000",
    x = "Adults 25+ with a bachelor's degree or higher",
    y = "Median household income",
    caption  = "Source: U.S. Census Bureau, ACS 5-year estimates"
  ) +
  coord_flip()+
  theme_minimal(base_size = 12)
