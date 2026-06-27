
library(tidyverse)

crime <- read_csv(
  "https://raw.githubusercontent.com/SMUREU/Hackathon_2026/main/violent_crimes.csv"
)

crime_long <- crime %>%
  rename(Location = 1) %>%      # rename the first column
  pivot_longer(
    cols = -Location,
    names_to = "Year",
    values_to = "Violent_Crimes"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Violent_Crimes = as.numeric(Violent_Crimes)
  )

cities <- c(
  "Chicago",
  "Dallas",
  "New York City",
  "Los Angeles",
  "Houston"
)

crime_long %>%
  filter(Location %in% cities) %>%
  ggplot(aes(x = Year,
             y = Violent_Crimes,
             color = Location)) +
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

df <- read_csv("C:/Users/isabe/OneDrive - Southern Methodist University/hackathon_summer2026/educational_attainment_cleaned.csv")

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