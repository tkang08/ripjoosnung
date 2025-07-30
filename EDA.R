
full.data <- read.csv("data/full_data.csv")
elected.data <- read.csv("data/elected_data.csv")



education_cols <- c(
  "pct_less_than_9th_grade", "pct_9th_to_12th_grade_no_diploma",
  "pct_high_school_graduate", "pct_college_no_degree",
  "pct_associate_degree", "pct_bachelor_degree", "pct_graduate_professional_degree"
)

marriage_cols <- c(
  "pct_never_married", "pct_married", "pct_seperated_widowed_divorced"
)

poverty_cols <- c("pct_below_poverty", "pct_above_poverty")

race_cols <- c(
  "pct_white", "pct_black", "pct_native_american", "pct_asian",
  "pct_pacific_islander", "pct_other", "pct_two_or_more_races"
)

sex_cols <- c("pct_male", "pct_female")

age_cols <- c(
  "pct_under_18", "pct_18.24", "pct_25.34",
  "pct_35.44", "pct_45.54", "pct_55.64", "pct_over_65"
)

library(tidyverse)

# Set valid years (excluding 2020)
valid_suffixes <- paste0("_", substr(c(2014:2019, 2021:2023), 3, 4))

# Filter relevant years
filtered_data <- full.data %>%
  filter(str_sub(ID, -3, -1) %in% valid_suffixes) %>%
  mutate(year = paste0("20", str_sub(ID, -2, -1)))

# Education Plot
education_long <- filtered_data %>%
  select(year, all_of(education_cols)) %>%
  pivot_longer(cols = education_cols, names_to = "education_level", values_to = "percentage")

ggplot(education_long, aes(x = education_level, y = percentage)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  facet_wrap(~ year) +
  labs(title = "Educational Attainment Distribution (2014–2019, 2021–2023)",
       x = "Education Level", y = "Proportion") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),  # facet labels
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Marriage Plot
marriage_long <- filtered_data %>%
  select(year, all_of(marriage_cols)) %>%
  pivot_longer(cols = marriage_cols, names_to = "marriage_status", values_to = "percentage")

ggplot(marriage_long, aes(x = marriage_status, y = percentage)) +
  geom_boxplot(fill = "mistyrose", color = "red3") +
  facet_wrap(~ year) +
  labs(title = "Marriage Status Distribution", x = "Marriage Status", y = "Proportion") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Poverty Plot
poverty_long <- filtered_data %>%
  select(year, all_of(poverty_cols)) %>%
  pivot_longer(cols = poverty_cols, names_to = "poverty_status", values_to = "percentage")

ggplot(poverty_long, aes(x = poverty_status, y = percentage)) +
  geom_boxplot(fill = "khaki", color = "darkorange") +
  facet_wrap(~ year) +
  labs(title = "Poverty Status Distribution", x = "Poverty Status", y = "Proportion") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  )

# Race Plot
race_long <- filtered_data %>%
  select(year, all_of(race_cols)) %>%
  pivot_longer(cols = race_cols, names_to = "race", values_to = "percentage")

ggplot(race_long, aes(x = race, y = percentage)) +
  geom_boxplot(fill = "plum", color = "purple4") +
  facet_wrap(~ year) +
  labs(title = "Race Distribution", x = "Race", y = "Proportion") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Sex Plot
sex_long <- filtered_data %>%
  select(year, all_of(sex_cols)) %>%
  pivot_longer(cols = sex_cols, names_to = "sex", values_to = "percentage")

ggplot(sex_long, aes(x = sex, y = percentage)) +
  geom_boxplot(fill = "lightcyan", color = "steelblue4") +
  facet_wrap(~ year) +
  labs(title = "Sex Distribution", x = "Sex", y = "Proportion") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  )

# Age Plot
age_long <- filtered_data %>%
  select(year, all_of(age_cols)) %>%
  pivot_longer(cols = age_cols, names_to = "age_group", values_to = "percentage")

ggplot(age_long, aes(x = age_group, y = percentage)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  facet_wrap(~ year) +
  labs(title = "Age Group Distribution (2014–2019, 2021–2023)",
       x = "Age Group",
       y = "Proportion of Population") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




library(tidyverse)
library(usmap)
library(scales)
library(ggpubr)  # for `ggarrange`

# Years to loop through
years <- c(2014:2019, 2021:2023)

# Clean and prepare data
map_data <- full.data %>%
  filter(str_sub(ID, -2, -1) %in% str_sub(years, 3, 4)) %>%
  mutate(
    year = paste0("20", str_sub(ID, -2, -1)),
    state = str_sub(ID, 1, 2),
    pct_non_white = 1 - pct_white,
    pct_more_than_hs = pct_college_no_degree + pct_associate_degree +
      pct_bachelor_degree + pct_graduate_professional_degree
  )

# Helper function to generate maps
make_map_list <- function(value_col, title_prefix, fill_name, limits, viridis_option = "magma") {
  plots <- list()
  for (yr in years) {
    yr_data <- map_data %>% filter(year == as.character(yr))
    p <- plot_usmap(data = yr_data, values = value_col, regions = "states") +
      scale_fill_viridis_c(
        name = fill_name,
        label = percent_format(accuracy = 1),
        limits = limits,
        option = viridis_option
      ) +
      labs(title = paste0(title_prefix, " (", yr, ")")) +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    plots[[as.character(yr)]] <- p
  }
  return(plots)
}



# Generate plot lists for each category
plots_race <- make_map_list("pct_non_white", "Percent Non-White by State", "% Non-White", c(0, 1), viridis_option = "magma")
plots_edu <- make_map_list("pct_more_than_hs", "Percent With > High School Degree", "% > HS Degree", c(0.4, 1), viridis_option = "plasma")
plots_sex <- make_map_list("pct_female", "Percent Female Population by State", "% Female", c(0.45, 0.55), viridis_option = "inferno")
plots_poverty <- make_map_list("pct_below_poverty", "Poverty Rate by State", "% Below Poverty", c(0, 0.3), viridis_option = "cividis")


# Arrange 9 maps into one figure per category
panel_race <- ggarrange(plotlist = plots_race, ncol = 3, nrow = 3)
panel_edu <- ggarrange(plotlist = plots_edu, ncol = 3, nrow = 3)
panel_sex <- ggarrange(plotlist = plots_sex, ncol = 3, nrow = 3)
panel_poverty <- ggarrange(plotlist = plots_poverty, ncol = 3, nrow = 3)

# These will each show in their own RStudio Plots tab if run one at a time
panel_race
panel_edu
panel_sex
panel_poverty

print(names(elected.data))

