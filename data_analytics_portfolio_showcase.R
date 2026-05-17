# data_analytics_portfolio_showcase.R
# Author: Gilbert Morgan
# Purpose: Demonstrate R data analytics skills for a GitHub portfolio.
# Skills shown: data creation, data cleaning, summary statistics, grouped analysis,
# visualization, and basic linear modeling.

# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(tidyverse)

# ------------------------------------------------------------
# 2. Create a sample analytics dataset
# ------------------------------------------------------------

analytics_data <- tibble(
  project_area = c(
    "Healthcare Analytics",
    "Machine Learning",
    "SQL Databases",
    "Natural Language Processing",
    "Data Visualization",
    "Statistical Modeling"
  ),
  projects_completed = c(4, 5, 6, 4, 7, 5),
  skill_score = c(88, 91, 86, 84, 90, 89),
  primary_tool = c("R", "Python", "SQL", "Python", "R", "R")
)

# View the dataset
print(analytics_data)

# ------------------------------------------------------------
# 3. Data cleaning and transformation
# ------------------------------------------------------------

cleaned_data <- analytics_data |>
  mutate(
    project_area = str_to_title(project_area),
    primary_tool = str_to_upper(primary_tool),
    performance_level = case_when(
      skill_score >= 90 ~ "Advanced",
      skill_score >= 85 ~ "Strong",
      TRUE ~ "Developing"
    )
  )

print(cleaned_data)

# ------------------------------------------------------------
# 4. Summary statistics
# ------------------------------------------------------------

summary_stats <- cleaned_data |>
  summarise(
    total_projects = sum(projects_completed),
    average_projects = mean(projects_completed),
    average_skill_score = mean(skill_score),
    highest_skill_score = max(skill_score),
    lowest_skill_score = min(skill_score)
  )

print(summary_stats)

# ------------------------------------------------------------
# 5. Grouped analysis
# ------------------------------------------------------------

tool_summary <- cleaned_data |>
  group_by(primary_tool) |>
  summarise(
    number_of_project_areas = n(),
    total_projects = sum(projects_completed),
    average_skill_score = mean(skill_score),
    .groups = "drop"
  ) |>
  arrange(desc(average_skill_score))

print(tool_summary)

# ------------------------------------------------------------
# 6. Data visualization
# ------------------------------------------------------------

ggplot(cleaned_data, aes(x = reorder(project_area, skill_score), y = skill_score)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Analytics Skill Areas",
    subtitle = "Portfolio skill score by project area",
    x = "Project Area",
    y = "Skill Score"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 7. Basic predictive model
# ------------------------------------------------------------

model <- lm(skill_score ~ projects_completed, data = cleaned_data)

summary(model)

# ------------------------------------------------------------
# 8. Interpretation helper
# ------------------------------------------------------------

top_skill_area <- cleaned_data |>
  arrange(desc(skill_score)) |>
  slice(1)

print(top_skill_area)

cat(
  "This R script demonstrates data cleaning, transformation, summary statistics,",
  "grouped analysis, visualization, and basic predictive modeling for a data analytics portfolio.\n"
)
