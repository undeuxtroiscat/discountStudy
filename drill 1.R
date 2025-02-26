# Tasks: clean and visualize a dataset that tracks daily fruit and vegetable consumption among study participants.
# Data Wrangling: (1) filter the data for participants aged 18 to 65. (2) Categorize participants as “Low”, “Moderate”, or “High” intake based on total daily intake: Low: Less than 200 grams; Moderate: Between 200 and 400 grams; High: More than 400 grams
# Data Visualization: (1) average daily fruit and vegetable intake by gender.(2) shows the distribution of total daily intake.(3) comparing total daily intake across the three intake categories. Add labels and titles to your plots & use different colors to distinguish between groups in the bar plot.

# attach packages
library(dplyr)
library(ggplot2)

# read data
df <- read.csv("supermarket_dat.csv", na.strings = c("NA", ""), stringsAsFactors = TRUE)

# view  variables
glimpse(df)

# no age var, so create random
df$age <- sample(18:65, nrow(df), replace = TRUE)
df$gender <- sample(c("m", "f"), nrow(df), replace = TRUE)

# create new df
dat <- df |>
  # select relevant variables
  select(age, gender, fv_pre) |>
  # filter age 18 - 65
  filter(age >= 18 | age <= 65) |>
  # move age to front
  relocate(age) |> 
  # create new var to account for categories of daily f/v intake
  mutate(
    fv_pre_cat =  case_when(
      fv_pre < 200 ~ "low",
      fv_pre >= 200 & fv_pre <= 400 ~ "moderate",
      fv_pre > 400 ~ "high"
  )
)

# create plot to view intake by gender

# summarize 
dat_sum <- dat |> 
  summarise(avg_fv_pre = mean(fv_pre), .by = gender)

# plot
fruit_gender <- ggplot(
  dat_sum, 
  aes(x = gender, y = avg_fv_pre, fill = gender)
  ) +
  geom_bar(stat = "identity") +
  labs(
    title = "Baseline fruit and vegetable (g/day) intake by gender",
    x = "Gender",
    y = "Fruit and vegetable intake (g/day)",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("f" = "pink", "m" = "purple")) +
  theme_minimal()


# view distribution of total intake
fruit_distr <- ggplot(
  dat,
  aes(x = fv_pre)
) +
  geom_histogram(binwidth = 50, color = "hotpink", fill = "black") +
  labs(
    title = "Distribution of FV intake",
    x = "FV intake (g) at baseline"
  ) +
  theme_minimal()

# compare intake across different intake categories

fruit_box <- ggplot(
  dat,
  aes(x = fv_pre_cat, y = fv_pre, color = fv_pre_cat)
) +
  geom_boxplot() +
  labs(
    title = "Comparing baseline FV intake categories among \nstudy participants",
    x = NULL,
    y = "FV intake (g/day)",
    color = "Intake category"
  )

# save plots
ggsave("Average FV intake by gender.png", fruit_gender, dpi = 300)
ggsave("baseline FV distribution.png", fruit_distr, dpi = 300)
ggsave("FV intake cat for all ppts.png", fruit_box, dpi = 300)
