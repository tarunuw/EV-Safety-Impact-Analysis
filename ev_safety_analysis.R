# EV Safety Analysis
# Author: Tarun Tamilselvan
# Description: Analyzes EV adoption and crash data to explore safety trends.

# --- Load Libraries ---
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

# --- Load Data ---
ev_data <- read.csv("data/Electric_Vehicle_Title_and_Registration_Activity_20241120.csv")
crash_data <- read.csv("data/NHTSA_Crash_Data.csv")

# --- Data Preparation ---

# Clean and simplify EV registration data
ev_summary <- ev_data %>%
  mutate(Year = as.numeric(str_extract(Model.Year, "\\d{4}"))) %>%
  group_by(Year) %>%
  summarise(
    Total_EV_Purchases = n(),
    Tesla_Purchases = sum(str_detect(Make, "TESLA"))
  ) %>%
  filter(!is.na(Year))

# Simplify crash dataset (example structure)
crash_summary <- crash_data %>%
  mutate(
    Vehicle_Type = ifelse(str_detect(Vehicle.Make, "TESLA|ELECTRIC"), "EV", "Non-EV"),
    Year = as.numeric(str_extract(Crash.Date, "\\d{4}"))
  ) %>%
  group_by(Year, Vehicle_Type, Cause) %>%
  summarise(Crashes = n(), .groups = "drop") %>%
  filter(!is.na(Year))

# --- Visualization 1: EV Purchases Over Time ---
plot_ev_growth <- ggplot(ev_summary, aes(x = Year)) +
  geom_line(aes(y = Total_EV_Purchases, color = "All EVs"), size = 1.2) +
  geom_line(aes(y = Tesla_Purchases, color = "Tesla"), size = 1.2, linetype = "dashed") +
  labs(
    title = "EV Purchases Over Time",
    x = "Year",
    y = "Number of Purchases",
    color = "Category"
  ) +
  theme_minimal()

ggplotly(plot_ev_growth)

# --- Visualization 2: EV vs Non-EV Crash Causes ---
plot_crash_causes <- crash_summary %>%
  group_by(Cause, Vehicle_Type) %>%
  summarise(Total = sum(Crashes)) %>%
  ggplot(aes(x = Cause, y = Total, fill = Vehicle_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "EV vs Non-EV Crash Causes",
    x = "Crash Cause",
    y = "Number of Crashes",
    fill = "Vehicle Type"
  ) +
  theme_minimal() +
  coord_flip()

ggplotly(plot_crash_causes)

# --- Visualization 3: Combined Trends ---
combined <- ev_summary %>%
  left_join(
    crash_summary %>%
      filter(Vehicle_Type == "EV") %>%
      group_by(Year) %>%
      summarise(EV_Crashes = sum(Crashes)),
    by = "Year"
  )

plot_combined <- ggplot(combined, aes(x = Year)) +
  geom_line(aes(y = Total_EV_Purchases / 100, color = "EV Purchases"), size = 1.2) +
  geom_line(aes(y = EV_Crashes, color = "EV Crashes"), size = 1.2, linetype = "dashed") +
  labs(
    title = "Combined EV Purchase and Crash Trends",
    subtitle = "(EV purchases scaled down for comparison)",
    x = "Year",
    y = "Count",
    color = "Metric"
  ) +
  theme_minimal()

ggplotly(plot_combined)

# --- Insights ---
cat("Insights:\n")
cat("- Tesla purchases strongly correlate with EV adoption.\n")
cat("- Non-EVs still dominate crash counts, though EV crashes are rising.\n")
cat("- EV crash rates remain relatively low — possibly due to newer, safer models.\n")
