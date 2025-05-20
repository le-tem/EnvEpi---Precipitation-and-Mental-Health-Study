#setwd and load the data
setwd ("YOUR FILE PATH")
df <- read.csv("hospit_precip.csv")
head(df)

# Load necessary packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Convert Date column to Date type
df$Date <- as.Date(df$Date, format = "%d.%m.%Y")

# Extract year-month
df$Month <- floor_date(df$Date, "month")

# Group by month and calculate means
monthly_df <- df %>%
  group_by(Month) %>%
  summarise(
    Mean_Temp = mean(TabsD, na.rm = TRUE),
    Mean_Precip = mean(RhiresD, na.rm = TRUE),
    Mean_Hosp = mean(all, na.rm = TRUE)
  )

# Plot
ggplot(monthly_df, aes(x = Month)) +
  geom_bar(aes(y = Mean_Hosp * 5), stat = "identity", fill = "gray80", alpha = 0.6) +
  geom_line(aes(y = Mean_Temp, color = "Temperature"), size = 1.2) +
  geom_line(aes(y = Mean_Precip, color = "Precipitation"), size = 1.2) +
  scale_y_continuous(
    name = "Temperature / Precipitation",
    sec.axis = sec_axis(~./5, name = "Hospitalisations (avg per month)")
  ) +
  scale_color_manual(values = c("Temperature" = "red", "Precipitation" = "blue")) +
  labs(
    title = "Monthly Average Temperature, Precipitation, and Hospitalisations",
    x = "Month",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "gray40"),
    legend.position = "top"
  )

