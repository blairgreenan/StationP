# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)


# Set file path
file_path <- "StationP_delta_rho.csv"

# Import the data
StationPdeltarho <- read_csv(file_path, col_names = c("Year", "DeltaRhoMainPycnocline", "DeltaRhoSeasonalPycnocline", "Notes"), skip = 1)

# Extract necessary columns
years <- StationPdeltarho$Year
delta_annual <- StationPdeltarho$DeltaRhoMainPycnocline
delta_summer <- StationPdeltarho$DeltaRhoSeasonalPycnocline

# Prepare data for plotting
plot_data <- data.frame(
  Year = years,
  DeltaAnnual = delta_annual,
  DeltaSummer = delta_summer
)

p3 <- ggplot(plot_data, aes(x = Year)) +
  geom_line(aes(y = DeltaAnnual), color = "black", linewidth = 1) +
  geom_point(aes(y = DeltaAnnual), color = "black", size = 1.5) +
  labs(x = "", y = expression(paste(Delta, rho, " (kg ", m^-3, ")")),
       title = "") +
  theme(legend.position = "none")

# fit a linear model to the anomaly data
model <- lm(plot_data$DeltaAnnual ~ plot_data$Year)
# estimate the 95% confidence interval
confidence_95 <- confint(model, level = 0.95)

# Save the plots
ggsave("Annual_mean_Delta_rho_plot_v2.png", plot = p3, dpi = 1200, width = 10, height = 6, scale = 0.5)

