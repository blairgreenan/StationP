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

# Fit a linear model to the anomaly data
model <- lm(DeltaAnnual ~ Year, data = plot_data)

# Predict values and confidence intervals
predictions <- predict(model, newdata = plot_data, interval = "confidence", level = 0.95)

# Add the predictions to the plot data
plot_data <- cbind(plot_data, predictions)

# Plot data and linear fits with confidence intervals
p3 <- ggplot(plot_data, aes(x = Year)) +
  geom_line(aes(y = DeltaAnnual), color = "red", linewidth = 1) +
  geom_point(aes(y = DeltaAnnual), color = "red", size = 1.5) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5, fill = "grey") +
  geom_line(aes(y = fit), color = "black", size = 1) +
#  labs(x = "", y = expression(paste(Delta, rho, " (kg ", m^-3, ")")), title = "") +
  labs(x = "", y = expression(paste("Stratification ", " (kg ", m^-3, ")")), title = "") +
  theme(legend.position = "none")

# Save the plot
ggsave("Annual_mean_Delta_rho_plot_v3.png", plot = p3, dpi = 1200, width = 6, height = 3, scale = 1)

# Display the plot
print(p3)
