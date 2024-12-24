# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)

# Set file path
file_path <- "StationP_delta_rho.csv"

# Import the data
StationPdeltarho <- read_csv(file_path, col_names = c("Year", "DeltaRhoMainPycnocline", "DeltaRhoSeasonalPycnocline", "Notes"), skip = 1)

# Extract necessary columns
years <- StationPdeltarho$Year
delta_annual <- StationPdeltarho$DeltaRhoMainPycnocline
delta_summer <- StationPdeltarho$DeltaRhoSeasonalPycnocline

# Results of least squares fits
coeff_lin_a <- c(0.0022535, 1.0006)  # kg m^{-3} yr^{-1}, kg m^{-3}
coeff_lin_s <- c(0.0067033, 0.99804)  # kg m^{-3} yr^{-1}, kg m^{-3}

# Results of Theil-Sen fits
b_a <- 0.99065  # kg m^{-3}
m_a <- 0.0021146  # kg m^{-3} yr^{-1}
b_s <- 1.0599  # kg m^{-3}
m_s <- 0.0061259  # kg m^{-3} yr^{-1}

# Construct the least squares linear fits
x0 <- years[1]
fit_lin_a <- coeff_lin_a[1] * (years - x0) + coeff_lin_a[2]
fit_lin_s <- coeff_lin_s[1] * (years - x0) + coeff_lin_s[2]

# Theil-Sen linear fits
fit_ts_a <- b_a + m_a * (years - x0)
fit_ts_s <- b_s + m_s * (years - x0)

# Prepare data for plotting
plot_data <- data.frame(
  Year = years,
  DeltaAnnual = delta_annual,
  DeltaSummer = delta_summer,
  FitLinA = fit_lin_a,
  FitLinS = fit_lin_s,
  FitTsA = fit_ts_a,
  FitTsS = fit_ts_s
)

# Plot data and linear fits
p1 <- ggplot(plot_data, aes(x = Year)) +
  geom_line(aes(y = DeltaSummer), color = "black", linewidth = 1.5) +
  geom_line(aes(y = FitLinS), color = "blue", linewidth = 2) +
  geom_line(aes(y = FitTsS), color = "red", linewidth = 2) +
  xlim(1955, 2025) +
  ylim(0.4, 2.0) +
  labs(y = expression(paste(Delta, rho, " (kg m^{-3})")),
       title = "Summer mean \u0394rho") +
  theme(legend.position = "none")

p2 <- ggplot(plot_data, aes(x = Year)) +
  geom_line(aes(y = DeltaAnnual), color = "black", linewidth = 1.5) +
  geom_line(aes(y = FitLinA), color = "blue", linewidth = 2) +
  geom_line(aes(y = FitTsA), color = "red", linewidth = 2) +
  xlim(1955, 2025) +
  ylim(0.4, 2.0) +
  labs(y = expression(paste(Delta, rho, " (kg m^{-3})")),
       title = "Annual mean \u0394rho") +
  theme(legend.position = "none")

p3 <- ggplot(plot_data, aes(x = Year)) +
  geom_line(aes(y = DeltaAnnual), color = "black", linewidth = 1.5) +
#  geom_line(aes(y = FitLinA), color = "blue", linewidth = 2) +
#  geom_line(aes(y = FitTsA), color = "red", linewidth = 2) +
  xlim(1955, 2025) +
  ylim(0.4, 2.0) +
  labs(y = expression(paste(Delta, rho, " (kg m^{-3})")),
       title = "Annual mean \u0394rho") +
  theme(legend.position = "none")

# fit a linear model to the anomaly data
model <- lm(plot_data$DeltaAnnual ~ plot_data$Year)
# estimate the 95% confidence interval
confidence_95 <- confint(model, level = 0.95)
# add the linear fit to the plot

# Save the plots
ggsave("Summer_mean_Delta_rho_plot.png", plot = p1, dpi = 1200, width = 10, height = 6, scale = 0.75)
ggsave("Annual_mean_Delta_rho_plot.png", plot = p2, dpi = 1200, width = 10, height = 6, scale = 0.75)
ggsave("Annual_mean_Delta_rho_plot_v2.png", plot = p3, dpi = 1200, width = 10, height = 6, scale = 0.75)

