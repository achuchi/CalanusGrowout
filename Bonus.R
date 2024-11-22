# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create dataframes for each method
# Groups: Cos (1-3,16-18), Rho (4-6,13-15), Amp (7-12)

# Function to create measurement data
create_measurement_data <- function(data_list, method_name) {
  data.frame(
    Time = rep(c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.3, 3.8, 4.3, 4.8), length(data_list)),
    Group = rep(names(data_list), each = 10),
    Value = unlist(data_list),
    Method = method_name,
    Phytoplankton = rep(c(rep("Coscinodiscus", 6), 
                          rep("Rhodomonas", 6),
                          rep("Amphidinium", 6)), each = 10)
  )
}

# Count data
counts_list <- list(
  # Coscinodiscus groups
  Cos_G1 = c(60000, 20000, 20000, 20000, 20000, 10000, 10000, 5000, 15555, 7777),
  Cos_G2 = c(6667, 3333, 4444, 1111, 1111, 3333, 7778, 3333, 3333, 0),
  Cos_G3 = c(3333, 1111, 0, 0, 2222, 1111, 3333, 3333, 1111, NA),
  # Rhodomonas groups
  Rho_G13 = c(30000, 24400, 14400, 25000, 36000, 17000, 6700, 40000, 26600, NA),
  Rho_G14 = c(25600, 20000, 11100, 15600, 4400, 12100, 11500, 5400, 13900, 8490),
  Rho_G15 = c(50000, 46700, 56700, 35600, 37800, 30000, 11100, 10000, 10000, 4440),
  # Amphidinium groups
  Amp_G7 = c(42222, 34444, 26667, 21111, 18889, 21111, 4444, 13333, 25556, 8889),
  Amp_G8 = c(14444, 13333, 23333, 17778, 12222, NA, 13703, 4321, 4938, 3457),
  Amp_G10 = c(32222, 56667, 51111, 28889, 10000, 8889, 37778, 26600, 28800, 18800)
)

# FCM data
fcm_list <- list(
  # Coscinodiscus groups
  Cos_G1 = c(37600, 25300, 29300, 36100, 44700, 36500, 43300, 46300, 73500, 10800),
  Cos_G2 = c(29000, 23100, 26300, NA, 35600, NA, 79200, 75800, 90800, 76300),
  Cos_G3 = c(13800, 14800, 11600, 14100, 12000, 23900, 27700, 25000, 28400, NA),
  # Rhodomonas groups
  Rho_G13 = c(31100, NA, 31600, NA, 34800, 29300, 34300, 31700, 27200, NA),
  Rho_G14 = c(23500, NA, 20900, NA, 22000, 19400, 22300, 25500, NA, NA),
  Rho_G15 = c(11400, NA, 10400, NA, 11600, NA, 12900, 12900, 12300, 9780),
  # Amphidinium groups
  Amp_G7 = c(20500, 18300, 20300, 18500, 18500, 20000, 14200, 16200, 15000, 19300),
  Amp_G8 = c(8930, NA, 13800, 14000, 12500, NA, 9840, 9370, 9080, 8860),
  Amp_G10 = c(22600, NA, 18100, NA, 16000, NA, 21900, 15800, 17500, 17700)
)

# Fluorescence data
fluor_list <- list(
  # Coscinodiscus groups
  Cos_G1 = c(223.93, 138.95, 110.64, 170.64, 83.27, 62.14, 363.90, 531.20, 494.08, 444.00),
  Cos_G2 = c(227.68, 154.48, NA, 154.59, NA, 180.28, 191.08, 214.08, 209.47, 213.72),
  Cos_G3 = c(96.64, 167.40, 71.62, 54.66, 87.98, 122.51, 127.99, 120.88, 111.30, NA),
  # Rhodomonas groups
  Rho_G13 = c(382.01, 387.12, NA, 400.64, NA, 604.27, 385.6, 395.99, 372.64, NA),
  Rho_G14 = c(610.2, 610.35, NA, 558.05, NA, 261.49, 278.22, 238.86, NA, NA),
  Rho_G15 = c(180.82, 156.51, NA, 146.01, NA, 126.03, 159.92, 201.19, 152.78, 134.59),
  # Amphidinium groups
  Amp_G7 = c(140.932, 134.121, 134.032, 128.554, 125.767, 131.056, 125.441, 111.691, 106.42, 109.866),
  Amp_G8 = c(81.934, 98.139, NA, 88.003, 89.66, NA, 85.531, 79.052, 79.614, 75.912),
  Amp_G10 = c(1169.42, 1034.08, NA, 931.13, NA, 1015.68, 943.21, 745.64, 825.38, 849.64)
)

# Combining data
counts_data <- create_measurement_data(counts_list, "Counts")
fcm_data <- create_measurement_data(fcm_list, "FCM")
fluor_data <- create_measurement_data(fluor_list, "Fluorescence")

combined_data <- rbind(counts_data, fcm_data, fluor_data)

# Normalize values
normalized_data <- combined_data %>%
  group_by(Method, Group) %>%
  mutate(
    Normalized_Value = (Value / first(Value[!is.na(Value)])) * 100
  ) %>%
  ungroup()

# Create plot showing all methods
p1 <- ggplot(normalized_data, aes(x = Time, y = Normalized_Value, color = Group)) +
  geom_line() +
  geom_point() +
  facet_grid(Method ~ Phytoplankton) +
  theme_minimal() +
  labs(title = "Normalized Changes Across All Measurement Methods",
       x = "Time (hours)",
       y = "Normalized Value (%)",
       color = "Group") +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.5) +
  theme(legend.position = "bottom")

# Calculate average change by method and phytoplankton
summary_stats <- normalized_data %>%
  group_by(Method, Phytoplankton, Group) %>%
  summarize(
    Initial = first(Value[!is.na(Value)]),
    Final = last(Value[!is.na(Value)]),
    Percent_Change = ((Final - Initial) / Initial) * 100,
    .groups = 'drop'
  ) %>%
  group_by(Method, Phytoplankton) %>%
  summarize(
    Mean_Change = mean(Percent_Change, na.rm = TRUE),
    SD_Change = sd(Percent_Change, na.rm = TRUE),
    .groups = 'drop'
  )

# Create summary bar plot
p2 <- ggplot(summary_stats, aes(x = Phytoplankton, y = Mean_Change, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Mean_Change - SD_Change, 
                    ymax = Mean_Change + SD_Change),
                position = position_dodge(width = 0.9), width = 0.25) +
  theme_minimal() +
  labs(title = "Mean Changes by Method and Phytoplankton Type",
       x = "Phytoplankton Species",
       y = "Mean Percent Change (%)")

# Print plots and summary statistics
print(p1)
print(p2)
print(summary_stats)
