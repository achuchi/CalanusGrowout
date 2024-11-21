# Libraries I usually use in R for general applications
library(ggplot2)
library(tidyr)
library(dplyr)

# Data Sorting --> Normally I just read.csv from the xlsx file, but for small (n) counts it's easier to manually enter than fiddle with the columns, rownames, etc. 
group14_data <- data.frame(
  time = c(0.0, 0.5, 1.0, 1.5, 2.0, 3.3, 3.7, 4.2, 4.7, 5.2),
  hemocytometer = c(25600, 20000, 11100, 15600, 4400, 12100, 11500, 5400, 13900, 8490),
  fcm = c(23500, NA, 20900, NA, 22000, 19400, 22300, 25500, NA, NA),
  fluorescence = c(610.2, 610.35, NA, 558.05, NA, 261.49, 278.22, 238.86, NA, NA)
)

# Plot 1: Hemocytometer counts
plot1 <- ggplot(group14_data, aes(x = time, y = hemocytometer)) +
  geom_line(aes(group = 1)) +  # Group = 1 to force line connection --> ditto for other plots 
  geom_point(size = 3) +
  labs(title = "Rhodomonas Concentration vs Time (Hemocytometer)",
       x = "Time (hours)",
       y = "Concentration (cells/ml)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot 2: FCM counts
plot2 <- ggplot(group14_data %>% filter(!is.na(fcm)), aes(x = time, y = fcm)) +
  geom_line(aes(group = 1)) +  
  geom_point(size = 3) +
  labs(title = "Rhodomonas Concentration vs Time (FCM)",
       x = "Time (hours)",
       y = "Concentration (cells/ml)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot 3: Fluorescence
plot3 <- ggplot(group14_data %>% filter(!is.na(fluorescence)), aes(x = time, y = fluorescence)) +
  geom_line(aes(group = 1)) + 
  geom_point(size = 3) +
  labs(title = "Rhodomonas Concentration vs Time (Fluorescence)",
       x = "Time (hours)",
       y = "Fluorescence (RFU)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Print plots

print(plot1)
print(plot2)
print(plot3)
