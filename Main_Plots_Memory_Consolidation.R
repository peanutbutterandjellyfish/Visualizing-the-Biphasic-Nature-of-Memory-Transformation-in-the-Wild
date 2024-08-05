# About This Code
# This R script is designed to prepare and generate the graphs featured in the 
# main text of the study. The script focuses on creating visualizations for key 
# linguistic variables and their relationship to prediction error and memory 
# consolidation. It employs simple Generalized Additive Models (GAMs) to 
# illustrate these relationships, while more optimal GAMs are contained in a 
# separate file (RobustnessCheck.R).


# Install and Load Packages - R version 4.2.3 (2023-03-15 ucrt)
packages = c("data.table", "geosphere", "tidyverse", "glue", "haven", "ggplot2", "ggpubr", "rstatix", "ggmap")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# The script clears the workspace and sets the working directory to the location of the raw data:
rm(list = ls())

# Set Workspace
setwd("C:/Users/TinnerF/Dropbox/RCode/MemoryEngrams")

# Import Data
df <- data.table::fread("LIWC_Output.csv", encoding = "UTF-8") 
##############################################################################################
# Construct Prediction Error Variable
# A cumulative average rating excluding the current review is calculated to 
# construct the prediction error variable:
# Convert date columns to date type if not automatically recognized
df$reviewDate <- as.Date(df$reviewDate)
df$eingang <- as.Date(df$eingang)

# Sort data by hotel identifier and booking date for accurate cumulative operations
df <- df %>% arrange(hid, eingang)

# Function to calculate cumulative average rating excluding the current review
calculate_cumulative_average <- function(data) {
  data <- data %>% 
    mutate(CumulativeRating = lag(cummean(sonnen)))
  return(data)
}

# Apply the function to each group (by Hotel identifier)
df <- df %>%
  group_by(hid) %>%
  group_modify(~ calculate_cumulative_average(.x)) %>%
  ungroup()

# Compute Prediction Error Variable
df$PredictionError <- df$sonnen - df$CumulativeRating
df$PredictionError <- round(df$PredictionError, 1)
################################################################################
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggpubr)

# Function to create the plots without error bars and customized circle colors
create_plot <- function(data, x, y, y_label, y_lim) {
  ggplot(data, aes_string(x = x, y = y, size = "N")) +
    geom_smooth(color = "black") +
    geom_point(alpha = .1, shape = 21, fill = "white", color = "black", stroke = 1) +  # Circles with black edges and white fill
    scale_size_continuous(range = c(3, 10), guide = 'none') +  # Adjust circle size range and remove legend
    labs(
      x = "Prediction Error",
      y = y_label
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 18),
      plot.title = element_text(size = 18),
      plot.caption = element_text(size = 18),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.text.x = element_text(color = "black", size = 18),
      axis.text.y = element_text(color = "black", size = 18),
      legend.position = "none"  # Remove the legend
    ) +
    ylim(y_lim)
}

# Helper function to prepare data and create plots
prepare_and_plot <- function(df, var, y_label, y_lim) {
  df %>% 
    filter(!is.na(PredictionError)) %>%
    group_by(PredictionError) %>%
    summarize(
      Mean = mean(!!sym(var), na.rm = TRUE),
      SE = ifelse(n() > 1, sd(!!sym(var), na.rm = TRUE) / sqrt(n()), NA_real_),
      CI_lower = ifelse(n() > 1, Mean - qt(0.975, df = n() - 1) * SE, NA_real_),
      CI_upper = ifelse(n() > 1, Mean + qt(0.975, df = n() - 1) * SE, NA_real_),
      N = n(),
      .groups = 'drop'
    ) %>%
    create_plot(x = "PredictionError", y = "Mean", y_label = y_label, y_lim = y_lim)
}

# Creating individual plots
Posemo <- prepare_and_plot(df, "posemo", "Posemo", c(0, 10))
Negemo <- prepare_and_plot(df, "negemo", "Negemo", c(0, 10))
WC <- prepare_and_plot(df, "WC", "Word Count", c(0, 500))
BigWords <- prepare_and_plot(df, "BigWords", "Big Words", c(0, 50))

# Combining plots
combined_plots <- ggarrange(
  Posemo, Negemo, WC, BigWords,
  ncol = 2, nrow = 2, labels = c("A", "B", "C", "D")
)

# Annotate the combined figure with a note at the bottom right
annotated_plots <- annotate_figure(
  combined_plots
)

# Print the final annotated plot arrangement
print(annotated_plots)

##################################### # MEMORY CONSOLIDATION PLOTS #############
#################################################################################
# The script also constructs a DaysPassedReturnVacation variable to measure the time passed since 
# returning from vacation and standardizes key variables for further analysis:

# Construct DaysPassedReturnVacation Variable 
# (i.e., time that has passed between encoding and retrieval)
df$reviewDate <- as.Date(df$reviewDate) # Variable for Review Submission Date
df$reiseEnde <- as.Date(df$reiseEnde)   # Variable for End Date of Vacation
df$DaysPassedReturnVacation <- as.numeric(df$reviewDate-df$reiseEnde)

# CONSTRUCT DATA SET FOR GAM MODELS
# Filter the data and standardize certain variables
data_DaysPassedReturnVacation_cropped <- df %>%
  filter(DaysPassedReturnVacation <= 50) %>%
  mutate(
    WC_z = (WC - mean(WC, na.rm = TRUE)) / sd(WC, na.rm = TRUE),
    BigWords_z = (BigWords - mean(BigWords, na.rm = TRUE)) / sd(BigWords, na.rm = TRUE),
    posemo_z = (posemo - mean(posemo, na.rm = TRUE)) / sd(posemo, na.rm = TRUE),
    negemo_z = (negemo - mean(negemo, na.rm = TRUE)) / sd(negemo, na.rm = TRUE)
  )

########################################################################################################
# Plots are generated to visualize the relationship between latency and various 
# standardized linguistic variables, with a focus on how these variables change over time:
#################################### PLOT WC ###########################################################
# Prepare the BigWords data
BigWords_data <- df %>%
  filter(DaysPassedReturnVacation <= 50) %>%
  mutate(BigWords_z = (BigWords - mean(BigWords, na.rm = TRUE)) / sd(BigWords, na.rm = TRUE)) %>%
  group_by(DaysPassedReturnVacation) %>%
  summarize(
    MeanBigWords_z = mean(BigWords_z, na.rm = TRUE),
    SE = ifelse(n() > 1, sd(BigWords_z, na.rm = TRUE) / sqrt(n()), NA_real_),
    CI_lower = ifelse(n() > 1, MeanBigWords_z - qt(0.975, df = n() - 1) * SE, NA_real_),
    CI_upper = ifelse(n() > 1, MeanBigWords_z + qt(0.975, df = n() - 1) * SE, NA_real_),
    N = n(),
    .groups = 'drop'
  )

# Plotting
BigWords_Plot <- ggplot() +
  geom_point(data = BigWords_data, aes(x = DaysPassedReturnVacation, y = MeanBigWords_z, size = N), alpha = 0.3) +
  geom_errorbar(data = BigWords_data, aes(x = DaysPassedReturnVacation, ymin = CI_lower, ymax = CI_upper), width = 0.2, na.rm = TRUE) +
  geom_smooth(data = data_DaysPassedReturnVacation_cropped, aes(x = DaysPassedReturnVacation, y = BigWords_z),
              method = "gam", formula = y ~ s(x), se = TRUE, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Days Passed Since Return From Vacation",
    y = "BigWords (z-score)"
    ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 16),
    axis.text.y = element_text(color = "black", size = 16),
    legend.position = "right",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  ) +
  guides(size = FALSE)

# Print the plot
print(BigWords_Plot)

################################# NEGEMO #####################################
# Prepare the negemo data
negemo_data <- df %>%
  filter(DaysPassedReturnVacation <= 50) %>%
  mutate(negemo_z = (negemo - mean(negemo, na.rm = TRUE)) / sd(negemo, na.rm = TRUE)) %>%
  group_by(DaysPassedReturnVacation) %>%
  summarize(
    Meannegemo_z = mean(negemo_z, na.rm = TRUE),
    SE = ifelse(n() > 1, sd(negemo_z, na.rm = TRUE) / sqrt(n()), NA_real_),
    CI_lower = ifelse(n() > 1, Meannegemo_z - qt(0.975, df = n() - 1) * SE, NA_real_),
    CI_upper = ifelse(n() > 1, Meannegemo_z + qt(0.975, df = n() - 1) * SE, NA_real_),
    N = n(),
    .groups = 'drop'
  )

# Plotting
negemo_Plot <- ggplot() +
  geom_point(data = negemo_data, aes(x = DaysPassedReturnVacation, y = Meannegemo_z, size = N), alpha = 0.3) +
  geom_errorbar(data = negemo_data, aes(x = DaysPassedReturnVacation, ymin = CI_lower, ymax = CI_upper), width = 0.2, na.rm = TRUE) +
  geom_smooth(data = data_DaysPassedReturnVacation_cropped, aes(x = DaysPassedReturnVacation, y = negemo_z),
              method = "gam", formula = y ~ s(x), se = TRUE, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Days Passed Since Return From Vacation",
    y = "Negemo (z-score)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 16),
    axis.text.y = element_text(color = "black", size = 16),
    legend.position = "right",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  ) +
  guides(size = FALSE)

# Print the plot
print(negemo_Plot)

###############################################################################
################ POSEMO #######################################################
# Prepare the posemo data
posemo_data <- df %>%
  filter(DaysPassedReturnVacation <= 50) %>%
  mutate(posemo_z = (posemo - mean(posemo, na.rm = TRUE)) / sd(posemo, na.rm = TRUE)) %>%
  group_by(DaysPassedReturnVacation) %>%
  summarize(
    Meanposemo_z = mean(posemo_z, na.rm = TRUE),
    SE = ifelse(n() > 1, sd(posemo_z, na.rm = TRUE) / sqrt(n()), NA_real_),
    CI_lower = ifelse(n() > 1, Meanposemo_z - qt(0.975, df = n() - 1) * SE, NA_real_),
    CI_upper = ifelse(n() > 1, Meanposemo_z + qt(0.975, df = n() - 1) * SE, NA_real_),
    N = n(),
    .groups = 'drop'
  )

# Plotting
posemo_Plot <- ggplot() +
  geom_point(data = posemo_data, aes(x = DaysPassedReturnVacation, y = Meanposemo_z, size = N), alpha = 0.3) +
  geom_errorbar(data = posemo_data, aes(x = DaysPassedReturnVacation, ymin = CI_lower, ymax = CI_upper), width = 0.2, na.rm = TRUE) +
  geom_smooth(data = data_DaysPassedReturnVacation_cropped, aes(x = DaysPassedReturnVacation, y = posemo_z),
              method = "gam", formula = y ~ s(x), se = TRUE, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Days Passed Since Return From Vacation",
    y = "Posemo (z-score)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 16),
    axis.text.y = element_text(color = "black", size = 16),
    legend.position = "right",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  ) +
  guides(size = FALSE)

# Print the plot
print(posemo_Plot)
##################################### WC ########################################

# Prepare the WC data
WC_data <- df %>%
  filter(DaysPassedReturnVacation <= 50) %>%
  mutate(WC_z = (WC - mean(WC, na.rm = TRUE)) / sd(WC, na.rm = TRUE)) %>%
  group_by(DaysPassedReturnVacation) %>%
  summarize(
    MeanWC_z = mean(WC_z, na.rm = TRUE),
    SE = ifelse(n() > 1, sd(WC_z, na.rm = TRUE) / sqrt(n()), NA_real_),
    CI_lower = ifelse(n() > 1, MeanWC_z - qt(0.975, df = n() - 1) * SE, NA_real_),
    CI_upper = ifelse(n() > 1, MeanWC_z + qt(0.975, df = n() - 1) * SE, NA_real_),
    N = n(),
    .groups = 'drop'
  )

# Plotting
WC_Plot <- ggplot() +
  geom_point(data = WC_data, aes(x = DaysPassedReturnVacation, y = MeanWC_z, size = N), alpha = 0.3) +
  geom_errorbar(data = WC_data, aes(x = DaysPassedReturnVacation, ymin = CI_lower, ymax = CI_upper), width = 0.2, na.rm = TRUE) +
  geom_smooth(data = data_DaysPassedReturnVacation_cropped, aes(x = DaysPassedReturnVacation, y = WC_z),
              method = "gam", formula = y ~ s(x), se = TRUE, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Days Passed Since Return From Vacation",
    y = "WordCount (z-score)",
    size = "Number of Observations"  # Set legend title for size
  )+
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 16),
    axis.text.y = element_text(color = "black", size = 16),
    legend.position = c(0.8, 0.8),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  )

# Print the plot
print(WC_Plot)

# Finally, the script arranges the plots into a combined figure for a 
# comprehensive visualization of all the variables:
ggarrange(
  posemo_Plot, 
  WC_Plot, 
  negemo_Plot, 
  BigWords_Plot,
  ncol = 2, 
  nrow = 2,
  labels = c("A", "B", "C", "D")
)

####################################################################################################
