# This code aims to create additional control variables and run Generalized 
# Additive Models (GAMs) to perform robustness checks on the main effect of 
# 'DaysPassedReturnVacation' on the dependent variables (WC, Posemo, BigWords, Negemo)
# To run the Generalized Additive Models (GAMs) without constructing additional 
# variables, start executing the code from line 130.

# Install and Load Packages - R version 4.2.3 (2023-03-15 ucrt)
packages = c("data.table", "geosphere", "tidyverse", "glue", "haven", "ggplot2", 
             "ggpubr", "rstatix", "ggmap", "mgcv", "caret")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Clear Workspace
rm(list = ls())

# Set Workspace
setwd("C:/Users/TinnerF/Dropbox/RCode/MemoryEngrams")

# Import Data
df <- data.table::fread("LIWC_Output.csv", encoding = "UTF-8") 
################################################################################

# Construct DaysPassedReturnVacation Variable (i.e., time that has passed between encoding and retrieval)
df$reviewDate <- as.Date(df$reviewDate)
df$reiseEnde <- as.Date(df$reiseEnde)
df$DaysPassedReturnVacation <- as.numeric(df$reviewDate-df$reiseEnde)
############################### PredictionError  ###############################
# Construct Prediction Error Variable
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

# Compute Prediction Error
df$PredictionError <- df$sonnen - df$CumulativeRating

df <- df %>%
  mutate(
    reisePreis = as.numeric(as.character(reisePreis)),
    count = as.numeric(as.character(count)),
    reiseDauer = as.numeric(as.character(reiseDauer))
  )

# Check for zero values in both count and reiseDauer before division
df <- df %>%
  mutate(
    DailyPricePerson = ifelse(count == 0 | reiseDauer == 0, 
                              NA,  # Can choose another value like 0 if more appropriate
                              round((reisePreis / count / reiseDauer) / 10) * 10)
  )
######################## JETLAG ################################################
df$TimeZonesCrossed  <- df$longitude - df$laengengrad # longitude = home; laengengrad = hotel
df$TimeZonesCrossed <- round(df$TimeZonesCrossed / 15) # Number of TimeZones Crossed
################################################################################
# Estimate Crowdedness at Hotel During Stay
# Ensure dates are in Date format
df$reiseBeginn <- as.Date(df$reiseBeginn)
df$reiseEnde <- as.Date(df$reiseEnde)

# Create a function to calculate overlapping bookings, taking into account the count of people
calculate_crowdedness_hotel <- function(hotel_id, start_date, end_date) {
  df %>%
    filter(hid == hotel_id) %>%
    filter(!(reiseBeginn > end_date | reiseEnde < start_date)) %>%
    summarise(total_people = sum(count, na.rm = TRUE)) %>%
    pull(total_people)
}

# Apply the function to each row to get the crowdedness estimate at hotel
df$HotelCrowdedness <- mapply(calculate_crowdedness_hotel, df$hid, df$reiseBeginn, df$reiseEnde)
##################################################################################
# Rename Variables in the Data Frame
original_vars <- c("posemo", "negemo", "DaysPassedReturnVacation", 
                   "hotel_avg_temperature", "latitude", 
                   "longitude", "breitengrad", "laengengrad", "PredictionError", 
                   "TMK", "hotel_avg_precipitation",
                   "reiseDauer", "SDK")
new_vars <- c("Posemo", "Negemo", "DaysPassedReturnVacation", 
              "VacationTemperature", "ResidenceLatitude", 
              "ResidenceLongitude", "HotelLatitude", "HotelLongitude", "PredictionError", 
              "RetrievalTemperature", "VacationPrecipitation", 
              "VacationDuration", "RetrievalSunshineDuration")

# Rename the columns in the data frame
setnames(df, old = original_vars, new = new_vars)
################################################################################
# IMPORT AGE 
consumer <- data.table::fread("bookparticipant.csv", encoding = "UTF-8")
names(df)[names(df) == "buID.x"] <- "buID"

# Remove Records Without Correct Age Data
consumer <- consumer %>% filter(kundenalter != 0 & kundenalter >= 18 & kundenalter <= 100)

# Compute average age for each buID
average_age <- consumer %>%
  group_by(buID) %>%
  filter(n() > 1) %>%  # Only consider duplicated buID
  summarise(avg_age = mean(kundenalter, na.rm = TRUE))

# Join the average age back to the original data
consumer <- consumer %>%
  left_join(average_age, by = "buID") %>%
  mutate(kundenalter = ifelse(!is.na(avg_age), avg_age, kundenalter)) %>%
  select(-avg_age)

# Remove duplicates based on buID, keeping the first occurrence
consumer_unique <- consumer %>%
  distinct(buID, .keep_all = TRUE)

consumer_unique$ConsumerAge <- round(consumer_unique$kundenalter)

df_consumer <- merge(df, consumer_unique, by = "buID", all.x = TRUE)

# Check for duplicate column names
duplicate_columns <- names(df_consumer)[duplicated(names(df_consumer))]
if (length(duplicate_columns) > 0) {
  warning("The following column names are duplicated: ", paste(duplicate_columns, collapse = ", "))
  
  # Make the column names unique
  names(df_consumer) <- make.names(names(df_consumer), unique = TRUE)
}

write.csv(df_consumer, "df_consumer.csv")
##################################################################################
df_consumer <- data.table::fread("df_consumer.csv", encoding = "UTF-8") 
##################################################################################
# FIND BEST GAM MODEL - BASED ON TESTS FOR INTERACTION EFFECTS AND SMOOTH VERSUS
# LINEAR-PARAMETRIC TERMS - AND RANDOMIZES STEP-WISE-VARIABLE ENTRY)

# Find Best Generalized Additive Model - Import Function
source("C:/Users/TinnerF/Dropbox/RCode/MemoryEngrams/function_find_best_GAM.R")
best_model_Posemo <- run_gam_model(df_consumer, "Posemo")
best_model_WC <- run_gam_model(df_consumer, "WC")
best_model_Negemo <- run_gam_model(df_consumer, "Negemo")
best_model_BigWords <- run_gam_model(df_consumer, "BigWords")
###############################################################################
# COMPUTE BEST MODEL BASED ON BIGGER DATA SET (ONLY REMOVE NAs For RELEVANT PREDICTORS)
###############################################################################
############################ POSEMO ###########################################
###############################################################################
# The Algorith suggests that our main independent variable (DaysPassedReturnVacation) in addition with the
# following covariates (PredictionError, VacationTemperature) lead to the best model 
# (based on AIC) 
relevant_columns <- c("Posemo", "DaysPassedReturnVacation", "PredictionError", "VacationTemperature")

# Preprocessing: Subset the data to include only relevant columns for NA checking
cleaned_data_cropped <- df_consumer %>%
  select(all_of(relevant_columns)) %>%
  na.omit() %>%
  filter(DaysPassedReturnVacation <= 50)

# Standardize All Continuous Variables
cleaned_data_cropped <- cleaned_data_cropped %>%
  mutate(across(all_of(relevant_columns), ~scale(.)[, 1], .names = "{.col}_z"))

# Fit Best Model With Standardized Variables And More Observations
best_model_Posemo_Max_Obs_Z <- gam(Posemo_z ~ s(DaysPassedReturnVacation_z) + 
                             +s(PredictionError_z) + s(VacationTemperature_z), 
                           data = cleaned_data_cropped)


calculate_importance_scores(best_model_Posemo_Max_Obs_Z)
################################################################################
########################### WC #################################################
################################################################################
relevant_columns <- c("WC", "DaysPassedReturnVacation", "CustomerAge", "PredictionError",
                      "VacationDuration") 

# Preprocessing: Subset the data to include only relevant columns for NA checking
cleaned_data_cropped <- df_consumer %>%
  select(all_of(relevant_columns)) %>%
  na.omit() %>%
  filter(DaysPassedReturnVacation <= 50 & CustomerAge >= 18 & CustomerAge <= 100)

# Standardize All Continuous Variables
cleaned_data_cropped <- cleaned_data_cropped %>%
  mutate(across(all_of(relevant_columns), ~scale(.)[, 1], .names = "{.col}_z"))

# Fit the GAM model with moderators
best_model_WC_Max_Obs_Z <- gam(WC_z ~ s(DaysPassedReturnVacation_z) + 
                           s(CustomerAge_z) + 
                           s(PredictionError_z)+
                           s(VacationDuration_z),
                           data = cleaned_data_cropped)

calculate_importance_scores(best_model_WC_Max_Obs_Z)
################################################################################
##################### BIG WORDS ################################################
################################################################################
relevant_columns <- c("BigWords", "DaysPassedReturnVacation", "TimeZonesCrossed",  "CustomerAge")

# Preprocessing: Subset the data to include only relevant columns for NA checking
cleaned_data_cropped <- df_consumer %>%
  select(all_of(relevant_columns)) %>%
  na.omit() %>%
  filter(DaysPassedReturnVacation <= 50 & CustomerAge >= 18 & CustomerAge <= 100)

# Standardize All Continuous Variables
cleaned_data_cropped <- cleaned_data_cropped %>%
  mutate(across(all_of(relevant_columns), ~scale(.)[, 1], .names = "{.col}_z"))

# Fit the GAM model with moderators
best_model_BigWords_Max_Obs_Z <- gam(BigWords_z ~ s(DaysPassedReturnVacation_z) + s(TimeZonesCrossed_z) +
                                       s(CustomerAge_z),
                           data = cleaned_data_cropped)

calculate_importance_scores(best_model_BigWords_Max_Obs_Z)
################################################################################
########################### NEGEMO #############################################
################################################################################
#################################################################################
relevant_columns <- c("Negemo", "DaysPassedReturnVacation", "VacationTemperature", "PredictionError")

# Preprocessing: Subset the data to include only relevant columns for NA checking
cleaned_data_cropped <- df %>% # Note That Here We Use The df data frame since the best model does not include customer age (contained in df_consumer)
  select(all_of(relevant_columns)) %>%
  na.omit() %>%
  filter(DaysPassedReturnVacation <= 50)

# Standardize All Continuous Variables
cleaned_data_cropped <- cleaned_data_cropped %>%
  mutate(across(all_of(relevant_columns), ~scale(.)[, 1], .names = "{.col}_z"))

# Fit the GAM model with moderators
best_model_Negemo_Max_Obs_Z <- gam(Negemo_z ~ s(DaysPassedReturnVacation_z) + s(VacationTemperature_z) +
                             s(PredictionError_z),
                           data = cleaned_data_cropped)

calculate_importance_scores(best_model_Negemo_Max_Obs_Z)
################################################################################
################################################################################
################################################################################
# PLOT PREDICTORS - CONTROL VARIABLES 
################################################################################

# Custom plot function to adjust axis labels and font sizes
plot_custom <- function(model, term, xlab, ylab, cex.axis = 1.5, cex.lab = 1.5) {
  plot(model, select = term, shade = TRUE, 
       xlab = xlab, ylab = ylab, 
       cex.axis = cex.axis, cex.lab = cex.lab)
}

# Function to save plots for a given model to a PNG file with adjusted parameters
save_gam_plots <- function(model, output_file, ylab_title) {
  png(output_file, width = 800, height = 600)
  par(mfrow = c(2, 3))  # Adjust layout to fit multiple plots
  
  # Plot each term with automatic labels from variable names
  terms <- length(model$smooth)  # Get number of smooth terms
  for (i in 1:terms) {
    term_label <- model$smooth[[i]]$term  # Automatically retrieve the term label
    term_label <- gsub("_z$", "", term_label)  # Remove "_z" from the end of the term label
    plot_custom(model, term = i, 
                xlab = term_label, 
                ylab = ylab_title)
  }
  
  # Close the PNG device to finalize the file
  dev.off()
}
################################################################################
# COMPARE SIMPLE MODEL TO BEST MODEL ########
################################################################################
################################################################################
############################# POSEMO ###########################################
################################################################################

# Define relevant columns
relevant_columns <- c("Posemo", "DaysPassedReturnVacation", "PredictionError", "VacationTemperature")

# Preprocessing: Subset the data to include only relevant columns and filter for NA and specified ranges
cleaned_data_cropped <- df_consumer %>%
  select(all_of(relevant_columns)) %>%
  na.omit() %>%
  filter(DaysPassedReturnVacation <= 50)

# Fit the GAM model with moderators
gam_model_with_mods <- gam(Posemo ~ s(DaysPassedReturnVacation) + s(PredictionError) + 
                             s(VacationTemperature), 
                           data = cleaned_data_cropped)
# PLOT EFFECTS OF CONTROLS
save_gam_plots(gam_model_with_mods, "gam_plot_Posemo.png", "Effect on Posemo")

# Proceed with the filtering
data_simple_model <- df_consumer %>% filter(DaysPassedReturnVacation <= 50)

# Fit the GAM model without moderators
gam_model_no_mods <- gam(Posemo ~ s(DaysPassedReturnVacation), data = data_simple_model)

# Create a new data frame for predictions, holding moderators at their mean values
new_data_with_mods <- with(cleaned_data_cropped, expand.grid(
  DaysPassedReturnVacation = seq(min(DaysPassedReturnVacation), max(DaysPassedReturnVacation), length = 100),
  VacationTemperature = mean(VacationTemperature, na.rm = TRUE),
  PredictionError = mean(PredictionError, na.rm = TRUE)
))

# Predict POSEMO values based on the new data frame for both models, including standard errors
pred_with_mods <- predict(gam_model_with_mods, newdata = new_data_with_mods, se.fit = TRUE)
pred_no_mods <- predict(gam_model_no_mods, newdata = new_data_with_mods, se.fit = TRUE)

new_data_with_mods$posemo_with_mods <- pred_with_mods$fit
new_data_with_mods$posemo_with_mods_lower <- pred_with_mods$fit - 1.96 * pred_with_mods$se.fit
new_data_with_mods$posemo_with_mods_upper <- pred_with_mods$fit + 1.96 * pred_with_mods$se.fit
new_data_with_mods$posemo_no_mods <- pred_no_mods$fit
new_data_with_mods$posemo_no_mods_lower <- pred_no_mods$fit - 1.96 * pred_no_mods$se.fit
new_data_with_mods$posemo_no_mods_upper <- pred_no_mods$fit + 1.96 * pred_no_mods$se.fit

# Plot the relationship between DaysPassedReturnVacation and posemo for both models
Posemo <- ggplot(new_data_with_mods, aes(x = DaysPassedReturnVacation)) +
  geom_line(aes(y = posemo_with_mods, linetype = "Best Model")) +
  geom_ribbon(aes(ymin = posemo_with_mods_lower, ymax = posemo_with_mods_upper, fill = "Best Model"), alpha = 0.2) +
  geom_line(aes(y = posemo_no_mods, linetype = "Simple Model")) +
  geom_ribbon(aes(ymin = posemo_no_mods_lower, ymax = posemo_no_mods_upper, fill = "Simple Model"), alpha = 0.2) +
  labs(title = "", 
       x = "Days Passed Since Return From Vacation",
       y = "Posemo",
       linetype = "", 
       fill = "") +
  theme_minimal() +
  scale_linetype_manual(values = c("Best Model" = "solid", "Simple Model" = "dashed")) +
  scale_fill_manual(values = c("Best Model" = "gray70", "Simple Model" = "gray50")) +
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
    legend.key.size = unit(1, "cm"),  # Increase legend key size
    legend.key.height = unit(1, "cm"),  # Increase legend key height
    legend.key.width = unit(1, "cm"),  # Increase legend key width
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  )


# Display the plot
print(Posemo)
#################################################################################
################################## WC ###########################################
#################################################################################
# Define relevant columns
relevant_columns <- c("WC", "DaysPassedReturnVacation", "VacationDuration", "CustomerAge", "PredictionError")

# Preprocessing: Subset the data to include only relevant columns and filter for NA and specified ranges
cleaned_data_cropped <- df_consumer %>%
  select(all_of(relevant_columns)) %>%
  na.omit() %>%
  filter(DaysPassedReturnVacation <= 50 & CustomerAge >= 18 & CustomerAge <= 100)

# Fit the GAM model with moderators
gam_model_with_mods <- gam(WC ~ s(DaysPassedReturnVacation) + s(VacationDuration) + 
                             s(CustomerAge) + s(PredictionError), 
                           data = cleaned_data_cropped)
# PLOT EFFECTS OF CONTROLS
save_gam_plots(gam_model_with_mods, "gam_plot_WC.png", "Effect on WC")

# Fit the GAM model without moderators
gam_model_no_mods <- gam(WC ~ s(DaysPassedReturnVacation), data = data_simple_model)

# Create a new data frame for predictions, holding moderators at their mean values
new_data_with_mods <- with(cleaned_data_cropped, expand.grid(
  DaysPassedReturnVacation = seq(min(DaysPassedReturnVacation), max(DaysPassedReturnVacation), length = 100),
  VacationDuration = mean(VacationDuration, na.rm = TRUE),
  CustomerAge = mean(CustomerAge, na.rm = TRUE),
  PredictionError = mean(PredictionError, na.rm = TRUE)
))

# Predict WC values based on the new data frame for both models, including standard errors
pred_with_mods <- predict(gam_model_with_mods, newdata = new_data_with_mods, se.fit = TRUE)
pred_no_mods <- predict(gam_model_no_mods, newdata = new_data_with_mods, se.fit = TRUE)

new_data_with_mods$WC_with_mods <- pred_with_mods$fit
new_data_with_mods$WC_with_mods_lower <- pred_with_mods$fit - 1.96 * pred_with_mods$se.fit
new_data_with_mods$WC_with_mods_upper <- pred_with_mods$fit + 1.96 * pred_with_mods$se.fit
new_data_with_mods$WC_no_mods <- pred_no_mods$fit
new_data_with_mods$WC_no_mods_lower <- pred_no_mods$fit - 1.96 * pred_no_mods$se.fit
new_data_with_mods$WC_no_mods_upper <- pred_no_mods$fit + 1.96 * pred_no_mods$se.fit

# Plot the relationship between DaysPassedReturnVacation and WC for both models
WC <- ggplot(new_data_with_mods, aes(x = DaysPassedReturnVacation)) +
  geom_line(aes(y = WC_with_mods, linetype = "Best Model")) +
  geom_ribbon(aes(ymin = WC_with_mods_lower, ymax = WC_with_mods_upper, fill = "Best Model"), alpha = 0.2) +
  geom_line(aes(y = WC_no_mods, linetype = "Simple Model")) +
  geom_ribbon(aes(ymin = WC_no_mods_lower, ymax = WC_no_mods_upper, fill = "Simple Model"), alpha = 0.2) +
  labs(title = "", 
       x = "Days Passed Since Return From Vacation",
       y = "WC",
       linetype = "", 
       fill = "") +
  theme_minimal() +
  scale_linetype_manual(values = c("Best Model" = "solid", "Simple Model" = "dashed")) +
  scale_fill_manual(values = c("Best Model" = "gray70", "Simple Model" = "gray50")) +
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
    legend.key.size = unit(1, "cm"),  # Increase legend key size
    legend.key.height = unit(1, "cm"),  # Increase legend key height
    legend.key.width = unit(1, "cm"),  # Increase legend key width
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  )


# Display the plot
print(WC)

################################################################################

# Additoinal Check For Influence of Age
# Compute mean DaysPassedReturnVacation grouped by CustomerAge
df_consumer %>%
  group_by(CustomerAge) %>%
  summarise(
    mean_DaysPassedReturnVacation = mean(DaysPassedReturnVacation, na.rm = TRUE),
    N = n()) %>% 
ggplot() +
  geom_point(aes(x = CustomerAge, y = mean_DaysPassedReturnVacation))+
  labs(
       x = "Customer Age",
       y = "Mean Days Passed Since Return From Vacation") +
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
    legend.key.size = unit(1, "cm"),  # Increase legend key size
    legend.key.height = unit(1, "cm"),  # Increase legend key height
    legend.key.width = unit(1, "cm"),  # Increase legend key width
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  )


################################################################################
################################## BigWords ####################################
################################################################################
# Define relevant columns
relevant_columns <- c("BigWords", "DaysPassedReturnVacation", "CustomerAge", "TimeZonesCrossed")

# Preprocessing: Subset the data to include only relevant columns and filter for NA and specified ranges
cleaned_data_cropped <- df_consumer %>%
  select(all_of(relevant_columns)) %>%
  na.omit() %>%
  filter(DaysPassedReturnVacation <= 50 & CustomerAge >= 18 & CustomerAge <= 100)

# Fit the GAM model with moderators
gam_model_with_mods <- gam(BigWords ~ s(DaysPassedReturnVacation) + s(TimeZonesCrossed) + s(CustomerAge),
                           data = cleaned_data_cropped)

# Fit the GAM model without moderators
gam_model_no_mods <- gam(BigWords ~ s(DaysPassedReturnVacation), data = data_simple_model)

# PLOT EFFECTS OF CONTROLS
save_gam_plots(gam_model_with_mods, "gam_plot_BigWords.png", "Effect on BigWords")

# Create a new data frame for predictions, holding moderators at their mean values
new_data_with_mods <- with(cleaned_data_cropped, expand.grid(
  DaysPassedReturnVacation = seq(min(DaysPassedReturnVacation), max(DaysPassedReturnVacation), length = 100),
  CustomerAge = mean(CustomerAge, na.rm = TRUE),
  TimeZonesCrossed = mean(TimeZonesCrossed, na.rm = TRUE)
))

# Predict BigWords values based on the new data frame for both models, including standard errors
pred_with_mods <- predict(gam_model_with_mods, newdata = new_data_with_mods, se.fit = TRUE)
pred_no_mods <- predict(gam_model_no_mods, newdata = new_data_with_mods, se.fit = TRUE)

new_data_with_mods$BigWords_with_mods <- pred_with_mods$fit
new_data_with_mods$BigWords_with_mods_lower <- pred_with_mods$fit - 1.96 * pred_with_mods$se.fit
new_data_with_mods$BigWords_with_mods_upper <- pred_with_mods$fit + 1.96 * pred_with_mods$se.fit

new_data_with_mods$BigWords_no_mods <- pred_no_mods$fit
new_data_with_mods$BigWords_no_mods_lower <- pred_no_mods$fit - 1.96 * pred_no_mods$se.fit
new_data_with_mods$BigWords_no_mods_upper <- pred_no_mods$fit + 1.96 * pred_no_mods$se.fit

# Plot the relationship between DaysPassedReturnVacation and BigWords for both models
BigWords <- ggplot(new_data_with_mods, aes(x = DaysPassedReturnVacation)) +
  geom_line(aes(y = BigWords_with_mods, linetype = "Best Model")) +
  geom_ribbon(aes(ymin = BigWords_with_mods_lower, ymax = BigWords_with_mods_upper, fill = "Best Model"), alpha = 0.2) +
  geom_line(aes(y = BigWords_no_mods, linetype = "Simple Model")) +
  geom_ribbon(aes(ymin = BigWords_no_mods_lower, ymax = BigWords_no_mods_upper, fill = "Simple Model"), alpha = 0.2) +
  labs(title = "", 
       x = "Days Passed Since Return From Vacation",
       y = "BigWords",
       linetype = "", 
       fill = "") +
  theme_minimal() +
  scale_linetype_manual(values = c("Best Model" = "solid", "Simple Model" = "dashed")) +
  scale_fill_manual(values = c("Best Model" = "gray70", "Simple Model" = "gray50")) +
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
    legend.key.size = unit(1, "cm"),  # Increase legend key size
    legend.key.height = unit(1, "cm"),  # Increase legend key height
    legend.key.width = unit(1, "cm"),  # Increase legend key width
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  )

print(BigWords)

################################################################################
#################################################################################
################################## Negemo ###########################################
#################################################################################
# Define relevant columns
relevant_columns <- c("Negemo", "DaysPassedReturnVacation", "VacationTemperature", "PredictionError")

# Preprocessing: Subset the data to include only relevant columns and filter for NA and specified ranges
cleaned_data_cropped <- df %>% 
  select(all_of(relevant_columns)) %>%
  na.omit() %>%
  filter(DaysPassedReturnVacation <= 50)

# Fit the GAM model with moderators
gam_model_with_mods <- gam(Negemo ~ s(DaysPassedReturnVacation) + s(VacationTemperature) + s(PredictionError),
                           data = cleaned_data_cropped)

# PLOT EFFECTS OF CONTROLS
save_gam_plots(gam_model_with_mods, "gam_plot_NEGEMO.png", "Effect on Negemo")

# Fit the GAM model without moderators
gam_model_no_mods <- gam(Negemo ~ s(DaysPassedReturnVacation), data = data_simple_model)

# Create a new data frame for predictions, holding moderators at their mean values
new_data_with_mods <- with(cleaned_data_cropped, expand.grid(
  DaysPassedReturnVacation = seq(min(DaysPassedReturnVacation), max(DaysPassedReturnVacation), length = 100),
  VacationTemperature = mean(VacationTemperature, na.rm = TRUE),
  PredictionError = mean(PredictionError, na.rm = TRUE)
))

# Predict Negemo values based on the new data frame for both models, including standard errors
pred_with_mods <- predict(gam_model_with_mods, newdata = new_data_with_mods, se.fit = TRUE)
pred_no_mods <- predict(gam_model_no_mods, newdata = new_data_with_mods, se.fit = TRUE)

new_data_with_mods$Negemo_with_mods <- pred_with_mods$fit
new_data_with_mods$Negemo_with_mods_lower <- pred_with_mods$fit - 1.96 * pred_with_mods$se.fit
new_data_with_mods$Negemo_with_mods_upper <- pred_with_mods$fit + 1.96 * pred_with_mods$se.fit

new_data_with_mods$Negemo_no_mods <- pred_no_mods$fit
new_data_with_mods$Negemo_no_mods_lower <- pred_no_mods$fit - 1.96 * pred_no_mods$se.fit
new_data_with_mods$Negemo_no_mods_upper <- pred_no_mods$fit + 1.96 * pred_no_mods$se.fit

# Plot the relationship between DaysPassedReturnVacation and Negemo for both models
Negemo <- ggplot(new_data_with_mods, aes(x = DaysPassedReturnVacation)) +
  geom_line(aes(y = Negemo_with_mods, linetype = "Best Model")) +
  geom_ribbon(aes(ymin = Negemo_with_mods_lower, ymax = Negemo_with_mods_upper, fill = "Best Model"), alpha = 0.2) +
  geom_line(aes(y = Negemo_no_mods, linetype = "Simple Model")) +
  geom_ribbon(aes(ymin = Negemo_no_mods_lower, ymax = Negemo_no_mods_upper, fill = "Simple Model"), alpha = 0.2) +
  labs(title = "", 
       x = "Days Passed Since Return From Vacation",
       y = "Negemo",
       linetype = "", 
       fill = "") +
  theme_minimal() +
  scale_linetype_manual(values = c("Best Model" = "solid", "Simple Model" = "dashed")) +
  scale_fill_manual(values = c("Best Model" = "gray70", "Simple Model" = "gray50")) +
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
    legend.key.size = unit(1, "cm"),  # Increase legend key size
    legend.key.height = unit(1, "cm"),  # Increase legend key height
    legend.key.width = unit(1, "cm"),  # Increase legend key width
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  )

print(Negemo)
################################################################################
ggarrange(
Posemo, WC,
  ncol = 2, nrow = 1,common.legend = TRUE
)


