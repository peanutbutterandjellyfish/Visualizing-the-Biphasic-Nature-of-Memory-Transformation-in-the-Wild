library(dplyr)
library(tidyr)
library(mgcv)
library(data.table)
library(ggplot2)

# Define the function: FIND BEST MODEL
run_gam_model <- function(df_consumer, Posemo) {
  
  # Define the relevant columns
  relevant_columns <- c(Posemo, "DaysPassedReturnVacation", "CustomerAge", "VacationTemperature", 
                        "PredictionError", "RetrievalTemperature", "TimeZonesCrossed",
                        "VacationPrecipitation", "VacationDuration", 
                        "RetrievalSunshineDuration")
  
  # Preprocessing: Subset the data to include only relevant columns for NA checking
  cleaned_data_cropped <- df_consumer %>%
    select(all_of(relevant_columns)) %>%
    na.omit() %>%
    filter(DaysPassedReturnVacation <= 30 & CustomerAge >= 18 & CustomerAge <= 100)
  
  # Standardize All Continuous Variables
  cleaned_data_cropped <- cleaned_data_cropped %>%
    mutate(across(all_of(relevant_columns), ~scale(.)[, 1], .names = "{.col}_z"))
  
  # Define the dataset and relevant columns
  relevant_columns_z <- c(paste0(Posemo, "_z"), "DaysPassedReturnVacation_z", "CustomerAge_z", "VacationTemperature_z",
                          "PredictionError_z", "RetrievalTemperature_z", "TimeZonesCrossed_z",
                          "VacationPrecipitation_z", "VacationDuration_z", 
                          "RetrievalSunshineDuration_z")
  
  cleaned_data_cropped <- as.data.table(cleaned_data_cropped)
  data <- cleaned_data_cropped[, ..relevant_columns_z]
  
  # Define the dependent variable and initial independent variable
  dependent_var <- paste0(Posemo, "_z")
  main_independent_var <- "DaysPassedReturnVacation_z"
  
  # Fit the initial GAM model with only the main independent variable
  initial_formula <- formula(paste(dependent_var, "~ s(", main_independent_var, ")"))
  best_model <- gam(initial_formula, data = data)
  best_aic <- AIC(best_model)
  best_formula <- initial_formula
  
  # List of additional variables to consider
  additional_vars <- setdiff(relevant_columns_z, c(dependent_var, main_independent_var))
  
  # Function to add a variable as smooth or parametric and return the new model
  add_term_to_model <- function(base_formula, new_term, data, use_smooth) {
    tryCatch({
      if (use_smooth) {
        new_formula <- formula(paste(deparse(base_formula), "+ s(", new_term, ")"))
      } else {
        new_formula <- formula(paste(deparse(base_formula), "+", new_term))
      }
      new_model <- gam(new_formula, data = data)
      return(list(model = new_model, formula = new_formula))
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # Function to perform step-wise selection with smooth and parametric options, including interactions
  stepwise_selection <- function(dependent_var, main_independent_var, additional_vars, data) {
    initial_formula <- formula(paste(dependent_var, "~ s(", main_independent_var, ")"))
    best_model <- gam(initial_formula, data = data)
    best_aic <- AIC(best_model)
    best_formula <- initial_formula
    
    remaining_vars <- additional_vars
    improvement <- TRUE
    
    while (improvement && length(remaining_vars) > 0) {
      improvement <- FALSE
      for (var in remaining_vars) {
        # Try adding the variable as a smooth term
        smooth_model <- add_term_to_model(best_formula, var, data, TRUE)
        if (!is.null(smooth_model)) {
          smooth_aic <- AIC(smooth_model$model)
        } else {
          smooth_aic <- Inf
        }
        
        # Try adding the variable as a parametric term
        parametric_model <- add_term_to_model(best_formula, var, data, FALSE)
        if (!is.null(parametric_model)) {
          parametric_aic <- AIC(parametric_model$model)
        } else {
          parametric_aic <- Inf
        }
        
        if (smooth_aic < best_aic || parametric_aic < best_aic) {
          if (smooth_aic < parametric_aic) {
            best_model <- smooth_model$model
            best_aic <- smooth_aic
            best_formula <- smooth_model$formula
          } else {
            best_model <- parametric_model$model
            best_aic <- parametric_aic
            best_formula <- parametric_model$formula
          }
          improvement <- TRUE
          remaining_vars <- setdiff(remaining_vars, var)
          break
        }
        
        # Try adding interactions with the main independent variable
        interaction_term <- paste(main_independent_var, "*", var)
        smooth_interaction_model <- add_term_to_model(best_formula, interaction_term, data, TRUE)
        if (!is.null(smooth_interaction_model)) {
          smooth_interaction_aic <- AIC(smooth_interaction_model$model)
        } else {
          smooth_interaction_aic <- Inf
        }
        
        parametric_interaction_model <- add_term_to_model(best_formula, interaction_term, data, FALSE)
        if (!is.null(parametric_interaction_model)) {
          parametric_interaction_aic <- AIC(parametric_interaction_model$model)
        } else {
          parametric_interaction_aic <- Inf
        }
        
        if (smooth_interaction_aic < best_aic || parametric_interaction_aic < best_aic) {
          if (smooth_interaction_aic < parametric_interaction_aic) {
            best_model <- smooth_interaction_model$model
            best_aic <- smooth_interaction_aic
            best_formula <- smooth_interaction_model$formula
          } else {
            best_model <- parametric_interaction_model$model
            best_aic <- parametric_interaction_aic
            best_formula <- parametric_interaction_model$formula
          }
          improvement <- TRUE
          remaining_vars <- setdiff(remaining_vars, var)
          break
        }
      }
    }
    
    return(list(best_model = best_model, best_formula = best_formula, best_aic = best_aic))
  }
  
  # Run step-wise selection for multiple orders
  set.seed(123)  # For reproducibility
  orders <- replicate(10, sample(additional_vars), simplify = FALSE)
  results <- lapply(orders, function(order) stepwise_selection(dependent_var, main_independent_var, order, data))
  
  # Find the best model among all runs
  best_result <- results[[which.min(sapply(results, function(res) res$best_aic))]]
  
  # Print the best model and its AIC value
  print("The best model includes the following variables:")
  print(deparse(best_result$best_formula))
  print(summary(best_result$best_model))
  print(paste("AIC of the best model:", best_result$best_aic))
  
  return(best_result$best_model)
}

# Example usage:
# best_model <- run_gam_model(df_consumer, "Posemo")

# Continue analysis with the best model
run_full_analysis <- function(df_consumer) {
  # Run the GAM model selection and get the best model
  best_model <- run_gam_model(df_consumer, "Posemo")
  
  # Calculate Variable Importance Scores
  gam_summary <- summary(best_model)
  f_values <- gam_summary$s.table[, "F"]
  importance_scores <- f_values / sum(f_values)
  importance_df <- data.frame(Variable = names(f_values), Importance = importance_scores)
  print(importance_df)
  
  # Visualize Effects of Moderators
  plot_custom <- function(model, term, xlab, ylab, cex.axis = 1.5, cex.lab = 1.5) {
    plot(model, select = term, shade = TRUE, 
         xlab = xlab, ylab = ylab, 
         cex.axis = cex.axis, cex.lab = cex.lab)
  }
  
  x_labels <- c(
    "Days Passed Since Return From Vacation",
    "Latitude of Hotel",
    "Customer Age",
    "Temperature During Retrieval Day At Home",
    "Prediction Error"
  )
  
  png("gam_plot_POSEMO.png", width = 800, height = 600)
  par(mfrow = c(2, 3))  # Adjust layout to fit multiple plots
  
  terms <- length(best_model$smooth)
  for (i in 1:terms) {
    plot_custom(best_model, term = i, 
                xlab = x_labels[i], 
                ylab = "Effect on Posemo")
  }
  
  plot.new()
  text(0.5, 1, "Importance Scores", cex = 1.5, font = 2)
  
  for (i in 1:nrow(importance_df)) {
    variable_name <- importance_df$Variable[i]
    text(0.5, 1 - i*0.2, 
         paste("Variable:", variable_name, "Importance:", round(importance_df$Importance[i], 3)), 
         cex = 1.2, adj = 0)
  }
  
  mtext("All variables are standardized", side = 1, line = 4, adj = 1, cex = 1.2)
  dev.off()
  
  # Compare Models
  gam_model_with_mods <- gam(Posemo_z ~ s(DaysPassedReturnVacation_z) + s(HotelLongitude_z) + s(CustomerAge_z) + 
                               s(VacationTemperature_z) + s(PredictionError_z), data = data)
  gam_model_no_mods <- gam(Posemo_z ~ s(DaysPassedReturnVacation_z), data = data)
  
  new_data_with_mods <- with(data, expand.grid(
    DaysPassedReturnVacation_z = seq(min(DaysPassedReturnVacation_z), max(DaysPassedReturnVacation_z), length = 100),
    HotelLongitude_z = mean(HotelLongitude_z, na.rm = TRUE),
    VacationTemperature_z = mean(VacationTemperature_z, na.rm = TRUE),
    PredictionError_z = mean(PredictionError_z, na.rm = TRUE),
    CustomerAge_z = mean(CustomerAge_z, na.rm = TRUE)
  ))
  
  new_data_with_mods$posemo_with_mods <- predict(gam_model_with_mods, newdata = new_data_with_mods)
  new_data_with_mods$posemo_no_mods <- predict(gam_model_no_mods, newdata = new_data_with_mods)
  
  Posemo <- ggplot(new_data_with_mods, aes(x = DaysPassedReturnVacation_z)) +
    geom_line(aes(y = posemo_with_mods, color = "With Moderators")) +
    geom_line(aes(y = posemo_no_mods, color = "Without Moderators")) +
    labs(title = "Moderators: Hotel Longitude, Customer Age, Average Temperature At Hotel During Vacation, Prediction Error", 
         x = "Days Passed Since Return From Vacation",
         y = "Posemo",
         color = "Model") +
    theme_minimal() +
    scale_color_manual(values = c("With Moderators" = "blue", "Without Moderators" = "red"))
  print(Posemo)
}

# Example usage:
# run_full_analysis(df_consumer)

calculate_importance_scores <- function(gam_model) {
  # Check if the input is a GAM model
  if (!inherits(gam_model, "gam")) {
    stop("The input model is not a GAM model.")
  }
  
  # Extract the summary of the GAM model
  gam_summary <- summary(gam_model)
  
  # Initialize a variable to store the deviance explained by each predictor
  deviance_explained <- NULL
  
  # Check different possible structures for the summary object
  if ("s.table" %in% names(gam_summary)) {
    # Extract the deviance explained by each predictor
    deviance_explained <- gam_summary$s.table[, "edf"]
  } else if ("p.table" %in% names(gam_summary)) {
    deviance_explained <- gam_summary$p.table[, "Pr(>|t|)"]
  } else if ("chi.sq" %in% names(gam_summary)) {
    deviance_explained <- gam_summary$chi.sq
  }
  
  # Check if deviance_explained is still NULL
  if (is.null(deviance_explained)) {
    stop("The structure of the GAM model summary is not supported.")
  }
  
  # Calculate the relative importance of each predictor
  relative_importance <- deviance_explained / sum(deviance_explained, na.rm = TRUE)
  
  # Create a data frame with the results
  importance_scores <- data.frame(
    Predictor = rownames(gam_summary$s.table),
    Importance = relative_importance
  )
  
  # Sort the data frame by importance in descending order
  importance_scores <- importance_scores[order(-importance_scores$Importance), ]
  
  return(importance_scores)
}
