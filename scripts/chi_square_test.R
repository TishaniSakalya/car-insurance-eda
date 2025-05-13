# Assuming your data frame is named `your_data`
train_data=read.csv("C:/Users/sakal/Documents/ISMF/Sem 6/ST 3082-Statistical Learning I/EDA project/train_cleaned.csv")
# Define the categorical variables in the dataset
categorical_vars <- c('is_esc','is_adjustable_steering','is_tpms','is_parking_sensors','is_parking_camera',
                      'is_front_fog_lights','is_rear_window_wiper',
                      'is_rear_window_washer','is_rear_window_defogger',
                      'is_brake_assist',	'is_power_door_locks',
                      'is_central_locking',	'is_power_steering','is_driver_seat_height_adjustable',
                      'is_day_night_rear_view_mirror','is_ecw','is_speed_alert','is_claim')  # Add all other categorical columns if necessary

# Convert the relevant variables to factors if they are not already
train_data[categorical_vars] <- lapply(train_data[categorical_vars], factor)

# Initialize an empty list to store the Chi-squared test results
chisq_results <- list()

# Loop through each categorical variable and perform Chi-squared test with `is_claim`
for (var in categorical_vars) {
  
  # Create a contingency table for the variable and is_claim
  contingency_table <- table(train_data[[var]], train_data$is_claim)
  
  # Perform the Chi-squared test
  chisq_test <- chisq.test(contingency_table)
  
  # Store the result in the list with the variable name
  chisq_results[[var]] <- chisq_test
}

# Print out the results
for (result in names(chisq_results)) {
  cat(result, "\n")
  print(chisq_results[[result]])
  cat("\n")
}


# Set significance level (e.g., 0.05)
alpha <- 0.05  

# Get the list of significant variables
significant_vars <- names(chisq_results)[sapply(chisq_results, function(x) x$p.value < alpha)]

# Print significant variables
print(significant_vars)
