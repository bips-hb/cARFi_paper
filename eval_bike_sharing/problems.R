################################################################################
#                     Real Data Problem: Bike Sharing
#
# In this code, we define a function that loads and preprocesses the Bike
# Sharing dataset.
################################################################################

problem_bike <- function(data, job, factor = 1) {
  library(ISLR2)
  
  data(Bikeshare)
  bike <- data.table(Bikeshare)
  bike[, hr := as.numeric(as.character(hr))]
  bike[, workingday := factor(workingday, levels = c(0, 1), labels = c("No Workingday", "Workingday"))]
  bike[, season := factor(season, levels = 1:4, labels = c("Winter", "Spring", "Summer", "Fall"))]
  
  # Only one observation with this condition, removing it to make space.
  df <- bike[weathersit != "heavy rain/snow", c("hr", "temp", "workingday", "hum", "season", "bikers")]
  
  # Shuffle data
  df <- df[sample(nrow(df)),]
  
  # Reduce data size
  df <- df[1:round(nrow(df) * factor),]
  
  # Set outcome variable
  df <- data.table(y = as.numeric(df$bikers), df[, -"bikers"])
  
  df
}
