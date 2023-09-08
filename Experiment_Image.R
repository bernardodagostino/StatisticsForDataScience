setwd("C:\\Users\\berna\\Desktop\\StatisticsForDataScience")
source("Data_Generation.R")
source("c2st_Test.R")

image_test <- function(data_frame, way = "normal", rep = 500, significance_level = 0.01, lr = 0.001) {
  # Preprocess data
  processed_df <- process_image_dataframe(data_frame, way = way)
  
  # Set the number of errors
  error <- 0
  
  # Start the timer
  start_time <- Sys.time()
  
  if (way == "normal") {
    print("Normal Test")
  } else{
    print("Random Test")
  }
  
  # Perform the test over different train-test sets
  print("Repetition")
  for (i in 1:rep) {
    print(i)
    
    if (way == "normal") {
      # In this test H0 is false
      
      # Split the data into train and test sets
      split <- train_test_split(processed_df)
      
      # Perform the test and update the error variable
      error <- update_error(c2st(split, classifier = 'nn', lr = lr), significance_level, error, H0 = 0)
    } else if (way == "random") {
      # In this test H0 is true
      
      # Create random data
      random_df <- process_image_dataframe(data_frame, way = "random")
      
      # Split the data into train and test sets
      split <- train_test_split(random_df)
      
      # Perform the test and update the error variable
      error <- update_error(c2st(split, classifier = 'nn', lr = lr), significance_level, error, H0 = 1)
    }
  }
  
  # Print the time elapsed
  print("Time elapsed:")
  print(Sys.time() - start_time)
  
  # Print the percentage of errors
  if (way == "normal") {
    print("Percentage of errors when H0 is false:")
  } else if (way == "random") {
    print("Percentage of errors when H0 is true:")
  }
  
  print(error / rep)

  return(error / rep)
}

# Load data
image_dataframe <- read.csv("DATA/image_dataframe.csv", header = TRUE, sep = ",")

# Perform the test on the normal data
err_norm <- image_test(image_dataframe, way = "normal")

# Perform the test on the random data
err_rand <- image_test(image_dataframe, way = "random")