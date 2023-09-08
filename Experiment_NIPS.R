setwd("C:\\Users\\berna\\Desktop\\StatisticsForDataScience")
source("Data_Generation.R")
source("c2st_Test.R")

nips_test <- function(data_path = "DATA/bayes_bayes.csv", significance_level = 0.01, rep = 500, H0 = 1) {
  # Function to perform the test on the NIPS data

  # Arguments:
  # data_path: path to the data (character)
  # significance_level: significance level (numeric)
  # rep: number of repetitions (numeric)
  # H0: value of the null hypothesis (numeric) 0 false 1 true

  # Returns:
  # percentage of errors (numeric)

  # Load data
  data <- read.csv(data_path, header = TRUE, row.names = 1, sep = ",")
  
  # Initialize the number of errors
  n_error <- 0
  print(c("repetition", data_path))
  # Perform the test over different train-test sets
  for (i in 1:rep) {
  print(i)
  
  # Split the data into train and test sets with seed i
  split <- train_test_split(data)
  
  # Perform the test and update the error variable
  n_error <- update_error(c2st(split, classifier = 'nn'), significance_level, n_error, H0 = H0)                                                                                                         
  }

  print(c("Percentage of errors", data_path))
  print(n_error / rep)
  
  # Return the percentage of errors
  return(n_error / rep)
  

}

# Perform the test on bayes_bayes.csv
bb_res <- nips_test(data_path = "DATA/bayes_bayes.csv", H0 = 1)

# Perform the test on bayes_deep.csv
bd_res <- nips_test(data_path = "DATA/bayes_deep.csv", H0 = 0)

# Perform the test on bayes_learning.csv
bl_res <- nips_test(data_path = "DATA/bayes_learning.csv", H0 = 0)

# Perform the test on bayes_neuro.csv
bn_res <- nips_test(data_path = "DATA/bayes_neuro.csv", H0 = 0)

# Perform the test on deep_neuro.csv
dn_res <- nips_test(data_path = "DATA/deep_neuro.csv", H0 = 0)

# Perform the test on neuro_learning.csv
nl_res <- nips_test(data_path = "DATA/neuro_learning.csv", H0 = 0)


