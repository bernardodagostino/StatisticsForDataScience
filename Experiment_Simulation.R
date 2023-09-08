setwd("C:\\Users\\berna\\Desktop\\StatisticsForDataScience")
source('c2st_Test.R')
source('Data_Generation.R')
library(rgl)
library(maotai)
library(kuiper.2samp)

normal_repeated_experiment <- function(significance_level = 0.05, n_rep = 100, y_name = 'label'){
  # Function to run the experiments on the normal dataset

  # Aruments:
  # significance_level: the significance level of the tests
  # n_rep: the number of repetitions of the experiment
  # y_name: the name of the label column

  # Returns:
  # List of results:
  # c2st_knn_results: the error of the c2st knn test
  # c2st_nn_results: the error of the c2st nn test
  # wilcox_results: the error of the wilcox test
  # ks_results: the error of the ks test
  # kuiper_results: the error of the kuiper test
  # mmd_results: the error of the mmd test

  #in this experiment H0 is true
  
  # Initialize the result list
  c2st_knn_results <- c()
  c2st_nn_results <- c()
  wilcox_results <- c()
  ks_results <- c()
  kuiper_results <- c()
  mmd_results <- c()

  # Run the experiment for different number of observations
  for (n_observations in c(25, 50, 100, 500, 1000)){
    # Initialize the error counts for each test
    c2st_knn_error <- 0
    c2st_nn_error <- 0
    wilcox_error <- 0
    ks_error <- 0
    kuiper_error <- 0
    mmd_error <- 0

    # Start the timer
    start_time <- Sys.time()

    # Run the experiment for different repetitions
    print("value")
    print(n_observations)
    print("Repetition:")

    for (repetition in c(1:n_rep)) {
      print(repetition)
      # Create the dataset
      norm_data <- create_norm_data(n = n_observations)
      dataset <- norm_data$data
      split <- train_test_split(dataset, y_name)
      
      norm1 <- norm_data$norm1
      norm2 <- norm_data$norm2
      
      data_mmd <- preprocess_mmd(as.matrix(norm1), as.matrix(norm2))
      
      #perform tests and decide if reject null hypothesis or not
      c2st_knn_error <- update_error(c2st(split, classifier = 'knn'), significance_level, c2st_knn_error, 1)
      
      c2st_nn_error <- update_error(c2st(split, classifier = 'nn'), significance_level, c2st_nn_error, 1)
      
      wilcox_error <- update_error(wilcox.test(norm1, norm2, conf.level = 1 - significance_level)$p.value, significance_level, wilcox_error, 1)
      
      ks_error <- update_error(ks.test(norm1, norm2)$p.value, significance_level, ks_error, 1)
      
      kuiper_error <- update_error(kuiper.2samp(norm1, norm2)$p.value, significance_level, kuiper_error, 1)
      
      mmd_error <- update_error(mmd2test(data_mmd$kmat, data_mmd$lab)$p.value, significance_level, mmd_error, 1) 
    }

    # Update the results list
    c2st_knn_results <- c(c2st_knn_results, c2st_knn_error / n_rep)
    c2st_nn_results <- c(c2st_nn_results, c2st_nn_error / n_rep)
    wilcox_results <- c(wilcox_results, wilcox_error / n_rep)
    ks_results <- c(ks_results, ks_error / n_rep)
    kuiper_results <- c(kuiper_results, kuiper_error / n_rep)
    mmd_results <- c(mmd_results, mmd_error / n_rep)

  }

  # End the timer
  print(Sys.time() - start_time)

  # Print the varying parameter
  print("Sample size:")
  print(c(25, 50, 100, 500, 1000))

  # Print the results
  print("c2st_knn_results:")
  print(c2st_knn_results)
  print("c2st_nn_results:")
  print(c2st_nn_results)
  print("wilcox_results:")
  print(wilcox_results)
  print("ks_results:")
  print(ks_results)
  print("kuiper_results:")
  print(kuiper_results)
  print("mmd_results:")
  print(mmd_results)

  # Return the results list
  return(list(c2st_knn = c2st_knn_results, c2st_nn = c2st_nn_results, mmd = mmd_results, 
  wilcox = wilcox_results, ks = ks_results, kuiper = kuiper_results))
}


t_normal_repeated_experiment <- function(varying_param, significance_level = 0.05, n_rep = 100, y_name = 'label'){
  # Function to run the experiments on two samples one from normal distribution and the other from t distribution

  # Aruments:
  # varying_param: the parameter that is varied in the experiment
  # significance_level: the significance level of the tests
  # n_rep: the number of repetitions of the experiment
  # y_name: the name of the label column

  # Returns:
  # List of results:
  # c2st_knn_results: the error of the c2st knn test
  # c2st_nn_results: the error of the c2st nn test
  # wilcox_results: the error of the wilcox test
  # ks_results: the error of the ks test
  # kuiper_results: the error of the kuiper test
  # mmd_results: the error of the mmd test
  
  #In this experiment H0 is true
  
  # Initialize the result list
  c2st_knn_results <- c()
  c2st_nn_results <- c()
  wilcox_results <- c()
  ks_results <- c()
  kuiper_results <- c()
  mmd_results <- c()

  # Run the experiment for different number of observations
  if(varying_param == 'n') {
    value_list <- c(50, 100, 500, 1000)} 
  else if(varying_param == 'df') {
    value_list <- c(2,3,5,10,15,20)
  }
  
  # Start timer 
  start_time <- Sys.time()

  # Run the experiment for different parameter values
  for (value in value_list){
    print("Value:")
    print(value)
    print("Repetition:")
    
    # Initialize the error counts for each test
    c2st_knn_error <- 0
    c2st_nn_error <- 0
    wilcox_error <- 0
    ks_error <- 0
    kuiper_error <- 0
    mmd_error <- 0

    # Run the experiment for different repetitions
    for (repetition in c(1:n_rep)) {
      print(repetition)

      # Create the dataset
      if(varying_param == 'n'){
        #n varyes, while df is equal to the default value fixed in data generation
        t_data <- create_t_data(n = value) } 
      else if(varying_param == 'df') {
        t_data <- create_t_data(df = value)
      }
      
      dataset <- t_data$data
      split <- train_test_split(dataset, y_name)
      
      norm <- t_data$norm
      t <- t_data$t
      
      data_mmd <- preprocess_mmd(norm, t)
      
      #Perform tests and decide if reject null hypothesis or not
      c2st_knn_error <- update_error(c2st(split, classifier='knn'), significance_level, c2st_knn_error, 0)
      
      c2st_nn_error <- update_error(c2st(split, classifier = 'nn'), significance_level, c2st_nn_error, 0)
      
      wilcox_error <- update_error(wilcox.test(norm, t, conf.level = 1 - significance_level)$p.value, significance_level, wilcox_error, 0)
      
      ks_error <- update_error(ks.test(norm, t)$p.value, significance_level, ks_error, 0)
      
      kuiper_error <- update_error(kuiper.2samp(norm, t)$p.value, significance_level, kuiper_error, 0)
      
      mmd_error <- update_error(mmd2test(data_mmd$kmat, data_mmd$lab)$p.value, significance_level, mmd_error, 0)
      
    }

    # Update the results list
    c2st_knn_results <- c(c2st_knn_results, c2st_knn_error/n_rep)
    c2st_nn_results <- c(c2st_nn_results, c2st_nn_error/n_rep)
    wilcox_results <- c(wilcox_results, wilcox_error/n_rep)
    ks_results <- c(ks_results, ks_error/n_rep)
    kuiper_results <- c(kuiper_results, kuiper_error/n_rep)
    mmd_results <- c(mmd_results, mmd_error/n_rep)
  }

  # End the timer
  print(Sys.time() - start_time)

  # Print the results
  if (varying_param == 'n'){
    print("n:")
  } else if (varying_param == 'df'){
    print("df:")
  }
  print(value_list)

  print("c2st_nn_results:")
  print(c2st_nn_results)
  print("c2st_knn_results:")
  print(c2st_knn_results)
  print("wilcox_results:")
  print(wilcox_results)
  print("ks_results:")
  print(ks_results)
  print("kuiper_results:")
  print(kuiper_results)
  print("mmd_results:")
  print(mmd_results)

  # Return the results list
  return(list(c2st_knn = c2st_knn_results, c2st_nn = c2st_nn_results, mmd = mmd_results, 
  wilcox = wilcox_results, ks = ks_results, kuiper = kuiper_results))
}


# Run the experiment for the normal distribution
n_list <- normal_repeated_experiment()
plot_experiment(n_list, 'sample size', c(25, 50, 100, 500, 1000), 1)

# Run the experiment for the normal and t distribution
t_n_list <- t_normal_repeated_experiment('n')
t_df_list <- t_normal_repeated_experiment('df')

plot_experiment(t_n_list, 'sample size',c(25,50, 100, 500, 1000),2, T)
plot_experiment(t_df_list, 'degree of freedom',c(2,3,5,10,15,20),2 ,F)