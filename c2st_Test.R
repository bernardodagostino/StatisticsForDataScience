source("NN.R")
source("KNN.R")
library(RColorBrewer)

plot_experiment<-function(results_list, parameter_name, parameter_values, error_type, leg=T){
  #function for plotting experiments
  
  #Arguments:
  #results_list: list of values to be plotted on y axis
  #parameter_name: name of the varying parameter, it is the label of the x axis
  #parameter_values: values that the parameter takes, are the values on the x axis
  #error_type: type of the error (1 or 2) of the present experiment, it is the label of the y axis
  #leg: boolean, if true the legend is plotted

  #Returns:
  # Nothing is returned, the plot is shown

  #set the max value of y axis
  if (error_type == 1) {
    max_y = 0.20
  } else if(error_type == 2) {
    max_y = 1
  }
  
  #set the colors for the different tests
  cols <- brewer.pal(length(results_list), 'Set2')

  for (i in 1:length(results_list)) {
    #plot(parameter_values, results_list[[i]], type='l', ylim=c(0,max_y), ylab=paste('Type', error_type, 'error'), xlab=parameter_name, col=cols[i])
    
    #plot equidistant values on x axis
    plot(results_list[[i]], type = 'b', lwd = 2, ylim = c(0,max_y), ylab = paste('Type', error_type, 'error'), 
    xaxt = "n", xlab = parameter_name, col = cols[i])

    # Define x-axis manually
    axis(1, at = 1:length(parameter_values), labels = parameter_values)
    par(new = TRUE)
  }

  # Set the legend
  if (length(results_list) == 6) {
    tests <- c("C2ST KNN", "C2ST NN", "MMD","Wilcoxon", 'KS', "Kuiper")
  } else if (length(results_list) == 3) {
    tests <- c("C2ST KNN", "C2ST NN", "MMD")
  }

  # Plot the legend
  if (leg == T) {
    legend("topright", legend = tests, fill = cols[1:length(results_list)])}
}

preprocess_mmd <- function(data1, data2){
  # function which preprocess the data for the MMD test

  # Arguments:
  # data1: first dataset (df)
  # data2: second dataset (df)

  # Returns:
  # list containing the kernel matrix and the labels
  # kmat: kernel matrix (df)
  # lab: labels (vector numeric)

  # Compute the euclidean distance matrix
  dmat <- as.matrix(dist(rbind(data1, data2)))

  # build a gaussian kernel matrix
  kmat <- exp(-(dmat^2)) 

  # corresponding label
  lab  <- c(rep(1, nrow(data1)), rep(2, nrow(data1)))

  # regularize the kernel matrix 
  epsilon <- 1e-8
  kmat_reg <- kmat + epsilon * diag(nrow(kmat))

  # return the kernel matrix and the labels
  return(list(kmat = kmat_reg, lab = lab))
}


rejectH0 <- function(p, alpha){
  # Function which returns 1 if H0 is rejected, 0 otherwise

  # Arguments :
  # p : p-value of the test (numeric)
  # alpha : significance level of the test (numeric)

  # Returns :
  # 1 if H0 is rejected, 0 otherwise (numeric)

  if (p < alpha) {
    return(1)
  } else {
    return(0)
  }
}


update_error <- function(p, significance_level, error_sum, H0){
  #function which adds one to the count of type 1 error everytime when the true H0 is rejected (if H0 true)
  #function which adds one to the count of type 2 error everytime when the false H0 is not rejected (if H0 false)
  
  #Arguments:
  #p: p value of the test used to reject or not H0
  #significance_level: significance_level of the test
  #error_sum: count of errors to be updated
  #H0: 1 if H0 is true, 0 if false
  
  #Returns:
  #error_sum: the updated sum of errors

  if (H0 == 1) {
    error <- rejectH0(p, significance_level)
  } else if (H0 == 0){
    error <- 1 - rejectH0(p, significance_level)
  }

  error_sum <- error_sum + error
  
  return(error_sum)

}


calc_p_value <- function(t = 0.5, mean = 0.5, std = 1) {
  # Function to compute the p-value of the C2ST test statistic

  # Arguments :
  # t : test statistic (numeric)
  # mean : mean of the normal distribution (numeric)
  # std : standard deviation of the normal distribution (numeric)

  # Returns :
  # p-value of the C2ST test statistic (numeric)

  # Compute the p-value
  p <- (1 - pnorm(t, mean = mean, sd = std))

  return(p)
}


c2st <- function(data_list, classifier, topology = c(20), epochs = 100, lr = 0.01){ 
  
  # Funtion to compute the p-value of the C2ST test statistic

  # Arguments :
  # data_list : list containing the train and test sets (list)
  # x_train : train set (df)
  # x_test : test set (df)
  # y_train : train labels (df)
  # y_test : test labels (df)
  # classifier : classifier to use (character)
  # topology : topology of the neural network (vector of numeric)
  # epochs : number of epochs for the neural network (numeric)

  # Returns :
  # p-value of the C2ST test statistic (numeric)

  #Call the classifier function to compute test statistics
  if (classifier == "knn") {
    t <- knn_accuracy(data_list)
  }
  if (classifier == "nn") {
    t <- nn_accuracy(data_list, topology, epochs, lr)
  }

  # Get the length of the test set
  n_te <- length(data.matrix(data_list$y_test))

  # Return the p-value
  return(calc_p_value(t, mean = 0.5, std = sqrt(1 / (4 * n_te))))

}
