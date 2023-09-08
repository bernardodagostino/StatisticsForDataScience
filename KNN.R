library(class)

knn_accuracy<-function (data_list){
  # Function to compute the accuracy of the knn classifier

  # Arguments :
  # data_list : list containing the train and test sets (list)
  # x_train : train set (df)
  # x_test : test set (df)
  # y_train : train labels (df)
  # y_test : test labels (df)
 

  # Returns :
  # accuracy (numeric)

  # Convert the data to matrix
  x_train <- data.matrix(data_list$x_train)
  x_test <- data.matrix(data_list$x_test)
  y_train <- data.matrix(data_list$y_train)
  y_test <- data.matrix(data_list$y_test)
  
  # Compute the number of nn as the square root of the number of observations
  k <- floor((nrow(x_train))^0.5)
  
  # Compute the predictions
  y_pred <- knn(x_train, x_test, y_train, k = k)
  
  # Compute the accuracy
  accuracy <- sum(y_pred == y_test) / length(y_test)
  
  # Return the accuracy
  return(accuracy)
}