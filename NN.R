library(keras)
library(caret)

nn_accuracy <- function(data_list, topology = c(20), epochs = 100, lr = 0.01) {
  # Function that creates a NN and returns the accuracy on the test data

  # Parameters:
  # data_list: List containing the train and test sets (list)
  # x_train: Training data (df)
  # y_train: Training labels (df)
  # x_test: Test data (df)
  # y_test: Test labels (df)
  # topology: Number of neurons in each layer (vector)
  # epochs: Number of epochs (int)
  # lr: Learning rate (numeric)

  # Returns:
  # accuracy: Accuracy on the test data (numeric)

  # Convert the data to matrix
  x_train <- data.matrix(data_list$x_train)
  y_train <- data.matrix(data_list$y_train)
  x_test <- data.matrix(data_list$x_test)
  y_test <- data.matrix(data_list$y_test)

  # Create the model
  model <- keras_model_sequential()
  
  # Add layers to the model
  for (units in topology) {
    model %>% 
      layer_dense(units = units, activation = "relu")
  }
  
  # Add the output layer
  model %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  # Compile the model with Adam optimizer
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(learning_rate = lr),
    metrics = c("accuracy")
  )

  # Train the model
  model %>% fit(
    x_train, y_train,
    epochs = epochs,
    batch_size = nrow(x_train),
    verbose = 0
  )
  
  # Predict on the test data
  y_pred <- predict(model, x_test, verbose = 0)
  
  # Convert the predictions to binary
  y_pred <- ifelse(y_pred >= 0.5, 1, 0)
  
  # Calculate the accuracy
  accuracy <- sum(y_pred == y_test) / length(y_test)
  
  # Return the accuracy
  return(accuracy)
}