process_image_dataframe <- function(data, way = "normal") {
  # Function that preprocesses the image dataframe changing the labels 
  #to 0-1 in a random or non random way and removing sad emotions

  # Arguments :
  # image_dataframe : image dataframe (df) each column is a pixel and the last column is the label
  # way : way to preprocess the data (character) "normal" or "random"

  # Returns :
  # image dataframe (df) each column is a pixel and the last column is the label encoded 0-1

  # Create a new copy of the image dataframe
  image_dataframe <- data

  last_column <- names(image_dataframe)[ncol(image_dataframe)]
  
  if (way == "normal") {
  # Iterate over the last column and replace "HA", "NE", "SU" with 1,
  # "AF", "AN", "DI" with 0, and everything else with 2

    for (i in 1:nrow(image_dataframe)) {
      
      if (image_dataframe[i, last_column] %in% c("HA", "NE", "SU")) {
        image_dataframe[i, last_column] <- 1
      }

      else if (image_dataframe[i, last_column] %in% c("AF", "AN", "DI")) {
        image_dataframe[i, last_column] <- 0
      }

      else {
        image_dataframe[i, last_column] <- 2
      }
    }
  }

  if (way == "random") {
  # Create a list of 420 0 and 420 1 labels
  list_of_labels <- c(rep(0, 420), rep(1, 420))

  # Randomly shuffle the list
  list_of_labels <- sample(list_of_labels, replace = FALSE)

  # Iterate over the last column and replace randomly replace labels with 0-1 and SA with 2
  y <- 1
    for (i in 1:nrow(image_dataframe)) {
      if (image_dataframe[i, last_column] %in% c("HA", "NE", "SU", "AF", "AN", "DI")) {
        image_dataframe[i, last_column] <- list_of_labels[[y]]
        y <- y + 1
      } else {
        image_dataframe[i, last_column] <- 2
      }

    }
  }

  # Select only the observations with 0-1 labels
  image_dataframe <- image_dataframe[image_dataframe[, last_column] != 2, ]

  # convert the last column to numeric
  image_dataframe[, last_column] <- as.numeric(image_dataframe[, last_column])
  
  return(image_dataframe)
}


train_test_split <- function(data, y_name = "label", train_ratio = 0.7) {

  # Function to split the data into train and test sets

  # Arguments :
  # data : data frame to split (df)
  # y_name : name of the target variable (character)
  # train_ratio : ratio of the train set (numeric)

  # Returns :
  # list of train and test sets (list of df)

  # Split the data into train and test sets
  split <- createDataPartition(data[[y_name]], p = train_ratio, list = FALSE, )
  train <- data[split, ]
  test <- data[-split, ]

  # Shouffle the train and test sets
  train <- train[sample(nrow(train)), ]
  test <- test[sample(nrow(test)), ]

  # Split the train and test sets into x and y
  x_train <- train[, !names(train) %in% y_name]
  y_train <- train[[y_name]]

  x_test <- test[, !names(test) %in% y_name]
  y_test <- test[[y_name]]
  
  # Return the train and test sets
  return(list(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test))
}


create_dataset_univariate <- function(array1, array2) {
  # Function that given two arrays, creates a dataset containing the two arrays and the labels

  # Arguments :
  # array1 : first array (array)
  # array2 : second array (array)

  # Returns :
  # dataset containing the two arrays and the labels (df)

  # Union of the two arrays
  variables <- c(array1, array2)

  # Create the labels
  label <- c(rep(1, times = length(array1)), rep(0, times = length(array2)))

  # Create the dataset
  dd <- data.frame(x = variables, label = label)

  # Return the dataset
  return(dd)
}


create_norm_data <- function(n = 1000, mean = 0, std = 1) {
  # Function that creates a dataset containing two samples from same normal distribution

  # Arguments :
  # n : number of observations (numeric)
  # mean : mean of the normal distributions (numeric)
  # std : standard deviation of the normal distributions (numeric)

  # Returns :
  # A list containing :
  # norm1 : first normal distribution (array)
  # norm2 : second normal distribution (array)
  # data : dataset containing the two normal distributions and the labels (df)

  # Create the two normal distributions
  normal1 <- rnorm(n, mean = mean, sd = std)
  normal2 <- rnorm(n, mean = mean, sd = std)

  # Create the dataset using the create_dataset1 function and return the list
  return(list(norm1 = normal1, norm2 = normal2, data = create_dataset_univariate(normal1, normal2)))

}


create_t_data <- function(n = 1000, mean = 0, std = 1, df = 3) {
  # Function that creates a dataset containing two sampls from a normal distribution and a t distribution

  # Arguments :
  # n : number of observations (numeric)
  # mean : mean of the normal distribution (numeric)
  # std : standard deviation of the normal distribution (numeric)
  # df : degrees of freedom of the t distribution (numeric)

  # Returns :
  # A list containing :
  # norm : normal distribution (array)
  # t : t distribution (array)
  # data : dataset containing the two distributions and the labels (df)

  # Create the two distributions
  normal1 <- rnorm(n, mean = mean, sd = std)
  t2 <- rt(n, df, ncp = mean)

  #scale both distributions to have zero mean and unit variance
  normal1 <- scale(normal1)
  t2 <- scale(t2)

  # Create the dataset using the create_dataset1 function and return the list
  return(list(norm = normal1, t = t2, data = create_dataset_univariate(normal1, t2)))

}


create_dataset_multivariate <- function(data) {
    # Function that given a dataset, creates a dataset containing the original data
    # and a random permutation of the y variable

    # Arguments :
    # data : dataset (df)

    # Returns :
    # dataset containing the original dataset and a random permutation of the x variable (df)

    # Create a random permutation of the y variable
    random_permutation_y <- sample(data[, 2], replace = FALSE)

    # Create the dataset with the random permutation of the y variable and the original x variable
    independent_data <- cbind(data[, 1], random_permutation_y)

    # Union of the two datasets
    variables <- rbind(independent_data, data)

    # Create the labels
    label <- c(rep(1, times = length(random_permutation_y)), 
              rep(0, times = length(random_permutation_y)))

    # Create the dataset
    dd <- data.frame(x = variables, label = label)

    # Rename the columns
    colnames(dd) <- c("x", "y", "label")

    # Return the dataset
    return(list(dataset = dd, permutated_data = independent_data, original_data = data))   
}


sinusoid <- function(n_observations = 1000, gamma = 0.25, delta = 1){
    # Function that given the number of observations, gamma and delta, creates a sinusoid dataset with noise

    # Arguments :
    # n_observations : number of observations (int)
    # gamma : standard deviation of the noise (float)
    # delta : frequency of the sinusoid (float)

    # Returns :
    # sinusoid dataset (df)

    # Create the x variable
    x <- rnorm(n_observations, mean = 0, sd = 1)

    # Create the noise
    epsilon <- rnorm(n_observations, mean = 0, sd = gamma)

    # Create the y variable adding noise to the sinusoid
    y <- cos(delta * x) + epsilon

    # Create the dataset and return it
    create_dataset_multivariate(cbind(x, y))
}
