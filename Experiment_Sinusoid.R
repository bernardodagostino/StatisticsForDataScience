setwd("C:\\Users\\berna\\Desktop\\StatisticsForDataScience")
source("Data_Generation.R")
source("c2st_Test.R")
library(rgl)
library(maotai)
library(kernlab)


sinusoid_experiment <- function(significance_level = 0.05, n_rep = 100, kind = "sample", epochs = 100, lr = 0.01){
    # Function to run the experiments on the sinusoid dataset

    # Aruments:
    # significance_level: the significance level of the tests
    # n_rep: the number of repetitions of the experiment
    # kind: the kind of experiment to run, can be "sample", "delta" or "gamma"
    # epochs: the number of epochs for the nn test
    # lr: the learning rate for the nn test

    # Returns:
    # List of results:
    # c2st_knn_results: the error of the c2st knn test
    # c2st_nn_results: the error of the c2st nn test
    # mmd_results: the error of the mmd test

    # in this experiment H0 is False

    # set experiment parameters
    if (kind == "sample"){
        n <- c(25, 50, 100, 500, 1000)
    } else if (kind == "delta"){
        n <- c(2, 4, 6, 8, 10, 20)
    } else if (kind == "gamma"){
        n <- c(0.25, 0.5, 1, 2, 3)
    }

    # inizialise the result list
    c2st_knn_results <- c()
    c2st_nn_results <- c()
    mmd_results <- c()

    # Start the timer
    start_time <- Sys.time()

    # run for loop over n
    for (i in n) {
        print("value")
        print(i)
        print("repetition:")

        # inizialise the errorion counts for each test
        c2st_nn_error <- 0
        c2st_knn_error <- 0
        mmd_error <- 0

        for (repetition in c(1:n_rep)){

            # Create the dataset
            if (kind == "sample") {
                data <- sinusoid(n_observations = i)
            } else if (kind == "gamma") {
                data <- sinusoid(gamma = sqrt(i))
            } else if (kind == "delta") {
                data <- sinusoid(delta = i)
            }

            # Train test split
            split <- train_test_split(data$dataset, "label")

            # Preprocess the data for MMD
            data_mmd <- preprocess_mmd(data[[2]], data[[3]])

            # Run the tests
            print(repetition)
            
            # C2ST KNN
            c2st_knn_error <- update_error(c2st(split, classifier='knn'), significance_level, 
            c2st_knn_error, H0 = 0)

            # C2ST NN
            c2st_nn_error <- update_error(c2st(split, classifier='nn', epochs = epochs, lr = lr), significance_level, 
            c2st_nn_error, H0 = 0)

            # MMD
            mmd_error <- update_error(mmd2test(data_mmd$kmat, data_mmd$lab)$p.value, significance_level,
            mmd_error, H0 = 0)
        }

        # Append the percentage of errors for each test over the n_rep repetitions

        c2st_knn_results <- c(c2st_knn_results, c2st_knn_error / n_rep)
        c2st_nn_results <- c(c2st_nn_results, c2st_nn_error / n_rep)
        mmd_results <- c(mmd_results, mmd_error / n_rep)

    }

    # Print the time elapsed
    print("Time elapsed:")
    print(Sys.time() - start_time)


    # Print the varying parameter
    if (kind == "sample"){
        print("Sample size:")
    } else if (kind == "delta"){
        print("Delta:")
    } else if (kind == "gamma"){
        print("Gamma:")
    }
    print(n)

    # Print the results
    print("C2ST KNN error rate:")
    print(c2st_knn_results)
    print("C2ST NN error rate:")
    print(c2st_nn_results)
    print("MMD error rate:")
    print(mmd_results)

    # Return the results
    return(list(c2st_knn_results = c2st_knn_results, c2st_nn_results = c2st_nn_results, mmd_results = mmd_results))
}


# Run the varying sample experiment
result_sample <- sinusoid_experiment(kind = "sample")

# Run the varying delta experiment
result_delta <- sinusoid_experiment(kind = "delta")

# Run the varying gamma experiment
result_gamma <- sinusoid_experiment(kind = "gamma")

plot_experiment(result_sample, 'sample size',c(25, 50, 100, 500, 1000),2 ) #sample
plot_experiment(result_delta, 'frequency',c(2, 4, 6, 8, 10, 20),2,F ) #delta
plot_experiment(result_gamma, 'noise variance',c(0.25, 0.5, 1, 2, 3),2 ,F) #gamma