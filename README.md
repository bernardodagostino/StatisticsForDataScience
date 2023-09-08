# StatisticsForDataScience

Project for the exam of Statistics for Data Science. The project consists of replicating the experiments of "Revisiting classifier two-sample tests" by LOPEZ-PAZ, David; OQUAB, Maxime (2016)

'c2st_Test': 
* 'c2st': perform classifier two sample test, returning a p value (p value computed with 'calc_p_value'), the classifiers employed are defined in 'KNN' and 'NN'
* 'reject_h0': given a p value, return 1 if H0 is rejected
* other useful functions for plotting experiment results, preprocess data for mmd test and calculate error in experiments

'Data_Generation': generate data for simulations
* 'create_norm_data': creates a dataset with 2 samples from same normal distribution
* 'create_t_data': creates a dataset with a sample from a normal distribution and one from a t distribution
* 'sinusoid': create sinuoid dataset with noise, y~sin(x)+noise
* other useful functions: 'create_dataset_multivariate','create_dataset_univariate' ('create_dataset_multivariate'),'train_test_split','preprocess_image_dataframe'

Experiments
* 'Experiment_Simulation':
  * 'normal_repeated_experiment': run two sample tests (c2st and benchmarks) on 100 replications of 'create_norm_data' simulation, repeate for different sample size
  * 't_normal_repeated_experiment': run two sample tests (c2st and benchmarks) on 100 replications of 'create_t_data' simulation, repeate for different sample size and different degree of freedom of t
* 'Experiment_Sinusoid': run two sample tests (c2st and benchmarks) on 100 replications of 'sinuoid' simulation, repeate for different sample size, sinusoid frequency and different variance of error
* 'Experiment_NIPS': perform two sample test with C2ST-NN for samples of nips articles:
  * from same cluster(Bayes-Bayes)
  * from different clusters (e.g. Bayes-Deep) 
* Experiment_Image': perform two sample test with C2ST-NN for samples of facial expression image (preprocessed in 'Image_Preprocess'):
  * from same class (positive-positive, negative-negative)
  * from different classes (positive-negative) 
