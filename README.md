# Feature-Sensitivity

Given a pre-trained model, the main function takes in the parameter means and SDs and provides mean and covariances for the aymptotic GP. 
The paramers must be a list containing four lists: weight_mean, weight_sd, bias_mean, bias_sd. Each denoting list of mean or SD matrices in each layer.

That is
weight_mean[[1]] = mean of the weights in W^(1), a matrix of dimension h1 * p and so on. An example of the 'param' input for a 2 hidden layer BNN with 2, 3 hidden nodes respectively is 

