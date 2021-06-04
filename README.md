# Feature-Sensitivity

Given a pre-trained model, the main function takes in the parameter means and SDs and provides mean and covariances for the aymptotic GP. 
The paramers must be a list containing four lists: weight_mean, weight_sd, bias_mean, bias_sd. Each denoting list of mean or SD matrices in each layer.

That is
weight_mean[[1]] = mean of the weights in W^(1), a matrix of dimension h1 * p and so on 
