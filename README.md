# Feature-Sensitivity

Given a pre-trained model, the main function takes in the parameter means and SDs and provides mean and covariances for the aymptotic GP. 
The paramers must be a list containing four lists: weight_mean, weight_sd, bias_mean, bias_sd. Each denoting list of mean or SD matrices in each layer.

That is
weight_mean[[1]] = mean of the weights in W^(1), a matrix of dimension h1 * p and so on. An example of the 'param' input for a 2 hidden layer BNN with 2, 3 hidden nodes respectively is 

params
$weight_mean
$weight_mean[[1]]
           [,1]       [,2]
[1,] -0.3312555  0.7604682
[2,] -0.8464461 -2.4142158

$weight_mean[[2]]
          [,1]      [,2]
[1,] -1.588616 -1.935826
[2,] -2.726052 -1.512920
[3,] -1.281626 -0.442010

$weight_mean[[3]]
          [,1]        [,2]     [,3]
[1,] 0.0220627 -0.04267142 1.098648


$weight_sd
$weight_sd[[1]]
            [,1]        [,2]
[1,] 0.004703292 0.012791371
[2,] 0.005532311 0.005398576

$weight_sd[[2]]
            [,1]        [,2]
[1,] 0.933045626 0.447632492
[2,] 0.807383180 0.776472807
[3,] 0.009334729 0.003292023

$weight_sd[[3]]
           [,1]       [,2]        [,3]
[1,] 0.03165801 0.02364416 0.003298559


$bias_mean
$bias_mean[[1]]
[1] 0.1581854 0.2174778

$bias_mean[[2]]
[1] -1.912820 -1.828276  2.064564

$bias_mean[[3]]
[1] -0.04697182


$bias_sd
$bias_sd[[1]]
[1] 0.001769050 0.003032136

$bias_sd[[2]]
[1] 0.781237543 0.005683927 0.013987370

$bias_sd[[3]]
[1] 0.00535601


