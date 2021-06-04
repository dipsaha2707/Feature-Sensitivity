#########################     Helper function   ####################################

mult = function (m,s, l = 100){
  
  m_ = as.vector(m)
  s_  = as.vector (s)
  
  W = (lapply( 1 : length(m_), function (j) rnorm (l, m_[j], s_[j]  )) )
  W = do.call('rbind', W)
  W = lapply(1 : ncol(W), function (j) matrix(W[,j], ncol = ncol (m)) )
  
  return (W)
}
relu = function (x) x * (x > 0)
mse = function(x,y) mean((x-y)^2)
est = function (x, W, b, L ){
  
  J_all = vector(mode = "list", length = L)
  
  A = lapply(1:l, function (j) W[[1]][[j]] %*% x + b[[1]][[j]] )
  del_A = lapply(1:l, function (j) diag(as.vector(A[[j]] >= 0)*1))
  
  J_all[[1]] = del_A
  A_next = A 
  
  if (L > 1){
    for ( k in 2:L){
      
      A_next = lapply(1:l, function (j) W[[k]][[j]] %*% relu(A_next[[j]]) + b[[k]][[j]] )
      del_A_next = lapply(1:l, function (j) diag(as.vector(A_next[[j]] >= 0)*1))
      J_all[[k]] = del_A_next
      
    }
    
  }
  
  f =   W[[1]]
  
  for ( k  in 1:L){
    
    f_ = lapply(1:l, function (j) W[[k+1]][[j]]  %*% (J_all[[k]][[j]]) )
    f = lapply(1:l, function (j) f_[[j]] %*% f[[j]] )
    
  }
  
  return (f)
}
pad_zero_bef = function(x, L) c(rep(0, L - length(x)), x )
gp_draw <- function(draws, mu, Sigma, ...) {
  mvtnorm::rmvnorm(draws, mu, Sigma)
}

#############################################################


setwd("/Users/diptarka/Work/BNN_Research/BNNLiu/example") 
params <- readRDS("params.rds") #Get the parameters from a trained network
X = readRDS("train_X.rds") #Training data 




############   Main Function (using MC) : input -> paramers, training data 
############   output ->  asymptotic mean and covariance matrix of 
############              the feature sensitivies at the provided points ##########

sens = function(params, X, l = 100){
  
  num = 20
  sam = sample(nrow(X), num, replace = F)
  X_ = matrix(X[sam,], nrow = num) # Sample of the data, change sample size 
  
  
  M = params$weight_mean
  S = params$weight_sd
  Mb = params$bias_mean
  Mb = lapply(1:length(Mb), function (j) matrix(Mb[[j]], ncol = 1 ))
  Sb = params$bias_sd
  Sb = lapply(1:length(Sb), function (j) matrix(Sb[[j]], ncol = 1 ))
  
  L = length(M) - 1
  
  W = lapply(1:length(M), function(j) mult(M[[j]], S[[j]], l))
  b = lapply(1:length(Mb), function(j) mult(Mb[[j]], Sb[[j]], l ))
  
  #### means
  Li = lapply(1:nrow(X_), function (i) {
    
    x = matrix(X_[i,], ncol = 1)
    f = est(x, W,b, L)
    return(Reduce("+", f) / length(f))
    
  })
  
  means =  do.call('rbind', Li)
  
  #### Covar
  
  Li = mclapply(1:nrow(X_), function (i) {
    
    do.call('cbind', mclapply(i:nrow(X_), function (i2){
      
      x = matrix(X_[i,], ncol = 1)
      y = matrix(X_[i2,], ncol = 1)
      
      E_D_x = matrix(means[i,], ncol = 1)
      E_D_y = matrix(means[i2,], ncol = 1)
      
      f_x = est(x, W, b, L)
      f_y = est(y, W, b, L)
      
      f = mclapply(1:l, function (j) {( t(f_x[[j]]) %*% 
                                          (f_y[[j]])   
      ) %>%  as.matrix()}, mc.cores = 6 )
      
      Sigma =  diag((Reduce("+", f) / length(f)) - E_D_x %*% t(E_D_y))
      
      return(Sigma)
      
    }, mc.cores = 6)
    
    )
  }, mc.cores = 6
  )
  
  U = lapply (1:p, function (i){
    UV = lapply(Li, `[`,i,)
    UV = lapply(1: length(UV), function (i) pad_zero_bef(UV[[i]], length(UV[[1]]))  )
    return(do.call('rbind', UV))
  })
  covar = lapply(1: length(U), function (i)U[[i]] + t(U[[i]]) - diag(diag(U[[i]])) )
  
  return (list("means" = means, "covar" = covar))
  
}

resp = sens(params, X)
Sigma = (resp$covar)
M = resp$means

########  plotting the GP for any particular feature #########

i = 1

y <- gp_draw(1000, M[,i], Sigma[[i]])
U = cbind.data.frame("x" = X_[,i],"mean" = t(y) %>%  rowMeans(), 
                     "sd" = t(y) %>%  rowSds(), "D_x" = M[,i]) [order(X_[,i]),]

tp =cbind.data.frame( U[,1], U[,4], U[,2] + 5* U[,3], U[,2] - 5* U[,3])
names(tp) = c("x", "mean", "upper", "lower")
g<- ggplot(tp, aes(x))

p1 = g +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
  geom_line(aes(y = mean, colour = "blue")) + theme_classic() +guides(colour = "none")
p1





