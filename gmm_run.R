
gmm_run = function(y, k, threshhold, max_iter) {
  # DESCRIPTION: 
  # runs GMM alrogithm on simulated data
  
  # ARGUMENTS:
  # y: data (nxm*k), consecutive columns refer to x, y components of draws
  # k: number of clusters
  # threshhold: convergence threshhold
  # max_iter: maximum number of iterations allowed
  
  # initialise EM
  n = dim(y)[1]/k
  m = dim(y)[2]
  mus = replicate(k, runif(m, -8, 8))
  if (m == 1) {mus = t(mus)}
  sigmas = vector(mode = "list", length=k)
  for (i in 1:k) {
    sigmas[[i]] = diag(diag(matrix(runif(m, 2, 5), m, m)))
  }
  converged = 0
  ll = matrix(0, max_iter, 1)
  iter = 0
  ps = matrix(1, k)/k
  
  # repeat until convergence
  while (converged == 0 & iter < max_iter) {
    
    # E-step
    iter = iter + 1
    lhood_x_prior = matrix(0, n*k, k)
    for (i in 1:k) {
      lhood_x_prior[,i] = dmvnorm(y, mus[,i], sigmas[[i]])*ps[i]
    }
    a_post = lhood_x_prior/rowSums(lhood_x_prior)
    
    # M-step
    for (i in 1:k) {
      mus[,i] = t(a_post[,i]%*%y/sum(a_post[,i]))
      sigmas_i = matrix(0, m, m)
      for (j in 1:n*k) {
        sigmas_i = sigmas_i + a_post[j,i]*(y[j,] - mus[,i])%*%t(y[j,] - mus[,i])
      }
      sigmas[[i]] = sigmas_i/sum(a_post[,i])
      ps[i] = sum(a_post[,i])/(n*k)
    }
    
    # compute log-likelihood
    ll_iter = -1*sum(log(rowSums(lhood_x_prior)))
    ll[iter] = ll_iter
    
    # check for convergence
    if (iter > 1) {
      ll_diff = ll[iter-1] - ll[iter]
      if (ll_diff < threshhold) {
        converged = 1
      }
    }
    
  }
  
  # trim likelihood
  ll = ll[ll!=0]
  
  # return results
  results = list("iterations"=iter, "likelihood"=ll, "mus"=mus, "sigmas"=sigmas, "ps"=ps)
  return(results)
  
}
