
gmm_data_gen = function(k, n, m) {
  # DESCRIPTION: 
  # randomly generates data for GMM algorithm
  
  # ARGUMENTS:
  # k: true number of clusters
  # n: draws per cluster
  # m: dimension of data
  
  # mean/variance ranges
  mus = replicate(k, runif(m, -8, 8))
  if (m == 1) {mus = t(mus)}
  sigmas = vector(mode = "list", length=k)
  for (i in 1:k) {
    sigmas[[i]] = diag(diag(matrix(runif(m, 2, 5), m, m)))
  }
  
  # compute draws
  draws = matrix(0, n*k, m)
  for (i in 0:(k-1)) {
    draws_i = rmvnorm(n, mus[,i+1], sigmas[[i+1]])
    draws[(n*i+1):(n*(i+1)),] = draws_i
  }
  
  # collect results
  results = list("mus"=mus, "sigmas"=sigmas, "y"=draws)
  return(results)
  
}
