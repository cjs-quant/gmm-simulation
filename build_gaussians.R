
build_gaussians = function(mus, sigmas, k) {
  # DESCRIPTION: 
  # build gaussians for plotting in GMM simulation
  
  # ARGUMENTS:
  # mus: gaussian means (mxk)
  # sigmas: list of variance matrices (mxmxk)
  # k: number of gaussians
  
  # build gaussians
  gaussians = matrix(NA, 100*k, 3)
  for (i in 1:k) {
    gaussians[(100*(i-1)+1):(100*i),1:2] = ellipse(0, centre=mus[,i], scale=sqrt(diag(sigmas[[i]])))
    gaussians[(100*(i-1)+1):(100*i),3] = i
  }
  
  gaussians = data.frame(gaussians)
  return(gaussians)
  
}
