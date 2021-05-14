
# clear
rm(list=ls())

# load packages
library(ggplot2)
library(mvtnorm)
library(stats4)
library(ellipse)

# change wd
setwd("/Users/ChristopherSimard/Desktop/Economics/Simulations/em_algorithm/")

# source helper functions
source("gmm_data_gen.R")
source("gmm_run.R")

# parameters
k1 = 2 # number of clusters to generate
k2 = 2 # number of clusters to fit
m = 2 # number of dimensions
n = 50
threshhold = 0.001
max_iter = 100

# generate data
draws = em_data_gen(k1, n, m)
y = draws$y
mus = draws$mus
sigmas = draws$sigmas

# run GMM
results = em_run(y, k2, threshhold, max_iter)
mus_hat = results$mus
sigmas_hat = results$sigmas

# plot GMM data points
gmm_plot = ggplot(data.frame(y), aes(y[,1], y[,2])) + geom_point(mapping = aes(y[,1], y[,2]), size=1) + 
  xlab("") + ylab("") + coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) 

# build gaussians
gaussians = matrix(NA, 100*k2, 3)
for (i in 1:k2) {
  gaussians[(100*(i-1)+1):(100*i),1:2] = ellipse(0, centre=mus_hat[,i], scale=sqrt(diag(sigmas_hat[[i]])))
  gaussians[(100*(i-1)+1):(100*i),3] = i
}
gaussians = data.frame(gaussians)
gmm_plot = print(gmm_plot) + geom_polygon(gaussians, mapping=aes(gaussians[,1], gaussians[,2]), alpha=0.2, color=gaussians[,3]+1, group=gaussians[,3]+1, fill=gaussians[,3]+1)
gmm_plot
