
# clear
rm(list=ls())

# load packages
library(ggplot2)
library(mvtnorm)
library(stats4)
library(ellipse)

# change wd
setwd("/Users/ChristopherSimard/Desktop/Economics/Simulations/gmm_simulation/")

# source helper functions
source("gmm_data_gen.R")
source("gmm_run.R")
source("build_gaussians.R")

# parameters
k1 = 2 # number of gaussian to generate
k2 = 2 # number of gaussian to estimate
m = 2 # number of dimensions
n = 50 # observations per gaussian
threshhold = 0.01
max_iter = 100

# generate data
draws = gmm_data_gen(k1, n, m)
y = draws$y
mus = draws$mus
sigmas = draws$sigmas

# run GMM
results = gmm_run(y, k2, threshhold, max_iter)
mus_hat = results$mus
sigmas_hat = results$sigmas

# build gaussians
gaussians = build_gaussians(mus_hat, sigmas_hat, k2)

# plot GMM
gmm_plot = ggplot(data.frame(y), aes(y[,1], y[,2])) + geom_point(mapping=aes(y[,1], y[,2]), size=1) + 
  xlab("") + ylab("") + coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) + 
  geom_polygon(gaussians, mapping=aes(gaussians[,1], gaussians[,2]), alpha=0.2, color=gaussians[,3]+1, group=gaussians[,3]+1, fill=gaussians[,3]+1)
gmm_plot
