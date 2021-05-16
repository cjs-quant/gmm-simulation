
# clear
rm(list=ls())

# load packages
library(ggplot2)
library(mvtnorm)
library(stats4)
library(ellipse)

# source helper functions
source("gmm_data_gen.R")
source("gmm_run.R")
source("build_gaussians.R")

# create ui
ui = pageWithSidebar(
  
  # app title
  headerPanel("GMM Simulation"),
  
  # sidebar panel for inpute
  sidebarPanel(withMathJax(),
               sliderInput("k1", "Gaussians to Generate:", min=1, max=5, step=1, value=2),
               sliderInput("k2", "Gaussians to Estimate:", min=1, max=5, step=1, value=2),
               sliderInput("n", "Observations per Gaussian:", min=25, max=250, value=100),
               sliderInput("threshhold", "Likelihood-Threshhold:", min = 0.00001, max = 0.1, value=0.01)
  ),
  
  # main panel for outputs
  mainPanel(plotOutput("gmm_plot"), uiOutput("note"))
  
)

# server logic to plot variables
server = function(session, input, output) {
  
  # draw data
  draws = reactive({gmm_data_gen(input$k1, input$n, 2)})
  y = reactive({draws()$y})
  mus = reactive({draws()$mus})
  sigmas = reactive({draws()$sigmas})
  
  # run GMM
  results = reactive({gmm_run(y(), input$k2, input$threshhold, 100)})
  mus_hat = reactive({results()$mus})
  sigmas_hat = reactive({results()$sigmas})
  
  # build gaussians
  gaussians = reactive({build_gaussians(mus_hat(), sigmas_hat(), input$k2)})
  
  # plot GMM
  output$gmm_plot = renderPlot({
    ggplot(data.frame(y()), aes(y()[,1], y()[,2])) + geom_point(mapping=aes(y()[,1], y()[,2]), size=1) + 
      xlab("") + ylab("") + coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) + 
      geom_polygon(gaussians(), mapping=aes(gaussians()[,1], gaussians()[,2]), alpha=0.2, color=gaussians()[,3]+1, group=gaussians()[,3]+1, fill=gaussians()[,3]+1)
  })
  
  # links
  chris_email = a("christopher.simard@ny.frb.org,", href="christopher.simard@ny.frb.org")
  github = a("here.", href="https://github.com/csimard-econ/gmm-simulation")
  
  # create note
  output$note = renderUI({
    tagList("This simulation demonstrates the Gaussian Mixture Model (GMM) in two dimensions.
            The code itself works in n dimensions, but the simulation here is presented in 2
            dimensions for exposition. Simulation author: Christopher Simard, contact:", chris_email,
            ". The source code for this simulation can be found", github, 
            "The views expressed here are our own and do not necessarily 
            represent the views of the Federal Reserve Bank of New York or the Federal Reserve System.",
            tags$br(), tags$br(), "Notes: [to do]")
  })
  
}

# creates shinyapp object
shinyApp(ui, server)

