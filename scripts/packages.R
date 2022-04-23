
##############################################################################
############ AMDP Thesis - Deborah Ho, University of Michigan 2022 ###########
##############################################################################

# EXPLANATION ####
# RUN THIS SCRIPT FIRST!
# running this script will load in all packages required for all the scripts in
# this project to run
# with the exception of the KRSP package, which only needs to be run to
# re-import the tables into this project for any reason

## PACKAGES USED GLOBALLY ####
library(dplyr) #for data manipulation
library(tidyverse) #for data cleaning
library(lubridate) #for handling dates
library(car)

## PACKAGES USED SPECIFICALLY IN ANALYSIS.R ####
library(lmerTest) #for linear mixed models
library(QuantPsyc)
library(performance)
library(sjlabelled)

## PACKAGES USED SPECIFICALLY IN PLOTS.R ####
library(paletteer) #for colors
library(ggplot2) #for figures and graphs
library(ggprism)
library(ggResidpanel)
library(sjPlot)
library(sjmisc)
library(lattice)
library(gridExtra) #for putting plots onto a grid
library(ggpubr) #for putting p-values onto plots
