# Analisis de Datos
# Master Finctech UC3m
#
# Author: Tomas de la Rosa

# Load the initial libraries
library(ggplot2)
library(reshape2)
library(caTools)
library(lattice)
library(e1071)
library(MASS)
library(klaR)
library(nnet)
library(caret)
library(plotrix)

# Load the R scripts needed for the course
sourceFiles <- c("yahoodatatools.R","financialFuns.R","features.R")
sapply(sourceFiles,source,.GlobalEnv)

