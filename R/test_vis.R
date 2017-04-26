##----- Austin
#rm(list=ls())
#setwd("~/GitHub/RegressionModelPipeline/")


#### NOT RUN
if(F){

#install.packages("devtools")
library(devtools)
#install.packages("glinternet")
#install_github("easyGgplot2", "kassambara")
#install_github("bkellman/RegressionModelPipeline")
library(MASS)
library(ggplot2)
library(fastcluster)
library(glmnet)
library(reshape)
library(data.table)
library(easyGgplot2)
#library("RegressionModelPipeline")

source("R/vis.r")
source("R/RegressionModelPipeline.R")
source("R/univariate_screen.r")
source("R/performance_assessment.r")
source("R/cross_validation.r")
source("R/multivariate_model.r")

##-----------------------------------------------------------
#mod=model_selection(df=mtcars,colnames(mtcars)[-1],
#                    response = 'mpg',interactions=F,
#                    test='LRT',K=5,family = 'gaussian',model=glm)
#out=vis(mod)
#plot.new()
#mul <- out$multivar
#mul
#plot.new()
#uni <- out$univar
#uni

##-----------------------------------------------------------
# test example
load(paste0('~/Desktop/2017 UCSD/01_Project/Project02-FDAvirus/04_Regression/VirusRegModels_run2.Rda'))
mod <- PredResult[['mod']]
visResults <- vis_reg(mod,k=4)

#vv <- test(n=35,m=35)
#vv$p  
visResults$p1
ggplot2.multiplot(visResults$p2,visResults$p3, cols=2)
ggplot2.multiplot(visResults$p3,visResults$p4, cols=2)

}
