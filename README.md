# RegressionModelPipeline
Pipeline for feature selection, interaction assessment, regression modeling and prediction assessment for 1-1000s of features

# Installation
``` R
install.packages("devtools")
library(devtools)

install_github("LewisLabUCSD/RegressionModelPipeline")
```

# Quickstart

To get started, load the library
``` R
library(MASS)
library(ggplot2)
library(RegressionModelPipeline)
```

Then run the first example
``` R
## Run Main Function
mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',test='LRT',K=5,family = 'gaussian',model=glm)
## Visualize Output
out=vis(mod)
out[[1]] # multivariate model visual
out[[2]] # univariate screening visual
```

Follow the code in model selection which calls code for the univariate screening then decides on regularization vs model selection and interaction vs addative modeling. This is the standard usage of the package.

# Run Modes

## model_selection
All runs start by running the ```model_selection``` main function. Depending on the parameters different elements of the package will be engaged.

## Univariate Pre-screening
All runs undergoe a univariate screening. This is an assessment of the association of each individual observation to predict the response. Depending on the number of observation variable remaining after this screen different multivariate approaches will be utilized.