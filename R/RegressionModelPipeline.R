### main

#' model_selection
#' 
#' Main function for model selection from a set of many variables
#' @param df, a data.frame containing response and observations variables. Factors with more than 2 levels have only been implimented for test='LRT'
#' @param observations, a character vector of the names of independent/observations variables in df
#' @param response, a character vector of the names of dependent/response variables in df
#' @param family, a character string indicating the family associated with the submitted model c('gaussian','binomial','poisson'...)
#' @param model, a model associated for testing the variables c(glm,lm)
#' @param interactions, a boolean indicating if interactions should be assessed. Default is False.
#' @param test, a character string indicating Likelihood Ratio Test ('LRT') testing likelihood improvement of a model or Wald test ('Wald') testing coefficient > 0
#' @param thresh_screen, a numeric value indicating the p-value cutoff for the univariate screening
#' @param only_return_selected, a boolean value. If true, only models with p-value less than the threshold will be returned. Otherwise, all models will be returned.
#' @param K, a numeric value indicating the number of folds to use for k-fold cross-validation. K=10 by default. K=0 to skip k-fold validation.
#' @return a list containing: univariate models, the final selected model, and crossvalidation stats.
#' @export
#' @examples
#' mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=F,test='LRT',K=5,family = 'gaussian',model=glm)
#' out=vis(mod)
#' print(out[[1]])
#' print(out[[2]])
model_selection <- function(df,observations,response,family='gaussian',model=glm,interactions=FALSE,test=c('Wald','LRT'),thresh_screen=.2,only_return_selected=FALSE,K=10){
  if(length(response)!=1){stop('use multiresponse_model_selection()')}
  if(!test%in%c('Wald','LRT')){stop("test is not in c(Wald,LRT)")}
  #if(!interactions%in%c('signif','none','all')){stop("interactions is not in c(signif,none,all)")}
  
  # Univariate Screen
  observationsL <<- univariate_screen(df,observations,response,family,model,interactions,test,thresh=thresh_screen,only_return_selected=only_return_selected)
  
  # Multivariate Model
  selected_model = stepwise_multivariate_model_selection(df,names(observationsL)[attr(observationsL,'Pr')<thresh_screen],response,family,model,interactions)

  # Model Diagnostics

  # Cross Validation
  if(K>1){
    cv=cross_assess_wrapper(data=df,formula=selected_model$formula,resp=response,family=family,K,model,cv_function=cross_valid_kfold)
  }else{
    cv=NULL
  }
  
  return(list(screen=observationsL,final=selected_model,cv=cv))
}

#######################################
### Run Tests
iris_test<-function(){
  print(str(iris))
}

mtcars_tests <- function(){
  #	source('~/Desktop/modeling_functions.r')
  K=5
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=F,test='LRT',K=5,family = 'gaussian',model=glm)
  out=vis(mod)
  out[[1]]
  out[[2]]
  
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=F,test='LRT',family='poisson',K=K,thresh_screen = .05)

  
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=F,test='Wald',K=K)
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=T,test='LRT',K=K)
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=T,test='LRT',only_return_selected=FALSE,K=K)
  
  mtcars$log_mpg = log(mtcars$mpg)
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'log_mpg',interactions=F,test='LRT',K=K)
}
