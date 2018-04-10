#' stepwise_multivariate_model_selection
#' 
#' Multivariate model selection on all (significant) observations variables with respect to response. 
#' @param df, a data.frame containing response and observations variables. Factors with more than 2 levels have only been implimented for test='LRT'
#' @param observations, a character vector of the names of independent/observations variables in df
#' @param response, a character vector of the names of dependent/response variables in df
#' @param family, a character string indicating the family associated with the submitted model c('gaussian','binomial','poisson'...)
#' @param model, a model associated for testing the variables c(glm,lm)
#' @param interactions, a boolean indicating if interactions should be assessed. Default is False.
#' @return a multivariate model
#' @export 
#' 
#' @importFrom MASS stepAIC
stepwise_multivariate_model_selection <- function(df,observations,response,family,model,interactions=FALSE,aic_k=2,...){
  observations = na.omit(observations)
  if(length(observations)<=2){stop('There are fewer than 2 observations presented to the final model. The screening threshold is likely too severe')}
  f<-as.formula(paste(response,' ~ ',paste(observations,collapse='+'),sep=''))
  #	dbg <<- list(df,observations,response,f)
  df_tmp <- na.omit(df[,c(observations,response)])
  mod <- model(f,family=family,data=df_tmp ,...)
  if(interactions){
    f_inter<-as.formula(paste(' ~ ',paste(observations,collapse='*'),sep=''))
    step <- stepAIC(mod, direction="both",scope=list(upper=f_inter,lower=~1),k=aic_k)
  }else{
    step <- stepAIC(mod, direction="backward",k=aic_k)
  }
  return(step)
}