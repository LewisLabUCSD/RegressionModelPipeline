# TODO: IMPLIMENT CONTEXT VARIABLE


#' univariate_screen
#' 
#' Run univariate models on all observations variables with respect to response. Asseses p-value of model given LRT or Wald test. Returns all models or just models more significant than the given threshold: thresh.
#' @param df, a data.frame containing response and observations variables. Factors with more than 2 levels have only been implimented for test='LRT'
#' @param observations, a character vector of the names of independent/observations variables in df
#' @param response, a character vector of the names of dependent/response variables in df
#' @param family, a character string indicating the family associated with the submitted model c('gaussian','binomial','poisson'...)
#' @param model, a model associated for testing the variables c(glm,lm)
#' @param interactions, a boolean indicating if interactions should be assessed. Default is False.
#' @param test, a character string indicating Likelihood Ratio Test ('LRT') testing likelihood improvement of a model or Wald test ('Wald') testing coefficient > 0
#' @param thresh, a numeric value indicating the p-value cutoff for the univariate screening
#' @param only_return_selected, a boolean value. If true, only models with p-value less than the threshold will be returned. Otherwise, all models will be returned.
#' @param context_var, a character vector of variable(s) necessary for contextual assessment (e.g. age or time)
#' @return a list of univariate models, attr(,'Pr') houses the significance of each model
#' @export
univariate_screen <- function(df,observations,response,family='gaussian',model=glm,interactions=FALSE,test='LRT',thresh=.2,only_return_selected=FALSE,context_var=NULL){
  # checks
  if(any(!c(observations,response)%in%colnames(df))){stop(paste(c(observations,response)[!c(observations,response)%in%colnames(df)],'not found in df'))}
  if(!is.null(context_var)){warning('context_var is not yet implimented')}
  if( any( unlist(lapply(df,function(x) ifelse(is.factor(x),length(levels(x))>2,TRUE) ) ) ) ){
    if(test!='LRT'){ stop( "Factors with more than 2 levels have only been implimented for test='LRT'" ) }
  }
  # run univariate models
  mL <- list()
  Pr <- c()
  for(obs in observations){
    print(obs)
    mod1 = NULL
    try( mod1 <- model(as.formula(paste(response,'~',obs)),family=family,data=df) )
    if(!is.null(mod1)){
      Pr <- get_Pr(Pr,test,mod1)
      mL[[obs]] <- mod1
    }
  }
#  if(interactions!='none'){
#    if(interactions=='signif'){
#      observations_sel = observations[Pr<thresh]
#    }else if(interactions=='all'){
#      observations_sel = observations
#    }else{stop('interactions is not in c(none,signif,all)')}
#    inter = univariate_screen_interactions(df,observations_sel,response,family,model,test)
#    Pr = c(Pr,attr(inter,'Pr'))
#    mL = c(mL,inter)
#  }
  if(only_return_selected){
    mL = mL[Pr<thresh]
    attr(mL,'Pr') = Pr[Pr<thresh]
  }else{
    attr(mL,'Pr') = Pr
  }
  attr(mL,'test') = test
  mL
}

get_Pr <- function(Pr,test,mod1,mod0=NULL){
  if(is.null(mod0)){
    if(tolower(test)=='wald'){
      warning('LRT is generally preferred')
      Pr = c(Pr,coef(summary(mod1))[2,4])
    }else if(tolower(test)=='lrt'){
      Pr = c(Pr,anova(mod1,test='LRT')[2,5])			
    }else{stop('test is not in c(Wald,LRT)')}
  }else{
    if(tolower(test)=='wald'){
      warning('LRT is generally preferred')
      Pr = c(Pr,coef(summary(mod1))[4,4])
    }else if(tolower(test)=='lrt'){
      Pr = c(Pr,anova(mod0,mod1,test='LRT')[2,5])			
    }
  }
  return(Pr)
}