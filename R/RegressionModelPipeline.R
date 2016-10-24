### main

#' model_selection
#' 
#' Main function for model selection from a set of many variables
#' @param df, a data.frame containing response and observations variables. Factors with more than 2 levels have only been implimented for test='LRT'
#' @param observations, a character vector of the names of independent/observations variables in df
#' @param response, a character vector of the names of dependent/response variables in df
#' @param family, a character string indicating the family associated with the submitted model c('gaussian','binomial','poisson'...)
#' @param model, a model associated for testing the variables c(glm,lm)
#' @param interactions, a boolean indicating if interactions should be assessed. Default is NULL, by default, interactions will be examined according to the constraints set by sig_vars_thresh. If a value is set for interactions (T/F) this will override the recomendations of sig_vars_thresh
#' @param test, a character string indicating Likelihood Ratio Test ('LRT') testing likelihood improvement of a model or Wald test ('Wald') testing coefficient > 0
#' @param thresh_screen, a numeric value indicating the p-value cutoff for the univariate screening
#' @param only_return_selected, a boolean value. If true, only models with p-value less than the threshold will be returned. Otherwise, all models will be returned.
#' @param K, a numeric value indicating the number of folds to use for k-fold cross-validation. K=10 by default. K=0 to skip k-fold validation.
#' @param sig_vars_thresh a list specifying the maximal number of significant variables allowed for each final model generating method. NULL (self initializing) by default.
#' @param robust boolean indicating if regularization will be run multiple times to get a robust indication of the underlying structure
#' @param N, a numeric value, default N=1, indicating the number of cross validation iterations to perform
#' @return a list containing: univariate models, the final selected model, and crossvalidation stats.
#' @export
#' @examples
#' mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=F,test='LRT',K=5,family = 'gaussian',model=glm)
#' out=vis(mod)
#' print(out[[1]])
#' print(out[[2]])
#' @import glmnet
#' @import glinternet
model_selection <- function(df,observations,response,family='gaussian',model=glm,interactions=FALSE,test=c('Wald','LRT'),thresh_screen=.2,only_return_selected=FALSE,K=10,sig_vars_thresh=NULL,robust=FALSE,N=1){
  if(length(response)!=1){stop('use multiresponse_model_selection()')}
  if(!test%in%c('Wald','LRT')){stop("test is not in c(Wald,LRT)")}
  #if(!interactions%in%c('signif','none','all')){stop("interactions is not in c(signif,none,all)")}
  if(is.null(sig_vars_thresh)){
    sig_vars_thresh = list(model_sel_interaction=6,model_sel_additive=25,glmnet_interaction=200,glmnet_additive=2000)
  }
  
  # Univariate Screen
  observationsL <<- univariate_screen(df,observations,response,family,model,interactions,test,thresh=thresh_screen,only_return_selected=only_return_selected)
  
  # Multivariate Model
  obs_sign = na.omit( names(observationsL)[attr(observationsL,'Pr')<thresh_screen] )
  if( length( obs_sign ) == 0 ){ stop("There are no observations that pass the threshold set by thresh_screen. Consider increasing thresh_screen.")}
  # set interaction based on the number of variables passing the screening threshold
  if(is.null(interactions)){
    if(length(obs_sign) < sig_vars_thresh$model_sel_interaction){
      interactions=TRUE
      print('interactive model: selection')
    }else if(length(obs_sign) < sig_vars_thresh$model_sel_additive){
      interactions=FALSE
      print('additive model: selection')
    }else if(length(obs_sign) < sig_vars_thresh$glmnet_interaction){
      #glm_reg = glinternet ## not yet implimented
      glm_reg = cv.glmnet
      print('interactive model: regularization')
    }else if(length(obs_sign) < sig_vars_thresh$glmnet_additive){
      glm_reg = cv.glmnet
      print('additive model: regularization')
    }
  }
  # construct multivariate models
  if(length(obs_sign) < sig_vars_thresh$model_sel_additive){
    selected_model = stepwise_multivariate_model_selection(df,obs_sign,response,family,model,interactions)
  }else{
    if(robust){ # run glmnet several times
      selected_model_list = list()
      for(i in 1:500){
        indx = sample(1:nrow(df)) # randomize 
        indy = sample(1:length(obs_sign)) # randomize 
        selected_model_list[[i]] = glm_reg(y=df[indx,response],x=data.matrix(df[indx,obs_sign[indy]]),family=family) ## untested
      }
    }else{
      selected_model = glm_reg(y=df[,response],x=data.matrix(df[,obs_sign]),family=family) ## untested
    }
  }
  if(length(obs_sign) > sig_vars_thresh$glmnet_additive){
    warning(paste(length(obs_sign),'is a large number of variables that may pose a relative challenge to glmnet'))
  }

  # Model Diagnostics

  # Cross Validation
  if(length(obs_sign) < sig_vars_thresh$model_sel_additive){
    if(K>1){
      cv=cross_assess_wrapper(data=df,formula=selected_model$formula,resp=response,family=family,K,model,cv_function=cross_valid_kfold,N=N)
    }else{
      cv=NULL
    }
  }else{
    if(robust){
      # multi-model characterization
      selected_model = vis_reg(selected_model_list)
    }else{
      # cross validation for regularization
      plot(selected_model)
      cv = list(auc=selected_model$cvm[selected_model$lambda==selected_model$lambda.min],other_stats=selected_model)
    }
  }
  
  return(list(screen=observationsL,final=selected_model,cv=cv))
}

#' model_selection_many_to_few
#' 
#' runs regularization then model selection to reduce many variables to a small robust model TODO
#' @param ..., see model_select parameters
#' @return a list containing: univariate models, the final selected model, and crossvalidation stats.
#' @export
model_selection_many_to_few <- function(...){
  # run regularization
  # (optional) run interaction regularization
  # identify regularization prototype models
  # for each prototypical model:
    # while not overfit:
      # select highest magnitude contributors
      # univariate screening and backward selection
  # return prototypical models
}

#######################################
### Run Tests
iris_test<-function(){
  print(str(iris))
}

mtcars_tests <- function(){
  #	source('~/Desktop/modeling_functions.r')
  K=5
  inter=NULL
  
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=inter,test='LRT',K=5,family = 'gaussian',model=glm)
  out=vis(mod)
  out[[1]]
  out[[2]]
  dev.off()
  sig_vars_thresh = list(model_sel_interaction=6,model_sel_additive=7,glmnet_interaction=200,glmnet_additive=2000) # see regularization output (lower upper bound for additive model selection)
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=inter,test='LRT',K=5,family = 'gaussian',model=glm,sig_vars_thresh=sig_vars_thresh)
  
  
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=inter,test='LRT',family='poisson',K=K,thresh_screen = .05)

  
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=inter,test='Wald',K=K)
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=inter,test='LRT',K=K)
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'mpg',interactions=inter,test='LRT',only_return_selected=FALSE,K=K)
  
  mtcars$log_mpg = log(mtcars$mpg)
  mod=model_selection(df=mtcars,colnames(mtcars)[-1],response = 'log_mpg',interactions=inter,test='LRT',K=K)
}

glmnet_test<-function(){
  sig_vars_thresh = list(model_sel_interaction=1,model_sel_additive=1,glmnet_interaction=1,glmnet_additive=1e5)
  robust=TRUE
  
}
