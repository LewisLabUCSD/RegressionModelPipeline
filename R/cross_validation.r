#' cross_valid_kfold
#' 
#' MAIN cross validation function. Includes performance assessment. Runs and assesses crossvalidation.
#' @param data, a data.frame containing response and observations variables used in the formula.
#' @param formula, relation of response ~ observation to be examined.
#' @param resp, a character vector of the names of dependent/response variables in df
#' @param family, a character string indicating the family associated with the submitted model c('gaussian','binomial','poisson'...)
#' @param K, a numeric variable indicating the number of folds for the k-fold validation
#' @param model, a model associated for testing the variables c(glm,lm)
#' @param cv_function, a crossvalidation function with the same inputs/outputs as cross_valid_kfold
#' @return list of performance assessments
#' @export
cross_assess_wrapper<-function(data,formula,resp,family,K,model,cv_function=cross_valid_kfold){
  # crossfold validation
  out = cv_function(data=data,formula=formula,resp=resp,family=family,K=K,model=model)
  if(nrow(out$pred_out)!=nrow(out$labels_out)){
    out <<- list(data=df,formula=formula,resp=resp,family=family,K=K,model=model,out=out)
    stop('prediction and truth must be the same dimension')
  }
  if(family=='binomial'){
    # assessment
    out=pred_assess(resp=resp,pred=out$pred_out,truth=out$labels_out,xlim=c(0,1),ylim=c(0,1)) 
  }else{
    truth=out$labels_out; pred=out$pred_out; corp=list();cors=list(); rmse=list()
    for(i in 1:ncol(truth)){
      corp[[i]] = cor( truth[,i],pred[,i] , method='pearson' )
      cors[[i]] = cor( truth[,i],pred[,i] , method='spearman' )
      rmse[[i]] = rmse( truth[,i],pred[,i] )
    }
    par(mfrow=c(1,2))
    boxplot(list(RMSE=unlist(rmse)),ylab='RMSE')
    boxplot(list(Pearson=unlist(corp),Spearman=unlist(cors)),ylab='Correlation')
    out = list(pearson=corp,spearmen=cors,rmse=rmse)
  }
  out
}

rmse <- function(y,yh){
  sqrt(mean( (yh-y)^2 ))
}

#' cross_valid_kfold
#' 
#' k-fold cross validation run to collect predictions for assessment
#' @param data, a data.frame containing response and observations variables used in the formula.
#' @param formula, relation of response ~ observation to be examined.
#' @param resp, a character vector of the names of dependent/response variables in df
#' @param family, a character string indicating the family associated with the submitted model c('gaussian','binomial','poisson'...)
#' @param K, a numeric variable indicating the number of folds for the k-fold validation
#' @param model, a model associated for testing the variables c(glm,lm)
#' @return list of cross-validation results: pred_out matrix of predictions (observations x fold), train_out matrix of indicies used for each fold of cross-validation (observations x fold), labels_out matrix of ground truth labels (observations x fold)
#' @export
cross_valid_kfold <- function(data,formula,resp=all.vars(formula)[1],family,K=10,model=glm){
  K=K+1
  ind = sample(1:nrow(data))
  interval = floor(nrow(data)/K)
  ranges = seq(1,nrow(data),interval)
  pred = list(); train = list(); labels = list()
  for(k in 1:(K-1)){
    low = ranges[k] ; high = ranges[k+1]
    valid_ind = ind[low:(high-1)]
    train_ind = ind[!ind %in% valid_ind]
    glm_i = model( formula , data=data[train_ind,] , family=family)
    pred[[as.character(k)]] = predict(glm_i,newdata=data[valid_ind,])
    train[[as.character(k)]] = train_ind
    labels[[as.character(k)]] = data[valid_ind,resp]
  }
  return(list(
    pred_out = do.call(cbind,pred),
    train_out = do.call(cbind,train),
    labels_out = do.call(cbind,labels)
  ))
} 
