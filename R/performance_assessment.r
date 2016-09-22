#' pred_assess
#' 
#' MAIN cross validation assessment. There are 2 main usage modes. 1) provide predictions by setting pred and truth, 2) provide mod and dat to generate predictions to evaluate.
#' @param resp, a character vector of the names of dependent/response variables in df
#' @param pred, option 1 parameter: matrix of predictions (observations x fold)
#' @param truth, option 1 parameter: matrix of ground truth values (observations x fold)
#' @param mod, option 2 parameter: a trained model: glm(resp~...) or lm(resp~...)
#' @param dat, option 2 parameter: a data.frame containing all variables used in the mod model.
#' @return list of performance assessments
#' @export
pred_assess<-function(resp,pred=NULL,truth=NULL,mod=NULL,dat=NULL,...){
  if( !is.null(dat) & !is.null(resp) ){
    if(any(!all.vars(mod$formula)%in%colnames(dat))){stop('all variables used in mod must be in columns in dat')}
    pred = predict(mod,dat)
    truth = dat[[resp]]
  }else if( is.null(pred) | is.null(truth) ){
    stop('either (pred & truth) or (mod & dat) must be defined')
  }
  # clean
  tmp = na.omit(data.frame(pred=pred,truth=truth))
  if(sum(!is.finite(as.vector(unlist(tmp))))>0){stop(paste(sum(!is.finite(as.vector(unlist(tmp)))),'elements of pred and truth are not finite (inf,na,nan)'))}
  pred = data.matrix(tmp[,grepl('pred',colnames(tmp))])
  truth= data.matrix(tmp[,grepl('truth',colnames(tmp))])
  #tmp<<-tmp
  
  #check for zero variance
  if(any(apply(pred,2,sd)==0)){
    warning('zero variance predictions')
    return(NULL)
  }
  
  # assessment    
  auc=ROC( predictions=pred , truth=truth , resp=c('auc') )
  auc = signif(unlist(auc@y.values),3)
  par(mfrow = c(2,2))
  roc=ROC( predictions=pred , truth=truth , resp=c('tpr','fpr') , abline=T, ... )
  if(length(auc)==1){
    text(paste('AUC:',signif(auc,3)),x=.3,y=.4)
  }else{
    text(paste('AUC: mean=',signif(mean(auc),2),', sd=',signif(sd(auc),2),sep=''),x=.6,y=.05)
  }
  predvalue1=ROC( predictions=pred , truth=truth , resp=c('ppv','npv'), abline=F, ... )
  predvalue2=ROC( predictions=pred , truth=truth , resp=c('prec','rec'), abline=F, ... )
  mcc=ROC( predictions=pred , truth=truth , resp=c('phi'),add=F )
  
  out=list(auc=auc,roc=roc,ppv_npv=predvalue1,prec_rec=predvalue2,mcc=mcc)
  out
}

#' @import ROCR
ROC <- function( predictions , truth , resp=c('tpr','fpr') , sign=c('+','+'), abline=TRUE ,...){
  pred <- prediction( predictions, truth )
  if(length(resp)==1){
    perf = performance(pred, measure = resp) 
    #        if(sign[1]=='-'){ perf@x.values = rev(perf@x.values) }
    if(resp!='auc'){
      try( plot(perf , ...) ) 
    }
  }else if(length(resp)==2){
    perf <- performance( pred, resp[1], resp[2])
    #        if(sign[1]=='-'){ perf@x.values = rev(perf@x.values) }
    #        if(sign[2]=='-'){ perf@y.values = rev(perf@y.values) }
    plot( perf , colorize=TRUE , ...)
    if(abline){ abline(a=0, b= 1) }
  }else{
    stop('resp may only have 1 or 2 values')
  }
  return(perf)
}

