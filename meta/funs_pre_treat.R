nadata<-function(data,na_method, data_old=NULL,data_name=NULL, k=NULL){
  transf<-attr(data,"transf")
  if(na_method=="median/mode"){
    data<-imputeMissings::impute(data, method = "median/mode")}
  if(na_method=="knn"){
    imp <- preProcess(data, method = "knnImpute", k = k)
    data <- predict(imp, data)
    transf["Scale",ncol(transf)]<-TRUE
    transf["Center",ncol(transf)]<-TRUE
  }
  if(na_method=="bagImpute"){
    imp <- preProcess(data, method = "bagImpute")
    data <- predict(imp, data)
  }
  if(na_method=="medianImpute"){
    imp <- preProcess(data, method = "medianImpute")
    data <- predict(imp, data)
  }

  data<-data_migrate(data_old,data,data_name)
  attr(data,"transf")<-transf
  return(data)
}
nafactor<-function(data,na_method, k=NULL){
  factor<-attr(data,"factors")
  if(na_method=="median/mode"){
    factor<-imputeMissings::impute(factor, method = "median/mode")
  }
  attr(data,"factors")<-factor
  return(data)
}


singles<-function(data){
  validate(need(sum(apply(data,2, is.integer), na.rm=T)==0,'"The Singletons option requires a counting data"'))


  data0<-decostand(data,"pa", na.rm=T)
  remove<-colSums(data0, na.rm=T)==1
  if(sum(remove, na.rm=T)>0){return(  data[,-  which(remove)]) } else {return(data)}


}

pctRares<-function(data, pct=1){
  pct=pct/100
  remove<-colSums(data, na.rm=T)<(sum(data, na.rm=T)*pct)
  if(sum(remove, na.rm=T)>0){return(  data[,-  which(remove)]) } else {return(data)}
}

pctPrev<-function(data, pct)
{

  data0<-decostand(data,"pa", na.rm=T)
  remove<-colSums(data0, na.rm=T)<= round(nrow(data0)*pct)
  if(sum(remove, na.rm=T)>0){return(  data[,-  which(remove)]) } else {return(data)}


}

