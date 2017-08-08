#' @title Out of Sample Prediciton of Panel Data Estiamtions
#' @description blup
#' @param estimate blup
#' @param data blup
#' @param pname blup
#' @param pindex blup
#' @param levelconstr blup
#' @details
#' See package vignette.
#' @return an array of predicted values
#' @author Elías Cisneros <ec@elias-cisneros.de>
#' @importFrom stats lm
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom utils head
#' @import plm
#' @note Blup
#' @export


plmNavi<-function(
  estimate,
  data,
  # model="fd",
  pname="y",
  pindex=NULL,
  levelconstr=T
){
  # estimate=e
  # data=x
  # pname="y"
  # pindex=NULL
  # levelconstr=T
  #
  # #get index of panel data
  # if (is.null(pindex) && class(data)[1]=="pdata.frame") {
  #   pindex<-names(attributes(data)$index)
  # } else {
  #   pindex<-names(data)[1:2]
  # }
  # #converet ot pdata.frame
  # if (class(data)[1]!="pdata.frame") {
  #   data<-pdata.frame(data)
  # }
  # #get formula of estimate
  # formula<-e$formula
  # #get model of estimate
  # model<-e$args$model
  #
  # #define variable names
  # y.t<-paste0(pname,".t")
  # y.l<-paste0(pname,".l")
  # y.t.hat<-paste0(pname,".t.hat")
  # y.l.hat<-paste0(pname,".l.hat")
  #
  # #model frame
  # mf<-model.frame(formula,data=data)
  # #model matrix - transformed data of explanatory variables
  # mn<-model.matrix(formula,mf,model)
  # #model matrix data
  # mx<-mn
  # colnames(mx)<-paste0("p.",colnames(mx))
  # mx<-data.frame(mx)
  # mx$rowid<-as.integer(rownames(mx))
  # #response - transformed data of outcome variable
  # ms<-pmodel.response(formula,mf,model)
  # ms<-data.frame(ms)
  # names(ms)<-y.t
  # ms$rowid<-as.integer(rownames(ms))
  #
  # #transformed data of explanatory variables
  # #exclude variables that were droped in estimation
  # n<-names(estimate$aliased[estimate$aliased==F])
  # i<-match(n,colnames(mn))
  # i<-i[!is.na(i)] #avoid omited constant variables in new data set
  # X<-mn[,i]
  #
  # #predict transformed outcome with X * beta
  # # p<- X %*% coef(estimate)
  # cf<-coef(estimate)
  # cf<-cf[colnames(X)] #reduce to new dataset variables
  # p<-crossprod(t(X),cf)
  # p<-data.frame(p)
  # names(p)<-y.t.hat
  # p$rowid<-as.integer(rownames(p))
  #
  # if (levelconstr==T){
  #
  #   #dataset with original outcome
  #   od<-data.frame(
  #     attributes(mf)$index,
  #     data.frame(mf)[,1]
  #   )
  #   rownames(od)<-rownames(mf) #preserve row names from model.frame
  #   od$rowid<-as.integer(rownames(od))
  #   names(od)[3]<-y.l
  #
  #   #merge with  model response
  #   nd<-merge(od,ms,by="rowid",all.x=T)
  #
  #   #merge with model matrix
  #   nd<-merge(nd,mx,by="rowid",all.x=T)
  #
  #   #merge with prediciton
  #   nd<-merge(nd,p,by="rowid",all.x=T)
  #   nd<-nd[order(nd$rowid),]
  #
  #   #residuals
  #   nd[,paste0(pname,".t.res")]<-nd[,"y.t"]-nd[,"y.t.hat"]
  #
  #   #construct predicted level outcome for FD estiamtions
  #   if (model=="fd"){
  #     #first observation from real data
  #     i<-which(is.na(nd[,y.t.hat])) #start index
  #     nd[i,y.l.hat]<-NA
  #     nd[i,y.l.hat]<-nd[i,y.l]
  #     #fill values over all years
  #     ylist<-unique(nd[-i,pindex[2]])
  #     ylist<-as.integer(as.character(ylist))
  #     for (y in ylist){
  #       # y<-2004
  #       # print(y)
  #       v.lag<-nd[nd[,pindex[2]]==(y-1),y.l.hat]
  #       v.dif<-nd[nd[,pindex[2]]==(y  ),y.t.hat]
  #       # print(length(v))
  #       # print(length(v.lag))
  #       nd[nd[,pindex[2]]==y    ,y.l.hat] <- v.lag + v.dif
  #     }
  #   }
  #   if (model=="within"){
  #     #group means of outcome
  #     gm<-aggregate(nd[, pname], list(nd[,pindex[1]]), mean)
  #     gl<-aggregate(nd[, pname], list(nd[,pindex[1]]), length)
  #     nd<-cbind(nd,groupmeans=rep(gm$x,gl$x))
  #     #predicted values + group means
  #     nd[,y.l.hat]<-nd[,y.t.hat] + nd[,"groupmeans"]
  #   }
  #   if (model!="fd" && model!="within") {
  #     stop('funciton works only for FD and FE estimations')
  #   }
  # }
  # #results
  # if (levelconstr==F){
  #   results<-p
  # }
  # if (levelconstr==T){
  #   results<-nd
  # }
  # return(results)
}



