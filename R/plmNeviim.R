#' @title Out of Sample Prediciton of Panel Data Estiamtions
#' @description blup
#' @param estimate a plm object,
#' @param data a data.frame to use for the outcome predictions,
#' @param pindex the panel index of the \code{estimate}.
#' @param pname the predicted outcome name. Defalt is "y",
#' @param levelconstr logical, if true a estimation of a logarithmic outcome is assumed and the level outcome is additonally created
#' @details
#' ...
#' @return a data frame with fitted / predicted values
#' @author Elías Cisneros <ec@elias-cisneros.de>
#' @importFrom stats lm
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats coef
#' @importFrom stats aggregate
#' @importFrom utils head
#' @seealso \code{\link{plm}}
#' @references
#' \url{https://rdrr.io/rforge/plm/src/R/plm.R} \cr
#' \url{https://r-forge.r-project.org/scm/viewvc.php/pkg/R/pfunctions.R?view=markup&root=plm&sortdir=down} \cr
#' \url{https://stackoverflow.com/questions/23143428/merge-plm-fitted-values-to-dataset/44185726#44185726} \cr
#' \url{http://r.789695.n4.nabble.com/fitted-from-plm-td3003924.html} \cr
#' \url{https://stackoverflow.com/questions/7123060/is-there-a-predict-function-for-plm-in-r/44185441#44185441} \cr
#' \url{https://stackoverflow.com/questions/7123060/is-there-a-predict-function-for-plm-in-r/44185441#44185441} \cr
#' @import plm
#' @note Blup
#' @export
###

prophecy.plm.out<-function(
  estimate,
  data,
  pindex=NULL,
  pname="y",
  levelconstr=T
){
  # estimate=e
  # data=d
  # pname="y"
  # pindex=c("id","year")
  # levelconstr=T


  ##setup
  formula<-estimate$formula
  model<-estimate$args$model


  ##restrictions
  if (is.null(pindex) && class(data)[1]!="pdata.frame") {
    stop('Please provide the index column names, e.g. c("id","year")')
  }
  if (model!="fd" && model!="within") {
    stop('funciton works only for FD and FE estimations')
  }

  ##index of panel data
  if (is.null(pindex) && class(data)[1]=="pdata.frame") {
    pindex<-names(attributes(data)$index)
  }

  ##convert to pdata.frame
  if (class(data)[1]!="pdata.frame") {
    data<-pdata.frame(data, index=pindex )
  }

  ##define variable names
  y.t<-paste0(pname,".t")
  y.l<-paste0(pname,".l")
  y.t.hat<-paste0(pname,".t.hat")
  y.l.hat<-paste0(pname,".l.hat")

  ##model frame
  mf<-model.frame(formula,data=data)
  ##model matrix - transformed data of explanatory variables
  mn<-model.matrix(formula,mf,model)
  ##model matrix data
  mx<-mn
  colnames(mx)<-paste0("p.",colnames(mx))
  mx<-data.frame(mx)
  mx$rowid<-as.integer(rownames(mx))

  ##transformed data
  #of outcome variable
  ms<-pmodel.response(formula,mf,model)
  ms<-data.frame(ms)
  names(ms)<-y.t
  ms$rowid<-as.integer(rownames(ms))
  #of explanatory variables
  n<-names(estimate$aliased[estimate$aliased==F]) #exclude variables that were droped in estimation
  i<-match(n,colnames(mn))
  i<-i[!is.na(i)] #avoid omited constant variables in new data set
  X<-mn[,i]

  ##predict transformed outcome with X * beta
  # p<- X %*% coef(estimate)
  cf<-coef(estimate)
  cf<-cf[colnames(X)] #reduce to new dataset variables
  p<-crossprod(t(X),cf)
  p<-data.frame(p)
  names(p)<-y.t.hat
  p$rowid<-as.integer(rownames(p))

  ##level construct from logarithmic outcome prediciton
  if (levelconstr==T){

    #dataset with original outcome
    od<-data.frame(
      attributes(mf)$index,
      data.frame(mf)[,1]
    )
    rownames(od)<-rownames(mf) #preserve row names from model.frame
    od$rowid<-as.integer(rownames(od))
    names(od)[3]<-y.l

    #merge with  model response
    nd<-merge(od,ms,by="rowid",all.x=T)

    #merge with model matrix
    nd<-merge(nd,mx,by="rowid",all.x=T)

    #merge with prediciton
    nd<-merge(nd,p,by="rowid",all.x=T)
    nd<-nd[order(nd$rowid),]

    #residuals
    nd[,paste0(pname,".t.res")]<-nd[,"y.t"]-nd[,"y.t.hat"]

    #construct predicted level outcome for FD estiamtions
    if (model=="fd"){
      #first observation from real data
      i<-which(is.na(nd[,y.t.hat])) #start index
      nd[i,y.l.hat]<-NA
      nd[i,y.l.hat]<-nd[i,y.l]
      #fill values over all years
      ylist<-unique(nd[-i,pindex[2]])
      ylist<-as.integer(as.character(ylist))
      for (y in ylist){
        # y<-2004
        # print(y)
        v.lag<-nd[nd[,pindex[2]]==(y-1),y.l.hat]
        v.dif<-nd[nd[,pindex[2]]==(y  ),y.t.hat]
        # print(length(v))
        # print(length(v.lag))
        nd[nd[,pindex[2]]==y    ,y.l.hat] <- v.lag + v.dif
      }
    }
    if (model=="within"){
      #group means of outcome
      gm<-aggregate(nd[, pname], list(nd[,pindex[1]]), mean)
      gl<-aggregate(nd[, pname], list(nd[,pindex[1]]), length)
      nd<-cbind(nd,groupmeans=rep(gm$x,gl$x))
      #predicted values + group means
      nd[,y.l.hat]<-nd[,y.t.hat] + nd[,"groupmeans"]
    }
  }

  ##results
  if (levelconstr==F){
    results<-p
  }
  if (levelconstr==T){
    results<-nd
  }

  ##out
  return(results)
}



