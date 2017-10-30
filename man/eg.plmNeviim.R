##packages
library(plm)
library(plmNeviim)
library(spd4testing)

#data
d<-spd4testing(missingX=F,missingY=F,missing.pre.years=T)
d<-spd4testing(missingX=F,missingY=F,missing.pre.years=T)
d

#test
#new data
o<-spd4testing(missingX=FALSE)
o

#formula
f<- formula(y ~ x + factor(year)*factor(gid))

#fixed effects or first difffernce estimation
e<-plm(f,data=d,model="within")
e<-plm(f,data=d,model="fd")
summary(e)

##fitted values
#with the plm-extended predict funciton
predict(e)
#from the plm object information
c(pmodel.response(e)-residuals(e)) #(shows row names)

##prediciton with plmNeviim
#fitted values
x<-prophecy.plm.out(estimate=e,data=d,pindex=c("id","year"),levelconstr=FALSE)
x$y.t.hat
#level construct
x<-prophecy.plm.out(estimate=e,data=d,pindex=c("id","year"),levelconstr=TRUE)
x[,c("id","year","y.l","y.t.hat","y.l.hat")]
m<-merge(d,x,by=c("id","year"),all.x=T)
m[,c("id","year","y","y.l","y.t","y.t.hat","y.l.hat")]
#out of sample prediciton
x<-prophecy.plm.out(estimate=e,data=o,pindex=c("id","year"),levelconstr=TRUE)
x[,c("id","year","y.l","y.t.hat","y.l.hat")]


\dontrun{
##standard out of sampel prediciton
predict(e,newdata=d)
predict(e,newdata=dn)
# Error in crossprod(beta, t(X)) : non-conformable arguments
# Whenever plm omits variables specified in the formula (e.g. base year in factor(year))
# it tries to multiply two matrices with different length of columns than regressors
# the new funciton avoids this and therefore is able to apply out of sample predicitons
# predict.out.plm(e,f,dn,"fd")
}



