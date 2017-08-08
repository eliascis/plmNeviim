# https://rdrr.io/rforge/plm/src/R/plm.R
# https://r-forge.r-project.org/scm/viewvc.php/pkg/R/pfunctions.R?view=markup&root=plm&sortdir=down
# https://stackoverflow.com/questions/23143428/merge-plm-fitted-values-to-dataset/44185726#44185726
# http://r.789695.n4.nabble.com/fitted-from-plm-td3003924.html
# https://stackoverflow.com/questions/7123060/is-there-a-predict-function-for-plm-in-r/44185441#44185441
# https://stackoverflow.com/questions/7123060/is-there-a-predict-function-for-plm-in-r/44185441#44185441


##packages
library(plm)

##estimate
#formula
f<- pFormula(y ~ x + factor(year)*factor(gid))
f<- pFormula(y ~ x + factor(year)*factor(gid) + x)
f<- pFormula(y ~ x + factor(year))
f
#fixed effects or first difffernce estimation
e<-plm(f,data=d,model="within",index=c("id","year"))
e<-plm(f,data=d,model="fd",index=c("id","year"))
e<-plm(f,data=dn,model="fd",index=c("id","year"))
summary(e)

##fitted values of estimation
#transformed outcome prediction
predict(e)
c(pmodel.response(e)-residuals(e))
predict.out.plm(e,f,d,"fd")$p
# "level" outcome prediciton
predict.out.plm(e,f,d,"fd")$df$y.l.hat
#both
p<-predict.out.plm(e,f,d,"fd")
p

##out of sampel prediciton
predict(e,newdata=d)
predict(e,newdata=dn)
# Error in crossprod(beta, t(X)) : non-conformable arguments
# if plm omits variables specified in the formula (e.g. one year in factor(year))
# it tries to multiply two matrices with different length of columns than regressors
# the new funciton avoids this and therefore is able to apply out of sample predicitons
# predict.out.plm(e,f,dn,"fd")


