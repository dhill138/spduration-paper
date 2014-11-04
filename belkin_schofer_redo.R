#  
#

user <- Sys.info()[["user"]]
wd <- switch(user,
             andybega="~/Work/spduration-paper",
             danielhill="/Users/danielhill/Documents/spduration")
dropbox <- switch(user,
                  andybega="~/Dropbox/Work/gallium")
setwd(wd)


library(spduration)
library(foreign)
library(separationplot)

coup.data<-read.dta(file.path(dropbox, "data/BelkinSchoferTable4.dta"))
coup.data$coup<-as.numeric(coup.data$coup)-1

### replicate Column 4 of Table 4
summary(rep.model<-glm(coup~couprisk+recentcoups+wealth+africa+eurnam+samerica+camerica+instab+milreg+regconf+rwar,data=coup.data,family="binomial"))

### without BDM et al previous coup indicator
summary(rep.model<-glm(coup~couprisk+wealth+africa+eurnam+samerica+camerica+instab+milreg+regconf+rwar,data=coup.data,family="binomial"))

### split-pop models
new.coup.data<-add_duration(coup.data, "coup", unitID="countryid", tID="year", freq="year")

weib.model <- spdur(duration ~ milreg + instab + rwar + regconf,
                    atrisk ~ couprisk + wealth + milreg + rwar + regconf + 
                      africa + eurnam + samerica + camerica,
                    data=new.coup.data)
summary(weib.model)
preds <- new.coup.data[-weib.model$na.action, ]
preds$weib <- as.numeric(predict(weib.model))

loglog.model <- spdur(duration ~ milreg + instab + rwar + regconf,
                      atrisk ~ couprisk + wealth + milreg + rwar + regconf + 
                        africa + eurnam + samerica + camerica,
                      data=new.coup.data, distr="loglog")
summary(loglog.model)
preds$loglog <- as.numeric(predict(loglog.model))

### re-estimate logit model excluding ongoing cases
new.coup.data$coup[is.na(new.coup.data$duration)] <- NA
summary(rep.model<-glm(coup~couprisk+wealth+africa+eurnam+samerica+camerica+instab+milreg+regconf+rwar,data=new.coup.data,family="binomial"))
preds$logit<-predict(rep.model,type="response")

### compare predictions
dev.new(width=9,height=5)
par(mfrow=c(3,1),mar=c(3,3,3,3))
separationplot(preds$logit,preds$coup,newplot=F,heading="Logit")
separationplot(preds$weib,preds$coup,newplot=F,heading="Weibull")
separationplot(preds$loglog,preds$coup,newplot=F,heading="Log-logistic")

AIC(rep.model)
AIC(weib.model)
AIC(loglog.model)
