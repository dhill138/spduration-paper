setwd("/Users/danielhill/Documents/spduration")
library(spduration)
library(foreign)
library(separationplot)

coup.data<-read.dta("Belkin_Schofer_Data/BelkinSchoferTable4.dta")
coup.data$coup<-as.numeric(coup.data$coup)-1

### replicate Column 4 of Table 4
summary(rep.model<-glm(coup~couprisk+recentcoups+wealth+africa+eurnam+samerica+camerica+instab+milreg+regconf+rwar,data=coup.data,family="binomial"))

### without BDM et al previous coup indicator
summary(rep.model<-glm(coup~couprisk+wealth+africa+eurnam+samerica+camerica+instab+milreg+regconf+rwar,data=coup.data,family="binomial"))

### split-pop models
new.coup.data<-add_duration(coup.data, "coup", unitID="countryid", tID="year", freq="year")

summary(weib.model<-spdur(duration~milreg+instab+rwar+regconf,atrisk~couprisk+wealth+milreg+rwar+regconf+africa+eurnam+samerica+camerica,data=new.coup.data))
preds<-new.coup.data[-weib.model$na.action,]
preds$weib<-as.numeric(predict(weib.model))

summary(loglog.model<-spdur(duration~milreg+instab+rwar+regconf,atrisk~couprisk+wealth+milreg+rwar+regconf+africa+eurnam+samerica+camerica,data=new.coup.data,distr="loglog"))
preds$loglog<-as.numeric(predict(loglog.model))

### re-estimate logit model excluding ongoing cases
new.coup.data$coup[is.na(new.coup.data$duration)]<-NA
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
