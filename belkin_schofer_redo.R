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

coup.data<-read.dta("Belkin_Schofer_Data/BelkinSchoferTable4.dta")
coup.data$coup<-as.numeric(coup.data$coup)-1

### replicate Column 4 of Table 4
summary(rep.model<-glm(coup~couprisk+recentcoups+wealth+africa+eurnam+samerica+camerica+instab+milreg+regconf+rwar,data=coup.data,family="binomial"))

### without BDM et al previous coup indicator
summary(rep.model<-glm(coup~couprisk+wealth+africa+eurnam+
		samerica+camerica+instab+milreg+regconf+rwar,data=coup.data,family="binomial"))

### split-pop models
new.coup.data<-add_duration(coup.data, "coup", unitID="countryid", tID="year", freq="year")

summary(weib.model<-spdur(duration~milreg+instab+rwar+regconf,
		atrisk~couprisk+wealth+milreg+rwar+regconf+samerica+camerica,data=new.coup.data))

summary(loglog.model<-spdur(duration~milreg+instab+rwar+regconf,
		atrisk~couprisk+wealth+milreg+rwar+regconf+samerica+camerica,data=new.coup.data,distr="loglog"))

### re-estimate logit model excluding ongoing cases
new.coup.data$coup[is.na(new.coup.data$duration)]<-NA

summary(rep.model<-glm(coup~couprisk+wealth+samerica+camerica+instab+milreg+
		regconf+rwar,data=new.coup.data,family="binomial"))
preds<-new.coup.data[-rep.model$na.action,]
preds$logit<-predict(rep.model,type="response")

### compare in-sample predictions, AIC
dev.new(width=9,height=5)
par(mfrow=c(3,1),mar=c(3,3,3,3))
separationplot(preds$logit,preds$coup,newplot=F,show.expected=T,lwd1=5,lwd2=2)
plot(weib.model,endSpellOnly=F)
plot(loglog.model,endSpellOnly=F)
AIC(rep.model)
AIC(weib.model)
AIC(loglog.model)

### compare out-of-sample predictions
coup.train<-coup.data[coup.data$year<1999,]
coup.train<-add_duration(coup.train, "coup", unitID="countryid", tID="year", freq="year")

coup.test<-add_duration(coup.data, "coup", unitID="countryid", tID="year", freq="year")
coup.test<-coup.test[coup.test$year==1999,]
coup.test<-na.omit(coup.test)

weib.model2<-spdurCrisp(duration~milreg+instab+rwar+regconf,atrisk~couprisk+wealth+
		milreg+rwar+regconf+samerica+camerica,
		last='end.spell',train=coup.train,test=coup.test,pred=coup.test)
weib.preds<-as.numeric(weib.model2$test.p)

loglog.model2<-spdurCrisp(duration~milreg+instab+rwar+regconf,atrisk~couprisk+wealth+
	milreg+rwar+regconf+samerica,
	last='end.spell',train=coup.train,test=coup.test,pred=coup.test,distr="loglog",iter=300)
loglog.preds<-as.numeric(loglog.model2$test.p)

dev.new(width=9,height=5)
par(mfrow=c(3,1),mar=c(3,3,3,3))
separationplot(weib.preds,coup.test$coup,newplot=F,show.expected=T,lwd1=5,lwd2=2)
separationplot(loglog.preds,coup.test$coup,newplot=F,show.expected=T,lwd1=5,lwd2=2)
