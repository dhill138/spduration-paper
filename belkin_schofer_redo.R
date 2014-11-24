#  
#

user <- Sys.info()[["user"]]
wd <- switch(user,
             andybega="~/Work/spduration-paper",
             danielhill="/Users/danielhill/Documents/spduration-paper")
dropbox <- switch(user,
                  andybega="~/Dropbox/Work/gallium/data",
                  danielhill="")
setwd(wd)


library(spduration)
library(foreign)
library(xtable)
library(separationplot)

coup.data <- read.dta(file.path(dropbox, "BelkinSchoferTable4.dta"))
coup.data$coup<-as.numeric(coup.data$coup)-1

### replicate Column 4 of Table 4 to make sure the data are ok
summary(rep.model<-glm(coup~couprisk+recentcoups+wealth+africa+eurnam+samerica+camerica+instab+milreg+regconf+rwar,data=coup.data,family="binomial"))

### split-pop models
new.coup.data<-add_duration(coup.data, "coup", unitID="countryid", tID="year", freq="year")
colnames(new.coup.data)[3:14]<-c("Coup.Risk","Recent.Coups","Recent.War","Military.Regime","GDP.cap.","Instability","Coup","Africa","Europe.North.Am.","South.Am.","Central.Am.","Regional.Conflict")

summary(weib.model<-spdur(duration~Military.Regime+Instability+Recent.War+Regional.Conflict,
		atrisk~Coup.Risk+GDP.cap.+Military.Regime+
		Recent.War+Regional.Conflict+
		South.Am.+Central.Am.,data=new.coup.data))
xtable(weib.model,caption="Coup model with Weibull hazard")

summary(loglog.model<-spdur(duration~Military.Regime+Instability+Recent.War+Regional.Conflict,
		atrisk~Coup.Risk+GDP.cap.+Military.Regime+
		Recent.War+Regional.Conflict+
		South.Am.+Central.Am.,data=new.coup.data,distr="loglog"))
xtable(loglog.model,caption="Coup model with log-logistic hazard")

### use plot function to compare in-sample predictions 
#pdf(file="in-sample.pdf",width=9,height=3)
dev.new(width=9,height=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
plot(weib.model,endSpellOnly=F)
plot(loglog.model,endSpellOnly=F)
#dev.off()

### AIC and BIC
AIC(weib.model)
AIC(loglog.model)
BIC(weib.model)
BIC(loglog.model)

### use Crisp wrapper to compare out-of-sample predictions
coup.train<-new.coup.data[coup.data$year<1999,]
coup.test<-new.coup.data[new.coup.data$year==1999,]
coup.test<-na.omit(coup.test)

weib.model2<-spdurCrisp(duration~Military.Regime+Instability+Recent.War+Regional.Conflict,
		atrisk~Coup.Risk+GDP.cap.+Military.Regime+Recent.War+Regional.Conflict+
		South.Am.+Central.Am.,
		last='end.spell',train=coup.train,test=coup.test,pred=coup.test,stat='conditional risk')
weib.preds<-as.numeric(weib.model2$test.p)

loglog.model2<-spdurCrisp(duration~Military.Regime+Instability+Recent.War+Regional.Conflict,
		atrisk~Coup.Risk+GDP.cap.+Military.Regime+Recent.War+Regional.Conflict+
		South.Am.,
		last='end.spell',train=coup.train,test=coup.test,pred=coup.test,,stat='conditional risk',distr="loglog")
loglog.preds<-as.numeric(loglog.model2$test.p)

#pdf(file="out-of-sample.pdf",width=9,height=3)
dev.new(width=9,height=3)
par(mfrow=c(2,1),mar=c(2,2,2,2))
separationplot(weib.preds,coup.test$Coup,newplot=F,show.expected=T,lwd1=5,lwd2=2)
separationplot(loglog.preds,coup.test$Coup,newplot=F,show.expected=T,lwd1=5,lwd2=2)
#dev.off()

### plot the hazard rate

plot.hazard<-function(thing){
	dur.dat<-thing$mf.dur
	risk.dat<-thing$mf.risk 
	ti<-seq(1, max(dur.dat[, 1])*1.2, length.out=100)
	X<-cbind(1, dur.dat[ ,2:dim(dur.dat)[2]])
	Z<-cbind(1, risk.dat[ ,2:dim(risk.dat)[2]])

	beta<-weib.model$coef[1:ncol(X)]
	gamma<-weib.model$coef[(ncol(X) + 1):(ncol(X) + ncol(Z))]
	a<-weib.model$coef[ncol(X) + ncol(Z) + 1]
	alpha<-exp(-a)

	mean.X<-apply(X,2,mean)
	mean.Z<-apply(Z,2,mean)

	lambda<-pmax(1e-10, exp(-mean.X %*% beta))
	cure<-1 - plogis(mean.Z %*% gamma)

	preds<-vector(length=length(ti))
	for(i in 1:length(ti)){
		st<-exp(-(lambda * ti[i])^alpha)
		cure.t<-cure / pmax(1e-10, (st + cure * (1 - st)))
		atrisk.t<-1 - cure.t
		ft<-lambda * alpha * (lambda * ti[i])^(alpha-1) * exp(-(lambda * ti[i])^alpha)
		preds[i]<-atrisk.t * ft / pmax(1e-10, (cure.t + atrisk.t * st))
	}
plot(ti,preds,type="l",xlab="Time",ylab="Conditional Hazard")
}

# playing around
shape <- exp(weib.model$coefficients["log(alpha)"])
t <- seq(1, 35, length.out=200)

hist(new.coup.data$duration[new.coup.data$failure==1], breaks=35, freq=F)
lines(t, dweibull(t, shape), col="orange", lwd=2)
lines(t, dweibull(t, 0.2), col="cyan", lwd=2)
lines(t, dweibull(t, 2), col="magenta", lwd=2)

plot(t, dweibull(t, shape)/pweibull(t, shape), type="l")
