# Why is CH greater than 1 sometimes?

user <- Sys.info()[["user"]]
wd <- switch(user,
             andybega="~/Work/spduration-paper",
             danielhill="/Users/danielhill/Documents/spduration")
dropbox <- switch(user,
                  andybega="~/Dropbox/Work/gallium/data",
                  danielhill="")
setwd(wd)


#library(spduration)
#library(survival)
library(foreign)

coup.data <- read.dta(file="BelkinSchoferTable4.dta")
coup.data$coup<-as.numeric(coup.data$coup)-1

### split-pop models
new.coup.data<-add_duration(coup.data, "coup", unitID="countryid", tID="year", freq="year")
colnames(new.coup.data)[3:14]<-c("Coup.Risk","Recent.Coups","Recent.War","Military.Regime","GDP.cap.","Instability","Coup","Africa","Europe.North.Am.","South.Am.","Central.Am.","Regional.Conflict")

summary(weib.model<-spdur(duration~Military.Regime+Instability+Recent.War+Regional.Conflict,
                          atrisk~Coup.Risk+GDP.cap.+Military.Regime+
                            Recent.War+Regional.Conflict+
                            South.Am.+Central.Am.,data=new.coup.data))

weib.preds <- predict(weib.model, truncate=FALSE, newdata=new.coup.data)
range(weib.preds)

# Flow:
# change p_min in predict.spdur
# run packagebuilder.r through load_all
# run this script through here

# p_min     max P
# 1e-10     1.517623
# 1e-15     1.517623
# 1e-16     1.517623

predict.spdur

