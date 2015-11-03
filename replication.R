#  Replication script for JSS manuscript examples

library("spduration")
library("foreign")
library("magrittr")
library("ggplot2")
library("xtable")
library("scales")


data(bscoup)
bscoup$coup <- ifelse(bscoup$coup=="yes", 1, 0)
bscoup      <- add_duration(bscoup, "coup", unitID="countryid", tID="year",
                            freq="year", ongoing = FALSE)

weib_model <- spdur(
  duration ~ milreg + instab + regconf,
  atrisk ~ couprisk + wealth + milreg + rwar + regconf + samerica + camerica,
  data=bscoup, silent = TRUE)

loglog_model <- spdur(
  duration ~ milreg + instab + regconf,
  atrisk ~ couprisk + wealth + milreg + rwar + regconf + samerica + camerica,
  data=bscoup, distr="loglog", silent = TRUE)

matrix(c(
  AIC(weib_model), AIC(loglog_model), BIC(weib_model), BIC(loglog_model)
  ), ncol = 2, dimnames = list(c("Weibull", "Loglog"), c("AIC", "BIC")))


library("xtable")
tbl <- xtable(loglog_model, caption="Coup model with log-logistic hazard",
              label="loglog_table")
print(tbl, caption.placement="top", comment=FALSE, include.rownames=FALSE)

#   Plotting functions
#   ____________________

pdf("graphics/hazard-ex.pdf", height=4, width=8)
par(mfrow = c(1, 2))
plot_hazard(loglog_model, main = "A")
plot_hazard(loglog_model, 
     xvals = c(1, 1, 10, 0.05), 
     zvals = c(1, 7, 8.64, 1, 1, 0.05, 0, 0),
     main = "B")
dev.off()

pdf("graphics/sepplots.pdf", height=4, width=10)
par(mfrow=c(2,1),mar=c(2,2,2,2))
plot(weib_model, type="sepplot", lwd1 = 1, lwd2 = 1)
title(main="Weibull")
plot(loglog_model, type="sepplot", lwd1 = 1, lwd2 = 1)
title(main="Loglog")
dev.off()

pdf("graphics/hazard-rates.pdf", height=4, width=8)
par(mfrow=c(1, 2))
plot(weib_model,   type="hazard", main="Weibull")
plot(loglog_model, type="hazard", main="Loglog")
dev.off()


#   Out-of-sample testing
#   _____________________

data(bscoup)
bscoup$coup <- ifelse(bscoup$coup=="yes", 1, 0)
coup_train <- bscoup[bscoup$year < 1996, ]
coup_train <- add_duration(coup_train, "coup", unitID="countryid", tID="year",
                           freq="year", ongoing = FALSE)

coup_test  <- add_duration(bscoup, "coup", unitID="countryid", tID="year",
                           freq="year", ongoing = FALSE)
coup_test  <- coup_test[coup_test$year >= 1996, ]

weib_model2   <- spdur(
  duration ~ milreg + instab + regconf,
  atrisk ~ couprisk + wealth + milreg + rwar + regconf + samerica + camerica,
  data = coup_train)

loglog_model2 <- spdur(
  duration ~ milreg + instab + regconf,
  atrisk ~ couprisk + wealth + milreg + rwar + regconf + samerica + camerica,
  data = coup_train, distr="loglog") 

weib2_test_p   <- predict(weib_model2, newdata = coup_test)
loglog2_test_p <- predict(loglog_model2, newdata = coup_test)


#   Out-of-sample separation plots
#   ______________________________

library("separationplot")

obs_y <- coup_test[complete.cases(coup_test), "coup"]

pdf("graphics/oos-sepplots.pdf", height=4, width=10)
par(mfrow=c(2,1),mar=c(2,2,2,2))
separationplot(weib2_test_p,   obs_y, newplot = FALSE, lwd1 = 1, lwd2 = 1,
               heading = "Weibull")
separationplot(loglog2_test_p, obs_y, newplot = FALSE, lwd1 = 1, lwd2 = 1,
               heading = "Loglog")
dev.off()

#   Comparison to logistic regression
#   ___________________________

glm_model2    <- glm(
  coup ~ milreg + instab + regconf + couprisk + wealth + rwar + regconf + 
    samerica + camerica,
  data = coup_train, family = "binomial"
)

c(Weibull = AIC(weib_model2), 
  Loglog = AIC(loglog_model2), 
  Logistic = AIC(glm_model2))
  
#   Plot of positive rates in several published papers
#   __________________________

fl2003 <- read.dta("data/repdata.dta")
rates <- data.frame(
  data = "Fearon and Laitin 2003",
  rate = mean(fl2003$onset > 0)
)

load("data/irc_data_mod.rda")
rates <- rbind(rates, data.frame(
  data = "Beger et al 2014",
  rate = mean(irc.data$irr.t)
))

belkin <- read.dta("data/BelkinSchoferTable4.dta")
belkin$coup <- ifelse(belkin$coup=="yes", 1, 0)
rates <- rbind(rates, data.frame(
  data = "Belkin & Schofer 2003",
  rate = mean(belkin$coup)
))

rates$rate <- rates$rate * 1000

# Convert data to factor ordered by rate, so plot will sort by rate
rates$data <- with(rates, factor(data, levels = data[order(rate)]))

#xtable(rates, digits = 2)

p <- ggplot(rates, aes(x = data, y = rate)) + 
  geom_bar(stat = "identity", fill = muted("blue"), width = 0.6) +
  coord_flip() +
  theme_minimal() +
  labs(y = "Positives per 1,000", x = "")

ggsave(plot = p, file = "graphics/rates.pdf", height = 1.3, width = 5, scale = 1)
  
  