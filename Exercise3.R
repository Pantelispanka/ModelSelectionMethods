gdp.countries.data <- read.csv("~/Data_Projects/General linear models SEMFE/Exercise3/gdp-countries.csv")

first.linear.model <- lm(GDP~LFG+EQP+NEQ+GAP,data = gdp.countries.data)

install.packages("xtable")
require("xtable")
xtable(gdp.countries.data, type="latex")
xtable(anova(first.model), type="latex")


#Making a corrplot
gdp.data.only <- data.frame(gdp.countries.data$GDP, gdp.countries.data$LFG,gdp.countries.data$EQP,gdp.countries.data$NEQ, gdp.countries.data$GAP)
correlations <- cor(gdp.data.only)
corrplot(correlations)




#Checking Multicollinearity
require("car")

sqrt(vif(first.model))

require("MASS")

#AIC test
both.selection.AIC<-stepAIC(first.model, direction = "both")
backward.selection.AIC <- stepAIC(first.model, direction = "backward")
forard.selection.AIC <- stepAIC(first.model, direction = "forward")

#F-test
both.selection.F <- stepAIC(lm(GDP~1),GDP ~ LFG+NEQ+GAP+EQP, direction = "both", test = "F")
backward.selection.F <- stepAIC(lm(GDP~1),GDP ~ LFG+NEQ+GAP+EQP, direction = "forward", test = "F")
forward.selection.F <- stepAIC(lm(GDP~1),GDP ~ LFG+NEQ+GAP+EQP, direction = "forward", test = "F")

#Models for R^2 test and anova tables
without.LFG <- lm(GDP ~ EQP + NEQ+GAP, data = gdp.countries.data)
without.EQP <- lm(GDP ~ LFG + NEQ+GAP, data = gdp.countries.data)
without.NEQ <- lm(GDP ~ LFG + GDP+GAP, data = gdp.countries.data)
without.NEQ <- lm(GDP ~ LFG + EQP +GAP, data = gdp.countries.data)
without.GAP <- lm(GDP ~ LFG + EQP +NEQ, data = gdp.countries.data)


#R-Squared values
summary(without.EQP)$r.squared
summary(without.GAP)$r.squared
summary(without.LFG)$r.squared
summary(without.NEQ)$r.squared
summary(without.GAP.EQP)$r.squared
summary(without.GAP.LFG)$r.squared
summary(without.GAP.NEQ)$r.squared
#And so on..




#Cp -mallows
leaps <- leaps(data.frame(gdp.data.only$gdp.countries.data.LFG, gdp.data.only$gdp.countries.data.EQP, gdp.data.only$gdp.countries.data.NEQ, gdp.data.only$gdp.countries.data.GAP), gdp.data.only$gdp.countries.data.GDP, method = c("Cp"))

#Points of influence
plot(dffits(first.model))
plot(cooks.distance(first.model))
plot(hatvalues(first.model))


#All tested values
require(broom)
glance(first.model)

#Added variable plots // Component+Residual (Partial Residual) Plots
library(car)
avPlots(first.model)
crPlots(first.model)









