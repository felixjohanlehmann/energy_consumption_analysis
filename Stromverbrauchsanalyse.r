setwd("E:/MONEY/strom")

data <- read.csv2("Stromverbrauch.csv", header = TRUE)
attach(data)

data

plot(day_counter, Durchschnitt_Jahresverbrauch_Intervall, type = "b", col = "grey", ylim=c(0, max(Durchschnitt_Jahresverbrauch_Intervall)), ylab = "extrapolierter Jahresverbrauch / kWh")
abline(h=1500, col="blue")
abline(v=1034, col="red") #confidential
abline(v=1970, col="red") #confidential
abline(v=3098, col="red") #confidential

plot(day_counter, Verbrauch_kummuliert, type = "b", col = "grey", ylim=c(0, max(Verbrauch_kummuliert)), ylab = "Kummulierter Stromverbrauch / kWh")
abline(v=1034, col="red") #confidential
abline(v=1970, col="red") #confidential
abline(v=3098, col="red") #confidential

###einfache lineare Regression
model1 <- lm(Verbrauch_kummuliert~day_counter)
summary(model1)
anova(model1)

#Überprüfung auf Normalverteilung der Residuen
model1$residuals
qqnorm(model1$residuals)

#Plot mit Regressionsgeraden
plot(day_counter, Verbrauch_kummuliert)
lines(day_counter, model1$fitted.values, col= "green", lty = "dashed")

###einfache lineare Regression vor #confidential
model2 <- lm(Verbrauch_kummuliert[1:24]~day_counter[1:24])
summary(model2)
anova(model2)

#Überprüfung auf Normalverteilung der Residuen
model2$residuals
qqnorm(model2$residuals)

#Plot mit Regressionsgeraden
plot(day_counter, Verbrauch_kummuliert)
lines(day_counter[1:24], model2$fitted.values, col= "red", lty = "dashed")

###einfache lineare Regression ab #confidential
model3 <- lm(Verbrauch_kummuliert[25:49]~day_counter[25:49])
summary(model3)
anova(model3)

#Überprüfung auf Normalverteilung der Residuen
model3$residuals
qqnorm(model3$residuals)

###einfache lineare Regression ab #confidential
model4 <- lm(Verbrauch_kummuliert[49:88]~day_counter[49:88])
summary(model4)
anova(model4)

#Überprüfung auf Normalverteilung der Residuen
model4$residuals
qqnorm(model4$residuals)

###einfache lineare Regression ab #confidential
model5 <- lm(Verbrauch_kummuliert[88:length(date)]~day_counter[88:length(date)])
summary(model5)
anova(model5)

#Überprüfung auf Normalverteilung der Residuen
model5$residuals
qqnorm(model5$residuals)


#Plot mit Regressionsgeraden
plot(day_counter, Verbrauch_kummuliert)
lines(day_counter[1:24], model2$fitted.values, col= "red", lty = "dashed")
lines(day_counter[25:49], model3$fitted.values, col= "blue", lty = "dashed")
lines(day_counter[49:88], model4$fitted.values, col= "pink", lty = "dashed")
lines(day_counter[88:length(date)], model5$fitted.values, col= "green", lty = "dashed")
abline(v=1034, col="red") #confidential
abline(v=1970, col="red") #confidential
abline(v=2951, col="red") #confidential


