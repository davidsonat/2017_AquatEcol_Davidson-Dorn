setwd("C:\\Users\\adavi\\Dropbox\\FAU\\Data\\Lab\\Feeding Rates 2.0\\FinalAnalysisFixed")

data1 <- read.delim("Davidson_RatesEvE.txt") #Finalized, no outliers omited, w/ 13.8 cutoff
data2 <- read.delim("Davidson_RatesEvN.txt") #
data3 <- read.delim("Davidson_RatesLEFinal.txt")
data4 <- read.delim("Davidson_RatesHNFinal.txt")
data5 <- read.delim("Davidson_RatesHEFinal.txt")
data6 <- read.delim("Davidson_RatesHEvN.txt")

library(car)
library(nlme)

reg1 <- lm(Rate ~ CraySize, data = data3) #Large Exotics
reg2 <- lm(Biomass ~ CraySize, data = data3) 
reg3 <- lm(Rate ~ CraySize, data = data4) #Hatchling Natives
reg4 <- lm(Biomass ~ CraySize, data = data4) 
reg5 <- lm(Rate ~ CraySize, data = data5) #Hatchling Exotics
reg6 <- lm(Biomass ~ CraySize, data = data5)
qqnorm(data3$lnRate)
qqline(data4$lnRate)
hist(data4$lnRate)
shapiro.test(data3$lnRate)
leveneTest(lnRate~Species*CraySize, data=data1)
summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)
summary(reg5)
summary(reg6)

mod1 <- aov(lnRate~CraySize*Species, data=data1) #Large vs. small Exotics
summary(mod1)
leveneTest(lnRate~Species, data=data1)
shapiro.test(residuals(mod1))
mod2 <- aov(lnBiomass~CraySize*Species, data=data1)
summary(mod2)
leveneTest(lnBiomass~Species, data=data1)
shapiro.test(residuals(mod2))

mod3 <- aov(lnRate~CraySize*Species, data=data2) #Size-matched Natives and Exotics
summary(mod3)
leveneTest(lnRate~Species, data=data2)
plot(mod3, 2)
shapiro.test(residuals(mod3))
mod4 <- aov(lnBiomass~CraySize*Species, data=data2)
summary(mod4)
plot(mod4, 2)
leveneTest(lnBiomass~Species, data=data2)
shapiro.test(residuals(mod4))

mod5 <- aov(lnRate~CraySize*Species, data=data6) #Hatchling Natives and Exotics
summary(mod5)
leveneTest(lnRate~Species, data=data6)
plot(mod5, 2)
shapiro.test(residuals(mod5))
mod6 <- aov(lnBiomass~CraySize*Species, data=data6)
summary(mod6)
plot(mod6, 2)
leveneTest(lnBiomass~Species, data=data6)
shapiro.test(residuals(mod6))


#Plots for exotic vs. exotic
small <- subset(data1, Species=="HE")
large <- data1[data1$Species=='LE',]

par(pty = "s", mar=c(6.5,6.5,6.5,6.5))

plot(Rate~CraySize, data=data1, xlab = "", xaxt="n", yaxt="n", ylab=expression(Mortality ~ Rate ~ (snails %.% h^{-1})), type='n', cex.lab=1.5, cex.axis=1.3)
points(small$CraySize,small$Rate, pch=0, cex = 1.7)
points(large$CraySize,large$Rate, pch=19, cex = 1.7)
abline(reg5, lty=1)
abline(reg1, lty=2, lwd = 2)
axis(2, las=2, cex.axis=1.3)


plot(Biomass~CraySize, data=data1, yaxt="n",xlab="Crayfish Length (mm CL)", ylab=expression(Feeding ~ Rate ~ (mg ~ soft ~ tissue %.% h^{-1})), type='n', cex.lab=1.5)
points(small$CraySize,small$Biomass, pch=0, cex = 1.7)
points(large$CraySize,large$Biomass, pch=19, cex = 1.7)
abline(reg6, lty=1)
abline(reg2, lty=2, lwd = 2)
axis(2, at=c(0,1,2,3),las=2, cex.axis=1.3)
legend("bottom", xpd = TRUE, horiz = TRUE, inset = c(0, -0.4), bty = "n", c("Hatchling","Juvenile"), lty=c(1,2), lwd = c(1,2), pch=c(0,19), cex = 1.3)

#Plots for Exotic vs. Native
Exotic <- subset(data2, Species=="LE")
Native <- data2[data2$Species=='HN',]

par(bg='transparent')
#par(mar=c(5,5.3,4,2))
#par(mfrow = c(2,1))

par(pty = "s", mar=c(6.5,6.5,6.5,6.5))

plot(Rate~CraySize, data=data2, xlab = "", xaxt = "n", yaxt="n", ylab = expression(Kill ~ Rate ~ (snails %.% h^{-1})), type='n', cex.lab=1.5, cex.axis=1.3)
points(Exotic$CraySize,Exotic$Rate, pch=19, cex = 1.7)
points(Native$CraySize,Native$Rate, pch=4, cex = 1.7)
abline(reg1, lty=2, lwd = 2)
abline(reg3, lty=4, lwd = 2)
axis(2, las=2, at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1), cex.axis=1.3)

plot(Biomass~CraySize, data=data2, yaxt="n", xlab="Crayfish Length (mm CL)", ylab=expression(Consumption ~ Rate ~ (mg ~ soft ~ tissue %.% h^{-1})), type='n', cex.lab=1.5, cex.axis=1.3)
points(Exotic$CraySize,Exotic$Biomass, pch=19, cex = 1.7)
points(Native$CraySize,Native$Biomass, pch=4, cex = 1.7)
abline(reg2, lty=2, lwd = 2)
abline(reg4, lty=4, lwd = 2)
axis(2, las=2, at=c(0,1,2,3), cex.axis=1.3)
legend("bottom", xpd = TRUE, horiz = TRUE, inset = c(0, -0.4), bty = "n", c("P. maculata","P. paludosa"), text.font=c(3), lty=c(2,4), lwd=c(2,2), pch=c(19,4), cex = 1.3)

#Plots for Hatch Exotic vs. Native
HExotic <- subset(data6, Species=="HE")
HNative <- data6[data6$Species=="HN",]

par(bg='transparent')

par(pty = "s", mar=c(6.5,6.5,6.5,6.5))

plot(Rate~CraySize, data=data6, xlab = "", xaxt = "n", yaxt="n", ylab = expression(Mortality ~ Rate ~ (snails %.% h^{-1})), type='n', cex.lab=1.5, cex.axis=1.3)
points(HExotic$CraySize,HExotic$Rate, pch=0, cex = 1.7)
points(HNative$CraySize,HNative$Rate, pch=4, cex = 1.7)
abline(reg5, lty=1)
abline(reg3, lty=4, lwd = 2)
axis(2, las=2, cex.axis=1.3)

plot(Biomass~CraySize, data=data6, yaxt="n", xlab="Crayfish Length (mm CL)", ylab=expression(Feeding ~ Rate ~ (mg ~ soft ~ tissue %.% h^{-1})), type='n', cex.lab=1.5, cex.axis=1.3)
points(HExotic$CraySize,HExotic$Biomass, pch=0, cex = 1.7)
points(HNative$CraySize,HNative$Biomass, pch=4, cex = 1.7)
abline(reg6, lty=1)
abline(reg4, lty=4, lwd = 2)
axis(2, at=c(0,1,2,3),las=2, cex.axis=1.3)
legend("bottom", xpd = TRUE, horiz = TRUE, inset = c(0, -0.4), bty = "n", c("P. maculata","P. paludosa"), text.font=c(3), lty=c(1,4), lwd =c(1,2), pch=c(0,4), cex = 1.3)
