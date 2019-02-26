#loading necessary packages
library(tidyverse)
library(broom)
library(ggfortify)
library(pwr)
library(DAAG)
library(GGally)
theme_set(theme_classic())

#Read the CSV file
geo <- read.csv("project_v3.csv")
View(geo)
dim(geo)

#Structure of variables
str(geo)

#Descriptive statistics for Continent
summary(geo$Continent)
plot(geo$Continent, xlab = "Continents", ylab = "Frequency", main = "Distribution of Continents", ylim=c(0,40))

#Descriptive statistics for Did you take geo in school
summary(geo$Did.you.take.Geography.in.school.)
plot(geo$Did.you.take.Geography.in.school., ylab = "Frequency", xlab = "Did you study Geography in School", main = "Distribution of previous Geographic Education")

#Descriptive statistics for Age
summary(geo$Age)
plot(geo$Age)
sd(geo$Age)
mode(geo$Age)
boxplot(geo$Age, xlab="age",ylab="Frequency")
hist(geo$Age, xlab = "Age of students", ylab = "Frequency", main = "Distribution of Age", labels = TRUE, ylim=c(0, 45))

#Descriptive statistics for Education level
summary(geo$Education.level)
plot(geo$Education.level, xlab = "Education Level", ylab = "Frequency", main = "Distribution of Education level")

#Descriptive statistics for Score
summary(geo$Score)
sd(geo$Score)
mode(geo$Score)
boxplot(geo$Score, xlab="Score",ylab="Frequency")
hist(geo$Score, xlab = "Score", ylab = "Frequency", main = "Distribution of Score", labels = TRUE, ylim=c(0, 35))

#Plotting Outcome vs Predictor Variables
plot(geo$Score, geo$Continent)

#Full model summary 
full_m <- lm(Score~Age + Continent + Education.level + Did.you.take.Geography.in.school., data=geo)
summary(full_m)

#checking collinearity
ggpairs(geo[,6:9])

#To check the variance and  nearly normal residues
full_m.diag.metrics <- augment(full_m)
head(model.diag.metrics)
plot(full_m)

# Checking linearity of residual vs each variable
plot.default(full_m$residuals~geo$Age,xlab="Age", ylab="Residuals")
abline(h = 0, lty = 3)

boxplot(full_m$residuals~geo$Continent, xlab="Continent", ylab="Residuals")
abline(h = 0, lty = 3)

boxplot(full_m$residuals~geo$Education.level,xlab="Education level", ylab="Residuals")
abline(h = 0, lty = 3)

plot.default(full_m$residuals~geo$Did.you.take.Geography.in.school.,xlab="Exprience with geography", ylab="Residuals")
abline(h = 0, lty = 3)

# Backward selection of model selection

null=lm(Score~1, data=geo)
full=lm(Score~Age + Continent + Education.level + Did.you.take.Geography.in.school., data=geo)
step(full,data=geo, direction="backward")

final_m <- lm(Score~Continent,data=geo)
summary(final_m)

# Calculating power for best fit model
pwr.f2.test(u = 2, v = 97 - 2 - 1, f2 = 0.612, sig.level = 0.05)





