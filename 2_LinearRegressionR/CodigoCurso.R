#Material do Curso
# To check which directory you are working in: 

getwd() 



# To import the data set 

# you need to change the “file” location to where you’ve stored the data set  

COPD <- read.csv("COPD_student_dataset.csv")




# To have a look at the first few rows of our data set: 

head(COPD) 
colnames(COPD)

hist(COPD$MWT1Best, main="Histogram of MWT1Best", xlab="MWT1Best", breaks=12)


#Observando valor mais extremo
subset(COPD, MWT1Best > 650)
subset(COPD, MWT1Best > 600 | MWT1Best < 150) 

hist(COPD$FEV1, main="Histogram of FEV1", xlab="FEV1") 

#Statistics
list("Summary" = summary(COPD$MWT1Best), "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), "Range" = range(COPD$MWT1Best, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE)) 
list("Summary" = summary(COPD$FEV1), "Mean" = mean(COPD$FEV1, na.rm=TRUE), "Standard Deviation" = sd(COPD$FEV1, na.rm=TRUE), "Range" = range(COPD$FEV1, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$FEV1, na.rm=TRUE)) 

# Scatterplot
plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best") 

#Correlação
cor.test(COPD$FEV1,COPD$MWT1Best, use="complete.obs", method = "pearson")

cor.test(COPD$FEV1,COPD$MWT1Best, use="complete.obs", method = "spearman")

#Regressão MWT1Best(FEV1)
#lm(outcome ~ predictor, data =dataframe)
#new vector name<-linear regression model

MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)
summary(MWT1Best_FEV1)
#Intervalo confiança 95%
confint(MWT1Best_FEV1)

#ver 4 graficos de uma vez
par(mfrow=c(2,2)) 

#plots regressão checar critérios
plot(MWT1Best_FEV1)

#voltar a ver um grafico por vez
par(mfrow=c(1,1)) 


#Regressão MWT1Best(FEV1)
#lm(outcome ~ predictor, data =dataframe)
#new vector name<-linear regression model

MWT1Best_AGE <- lm(MWT1Best~AGE, data = COPD)
summary(MWT1Best_AGE)
#Intervalo confiança 95%
confint(MWT1Best_AGE)

#ver 4 graficos de uma vez
par(mfrow=c(2,2)) 

#plots regressão checar critérios
plot(MWT1Best_AGE)

#voltar a ver um grafico por vez
par(mfrow=c(1,1)) 

#Histograma residuos
residualVals <- residuals(MWT1Best_AGE)
hist(residualVals, main = "Histogram of residuals", xlab = "Residuals") 

#Multiple regression - Y = α + β1*X1 + β2*X2 + ε 
#Model name <- lm(outcome ~ predictor1 + predictor2, data =dataframe) 

MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE, data = COPD) 

summary(MWT1Best_FEV1_AGE)
confint(MWT1Best_FEV1_AGE) 

#assess key features of a dataset in R
dim(COPD) 

head(COPD) 

class(COPD$AGE) 

summary(COPD$AGE) 

hist(COPD$AGE) 

class(COPD$COPDSEVERITY) 

table(COPD$COPDSEVERITY, exclude = NULL) 


class(COPD$MWT1Best)

class(COPD$copd)

COPD$copd <- factor(COPD$copd) #converte para categórico
#If you want to change a variable to a numeric data type, use the command as.numeric(). 
#If you want to change a variable to a character data type, use the command as.character(). 
#If you want to change a variable to a integer data type, use the command as.integer(). 

#If you wanted to use the ‘severe’ group as a reference category for COPD severity (i.e. level 3), you can use the relevel() function: 
  
#COPD$copd <- relevel(COPD$copd, ref=3) 

#comorbid:
class(COPD$Diabetes)
COPD$Diabetes <- factor(COPD$Diabetes)
class(COPD$muscular)
COPD$muscular <- factor(COPD$muscular)
class(COPD$hypertension)
COPD$hypertension <- factor(COPD$hypertension)
class(COPD$AtrialFib)
COPD$AtrialFib <- factor(COPD$AtrialFib)
class(COPD$IHD)
COPD$IHD <- factor(COPD$IHD)

comorbid <- length(COPD$Diabetes) 

comorbid[COPD$Diabetes == 1 | COPD$muscular == 1 | COPD$hypertension == 1 | COPD$AtrialFib == 1 | COPD$IHD ==1] <- 1
comorbid[is.na(comorbid)] <- 0
comorbid <- factor(comorbid)

COPD$comorbid <- comorbid

describe(COPD)

#install.packages("Hmisc")
#library(Hmisc)
describe(COPD)

class(COPD$copd)
class(COPD$gender)
class(COPD$smoking) 
COPD$copd <- factor(COPD$copd)

COPD$gender <- factor(COPD$gender)

COPD$smoking <- factor(COPD$smoking)
describe(COPD$gender)

install.packages("gmodels")
library(gmodels)
CrossTable(COPD$gender)

hist(COPD$CAT)
COPD$CAT[COPD$CAT > 40] <- NA 

CrossTable(COPD$gender, COPD$IHD)
pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+CAT+HAD+SGRQ, data = COPD) 
my_data <-COPD[,c("AGE","PackHistory","FEV1","FEV1PRED","FVC","CAT","HAD","SGRQ")]
cor_matrix <- cor(my_data,method = "spearman")
cor_matrix
round(cor_matrix,4)

#multivariable regression
mlr1 <- lm(MWT1Best~FEV1+AGE+factor(gender)+factor(COPDSEVERITY)+factor(comorbid), data = COPD)
summary(mlr1)
confint(mlr1)

install.packages("mctest")
library(mctest)
imcdiag(model.matrix(mlr1)[,-1],y= mlr1[[model]][1],method='VIF')


COPD$Diabetes <- c(0,1)[as.integer(COPD$Diabetes)]
COPD$AtrialFib <- c(0,1)[as.integer(COPD$AtrialFib)]
r2 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(Diabetes*AtrialFib), data=COPD)
summary(r2)
confint(r2)

install.packages("prediction")
library(prediction)
list("Diabetes" = prediction(r2, at = list(Diabetes = c(0,1))),
     "AtrialFib" = prediction(r2, at = list(AtrialFib= c(0,1))),
     "Diabetes*AtrialFib" = prediction(r2, at = list(Diabetes = c(0,1), AtrialFib = c(0,1))))
