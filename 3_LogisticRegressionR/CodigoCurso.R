#Material do Curso
# To check which directory you are working in: 

getwd() 



# To import the data set 

# you need to change the “file” location to where you’ve stored the data set  

g <- read.csv("final-diabetes-data-for-R-_csv_-_2_.csv",header=TRUE, sep=',')

# tipo o shape
dim(g)

# lista nome colunas
colnames(g)

dimnames(g)[[2]]

# convertendo algumas variaveis importantes, R sempre vai pensar que é continuo
chol <- g["chol"] # cholesterol is continuous, so it’s easy
gender <- as.factor(g[,"gender"]) # but gender isn’t.
dm <- as.factor(g[,"dm"]) # neither is dm
hdl <- g["hdl"]

# visualizando genero
t <- table(gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results
round(prop.table(t),digits=3) # get proportions rounded to 3dp

round(100*prop.table(t),digits=1) # get %s rounded to 1dp


# para considerar missing values
dm2 <- factor(dm, exclude=NULL) # make new factor from the old one
table(dm2) # display the counts including the missings (NAs)

# para variaveis continuas, usamos summary(x)
summary(chol)


height <- g[,'height']
weight <- g[,'weight']
summary(height)
summary(weight)


# calculando imc
height.si <- height*0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2

summary(bmi)

# categorizando variáveis contínuas
# imc underweight [<18.5], normal [18.5-25], overweight [>25] and obese [>30]
bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# check that the bmi_categorised variable has worked  
table(bmi_categorised, exclude = NULL) 

# tabela cruzada: obesidade vs diabetes
# frequencies of diabetes by BMI category 
dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL) 

# check 
dm_by_bmi_category 

# with the row percentages 
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) 
# dá pra testar se obesidade está associada com diabetes fazendo um teste 
#estatístico de chi-quadrado

# --- teste
# age group
age <- g[,'age']
summary(age)

age_groups <- ifelse(age < 45, "under 45", 
                          ifelse(age >= 45 & age <= 64, "45-64", 
                                 ifelse(age > 64 & age <= 74, "65-74", 
                                        ifelse(age > 75, "75 or over", NA)))) 
table(age_groups, exclude = NULL)

gender_by_age_groups <- table(age_groups, gender, exclude = NULL)

#enter the number of females aged under 45 in this box: 
gender_by_age_groups
#Now enter the percentage of all patients who are male and aged 65-74 in this box, 
#rounded to the nearest whole percentage:   
round(100 * prop.table(gender_by_age_groups, margin = NULL), digits = 0)

# optional extra check for the extra cautious! 
head(cbind(age_groups, age)) 

# -------------

# PREMISSAS LOGISTIC REGRESSION:
# SE PREDITORA É CONTÍNUA (NÃO PRECISA SER DISTRIBUIÇÃO NORMAL): ASSUMO 
#QUE MINHA VARIAVEL RESPOSTA VARIA LINEARMENTE COM A PREDITORA (ex: idade,
#cada ano impacta com taxa constante a chace de ter diabetes)

# VERIFICAR ESSA PREMISSA PLOTANDO OS DADOS ANTES DE AJUSTAR O MODELO
# se parecer uma curva, tentar tranformar a preditora, usando termo quadrado

# comando glm, função logit, distribuição binomial

#----------- Practice in R: Simple Logistic Regression

#The simplest model we can fit is one with no predictors (MESMA CHANCE DE 
#DIABETES PARA TODOS)

m <- glm(dm ~ 1, family=binomial (link=logit))
summary(m)

table(m$y)
# Here, there’s just one coefficient: the intercept. R prints out all the 
# coefficients on the scale on which the algorithm did its magic, i.e. the 
# log scale in the case of logistic regression as we are modelling the log 
# odds of having diabetes. With this rather unexciting null model, we are 
# saying that the log odds of having diabetes is -1.7047 and that it’s the 
# same for every patient. What does that mean? To interpret this, we first 
# need to exponentiate it to get the odds of having diabetes. To do this, 
# type: 
exp(-1.7047)
# …and you’ll get 0.182 to three decimal places. If you prefer to work in 
# probabilities rather than odds, you can use the relation between odds and 
# probability that we established earlier to convert. So, just divide the 
# odds by 1 plus the odds, to give 0.182/1.182 = 0.15, or 15%. How do these 
# compare with the raw data? If you type… 
table(dm)
# Using these numbers, the odds of having diabetes is 60/330 = 0.182 and the probability 
# is 60/(330+60) = 0.15, both exactly the same as from the model, which is entirely as we 
# had expected (and hoped!).


# ONE PREDICTOR: GENDER 
#log odds of having diabetes differs by gender alone
m <- glm(dm ~ gender, family=binomial (link=logit))
summary(m)

#(como o modelo está lidando com genero)
contrasts(gender)

# female é usado como referencia, se quiser mudar isso, basta:
gender <- relevel(gender, ref = "male") 
levels(gender) 
#log(A/B) = −log(B/A)

m$coefficients #log odds ratios

exp(m$coefficients) #odds ratios

exp(m$coefficients)/(1+exp(m$coefficients)) #%

#CONTINUOUS: AGE
m <- glm(dm ~ age, family=binomial (link=logit))
summary(m)

# This time there are two: the intercept and one for age. Now, with a predictor 
# (age in this case) in the model, the intercept is no longer the overall crude log 
# odds but is instead the log odds of diabetes when age is zero. This follows from the 
# equation for the model:


# Log odds of having diabetes= intercept + (coefficient for age) * age in years
# 
# = -4.4045 + 0.0525 * age in years

# So how do we interpret the coefficient for age? It’s the increase in log odds of having diabetes 
# for a one-year increase in age. 

# When we exponentiate 0.0525 we get 1.05 (to two decimal places). This is an odds ratio, which is 
# generally what’s reported when running logistic regression models (and is generally reported to 
# two decimal places). It’s the ratio of the odds of having diabetes if you’re 25 divided by the 
# odds of having diabetes if you’re 24. Or that if you’re 75 divided by that if you’re 74 etc. 
# It’s the amount by which your odds increases when you get a year older. So getting older is bad 
# news, at least in terms of diabetes, which is what we expected. 


m$coefficients #log odds ratios

exp(m$coefficients) #odds ratios

exp(m$coefficients)/(1+exp(m$coefficients)) #%






#^ this assumes that the relation between age and the log odds of 
#having diabetes is linear. Check if that is true with the plot:

# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) 

# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 

# calculate the log odds 
logodds <- log(odds) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 



# TESTE: Interpreting Simple Logistic Regression
#Of those with a recorded diabetes status, what percentage of people from 
#Buckingham have diabetes?

location <- as.factor(g[,"location"])
table(location, exclude = NULL)

dm_by_location <-table(location, dm)
round(100 * prop.table(dm_by_location, margin = 1), digits = 1)

#Now fit a logistic regression with “location” as the predictor variable. 
# What are the log odds of having diabetes being from Louisa compared with Buckingham?  
#   Give the answer (the log odds ratio) to two decimal places.
levels(location) 

m <- glm(dm ~ location,family=binomial(link=logit))
summary(m)

m$coefficients #log odds ratios

exp(m$coefficients) #odds ratios

exp(m$coefficients)/(1+exp(m$coefficients)) #%



# ---------------- Multiple Regression
hist(age, breaks = 20)


d <- density(age) 
plot(d,main = "") # gives warnings but the “main” argument suppresses the ugly default title 

# visualizando genero
t <- table(gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results
round(prop.table(t),digits=3) # get proportions rounded to 3dp

summary(chol)
# lidar com valores faltantes antes de aplicar densidade
chol.no.na <- chol[is.na(chol)==0]
d <- density(chol.no.na)
plot(d,main = "") 

summary(hdl)
hdl.no.na <- hdl[is.na(hdl)==0]
d <- density(hdl.no.na)
plot(d,main = "") 

summary(bmi)
bmi.no.na <- bmi[is.na(bmi)==0]
d <- density(bmi.no.na)
plot(d,main = "") 


# define the gender variable 
gender <- as.factor(g[,"gender"]) 

# cross tabulation 
dm_by_gender <- table(gender, dm) # not including NA values because there aren't that many 

# proportion of diabetes status by gender 
dm_by_gender_prop <- prop.table(dm_by_gender, margin = 1) 

# calculate the odds of having diabetes by gender 
odds_gender <- dm_by_gender_prop[, "yes"]/dm_by_gender_prop[, "no"] 

# calculate the log odds 
logodds_gender <- log(odds_gender) 

# plot the log odds of having diabetes by gender 
dotchart(logodds_gender)

plot(as.factor(names(logodds_gender)), logodds_gender) 
#### NÃO MUITO ÚTIL


### AGE
# define the age variable (continuous) 
age <- age <- g[,"age"] 


# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by age 
dm_by_age_prop <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds_age <- dm_by_age_prop[, "yes"]/dm_by_age_prop[, "no"] 

# calculate the log odds 
logodds_age <- log(odds_age) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_age_prop), logodds_age) 


## IDADE AGRUPANDO
# age grouping converting continuous variable to a categorical (ordinal) one  
age_grouped <- ifelse(age < 45, "under 45", 
                      ifelse(age >= 45 & age < 65, "45 - 64",  
                             ifelse(age >= 65 & age < 75, "65 - 74",  
                                    ifelse(age >= 75, "75 or over", NA)))) 

age_grouped <- factor(age_grouped, levels = c("under 45", "45 - 64", "65 - 74", "75 or over")) 



# create a cross tabulation of age and diabetes status  
dm_by_age_grouped <- table(age_grouped, dm) 

# output the frequencies of diabetes status by age 
age_grouped_prop <- prop.table(dm_by_age_grouped, margin = 1) 

# calculate the odds of having diabetes 
odds_age_grouped <- age_grouped_prop[, "yes"]/age_grouped_prop[, "no"] 

# calculate the log odds 
logodds_age_grouped <- log(odds_age_grouped) 

# plot the age groups found in the sample against the log odds of having diabetes 
dotchart(logodds_age_grouped)


###### COLESTEROL
# define chol as a continuous variable 
chol <- g[,"chol"] 


# create a cross tabulation of cholesterol and diabetes status  
dm_by_chol <- table(chol, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by cholesterol 
dm_by_chol_prop <- prop.table(dm_by_chol, margin = 1) 

# calculate the odds of having diabetes 
odds_chol <- dm_by_chol_prop[, "yes"]/dm_by_chol_prop[, "no"] 

# calculate the log odds 
logodds_chol <- log(odds_chol) 

# plot the cholesterol found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_chol_prop), logodds_chol, xlim=c(150, 300)) 


# categorising chol into an ordinal variable 

# https://www.medicalnewstoday.com/articles/315900.php 
chol_categorised <- ifelse(chol < 200, "healthy",  
                           ifelse(chol < 240, "borderline high", 
                                  ifelse(chol >= 240, "high", NA))) 

# make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
chol_categorised <- factor(chol_categorised, levels = c("healthy", "borderline high", "high")) 




# create a cross tabulation of cholesterol and diabetes status  
dm_by_chol_categorised <- table(chol_categorised, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by cholesterol 
dm_by_chol_categorised_prop <- prop.table(dm_by_chol_categorised, margin = 1) 

# calculate the odds of having diabetes 
odds_chol_categorised <- dm_by_chol_categorised_prop[, "yes"]/dm_by_chol_categorised_prop[, "no"] 

# calculate the log odds 
logodds_chol_categorised <- log(odds_chol_categorised) 

# plot the cholesterol categories found in the sample against the log odds of having diabetes 
dotchart(logodds_chol_categorised)


##### BMI
#bmi 
height <- g[,"height"] 
weight <- g[,"weight"] 
height.si <- height*0.0254 
weight.si <- weight*0.453592 
bmi <- weight.si/height.si^2 


# categorising BMI 

bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
bmi_categorised <- factor(bmi_categorised, levels = c("underweight", "normal", "overweight","obese")) 

# create a cross tabulation of BMI and diabetes status  
dm_by_bmi_categorised <- table(bmi_categorised, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by BMI 
dm_by_bmi_categorised_prop <- prop.table(dm_by_bmi_categorised, margin = 1) 

# calculate the odds of having diabetes 
odds_bmi_categorised <- dm_by_bmi_categorised_prop[, "yes"]/dm_by_bmi_categorised_prop[, "no"] 

# calculate the log odds 
logodds_bmi_categorised <- log(odds_bmi_categorised) 

# plot the BMI categories found in the sample against the log odds of having diabetes 
dotchart(logodds_bmi_categorised) 




###### CORRELAÇÃO
# define chol as a continuous variable 
chol <- g[,"chol"] 
hdl <- g[,"hdl"] 
cor.test(x=chol,y=hdl,method="pearson") 

## How to Run Multiple Logistic Regression in R
gender <- relevel(gender, ref = "female") 

m <- glm(dm ~ age + gender + bmi, family=binomial (link=logit)) 
summary(m) 

# calculando intrervalo de confiança para razaão de chance
exp(confint(m)) 
# assim, para idade, the odds ratio is 1.06 with 95% CI 1.04 to 1.08-profile-likelihood limit


m$coefficients #log odds ratios

exp(m$coefficients) #odds ratios

exp(m$coefficients)/(1+exp(m$coefficients)) #%

## TESTE Running A New Logistic Regression Model
colnames(g)
insurance <- as.factor(g[,"insurance"])
table(insurance)

summary(chol)

#fit model
m <- glm(dm~age+chol+insurance,family = binomial(link = "logit"))
summary(m)

exp(m$coefficients)
exp(confint(m)) 

### SEMANA 4 predictive power vs goodness-of-fit

# The other approach to evaluating model fit is to compute what’s called 
# a goodness-of-fit statistic. These kind of measures include the deviance 
# and the popular Hosmer-Lemeshow statistic. 

# R-squared measures
# It turns out that there are many ways to approximate an R-squared for logistic 
# regression. One of the best ways is with the McFadden (pseudo) R-squared
# 
#Deviance
#The two well-known statistics for comparing the observed number with the 
#expected number are the deviance and Pearson’s chi-square
#funciona bem quando é possivel ter um profiling (variaveis categoricas)
#quando os dados forem espalhados e não se agrupam, podemos usar o Hosmer-
#Lemeshow statistic.

#Calibration: Hosmer-Lemeshow statistic and test
#Here, patients are grouped together according to their predicted values 
# from the model, which are ordered from lowest to highest and then separated 
# into typically ten groups of roughly equal size. For each group, we calculate 
# the observed number of events – here that’s the number of patients with diabetes 
# – and the expected number of events, which is just the sum of the predicted 
# probabilities for all the patients in the group. Pearson’s chi-square test is 
# then applied to compare observed counts with expected counts. A large p value 
# (e.g. above the conventional 0.05) indicates that the model’s predicted values 
# are a good match for the real (observed) values, i.e. the model is a good fit.

#EXEMPLOS
#McFadden’s r-squared:
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 

# check your model 
summary(full_model) 
##  
## Call: 
## glm(formula = dm ~ age + chol + insurance, family = binomial(link = logit)) 
##  
## Deviance Residuals:  
## 	Min   	1Q   Median   	3Q  	Max   
## -1.5714  -0.5945  -0.3992  -0.2619   2.4399   
##  
## Coefficients: 
##          	Estimate Std. Error z value Pr(>|z|)     
## (Intercept) -5.794252   0.874555  -6.625 3.46e-11 *** 
## age      	0.049753   0.009770   5.092 3.54e-07 *** 
## chol     	0.008402   0.003153   2.665   0.0077 **  
## insurance1  -0.271955   0.359445  -0.757   0.4493     
## insurance2  -0.589803   0.377434  -1.563   0.1181     
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##  
## (Dispersion parameter for binomial family taken to be 1) 
##  
## 	Null deviance: 334.54  on 388  degrees of freedom 
## Residual deviance: 289.28  on 384  degrees of freedom 
##   (14 observations deleted due to missingness) 
## AIC: 299.28 
##  
## Number of Fisher Scoring iterations: 5 
# run a null model 
null_model <- glm(dm ~ 1, family=binomial (link=logit)) 

# check 
summary(null_model) 
##  
## Call: 
## glm(formula = dm ~ 1, family = binomial(link = logit)) 
##  
## Deviance Residuals:  
##	Min  	1Q  Median  	3Q 	Max   
## -0.578  -0.578  -0.578  -0.578   1.935   
##  
## Coefficients: 
##         	Estimate Std. Error z value Pr(>|z|)     
## (Intercept)  -1.7047 	0.1403  -12.15   <2e-16 *** 
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##  
## (Dispersion parameter for binomial family taken to be 1) 
##  
## 	Null deviance: 334.87  on 389  degrees of freedom 
## Residual deviance: 334.87  on 389  degrees of freedom 
##   (13 observations deleted due to missingness) 
## AIC: 336.87 
##  
## Number of Fisher Scoring iterations: 3 
# calculate McFadden's R-square 
R2 <- 1-logLik(full_model)/logLik(null_model) 

# print it 
R2 
## 'log Lik.' 0.1361385 (df=5)   
  
###
#c-statistic:

# install a package 
install.packages("DescTools") 
## Installing package into '' 
## (as 'lib' is unspecified) 
## package 'DescTools' successfully unpacked and MD5 sums checked 
##  
## The downloaded binary packages are in 
##   
# load package 
require(DescTools) 
## Loading required package: DescTools 
## Warning: package 'DescTools' was built under R version 3.5.1 
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 

# check your model 
summary(full_model) 
##  
## Call: 
## glm(formula = dm ~ age + chol + insurance, family = binomial(link = logit)) 
##  
## Deviance Residuals:  
## 	Min   	1Q   Median   	3Q  	Max   
## -1.5714  -0.5945  -0.3992  -0.2619   2.4399   
##  
## Coefficients: 
##          	Estimate Std. Error z value Pr(>|z|)     
## (Intercept) -5.794252   0.874555  -6.625 3.46e-11 *** 
## age      	0.049753   0.009770   5.092 3.54e-07 *** 
## chol     	0.008402   0.003153   2.665   0.0077 **  
## insurance1  -0.271955   0.359445  -0.757   0.4493     
## insurance2  -0.589803   0.377434  -1.563   0.1181     
## --- 
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##  
## (Dispersion parameter for binomial family taken to be 1) 
##  
## 	Null deviance: 334.54  on 388  degrees of freedom 
## Residual deviance: 289.28  on 384  degrees of freedom 
##   (14 observations deleted due to missingness) 
## AIC: 299.28 
##  
## Number of Fisher Scoring iterations: 5 
# generate the c-statistic 
Cstat(full_model) 
## [1] 0.764387 




#######
#Hosmer-Lemeshow statistic and test: 

# H-L test 

# install package "ResourceSelection" 
install.packages("ResourceSelection") 
## Installing package into '' 
## (as 'lib' is unspecified) 
## package 'ResourceSelection' successfully unpacked and MD5 sums checked 
##  
## The downloaded binary packages are in 
##   
# load package 
require(ResourceSelection) 
## Loading required package: ResourceSelection 
## Warning: package 'ResourceSelection' was built under R version 3.5.1 
## ResourceSelection 0.3-2   2017-02-28 
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit)) 

full_model$y


# run Hosmer-Lemeshow test 
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10) 
HL  
##  
##  Hosmer and Lemeshow goodness of fit (GOF) test 
##  
## data:  full_model$y, fitted(full_model) 
## X-squared = 11.25, df = 8, p-value = 0.1879 
# plot the observed vs expected number of cases for each of the 10 groups 
plot(HL$observed[,"y1"], HL$expected[,"yhat1"]) 

# plot the observed vs expected number of noncases for each of the 10 groups 
plot(HL$observed[,"y0"], HL$expected[,"yhat0"]) 

# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
     y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"])) 



##  
##  Hosmer and Lemeshow goodness of fit (GOF) test 
##  
## data:  full_model$y, fitted(full_model) 
## X-squared = 11.25, df = 8, p-value = 0.1879 
# verify result with another package? 

# install package("generalhoslem") 
install.packages("generalhoslem") 
## Installing package into '' 
## (as 'lib' is unspecified) 
## package 'generalhoslem' successfully unpacked and MD5 sums checked 
##  
## The downloaded binary packages are in 
##   
# load package 
require(generalhoslem) 
## Loading required package: generalhoslem 
## Warning: package 'generalhoslem' was built under R version 3.5.1 
## Loading required package: reshape 
## Warning: package 'reshape' was built under R version 3.5.1 
## Loading required package: MASS 
# run Hosmer-Lemeshow test 
logitgof(obs = full_model$y, exp = fitted(full_model), g = 10) 
##  
##  Hosmer and Lemeshow test (binary model) 
##  
## data:  full_model$y, fitted(full_model) 
## X-squared = 11.25, df = 8, p-value = 0.1879 


############################################################
#ANALISANDO NULL DEVIANCE VS RESIDUAL DEVIANCE (retorno - explicação da variação 
#no resultado vs investimento - mais graus de liberdade das variáveis preditoras
#incluidas)

# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit)) 

# analyse table of deviance 
#To test whether each added parameter increases the deviance by a significant 
#amount, we asked R to compare it with a chi-square value for the number of 
#degrees of freedom lost.
anova(full_model, test = "Chisq") 

#AIC This is short for Akaike Information Criterion and measures the quality 
#of a model in terms of the amount of information lost by that model.
#It’s of no use by itself but is used for comparing two or more models. 
#Small AIC values are best.


#####################################################
##### Make the variables and run the models #####

dm <- as.factor(g[,"dm"]) 
insurance <- as.factor(g[,"insurance"])# let's say 0=none, 1=gov, 2=private 
fh <- as.factor(g[,"fh"]) # 1=FH, 0=no FH 
smoking <- as.factor(g[,"smoking"]) # 1,2,3 
chol <- g[,'chol'] 
hdl <- g[,'hdl'] 
ratio <- g[,'ratio'] 
location <- as.factor(g[,'location']) 
age <- g[,'age'] 
gender <- as.factor(g[,'gender']) 
frame <- as.factor(g[,'frame']) 
systolic <- g[,'bp.1s'] 
diastolic <- g[,'bp.1d'] 

model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit)) 

summary(model)

### sem pressões

model <- glm(dm ~ age + bmi + chol + hdl, family = binomial(link = logit)) 

summary(model)


#Have any of the coefficients for the four remaining variables changed? Not much, which
# is good. But why is blood pressure not significant here despite what the literature 
# says? One way to find out is to see if it correlates with other variables. Here's the 
# code to do that and the output.

cor.test(systolic, hdl) # not significant 
cor.test(systolic, bmi) # significant 
cor.test(systolic, chol) # very significant
cor.test(systolic, age) # extremely significant 

# testando sem idade, mas com pressões
model <- glm(dm ~  bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit)) 

summary(model)

###Practice in R: Run a Model with Different Predictors

model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic + gender + location + frame + insurance + smoking, family = binomial(link = logit)) 

summary(model)
