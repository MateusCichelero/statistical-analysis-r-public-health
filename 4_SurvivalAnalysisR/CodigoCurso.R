getwd()

g <- read.csv(file = "simulated-HF-mort-data-for-GMPH.csv", header=TRUE, sep=',')

dim(g)

names(g)

head(g)

# death (0/1)
# 
# los (hospital length of stay in nights)
# 
# age (in years)
# 
# gender (1=male, 2=female)
# 
# cancer
# 
# cabg (previous heart bypass)
# 
# crt (cardiac resynchronisation device - a treatment for heart failure)
# 
# defib (defibrillator implanted)
# 
# dementia
# 
# diabetes (any type)
# 
# hypertension
# 
# ihd (ischaemic heart disease)
# 
# mental_health (any mental illness)
# 
# arrhythmias
# 
# copd (chronic obstructive lung disease)
# 
# obesity
# 
# pvd (peripheral vascular disease)
# 
# renal_disease
# 
# valvular_disease (disease of the heart valves)
# 
# metastatic_cancer
# 
# pacemaker
# 
# pneumonia
# 
# prior_appts_attended (number of outpatient appointments attended in the previous year)
# 
# prior_dnas (number of outpatient appointments missed in the previous year)
# 
# pci (percutaneous coronary intervention)
# 
# stroke (history of stroke)
# 
# senile
# 
# quintile (socio-economic status for patient's neighbourhood, from 1 (most affluent) to 5 (poorest))
# 
# ethnicgroup (see below for categories)
# 
# fu_time (follow-up time, i.e. time in days since admission to hospital) 
# 
# Ethnic group has the following categories in this extract:
# 
# 1=white 
# 
# 2=black 
# 
# 3=Indian subcontinent 
# 
# 8=not known 
# 
# 9=other


install.packages("survival")
install.packages("ggplot")

library(survival) # this is the cornerstone command for survival analysis in R
library(ggplot2) # newer package that does nice plots


gender <- as.factor(g[,"gender"]) # R calls categorical variables factors
fu_time <- g[,"fu_time"] # continuous variable (numeric) 
death <- g[,"death"] # binary variable (numeric) 
age <- g[,"age"]

km_fit <- survfit(Surv(fu_time, death) ~ 1)

plot(km_fit)

summary(km_fit, times = c(1:7,30,60,90*(1:10))) 


####Now let’s extend this by splitting the curve by gender: 
km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 
plot(km_gender_fit)
# comparando curvas de sobrevivência (logrank test)
#With rho = 0, which is the default so we don’t need to 
#write this bit, it yields the log-rank or Mantel-Haenszel 
#test
survdiff(Surv(fu_time, death) ~ gender, rho=0) 


#teste: dicotomizando idade
# categorise BMI by category 
age_groups <- ifelse(age < 65, "non-elderly", 
                          ifelse(age >= 65, "elderly", NA)) 
table(age_groups)

km_age_group_fit <- survfit(Surv(fu_time, death) ~ age_groups) 
plot(km_age_group_fit)

survdiff(Surv(fu_time, death) ~ age_groups, rho=0) 


#### Week 2: Cox proportional hazards model.
#With this method you will be able to compare 
#the survival of multiple groups of patients at the same time.
#https://data.princeton.edu/wws509/notes/c7s1

library(survival)
install.packages("survminer")
library(survminer)


ethnicgroup <- factor(g[,"ethnicgroup"]) # can also use “as.factor” rather than “factor”
fu_time <- g[,"fu_time"]
death <- g[,"death"]
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

#assumindo os NAs como uma categoria em si 
levels(ethnicgroup)<-c(levels(ethnicgroup),"8") # add level 8 to the factor

ethnicgroup[is.na(ethnicgroup)] <- "8" # Change NA to "None"

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup) 
summary(cox) 

## explorando variaveis
# Age (assumed to be continuous)
summary(age)
# 
# Gender
table(gender, exclude=NULL)
# 
# Prior OPD appointments missed (“prior_dnas”)
prior_dnas <- g[,"prior_dnas"] 
summary(prior_dnas)
t <- table(prior_dnas, exclude=NULL) 
addmargins(t) # adds the total (a "sum" column) 
round(100*prop.table(t),digits=1) # get %s rounded to 1dp 


# 
# Ethnic group
table(ethnicgroup, exclude=NULL)
# 
# COPD (chronic obstructive pulmonary disease)
copd <- factor(g[,"copd"])
t <- table(copd, exclude=NULL)

addmargins(t) # adds the total (a "sum" column)

round(100*prop.table(t),digits=1) # get %s rounded to 1dp


#How to run multiple Cox model in R
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + prior_dnas + ethnicgroup)
summary(cox)


### como saber quantas categorias separar uma variável numérica? Olha pro intervalo de confiança,
#se não ficar gigante, tá bom


#Practice in R: Running a multiple Cox model that doesn't converge
quintile <- factor(g[,"quintile"])

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)
summary(cox)

#problema está no quintile
table(quintile, exclude=NULL) 
t <- table(quintile,death) 
t
round(100*prop.table(t,1),digits=1)
# trocar a variavel de referencia, ou excluir os casos da categoria que deu erro se forem
#poucos exemplos

#1. trocando a categoria de referencia:
quintile <- relevel(quintile, ref = "1") # make the reference category quintile=1
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)
summary(cox)

#2. trocando a categoria de referencia:
quintile_5groups <- g[,"quintile"] # best start again with the original data set, not from the existing object called “quintile” 

quintile_5groups[quintile_5groups==0] <- 5 # This picks the individuals with quintile=0 (note the double equals sign) and sets them to 5

quintile_5groups <- factor(quintile_5groups) # lastly, tell R that this is a categorical variable and not a continuous one

table(quintile_5groups,exclude=NULL)
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup)
summary(cox)

#2. sem a categoria 0:
quintile_withoutZero <- g[,"quintile"] # best start again with the original data set, not from the existing object called “quintile” 
quintile_withoutZero[quintile_withoutZero==0] <- NA # This picks the individuals with quintile=0 (note the double equals sign) and sets them to 5
quintile_withoutZero <- factor(quintile_withoutZero) # lastly, tell R that this is a categorical variable and not a continuous one
table(quintile_withoutZero,exclude=NULL)

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_withoutZero + ethnicgroup)
summary(cox)

#2. sem a coluna quantile:
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + ethnicgroup)
summary(cox)

### COMO SABER SE O MODELO É BOM? VERIFICANDO RESÍDUOS
#Schoenfeld residuals -> para comparar hazard ratios entre dois grupos, as duas
# hazard functions tem que ser proporcionais/ paralelos, esse reíduo pode ajudar a verificar isso
# linha reta horizintal é o que queremos
# p value grande é o que queremos para testar essa premissa
# hazard porportional -> unique to Cox model (precisa plotar em escala log)

# testando para gênero
## o default do modelo é esse cox.zph(fit, transform="km", global=TRUE)
fit <- coxph(Surv(fu_time, death) ~ gender) # fit the desired model
temp <- cox.zph(fit)# apply the cox.zph function to the desired model
print(temp) # display the results
plot(temp) # plot the curves

km_fit <- survfit(Surv(fu_time, death) ~ gender) 
autoplot(km_fit)
plot(km_fit, xlab = "time", ylab = "Survival probability") # label the axes 

# Maringale residual: ajuda a verificar se preditor contínuo, como idade, tem relação linear
# com outcome (se premissa é válida, grafico deve dar linha reta)

# Another issue is whether any continuous variables that you assume to have a linear relation with the outcome 
# actually do have a linear relation. If you fit age as a single term in the model, then that’s what you’re assuming
# . The martingale residual is used to test this assumption: 
ggcoxfunctional(Surv(fu_time, death) ~ age + log(age) + sqrt(age), data = g) 

# ggcoxfunctional() is part of the survminer R package. Martingale residuals may 
# present any value between minus infinity and 1) and have a mean of zero:
#   
#   Martingale residuals near 1 represent individuals that “died too soon”
# 
# Large negative values correspond to individuals that “lived too long”
# 
# The plots should give you nice straight line if the assumption is valid. 


#Deviance residual: para achar influential points - pontos discrepantes que tem impacto
#grande nos coefiecientes do modelo (hazard ratios) - patients with unusual data
# Deviance residuals are transformations of martingale residuals and help you look for 
# outliers or influential data points. You can either examine the influence of each data point 
# on the coefficients or plot the distribution of the residuals against the covariate. 

# Specifying the argument type = “dfbeta” plots the estimated changes in the regression coefficients 
# on deleting each observation (patient) in turn: 
res.cox <- coxph(Surv(fu_time, death) ~ age) 
ggcoxdiagnostics(res.cox, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw()) 


# It’s also possible to check outliers by visualizing the deviance residuals, which are normalized 
# transformations of the martingale residual and should be roughly symmetrically distributed about 
# zero with a standard deviation of 1. If you remember the normal distribution, then 5% of observations 
# are more than 1.96 standard deviations from the mean. So if the SD is 1, then only 5% of observations 
# should be bigger than 1.96 or more negative than -1.96. If you have more than that proportion, then your 
# model doesn’t fit the data as well as it should and some observations are a problem. This is just the same
# issue as with the other types of regression. The maths behind the calculation of the residuals is different, 
# mostly because of the censoring, but we don’t need to worry about that.
# 
# Positive values correspond to individuals that “died too soon” compared with expected survival times.
# 
# Negative values correspond to individual that “lived too long” compared with expected survival times.
# 
# Very large or small values are outliers, which are poorly predicted by the model. 


res.cox <- coxph(Surv(fu_time, death) ~ age) 
ggcoxdiagnostics(res.cox, type = "deviance", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 
#dev.off() #para resolver o problema do plot

####
##Testing the proportionality assumption with another variable - COPD
fit <- coxph(Surv(fu_time, death) ~ copd) # fit the desired model
temp <- cox.zph(fit)# apply the cox.zph function to the desired model
print(temp) # display the results
plot(temp) # plot the curves

km_fit <- survfit(Surv(fu_time, death) ~ copd) 
#autoplot(km_fit)
plot(km_fit, xlab = "time", ylab = "Survival probability") # label the axes 



######## O QUE FAZER SE PROPORCIONALIDADE NÃO FOR ATENDIDA?
#pode ser que exista uma influencia estatísticamente relevante do tempo na variável predito
#uma possível solução é incluir um termo que represente esse fator temporal:
#Incluindo termos
fit <- coxph(Surv(fu_time, death) ~ gender + tt(gender)) # "tt" is the time-transform function 
summary(fit) 
# This output agrees with the earlier approach and says that the interaction between gender 
# and (transformed) time is not statistically significant, i.e. there’s no apparent violation 
# of the proportionality assumption. Again, good news. The p-value from this approach (about 0.5)
# isn’t the same as that from the earlier one because the methods are different, though it’s always
# nice when they give the same message!



# How to apply backwards elimination
# 
# Here are the steps:
#   
#   Fit the model containing all your chosen predictors – either all your a priori ones or all your 
# available ones (if your data set isn’t too large)
# 
# Store all the coefficients from that model
# 
# Remove in one go all predictors whose p value is above the preset threshold, typically the usual 0.05 
# (in a variant of this, you remove the predictor with the highest p value and refit the model, repeating 
#   steps until all the predictors have p values above the chosen threshold)
# 
# Compare the coefficients for the remaining predictors with their coefficients from the original model

