# Script for import cancer data for statistical thinking for public health mooc

g <- read.csv(file="cursos/coursera_estatisticaSudePublica/1_IntroStatisticsPublicHealth/cancer-data-for-MOOC-1-_1_.csv", header=TRUE, sep=',')

fruit <- g[,'fruit']
veg <- g[,'veg']

fruitveg <- fruit + veg
table(fruitveg)

g$five_a_day <- ifelse(fruitveg >= 5, 1, 0)
table(g$five_a_day)

# Teste Chi-quadrado para porções de frutas vs cancer
chisq.test(x=g$five_a_day,y=g$cancer) 

# Teste t-student para comparar as medias de bmi entre amostras com cancer e não cancer
t.test(g$bmi~g$cancer)
t.test(g$bmi,mu=25) # the null hypothesis here is that the mean BMI is 25

# Teste para sobrepeso vs cancer
g$overweight <- ifelse(g$bmi >= 25, 1, 0)
head(g)

# Teste Chi-quadrado para sobrepeso vs cancer
chisq.test(x=g$overweight,y=g$cancer) 
