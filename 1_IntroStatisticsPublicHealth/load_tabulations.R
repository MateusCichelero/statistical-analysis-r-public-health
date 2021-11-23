# Script for import cancer data for statistical thinking for public health mooc

g <- read.csv(file="cursos/coursera_estatisticaSudePublica/1_IntroStatisticsPublicHealth/cancer-data-for-MOOC-1-_1_.csv", header=TRUE, sep=',')

fruit <- g[,'fruit']
veg <- g[,'veg']

fruitveg <- fruit + veg
table(fruitveg)


summary(g$age)
hist(g$age)

table(g$smoking, exclude = NULL)

g$five_a_day <- ifelse(fruitveg >= 5, 1, 0)
table(g$five_a_day)


hist(fruitveg, xlab = "Portions of fruit and vegetables",
     main = "Daily consumption of fruit and vegetables combined")

hist(fruitveg, xlab = "Portions of fruit and vegetables",
     
     main = "Daily consumption of fruit and vegetables combined", axes = F)

axis(side = 1, at = seq(0, 11, 1))

axis(side = 2, at = seq(0, 16, 2))

require(ggplot2)
ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = "darkgreen", col = "black") +
  labs(x = "Portions of fruit and vegetables", y = "Frequency") +
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1)) + theme_bw()


g$healthy_BMI <- ifelse(g$bmi > 18.5 & g$bmi < 25, 1, 0)
table(g$healthy_BMI)

hist(fruit, xlab = "Portions of fruit",
     main = "Daily consumption of fruit", axes = F)
axis(side = 1, at = seq(0, 4, 1))
axis(side = 2, at = seq(0, 24, 4))

ggplot() + geom_histogram(data = g, aes(x = fruit), bins = 5, fill = "darkgreen", col = "black") +
  theme_bw() + labs(x = "Portions of fruit", y = "Frequency") +
  scale_x_continuous(breaks = seq(from = 0, to = 4, by = 1))




hist(veg, xlab = "Portions of vegetables",
     main = "Daily consumption of vegetables", axes = F)
axis(side = 1, at = seq(0, 9, 1))
axis(side = 2, at = seq(0, 18, 2))

ggplot() + geom_histogram(data = g, aes(x = veg), bins = 10, fill = "darkgreen", col = "black") + 
  theme_bw() + labs(x = "Portions of vegetables", y = "Frequency") + 
  scale_x_continuous(breaks = seq(from = 0, to = 9, by = 1))
