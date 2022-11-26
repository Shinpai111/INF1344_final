#installing required packages
install.packages("car")
require("dplyr")
library(car)


MCPD <- read.csv('insurance.csv', header =TRUE, sep = ',')

#set factors
MCPD$sex.f <- as.factor(MCPD$sex)
print(class(MCPD$sex.f))

MCPD$region.f <- as.factor(MCPD$region)
print(class(MCPD$region.f))

MCPD$children.f <- as.factor(MCPD$children)
print(class(MCPD$children.f))

result <- lm(charges ~ age + sex.f, data = MCPD)
print(summary(result))

result1 <- lm(charges ~ age + sex.f + children.f, data = MCPD)
print(summary(result1))

result2 <- lm(charges ~ bmi + children.f, data = MCPD)
print(summary(result1))

result3 <- lm(charges ~ bmi + sex.f, data = MCPD)
print(summary(result1))

result4 = aov(charges ~ age, data = MCPD)
summary(result4)

result5 = aov(charges ~ bmi, data = MCPD)
summary(result5)

ageL <- MCPD$charges[MCPD$age <= 39]
ageH <- MCPD$charges[MCPD$age > 39]
t.test(ageL, ageH)

chargesM <- MCPD$charges[MCPD$sex == "male"]
chargesF <- MCPD$charges[MCPD$sex == "female"]
t.test(chargesM, chargesF)

mean(MCPD$bmi)

MCPD$age.c <- ifelse(MCPD$age>39, "high", "low")
MCPD$bmi.c <- ifelse(MCPD$bmi>30, "high", "low")
View(MCPD)

anovaACS <- aov(charges ~ age.c * sex, data = MCPD)
Anova(anovaACS, type = "III")  # Not really sure should I use type 3 or other 2 types.
print(summary(anovaACS))

anova1 <- aov(charges ~ age.c * bmi.c, data = MCPD)
Anova(anova1, type = "III")
print(summary(anova1))
#anovaACS stands for ANOVA of "age", "charges", and "sex".
plot(anova1, 1)
plot(anovaACS, 1)
#Plotting the residuals versus fits plot to check the homogeneity of variance.

leveneTest(charges ~ sex.f*age.c, data = MCPD)
leveneTest(charges ~ bmi.c*age.c, data = MCPD)
#run a Levene's test to check the homogeneity of variances.

plot(anovaACS, 2)
plot(anova1, 2)
#plotting the normality plot of the residuals

anovaACS_residuals_1 <- residuals(object = anova1)
shapiro.test(x = anovaACS_residuals_1)

anovaACS_residuals <- residuals(object = anovaACS)
shapiro.test(x = anovaACS_residuals)
#extract the residuals and run Shapiro's test

group_by(MCPD, bmi.c, age.c) %>%
  summarise(
    count = n(),
    mean = mean(charges, na.rm = TRUE),
    sd = sd(charges, na.rm = TRUE)
  )

group_by(MCPD, sex.f, age.c) %>%
  summarise(
    count = n(),
    mean = mean(charges, na.rm = TRUE),
    sd = sd(charges, na.rm = TRUE)
  )
#This will give us the means and standard diviations for all four gourps.

tukey_test <- TukeyHSD(anovaACS)
print(tukey_test)

tukey_test <- TukeyHSD(anova1)
print(tukey_test)
