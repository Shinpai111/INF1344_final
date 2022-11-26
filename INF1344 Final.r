install.packages("car")
require("dplyr")
library(car)
#installing required packages

MCPD <- read.csv('insurance.csv', header =TRUE, sep = ',')
#reading the dataset
#dataset named MCPD, which stands for Medical Cost Personal Dataset
#noticing that there are 7 variables in this dataset, we will categorize them first
#native categorical variables: sex, smoker, region
#native numerical variables: age, bmi, children, charge
#for the sake of this project, I am conducting a preliminary exploratory data analysis
#the results here will be used to decide our resaerch question
#these comments ought to be remained internally within the group

MCPD
#displaying our dataset

range(MCPD$age)
#checking the maximum and minimum value for "age"

length(MCPD$age)
#checking the count of "age"

median(MCPD$age)
#checking the median of "age"

mean(MCPD$age)
#checking the mean of "age"

unique(MCPD$age)
#checking all the unique values of "age"

boxplot(MCPD$age,
        ylab = "age"
)
#checking if there are any outliers in "age"

length(MCPD$sex)
#checking the count of "sex"

unique(MCPD$sex)
#checking all the unique values of "sex"

range(MCPD$charges)
#checking the maximum and minimum value for "charges"

length(MCPD$charges)
#checking the count of "charges"

median(MCPD$charges)
#checking the median of "charges"

mean(MCPD$charges)
#checking the mean of "charges"

boxplot(MCPD$charges,
        ylab = "Personal Health Care Spend"
)
#checking if there are any outliers in "charges"

AgeChargescor <- cor.test(MCPD$age, MCPD$charges, method = "pearson")
AgeChargescor

MCPD$sex.f <- as.factor(MCPD$sex)

print(class(MCPD$sex.f))

lmACS <- lm(charges ~ age + sex.f, data = MCPD)
print(summary(lmACS))
#lmACS stands for linear model for the relationship between Age, Charges, and Sex.
#Great (or not great), age is significantly positively correlated with medical charges; however, sex is a significant control variable.

ageL <- MCPD$charges[MCPD$age <= 39]
ageH <- MCPD$charges[MCPD$age > 39]
t.test(ageL, ageH)

chargesM <- MCPD$charges[MCPD$sex == "male"]
chargesF <- MCPD$charges[MCPD$sex == "female"]
t.test(chargesM, chargesF)

MCPD$age.c <- ifelse(MCPD$age>39, "high", "low")
View(MCPD)

sum(MCPD$sex.f == 'male' & MCPD$age.c == "low")

sum(MCPD$sex.f == 'male' & MCPD$age.c == "high")

sum(MCPD$sex.f == 'female' & MCPD$age.c == "low")

sum(MCPD$sex.f == 'female' & MCPD$age.c == "high")

anovaACS <- aov(charges ~ age.c * sex, data = MCPD)
Anova(anovaACS, type = "III")
print(summary(anovaACS))
#anovaACS stands for ANOVA of "age", "charges", and "sex".

plot(anovaACS, 1)
#Plotting the residuals versus fits plot to check the homogeneity of variance.

leveneTest(charges ~ sex.f*age.c, data = MCPD)
#run a Levene's test to check the homogeneity of variances.

plot(anovaACS, 2)
#plotting the normality plot of the residuals

anovaACS_residuals <- residuals(object = anovaACS)
shapiro.test(x = anovaACS_residuals)
#extract the residuals and run Shapiro's test

group_by(MCPD, sex.f, age.c) %>%
  summarise(
    count = n(),
    mean = mean(charges, na.rm = TRUE),
    sd = sd(charges, na.rm = TRUE)
)
#This will give us the means and standard diviations for all four gourps.

tukey_test <- TukeyHSD(anovaACS)
print(tukey_test)
