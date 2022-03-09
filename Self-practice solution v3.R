library(tidyverse)

data <- healthstatus5 %>%
  select(age:hba1c)

# Describe Age
# 1. Visualise the curve
hist(data$age) ## Frequency histogram
hist(data$age, probability = TRUE)
curve(dnorm(x, mean = mean(data$age), sd = sd(data$age)), add= T)

# 2. Describe the moments value
library(psych)
describe(data$age)

# Describe
# Mean age is 42 (8.9) years

# Describe Sex
table(data$sex)

library(gtsummary)
data %>% 
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})"
  )

# Data transformation

## Create BMI

data$bmi <- data$wt/(data$ht/100)^2
data$bmi_status <- cut(data$bmi, 
                       c(0, 25, 30, Inf),
                       labels = c("Normal", "Overweight", "Obese"))

table(data$bmi_status)

data %>% 
  select(bmi_status) %>% 
  tbl_summary()

## Create blood pressure

data$bp <- ifelse(data$sbp >= 140 | data$dbp >= 90,
                  "High",
                  "Normal")

table(data$bp)

data %>% 
  select(bp) %>% 
  tbl_summary()

# Create Table 1

data %>% 
  select(age, sex, exercise, smoking, bmi_status, bp, hba1c) %>% 
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})"
  )


# Sex vs BMI Status
table1 <- table(data$sex, data$bmi_status)

prop.table(table1, 1)*100
chisq.test(table1, correct = F)

# Obese are seen more among male (24%) compared to female (6%) (p=0.007)

data %>% 
  select(sex, bmi_status) %>% 
  tbl_summary(by = sex) %>% 
  add_p()

# HbA1c vs BP

num1 <- data$hba1c ~ data$bp
var.test(num1)
t.test(num1)

data %>% 
  select(bp, hba1c) %>% 
  tbl_summary(
    by = bp,
    statistic = all_continuous() ~ "{mean} ({sd})") %>% 
  add_p(test.args = all_tests("t.test") ~ list(var.equal = T))


# HbA1c vs BMI Status

anova <- aov(hba1c ~ bmi_status, data)
summary(anova)

data %>% 
  select(hba1c, bmi_status) %>% 
  tbl_summary(by = bmi_status,
              statistic = hba1c ~ "{mean} ({sd})") %>% 
  add_p(hba1c ~ "aov")

# Age vs HbA1c

cor.test(data$sbp, data$dbp, method = "pearson")




