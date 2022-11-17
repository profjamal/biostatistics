# Set the working directory
setwd("~/iCloud Drive (Archive)/~My Documents/~Academic/Courses Conducted/~Biostatistics Workshop/Biostatistics with R/Basic Biostatistics with R/Workshop")

# PART 1 - PREPARING THE DATA

# Part 1.1A - BASIC DATA CREATION

col1 <- c(20, 30, 40, 20, 50)
col1

col2 <- c("Male", "Male", "Female", "Female", "Male")
col2

data <- data.frame(col1, col2)
data

colnames(data) <- c("Age", "Sex")
data

# Part 1.1B - MORE COMPLETE DATAFRAME CREATION

set.seed(1234)
data <- cbind(1:120, #id
              rnorm(120,40,5), #age
              rbinom(120,1,.4), #sex with 1 = female
              rnorm(120,130,10), #sbp
              rnorm(120,80,10) #dbp
              )
### This will overwrite the previous "data"

### You can also round the numbers for all numerical variables
data <- cbind(1:120, 
              round(rnorm(120,40,5), digits=1),
              rbinom(120,1,.4),
              round(rnorm(120,130,10)),
              round(rnorm(120,80,10))
)

data <- as.data.frame(data)

colnames(data) <- c("ID", "Age", "Sex", "SBP", "DBP")
data$Sex <- factor(data$Sex, 
                   c(0,1), 
                   labels = c("Female", "Male") 
                   )

# Part 1.2 - EXPORT DATA

# How to export data to text or csv

write.csv(data, "data.csv") # Will save into the working directory


# Part 1.3 - IMPORT (READ) EXTERNAL DATA

training <- read.csv("https://raw.githubusercontent.com/profjamal/biostatistics/main/training.csv")


# Part 1.4 - PREPARE THE DATA
attach(training)

# Assign proper data type for each variable
training$Sex <- factor(training$Sex)  
training$Intervention <- factor(training$Intervention, levels = c(0,1), labels = c("Diet", "Diet & Exercise"))
training$Smoking2 <- factor(training$Smoking, levels = c(0,1), labels = c("No", "Yes")) 
## To show the different between values vs. value labels


# PART 2 - CHECKING THE DATA

# Checking distribution for (selected) numerical variables

## Visually
hist(Age)

hist(training$Age)
hist(training$FBS)
hist(training$FBS_post)

### Display as composite images
par(mfrow= c(1,3))

### Repeat Visual inspection
hist(training$Age)
hist(training$FBS)
hist(training$FBS_post)

# Showing Normal curve
hist(training$Age, probability = TRUE)
curve(dnorm(x, mean=mean(training$Age), sd=sd(training$Age)), add=TRUE) 

hist(training$FBS, probability = TRUE)
curve(dnorm(x, mean=mean(training$FBS), sd=sd(training$FBS)), add=TRUE) 

hist(training$FBS_post, probability = TRUE)
curve(dnorm(x, mean=mean(training$FBS_post), sd=sd(training$FBS_post)), add=TRUE) 

# Q-Q Plot
qqnorm(training$Age)
qqline(training$Age)

qqnorm(training$FBS)
qqline(training$FBS)

qqnorm(training$FBS_post)
qqline(training$FBS_post)

## Showing the moment values
library(psych)
describe(training$Age)
describe(training$FBS)
describe(training$FBS_post)

# Normality test
shapiro.test(training$Age)
shapiro.test(training$FBS)
shapiro.test(training$FBS_post)

#PART 3 - DESCRIPTIVE STATISTICS

# Describe categorical variable (Sex)
table(training$Sex)
prop.table(table(Sex))
prop.table(table(Sex))*100
round(prop.table(table(training$Sex))*100, digit=1)

par(mfrow= c(1,1))
plot(training$Sex) # Bar chart for the count

# Describe categorical variable (Smoking)
table(training$Smoking2)
prop.table(table(Smoking2))
prop.table(table(Smoking2))*100
round(prop.table(table(training$Smoking2))*100, digit=1)

par(mfrow= c(1,1))
plot(training$Smoking2) # Bar chart for the count


# Describe numerical variables (Age, SBP & DBP)
library(psych)

describe(training$Age) # Age (SD) = 39 (5) years old
describe(training$SBP)
describe(training$DBP)

# Using gtsummary to describe all variables
library(gtsummary)
library(tidyverse)

# The simplest format
training %>%
  tbl_summary()

tbl_summary(training)

# A better format
training %>% 
  tbl_summary(
    statistic = list(all_continuous()  ~ "{mean} ({sd})", 
                     all_categorical() ~ "{n}/{N} ({p}%)"),
    digits    =      all_continuous()  ~ 1
  )

# Even a better format
training %>% 
  select(Age, Sex, Smoking2, SBP, DBP, FBS, FBS_post) %>% 
  tbl_summary(
    statistic = list(c(Age, SBP, DBP)                ~ "{mean} ({sd})",
                     c(Sex, Smoking2, FBS, FBS_post) ~ "{median} ({p25}, {p75})",
                     all_categorical()               ~ "{n}/{N} ({p}%)"),
    digit = all_continuous() ~ 1
    )

# CATEGORISE NUMERICAL INTO CATEGORICAL VARIABLES
## Create BP from SBP & DBP
training$BP <- factor(ifelse(training$SBP>=140 | training$DBP >=90, "High", "Normal"))


## Calculate BMI & categorise BMI into BMI Status
training$BMI <- as.numeric(training$Weight/((training$Height/100)^2))
training$BMI_Status <- cut(training$BMI, 
                           c(0,25,30, Inf), 
                           right = FALSE,
                           labels=c("Normal", "Overweight", "Obese"))


#PART 4 - COMPARE PROPORTIONS

# Practice 1 - Sex & Smoking

## Cross-tabulate Sex with Smoking (Compare proportions)
table(training$Sex)
table(training$Smoking2)

table(training$Smoking2, training$Sex)
table1 <- table(training$Smoking2, training$Sex)

table1

margin.table(table1, 1) # Row (Smoking) total
margin.table(table1, 2) # Column (Sex) total

## Results in %
prop.table(table1, 1) # By Smoking status
prop.table(table1, 2) # By Sex

## Results in % & rounded to 1 decimal point
round(prop.table(table1, 1)*100, digit=1)
round(prop.table(table1, 2)*100, digit=1) ## More relevant

## Chi-square test
chisq.test(table1, correct=F) 

## Do continuity correction if > 20% 
## of cells contain expected count < 5
## To obtain the observed & expected count
chisq.test(table1)$observed
chisq.test(table1)$expected

## Using gtsummary to compare proportions
training %>% 
  select(Smoking2,Sex) %>% 
  tbl_summary(by = Sex)%>% 
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3))

# Practice 2 -  Sex and BMI Status

training$BMI <- training$Weight/((training$Height/100)^2)

## First, need to create BMI
training$BMI <- as.numeric(training$Weight/(training$Height/100)^2)

## Next, to categorise BMI into BMI status
training$BMI_status <- cut(training$BMI,
                           c(0, 25, 30, Inf),
                           right = FALSE,
                           labels = c("Normal", "Overweight", "Obese"))

table(training$BMI_status)

## Crosstab Sex and BMI Status

# Crosstab & chi-square using Base R
table2 <- table(training$Sex, training$BMI_status)
table2

round(prop.table(table2, 1)*100, digits = 1)
chisq.test(table2)$expected
chisq.test(table2, correct=FALSE)

## Crosstab & chi-square using gtsummary
training %>% 
  select(Sex, BMI_status) %>% 
  tbl_summary(by = Sex) %>% 
  add_p(pvalue_fun =~style_pvalue(.x, digits = 3))



#PART 5 - COMPARE TWO MEANS

# Practice 1 - FBS by Smoking

var.test(training$FBS ~ training$Smoking2) # To check the variance assumption

# y = bx +c
# y ~ x1 + x2 + x3....

t.test(training$FBS ~ training$Smoking2) # By default, equal variance assumed
t.test(training$FBS ~ training$Smoking2, var.equal = FALSE) # If equal variance can't be assumed

# Alternative method to do t-test using gtsummary with equal variance can't be assummed
training %>% 
  select(FBS, Smoking2)%>% 
  tbl_summary(by = Smoking2) %>% 
  add_p()

training %>% 
  select(FBS, Smoking2) %>% 
  tbl_summary(by        = Smoking2,
              statistic = FBS                 ~ c("{mean} ({sd})")) %>% 
  add_p(test.args       = all_tests("t.test") ~ list(var.equal = TRUE),
        pvalue_fun      =                     ~ style_pvalue(.x, digits = 3)) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Smoking**")

# Visualising mean distribution
library(tidyverse)
library(ggpubr)

A <- ggbarplot(data = training, 
               x = "Smoking2", 
               y = "FBS",
               main = "Distribution of FBS by Smoking status",
               add = "mean_se")
A

B <- ggboxplot(data = training, 
               x = "Smoking2", 
               y = "FBS",
               main = "Distribution of SBP by Smoking status")
B

C <- ggerrorplot(data = training, x="Smoking2", y="FBS", 
                 main = "Distribution of FBS by Smoking status",
                 desc_stat = "mean_ci", 
                 add = "jitter", 
                 add.params = list(color = "darkgray"))
C

ggarrange(A, B, C + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)


# Effect size
library(effectsize)
omega_squared(aov(FBS ~ Smoking2, data=training))  
interpret_omega_squared(0.000645, rules = "field2013") # Insert the ES value obtained

# Practice 2 - FBS and Blood Pressure status

## First create BP
training$BP <- factor(ifelse(training$SBP>=140 | training$DBP >=90, "High", "Normal"))

# Compare means FBS by BP
var.test(training$FBS ~ training$BP) 
training %>% 
  select(BP, FBS) %>% 
  tbl_summary(
    by             = BP,
    statistic      = FBS                 ~ "{mean} ({sd})") %>%
  add_p(test.args  = all_tests("t.test") ~ list(var.equal = TRUE),
        pvalue_fun =                     ~ style_pvalue(.x, digits = 3)) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Blood Pressure**")


#PART 6 - COMPARE MORE THAN TWO MEANS

# Create BMI
training$BMI <- as.numeric(training$Weight/((training$Height/100)^2))
training$BMI_Status <- cut(training$BMI, 
                           c(0,25,30, Inf), 
                           right = FALSE,
                           labels=c("Normal", "Overweight", "Obese"))

# Compare means FBS by BMI Status
aov(training$FBS ~ training$BMI_Status)

# y = bx + c
# FBS = beta(bmi status) + constant
# FBS ~ bmi_status

summary(aov(training$FBS ~ training$BMI_Status))

# Visualising FBS by BMI Status
ggbarplot(data = training, 
          x = "BMI_status", 
          y = "FBS",
          main = "Distribution of FBS by Smoking status",
          add = "mean_se",
          xlab = "BMI Status")

# Testing homogeneity assumption
library(car)

leveneTest(FBS ~ BMI_Status, training, center=mean)

# Post-hoc test if equal variance
pairwise.t.test(training$FBS, training$BMI_Status, p.adj = "bonf")

# Post-hoc test if can't assume equal variance
TukeyHSD(aov(training$FBS ~ training$BMI_Status))

# Compare means using gtsummary
training %>% 
  select(FBS, BMI_Status) %>% 
  tbl_summary(by        = BMI_Status, 
              statistic = FBS         ~ c("{mean} ({sd})"),
              digits    = FBS         ~ 1)%>% 
  add_p(FBS ~ "aov") %>% 
  modify_spanning_header(all_stat_cols() ~ "**BMI Status**")

#PART 7 - CORRELATION

# FBS vs SBP

# y = bx + c
# FBS ~ SBP

library(ggpubr)
# Visualising
ggscatter(data = training,
          y = "FBS",
          x = "SBP")

## Can add more functions to ggscatter
ggscatter(data = training,
          y = "FBS",
          x = "SBP",
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE,
          main = "Correlation between blood sugar and systolic blood pressure")

# rho = correlation coefficient
# weak = < 0.3, moderate = 0.3-0.7, strong = >= 0.8


# Correlation test
cor.test(training$FBS, training$SBP, method = "pearson") # If Normal
cor.test(training$FBS, training$SBP, method = "spearman", exact=FALSE) # If not Normal



#PART 8 - NON-PARAMETRIC TESTS
#(For analyses using numerical not Normally distributed variables)

# Compare two numericals
# independent 2-group Mann-Whitney U Test (also known as Wilcoxon rank-sum test)
# Refer https://www.sciencedirect.com/topics/medicine-and-dentistry/wilcoxon-signed-ranks-test

t.test(training$FBS ~ training$BP) # if Normally distributed
wilcox.test(training$FBS ~ training$BP) # for not Normal

training %>% 
  select(FBS, BP) %>% 
  tbl_summary(
    by = BP) %>% 
  add_p(all_continuous() ~ "wilcox.test") %>% 
  modify_spanning_header(all_stat_cols() ~ "**Blood Pressure**")


# Compare more than two numericals

summary(aov(training$FBS ~ training$BMI_Status))

# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(training$FBS ~ training$BMI_Status)

# Dunn Test, a post-hoc test for Kruskall Wallis
library(FSA)
dunnTest(FBS ~ BMI_status,
         data = training,
         method = "bonferroni")

training %>% 
  select(FBS, BMI_status) %>% 
  tbl_summary(
    by = BMI_status,
    statistic = FBS ~ "{median} ({p25}, {p75})"
  ) %>% 
  add_p(all_continuous() ~ "kruskal.test") %>% 
  modify_spanning_header(all_stat_cols() ~ "**BMI Status**")



#PART 9 - REPEATED MEASURES ANOVA

# Paired means - compare means FBS & FBS after as overall (within changes)
t.test(training$FBS, training$FBS_post, paired = TRUE, alternative = "two.sided")

# Compare ranks - If not Normal use Wilcoxon signed-rank test
wilcox.test(training$FBS, training$FBS_post, paired = TRUE, alternative = "two.sided")

# Table to show within and between changes
training %>% 
  select(Intervention, FBS, FBS_post) %>% 
  tbl_summary(
    by = Intervention,
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(FBS ~ "Before",FBS_post ~ "After")) %>% 
  add_n() %>% 
  add_difference() %>% 
  modify_header(label ~ "**FBS**") %>% 
  modify_spanning_header(all_stat_cols() ~ "**Type of Intervention**")

# Convert wide to long format data
training2 <- training %>% 
  select(Intervention, FBS, FBS_post) %>% 
  gather("Time", "FBS", c("FBS", "FBS_post"))

model <- aov(FBS ~ Time+Intervention, data = training2)
summary(model)

training2 %>% 
  select (Time, FBS, Intervention) %>% 
  tbl_summary(
    by = Time,
    statistic = all_continuous() ~ c("{mean} ({sd})")) %>% 
  add_n() %>%
  add_difference()




