# Set the working directory
setwd("~/iCloud Drive (Archive)/~My Documents/~Academic/Courses Conducted/~Biostatistics Workshop/Biostatistics with R/Basic Biostatistics with R/Workshop")

# PART 1 - PREPARING THE DATA

# Part 1.1 - CREATE DATA
set.seed(1234)
data <- cbind(1:120, 
              rnorm(120,40,5),
              rbinom(120,1,.4),
              rnorm(120,130,10), 
              rnorm(120,80,10)
)
data <- as.data.frame(data)

colnames(data) <- c("ID", "Age", "Sex", "SBP", "DBP")
data$Sex <- factor(data$Sex, c(0,1), labels = c("Female", "Male") )

### Alternative method, and rounding numerics
set.seed(1234)
data2 <- data.frame(
  ID=1:120,
  Age=round(as.numeric(rnorm(120,40,5)),0),
  Sex=factor(rbinom(120,1,.4)),
  SBP=round(as.numeric(rnorm(120,130,10)), 0),
  DBP=round(as.numeric(rnorm(120,80,10)),0)
)

data2$Sex <- factor(data2$Sex, c(0,1), labels = c("Female", "Male") )

# Part 1.2 - EXPORT DATA

# How to export data to text or csv
library(readr)
write_csv(data2, file = "filename.csv") # Will save into the working directory

# Part 1.3 - IMPORT (READ) EXTERNAL DATA

training <- read_csv("https://raw.githubusercontent.com/profjamal/biostatistics/main/training.csv")


# Part 1.4 - PREPARE THE DATA

# Assign proper data type for each variable
training$Sex <- factor(training$Sex)
training$Intervention <- factor(training$Intervention, levels = c(0,1), labels = c("Diet", "Diet & Exercise"))
training$Smoking2 <- factor(training$Smoking, levels = c(0,1), labels = c("No", "Yes"))


# PART 2 - CHECKING THE DATA

# Checking distribution for numerical variables

attach(training)
detach(training)

## Visually
par(mfrow= c(2,4))
hist(training$Age)
hist(training$SBP)
hist(training$DBP)
hist(training$Weight)
hist(training$Height)
hist(training$FBS)
hist(training$FBS_post)

attach(training)

# Showing Normal curve
hist(Age, probability = TRUE)
curve(dnorm(x, mean=mean(Age), sd=sd(Age)), add=TRUE) 

hist(SBP, probability = TRUE)
curve(dnorm(x, mean=mean(SBP), sd=sd(SBP)), add=TRUE) 

hist(DBP, probability = TRUE)
curve(dnorm(x, mean=mean(DBP), sd=sd(DBP)), add=TRUE) 

hist(Weight, probability = TRUE)
curve(dnorm(x, mean=mean(Weight), sd=sd(Weight)), add=TRUE) 

hist(Height, probability = TRUE)
curve(dnorm(x, mean=mean(Height), sd=sd(Height)), add=TRUE) 

hist(FBS, probability = TRUE)
curve(dnorm(x, mean=mean(FBS), sd=sd(FBS)), add=TRUE) 

hist(FBS_post, probability = TRUE)
curve(dnorm(x, mean=mean(FBS_post), sd=sd(FBS_post)), add=TRUE) 

# Q-Q Plot

qqnorm(Age)
qqline(Age)

qqnorm(SBP)
qqline(SBP)

qqnorm(DBP)
qqline(DBP)

qqnorm(Weight)
qqline(Weight)

qqnorm(Height)
qqline(Height)

qqnorm(FBS)
qqline(FBS)

qqnorm(FBS_post)
qqline(FBS_post)

## Showing the moment values - determine
library(psych)
describe(Age)
describe(SBP)
describe(DBP)

# Normality test
shapiro.test(Age)
shapiro.test(SBP)
shapiro.test(DBP)

#PART 3 - DESCRIPTIVE STATISTICS

# Describe categorical variable (Sex)
table(Sex)
round(prop.table(table(Sex))*100, digit=1)

par(mfrow= c(1,1))
plot(Sex) # Bar chart for the count

library(ggpubr)
ggbarplot(training, Sex)

# Describe numerical variables (Age, SBP & DBP)
library(psych)
describe(Age)
describe(SBP)
describe(DBP)

# Using gtsummary to describe all variables
library(gtsummary)
library(tidyverse)

# The simplest format
training %>%
  tbl_summary()

# A better format
training %>% 
  tbl_summary(
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}/{N} ({p}%)"),
    digits    =      all_continuous()  ~ 1
  )

# Even a better format
training %>% 
  select(Age, Sex, Smoking2, SBP, DBP, Weight, Height, FBS, FBS_post) %>% 
  tbl_summary(
    statistic = list(c(Age, SBP, DBP, Weight, Height) ~ "{mean} ({sd})",
                     c(FBS, FBS_post) ~ "{median} ({p25}, {p75})",
                     all_categorical() ~ "{n}/{N} ({p}%)"),
    digit = all_continuous() ~ 1
  )



#PART 4 - COMPARE PROPORTIONS

# Practice 1 - Sex & Smoking

## Cross-tabulate Sex with Smoking (Compare proportions)
table1 <- table(Smoking2, Sex)
table1

margin.table(table1, 1) # Row (Smoking) total
margin.table(table1, 2) # Column (Sex) total

## Results in %
prop.table(table1, 1) # By Smoking status
prop.table(table1, 2) # By Sex

## Results in % & rounded to 1 decimal point
round(prop.table(table1, 1)*100, digit=1)
round(prop.table(table1, 2)*100, digit=1)

## Chi-square test
chisq.test(table1, correct=FALSE) # Do continuity correction if > 20% of cells contain expected count < 5

## To obtain the observed & expected count
chisq.test(table1)$observed
chisq.test(table1)$expected

## Using gtsummary to compare proportions
training %>% 
  select(Smoking2,Sex) %>% 
  tbl_summary(by = Sex)%>% 
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3))

# Practice 2 -  Sex and BMI Status

## First, need to create BMI
training$BMI <- as.numeric(Weight/(Height/100)^2)

## Next, to categorise BMI into BMI status
training$BMI_status <- factor(cut(training$BMI, 
                                  c(0,25,30, Inf), 
                                  labels=c("Normal", "Overweight", "Obese")))

## Crosstab Sex and BMI Status

detach(training)
attach(training)

# Crosstab & chi-square using Base
table2 <- table(Sex, BMI_status)
table2

round(prop.table(table2, 1)*100, digits = 1)
chisq.test(table2)$expected
chisq.test(table2, correct=FALSE)

## Crosstab & chi-square using gtsummary
training %>% 
  select(Sex, BMI_status) %>% 
  tbl_summary(by = Sex) %>% 
  add_p(pvalue_fun =~style_pvalue(.x, digits = 3))



#PART 5 - COMPARE MEANS

# Practice 1 - FBS by Smoking

var.test(FBS ~ Smoking2) # To check the variance assumption
t.test(FBS ~ Smoking2) # By default, equal variance assumed
t.test(FBS ~ Smoking2, var.equal = FALSE) # If equal variance can't be assumed

# Alternative method to do t-test using gtsummary with equal variance can't be assummed
training %>% 
  select(FBS, Smoking2) %>% 
  tbl_summary(by = Smoking2,
              statistic = FBS ~ c("{mean} ({sd})")) %>% 
  add_p(test.args = all_tests("t.test") ~ list(var.equal = TRUE),
        pvalue_fun = ~style_pvalue(.x, digits = 3)) %>% 
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
training$BP <- factor(ifelse(SBP>=140 | DBP >=90, "High", "Normal"))

detach(training)
attach(training)

# Compare means FBS by BP
var.test(training$FBS ~ training$BP) 
training %>% 
  select(BP, FBS) %>% 
  tbl_summary(
    by = BP,
    statistic = FBS ~ "{mean} ({sd})") %>%
  add_p(test.args = all_tests("t.test") ~ list(var.equal = TRUE),
        pvalue_fun = ~style_pvalue(.x, digits = 3)) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Blood Pressure**")


#PART 6 - COMPARE MORE THAN TWO MEANS

# Create BMI
training$BMI <- as.numeric(training$Weight/((training$Height/100)^2))
training$BMI_Status <- cut(training$BMI, c(0,25,30, Inf), labels=c("Normal", "Overweight", "Obese"))

# Compare means FBS by BMI Status
aov(training$FBS ~ training$BMI_Status)
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
  tbl_summary(by = BMI_Status, 
              statistic = FBS ~ c("{mean} ({sd})"),
              digits = FBS ~ 1)%>% 
  add_p(FBS ~ "aov", 
        pvalue_fun = ~style_pvalue(.x, digits = 3)) %>% 
  modify_spanning_header(all_stat_cols() ~ "**BMI Status**")

#PART 7 - CORRELATION

par(mfrow = c(1,1))

# Checking distribution for FBS
hist(training$FBS, probability = TRUE, ylim = c(0, 0.3))
curve(dnorm(x, mean=mean(training$FBS), sd=sd(training$FBS)), add=TRUE) 

# Checking distribution for FBS using ggpubr
ggqqplot(training$FBS)

# Checking using Shapiro Wilk Test
shapiro.test(training$FBS)

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

# Correlation test
cor.test(training$FBS, training$SBP, method = "pearson") # If Normal
cor.test(training$FBS, training$SBP, method = "spearman", exact=FALSE) # If not Normal



#PART 8 - NON-PARAMETRIC TESTS
#(For analyses using numerical not Normally distributed variables)

# Compare two numericals
# independent 2-group Mann-Whitney U Test 
wilcox.test(training$FBS ~ training$BP) 

training %>% 
  select(FBS, BP) %>% 
  tbl_summary(
    by = BP,
    statistic = FBS ~ "{median} ({p25}, {p75})"
  ) %>% 
  add_p(all_continuous() ~ "wilcox.test") %>% 
  modify_spanning_header(all_stat_cols() ~ "**Blood Pressure**")


# Compare more than two numericals
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
  add_n() %>% d_difference()




