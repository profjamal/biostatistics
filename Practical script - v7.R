# Set the working directory

# PART 1 - PREPARING THE DATA

# Part 1.1 - Import data

training <- read.csv("https://raw.githubusercontent.com/profjamal/biostatistics/main/training.csv")

# Part 1.2 - Export data
# How to export data to text or csv

write.csv(training, "training.csv") # Will save into the working directory


# Part 1.3 - Assign levels and labels to categorical data
# Assign proper data type for each variable

training$Sex <- factor(training$Sex)
training$Sex <- factor(training$Sex, 
                       levels = c(0,1), 
                       labels = c("Male", "Female"))

training$Intervention <- factor(training$Intervention, 
                                levels = c(0,1), 
                                labels = c("Diet", "Diet & Exercise"))

training$Smoking <- factor(training$Smoking, 
                           levels = c(0,1), 
                           labels = c("No", "Yes")) 



# PART 2 - CHECKING THE DISTRIBUTION
## For numerical variables

##Age
hist(training$Age) ## Showing frequency
hist(training$Age, probability = TRUE) ## Showing density
curve(dnorm(x, mean=mean(training$Age), sd=sd(training$Age)), add=TRUE) ## Adding the curve
qqnorm(training$Age) ## Showing qq plot
qqline(training$Age) ## Showing qq line
library(psych)
describe(training$Age) ## Showing mean=median, skewness & kurtosis
shapiro.test(training$Age) ## Normality test

# FBS
hist(training$FBS)
hist(training$FBS, probability = TRUE)
curve(dnorm(x, mean=mean(training$FBS), sd=sd(training$FBS)), add=TRUE)
describe(training$FBS)
shapiro.test(training$FBS)


#PART 3 - DESCRIPTIVE STATISTICS

# Describe Age
summary(training$Age)
describe(training$Age)

# Describe categorical variable (Sex)
table(training$Sex)
prop.table(table(training$Sex))
prop.table(table(training$Sex))*100
round(prop.table(table(training$Sex))*100, digit=1)
plot(training$Sex) # Bar chart for the count, if needed

# Describe categorical variable (Smoking)
table(training$Smoking)
prop.table(table(training$Smoking))
prop.table(table(training$Smoking))*100
round(prop.table(table(training$Smoking))*100, digit=1)
plot(training$Smoking) # Bar chart

# Alternatively, use gtsummary to describe all variables
library(gtsummary)
library(tidyverse)


# CATEGORISE NUMERICAL INTO CATEGORICAL VARIABLES

## Create BP from SBP & DBP
training$BP <- factor(ifelse(training$SBP>=140 | training$DBP >=90, "High", "Normal"))

## Describe BP
table(training$BP)
prop.table(table(training$BP))
round(prop.table(table(training$BP))*100, digit=1)


## Calculate BMI & categorise BMI into BMI Status
training$BMI <- as.numeric(training$Weight/((training$Height/100)^2))

training$BMI_status <- cut(training$BMI, 
                           c(0,25,30, Inf), 
                           right = FALSE,
                           labels=c("Normal", "Overweight", "Obese"))

## Describe BMI_status
round(prop.table(table(training$BMI_status))*100, digit=1)

# Create Table 1
training %>% 
  select(Age, Sex, Smoking, BP, BMI_status) %>% 
  tbl_summary(
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}/{N} ({p}%)"),
    digit = all_continuous() ~ 1
  )

#PART 4 - COMPARE PROPORTIONS

# Practice 1 - Sex & Smoking

## Cross-tabulate Sex with Smoking (Compare proportions)
table(training$Sex)
table(training$Smoking)

table(training$Smoking, training$Sex)
table1 <- table(training$Smoking, training$Sex)

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

## Do continuity correction if > 20% of cells contain expected count < 5
## To obtain the observed & expected count
chisq.test(table1)$observed
chisq.test(table1)$expected

## Using gtsummary to compare proportions
training %>% 
  select(Smoking,Sex) %>% 
  tbl_summary(by = Sex)%>% 
  add_p()

# Practice 2 -  Sex and BMI Status

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
  add_p()

## To specify the test & decimal points
training %>% 
  select(Sex, BMI_status) %>% 
  tbl_summary(by = Sex) %>% 
  add_p(
    test = list(all_categorical() ~ "chisq.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  )


#PART 5 - COMPARE TWO MEANS

# Practice 1 - FBS by Smoking

var.test(training$FBS ~ training$Smoking) # To check the variance assumption

# y = bx +c
# y ~ x1 + x2 + x3....

t.test(training$FBS ~ training$Smoking) # By default, equal variance assumed
t.test(training$FBS ~ training$Smoking, var.equal = FALSE) # If equal variance can't be assumed

# Alternative method to do t-test using gtsummary with equal variance can't be assummed
training %>% 
  select(FBS, Smoking)%>% 
  tbl_summary(by = Smoking) %>% 
  add_p()

# A better table
training %>% 
  select(FBS, Smoking) %>% 
  tbl_summary(by        = Smoking,
              statistic = FBS                 ~ c("{mean} ({sd})")) %>% 
  add_p(
    test = list(all_continuous()  ~ "t.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
    ) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Smoking**")

# Visualising mean distribution
library(tidyverse)
library(ggpubr)

A <- ggbarplot(data = training, 
               x = "Smoking", 
               y = "FBS",
               main = "Distribution of FBS by Smoking status",
               add = "mean_se")
A

B <- ggboxplot(data = training, 
               x = "Smoking", 
               y = "FBS",
               main = "Distribution of SBP by Smoking status")
B

C <- ggerrorplot(data = training, x="Smoking", y="FBS", 
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
omega_squared(aov(FBS ~ Smoking, data=training))  
interpret_omega_squared(0.000645, rules = "field2013") # Insert the ES value obtained

# Practice 2 - FBS and Blood Pressure status

# Compare means FBS by BP
training %>% 
  select(BP, FBS) %>% 
  tbl_summary(by = BP,
              statistic  = FBS   ~ "{mean} ({sd})") %>%
  add_p(
    test = list(all_continuous()  ~ "t.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Blood Pressure**")

# Compare means considering variances??
var.test(training$FBS ~ training$BP) 
training %>% 
  select(BP, FBS) %>% 
  tbl_summary(by= BP,
              statistic = FBS ~ "{mean} ({sd})") %>%
  add_p(
    test.args = all_tests("t.test") ~ list(var.equal = TRUE),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Blood Pressure**")

#### Wilcoxon rank sum test = Mann-Whitney U test


#PART 6 - COMPARE MORE THAN TWO MEANS

# Compare means FBS by BMI Status
aov(training$FBS ~ training$BMI_status)

# y = bx + c
# FBS = beta(bmi status) + constant
# FBS ~ bmi_status

summary(aov(training$FBS ~ training$BMI_status))

# Visualising FBS by BMI Status
ggbarplot(data = training, 
          x = "BMI_status", 
          y = "FBS",
          main = "Distribution of FBS by Smoking status",
          add = "mean_se",
          xlab = "BMI Status")

# Testing homogeneity assumption
library(car)

leveneTest(FBS ~ BMI_status, training, center=mean)

# Post-hoc test if equal variance
pairwise.t.test(training$FBS, training$BMI_status, p.adj = "bonf")

# Post-hoc test if can't assume equal variance
TukeyHSD(aov(training$FBS ~ training$BMI_status))

# Compare means using gtsummary
training %>% 
  select(FBS, BMI_status) %>% 
  tbl_summary(by        = BMI_status, 
              statistic = FBS         ~ c("{mean} ({sd})"),
              digits    = FBS         ~ 1)%>% 
  add_p(FBS ~ "aov") %>% 
  modify_spanning_header(all_stat_cols() ~ "**BMI Status**")

#PART 7 - CORRELATION

# FBS vs Age

# y = bx + c
# FBS ~ Age

library(ggpubr)
# Visualising
ggscatter(data = training,
          y = "FBS",
          x = "Age")

## Can add more functions to ggscatter
ggscatter(data = training,
          y = "FBS",
          x = "Age",
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE,
          main = "Correlation between blood sugar and systolic blood pressure")

# rho = correlation coefficient
# weak = < 0.3, moderate = 0.3-0.7, strong = >= 0.8


# Correlation test
cor.test(training$FBS, training$Age, method = "pearson") # If Normal
cor.test(training$FBS, training$Age, method = "spearman", exact=FALSE) # If not Normal



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

summary(aov(training$FBS ~ training$BMI_status))

# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(training$FBS ~ training$BMI_status)

# Dunn Test, a post-hoc test for Kruskall Wallis
library(FSA)
dunnTest(FBS ~ BMI_status,
         data = training,
         method = "bonferroni")

training %>% 
  select(FBS, BMI_status) %>% 
  tbl_summary(
    by        = BMI_status,
    statistic = FBS ~ "{median} ({p25}, {p75})",
    digits    = FBS ~ 1
      ) %>% 
  add_p(all_continuous() ~ "kruskal.test") %>% 
  modify_spanning_header(all_stat_cols() ~ "**BMI Status**")



#PART 9 - REPEATED MEASURES ANOVA

# Paired means - compare means FBS & FBS after as overall (within changes)
t.test(training$FBS, training$FBS2, paired = TRUE, alternative = "two.sided")

# Compare ranks - If not Normal use Wilcoxon signed-rank test
wilcox.test(training$FBS, training$FBS2, paired = TRUE, alternative = "two.sided")

# Table to show within and between changes
training %>% 
  select(Intervention, FBS, FBS2) %>% 
  tbl_summary(
    by = Intervention,
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(FBS ~ "Before",FBS2 ~ "After")) %>% 
  add_n() %>% 
  add_difference() %>% 
  modify_header(label ~ "**FBS**") %>% 
  modify_spanning_header(all_stat_cols() ~ "**Type of Intervention**")

# Convert wide to long format data
training2 <- training %>% 
  select(Intervention, FBS, FBS2) %>% 
  gather("Time", "FBS", c("FBS", "FBS2"))

model <- aov(FBS ~ Time+Intervention, data = training2)
summary(model)

training2 %>% 
  select (Time, FBS, Intervention) %>% 
  tbl_summary(
    by = Time,
    statistic = all_continuous() ~ c("{mean} ({sd})")) %>% 
  add_n() %>%
  add_difference()




