# Preparing the data
library(readr)
library(dplyr)


healthstatus <- read_csv("https://raw.githubusercontent.com/profjamal/biostatistics/main/healthstatus.csv")
mydata <- healthstatus
str(mydata)


setwd(getwd())

mydata$sex <- factor(mydata$sex, labels = c("Male", "Female"))
mydata$exercise <- ordered(mydata$exercise, labels = c("Low", "Moderate", "High"))
mydata$smoking <- factor(mydata$smoking, labels = c("No", "Yes"))
attach(mydata)

# Linear regression

## How age affect sbp?

### Check Normal distribution

#### Method 1
hist(age, main="Distribution of Age (years)", las=1) # Display histogram with y-axis showing the frequency
hist(age, main="Distribution of Age (years)", las=1, prob=TRUE) # Display the density, then only the normal curve line can be shown
curve(dnorm(x, mean=mean(age), sd=sd(age)), add=TRUE) # Showing Normal curve (based on density)

#### Method 2
library(ggplot2)

# Histogram
histo.age <- ggplot(mydata, aes(age)) +       #This is for count
              geom_histogram() +
              labs(title="Distribution of age",
                   x="Age (years)",
                   y="Count")

histo.age

#Q-Q Plot

### Method 1
qqnorm(age)
qqline(age)

###Method 2
qqplot.age <- ggplot(mydata, aes(sample=age)) +
              geom_qq() + geom_qq_line(color="red")

qqplot.age

#Normality test
shapiro.test(age)

### Do scatterplot between SBP and age to visualise the association

plot(sbp~age, 
     main="Scatterplot between SBP and Age",
     xlab="Age (years)",
     ylab="SBP (mmHg)",
     las=1)
abline(lm(sbp~age), col="red")

### Correlation test

cor.test(sbp, age)

# Simple linear regression

model1 <- lm(sbp ~ age)
summary(model1)

# Multiple linear regression

model2 <- lm(sbp ~ age + sex)
summary(model2)
anova(model2)

model3 <- lm(sbp ~ age + sex + exercise + smoking + sex*smoking)
summary(model3)
anova(model3)
library(car)
Anova(model3, type=3)

# How good is the model?

AIC(model1)
BIC(model1)

AIC(model2)
BIC(model2)

AIC(model3)
BIC(model3)

## (Note: Lower the better)

# Testing assumptions
# Linear, mean residual = 0, equal variance of residuals, no auto correlation,
# residuals not correlated with IVs, n > predictors, no multicollinearity,
# resdiuals is Normal

mean(model3$residuals) ## Approaching zero

par(mfrow=c(2,2))
plot(model2) ## Contant residuals, or around zero = Homogenous

library(ggplot2)
acf(model2$residuals) # No auto correlation. Lag 0 always = 1, others hould be around 0

lmtest::dwtest(model2) # Durbin-Watson's test, P>0.05, no autocorrelation

vif(model2) # VIF < 4 = no auto-correlation

# Or test important assumptions automatically
library(gvlma)
gvlma::gvlma(model2)

# Global Stat
# Are the relationships between your X predictors and Y roughly
# linear?. Rejection of the null (p < .05) indicates a non-linear relationship
# between one or more of your X’s and Y 

# Skewness
# Is your distribution skewed
# positively or negatively, necessitating a transformation to meet the
# assumption of normality? Rejection of the null (p < .05) indicates that you
# should likely transform your data. 
# 
# Kurtosis
# Is your distribution kurtotic
# (highly peaked or very shallowly peaked), necessitating a transformation to
# meet the assumption of normality? Rejection of the null (p < .05) indicates
# that you should likely transform your data. 
# 
# Link Function
# Is your dependent
# variable truly continuous, or categorical? Rejection of the null (p < .05)
# indicates that you should use an alternative form of the generalized linear
# model (e.g. logistic or binomial regression). 
# 
# Heteroscedasticity
# Is the variance of your model residuals constant across the range of X (assumption of
# homoscedastiity)? Rejection of the null (p < .05) indicates that your
# residuals are heteroscedastic, and thus non-constant across the range of X.
# Your model is better/worse at predicting for certain ranges of your X scales.
