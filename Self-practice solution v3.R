library(tidyverse)
library(gtsummary)
library(flextable)

data <- read.csv("https://raw.githubusercontent.com/profjamal/biostatistics/main/healthstatus.csv")


# PREPARING THE VARIABLE

## Format for proper data type
data$sex <- factor(data$sex)
data$exercise <- factor(data$exercise)
data$smoking <- factor(data$smoking)

## Create BP from SBP & DBP
data$bp <- factor(ifelse(data$sbp>=140 | data$dbp >=90, "High", "Normal"))

## Calculate BMI & categorise BMI into BMI Status
data$bmi <- as.numeric(data$wt/((data$ht/100)^2))
data$bmi_status <- factor(cut(data$bmi, 
                           c(0,25,30,Inf), 
                           right = FALSE,
                           labels=c("Normal", "Overweight", "Obese")))
# SOLUTION 1
## Baseline characteristics
table1 <- data %>% 
  select(age, sex, exercise, smoking, bp, bmi_status) %>% 
  mutate(exercise = factor(exercise, levels = c("Low", "Moderate", "High"))) %>% 
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(age ~ "Age (years)",
                 sex ~ "Sex",
                 exercise ~ "Exercise Intensity",
                 smoking ~ "Smoking",
                 bp ~ "Blood Pressure",
                 bmi_status ~ "BMI Status")) %>% 
  modify_caption("Baseline characteristics")

## Output on screen
table1 
  
## Output as docx file
table1 <- table1 %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "Table 1.docx")
  

# SOLUTION 2
## Association with blood pressure

table2 <- data %>% 
  select(age, sex, exercise, smoking, bp, bmi_status) %>% 
  mutate(exercise = factor(exercise, levels = c("Low", "Moderate", "High"))) %>% 
  tbl_summary(
    by = bp,
    statistic = all_continuous() ~ "{mean} ({sd})",
    label = list(age ~ "Age (years)",
                 sex ~ "Sex",
                 exercise ~ "Exercise Intensity",
                 smoking ~ "Smoking",
                 bp ~ "Blood Pressure",
                 bmi_status ~ "BMI Status")) %>% 
  modify_caption("Factors associated with blood pressure") %>% 
  add_p(
    test = list(all_continuous()  ~ "t.test",
                all_categorical() ~ "chisq.test"),
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  )

## Output on screen
table2

## Output as docx file
table2 <- table2 %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "Table 2.docx")






