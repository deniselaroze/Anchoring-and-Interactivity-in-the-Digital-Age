library(data.table)
library(knitr)
library(kableExtra)
library(tidyverse)
library(broom)
library(stargazer)
library(gridExtra)
library(stringr)
library(sandwich)
library(lmtest)
library(scales) 

setwd("/Users/elisaduranmicco/Dropbox/2103 - ANID - FONDEF Sernac/11.Pago/replication USA")

# open data
data <- read.csv("data/FinalData.csv") #2503
data <- as.data.table(data)

# Results: Chi-square of independence
tbl <- table( data$pay,data$condition)
chisq.test(tbl)

# Results: Kruskal-Wallis
kruskal.test(pay ~ condition, data = data)

### Table 2: Linear regression models (OLS) for repayment outcomes ###
data$Min <- ifelse(data$y0==34, 1, 0)
data$Partial <- ifelse(data$y0>34 & data$y0<1678 , 1, 0)
data$Total <- ifelse(data$y0==1678 , 1, 0)

r_payment <- lm(share ~ condition, data = data)
r_min <- lm(Min ~ condition, data = data)
r_partial <- lm(Partial ~ condition, data = data)
r_total <- lm(Total ~ condition, data = data)

coeftest(r_payment, vcov = vcovHC(r_payment, type = "HC1"))
coeftest(r_min, vcov = vcovHC(r_min, type = "HC1"))
coeftest(r_partial, vcov = vcovHC(r_partial, type = "HC1"))
coeftest(r_total, vcov = vcovHC(r_total, type = "HC1"))

stargazer(r_payment, r_min, r_partial, r_total, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

# Results: Linear Regression base SB
dataSB <- data
dataSB$condition <- relevel(factor(dataSB$condition), ref = "SB")

r_total    <- lm(Total   ~ condition, data = dataSB)

robust_se <- lapply(list(r_total),
                    function(x) sqrt(diag(vcovHC(x, type = "HC1"))))

stargazer(r_total,
          se = robust_se,
          type = "text",
          star.cutoffs = c(0.05, 0.01, 0.001))

# Results: Linear Regression base Slider-High
dataSH <- data
dataSH$condition <- relevel(factor(dataSH$condition), ref = "SliderHigh")

r_payment <- lm(share ~ condition, data = dataSH)
r_min <- lm(Min ~ condition, data = dataSH)
r_partial <- lm(Partial ~ condition, data = dataSH)
r_total <- lm(Total ~ condition, data = dataSH)

coeftest(r_payment, vcov = vcovHC(r_payment, type = "HC1"))
coeftest(r_min, vcov = vcovHC(r_min, type = "HC1"))
coeftest(r_partial, vcov = vcovHC(r_partial, type = "HC1"))
coeftest(r_total, vcov = vcovHC(r_total, type = "HC1"))

stargazer(r_payment, r_min, r_partial, r_total, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")

robust_p  <- lapply(list(r_payment, r_min, r_partial, r_total), function(m) {
  coefs <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
  coefs[, "Pr(>|t|)"]
})

### Table 3: Repayment behavior and use of interactive elements by condition.

data %>%
  group_by(condition) %>%
  summarise_at(vars(y0), list(mean)) %>%
  group_by(condition) %>%
  mutate(percentage = 100*y0/1678)

data %>%
  group_by(condition, pay) %>%
  summarise(count = n()) %>%
  group_by(condition) %>%
  mutate(percentage = 100*count / sum(count))

dataSliderH <- subset(data, data$condition == "SliderHigh")
dataSliderH$slider <- ifelse(dataSliderH$sliderCounts == "{\"58\":0,\"100\":0,\"200\":0,\"300\":0,\"400\":0,\"500\":0,\"600\":0,\"700\":0,\"800\":0,\"900\":0,\"1000\":0,\"1100\":0,\"1200\":0,\"1300\":0,\"1400\":0,\"1500\":0,\"1600\":0,\"1678\":1}" , "No", "Yes")

dataSliderH  %>%
  group_by(slider) %>%
  summarise_at(vars(y0), list(mean)) %>%
  group_by(slider) %>%
  mutate(percentage = 100*y0/1678)

dataSliderH %>%
  group_by(slider, pay) %>%
  summarise(count = n()) %>%
  group_by(slider) %>%
  mutate(percentage = 100*count / sum(count))

mean(subset(dataSliderH, dataSliderH$y0 < 1678 & dataSliderH$y0>34  & dataSliderH$slider == "Yes")$y)

dataSliderL <- subset(data, data$condition == "SliderLow")
dataSliderL$slider <- ifelse(dataSliderL$sliderCounts == "{\"58\":1,\"100\":0,\"200\":0,\"300\":0,\"400\":0,\"500\":0,\"600\":0,\"700\":0,\"800\":0,\"900\":0,\"1000\":0,\"1100\":0,\"1200\":0,\"1300\":0,\"1400\":0,\"1500\":0,\"1600\":0,\"1678\":0}" , "No", "Yes")
dataSliderL  %>%
  group_by(slider) %>%
  summarise_at(vars(y0), list(mean)) %>%
  group_by(slider) %>%
  mutate(percentage = 100*y0/1678)

dataSliderL %>%
  group_by(slider, pay) %>%
  summarise(count = n()) %>%
  group_by(slider) %>%
  mutate(percentage = 100*count / sum(count))

mean(subset(dataSliderL, dataSliderL$y0 < 1678 & dataSliderL$y0>34 & dataSliderL$slider == "Yes")$y)

dataTable <- subset(data, data$condition == "Table")
dataTable  %>%
  group_by(tableClickButton) %>%
  summarise_at(vars(y0), list(mean)) %>%
  group_by(tableClickButton) %>%
  mutate(percentage = 100*y0/1678)

dataTable %>%
  group_by(tableClickButton, pay) %>%
  summarise(count = n()) %>%
  group_by(tableClickButton) %>%
  mutate(percentage = 100*count / sum(count))

data$time <- as.numeric(data$Time_Condition_Page.Submit)
data %>%
  group_by(condition) %>%
  summarise_at(vars(time), list(mean,sd, max, min))

dataSliderH$time <- dataSliderH$Time_Condition_Page.Submit
dataSliderH %>%
  group_by(slider) %>%
  summarise_at(vars(time), list(mean,sd, max, min))

dataSliderL$time <- dataSliderL$Time_Condition_Page.Submit
dataSliderL %>%
  group_by(slider) %>%
  summarise_at(vars(time), list(mean,sd, max, min))

dataTable$time <- dataTable$Time_Condition_Page.Submit
dataTable  %>%
  group_by(tableClickButton) %>%
  summarise_at(vars(time), list(mean,sd, max, min))

# Results: Response times

log_time <- lm(log(time) ~ condition, data = data)
coeftest(log_time, vcov = vcovHC(log_time, type = "HC1"))
stargazer(log_time, 
          type = "text")
bptest(log_time)

coefs <- coef(log_time)
pct_change <- (exp(coefs) - 1) * 100

results <- data.frame(
  Condition = names(coefs),
  Estimate  = round(coefs, 4),
  Percent_Change = round(pct_change, 2)
)

# Results: Confidence

satis <- subset(data, !is.na(data$satis))
satis$satis <- as.numeric(as.character(satis$satis))
r_satis <- lm(satis ~ condition, data = satis)

coeftest(r_satis, vcov = vcovHC(r_satis, type = "HC1"))
robust_p  <- lapply(list(r_satis), function(m) {
  coefs <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
  coefs[, "Pr(>|t|)"]
})

stargazer(r_satis, 
          type = "text")

### Figure 2: Distribution of repayment amounts among users of the interactive tools.

plot_data <- data %>%
  group_by(y0, condition) %>%
  summarise(count = n()) %>%
  group_by(condition) %>%
  mutate(percentage = count / sum(count))

plot_data1 <- subset(plot_data, plot_data$condition %in% c("SliderHigh", "SliderLow", "Table"))
plot_data1 <- subset(plot_data1, plot_data1$y0 < 1678)

plot_data1$y0[plot_data1$y0 == 102.01] <- 102

plot_data2 <- subset(plot_data, plot_data$condition %in% c("SliderHigh", "SliderLow", "Table"))
plot_data2 <- subset(plot_data2, plot_data2$y0 == 1678)

plot_data1$condition <- factor(plot_data1$condition,
                               levels = c("SliderHigh", "SliderLow", "Table"),
                               labels = c("Slider-High", "Slider-Low", "Reference Table"))

plot_data2$condition <- factor(plot_data2$condition,
                               levels = c("SliderHigh", "SliderLow", "Table"),
                               labels = c("Slider-High", "Slider-Low", "Reference Table"))

p1 <- ggplot(plot_data1, aes(x = as.factor(y0), y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Payment Amount",
    y = "Percentage (%)",
    fill = NULL  
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_blank()  # Ensures no space is reserved for title
  )

p2 <- ggplot(plot_data2, aes(x = as.factor(y0), y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Payment Amount",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

grid.arrange(
  p1, p2,
  ncol = 2,
  widths = c(3, 1)
)

### Table A.1: Characteristics of participants assigned to each of the treatments.
Deno <-  data %>% 
  group_by(condition) %>%
  summarise(counts = n())

age <- data %>%
  group_by(condition) %>%
  summarise_at(vars(Age), list(mean,sd))

anova_age <- aov(Age ~ condition, data)
summary(anova_age)

data$Sex[data$Sex == "Female, Female"] <- "Female"
sex0 <- table(data$Sex, data$condition)
sex <-cbind(100*sex0[,1]/503, 100*sex0[,2]/501, 100*sex0[,3]/495, 100*sex0[,4]/501 ,100*sex0[,5]/503)

data$female <-  ifelse(data$Sex == "Female", 1, 0)
chisq.test(table(data$condition, data$female))

kable(sex,
      col.names = c("Sex", "Control", "Statement Balance", "Slider High", "Slider Low", "Table"),
      # "latex",
      digits = c(2, 2, 2, 2, 2))

data$Demo_Income <- factor(data$Demo_Income, levels = c("$24,999 or under", "$25,000 to $44,999", "$45,000 to $64,999", "$65,000 to $94,999", "$95,000 to $134,999", "$135,000 to $249,999", "$250,000 or over"))
inc0 <- table(data$Demo_Income, data$condition)
inc <- cbind(100*inc0[,1]/503, 100*inc0[,2]/501, 100*inc0[,3]/495, 100*inc0[,4]/501 ,100*inc0[,5]/503)

kable(inc,
      col.names = c("Income", "Control", "Statement Balance", "Slider High", "Slider Low", "Table"),
      #     "latex",
      digits = c(2, 2, 2, 2, 2))

data$inc1 <-  ifelse(data$Demo_Income == "$24,999 or under", 1, 0)
chisq.test(table(data$condition, data$inc1))

data$inc2 <-  ifelse(data$Demo_Income == "$25,000 to $44,999", 1, 0)
chisq.test(table(data$condition, data$inc2))

data$inc3 <-  ifelse(data$Demo_Income == "$45,000 to $64,999", 1, 0)
chisq.test(table(data$condition, data$inc3))

data$inc4 <-  ifelse(data$Demo_Income == "$65,000 to $94,999", 1, 0)
chisq.test(table(data$condition, data$inc4))

data$inc5 <-  ifelse(data$Demo_Income == "$95,000 to $134,999", 1, 0)
chisq.test(table(data$condition, data$inc5))

data$inc6 <-  ifelse(data$Demo_Income == "$135,000 to $249,999", 1, 0)
chisq.test(table(data$condition, data$inc6))

data$inc7 <-  ifelse(data$Demo_Income == "$250,000 or over", 1, 0)
chisq.test(table(data$condition, data$inc7))

data$Demo_Education <- factor(data$Demo_Education, levels = c("Less than a high school diploma or equivalent", "High school graduate", "Some college but no degree", "Bachelor's degree", "Advanced degree (Master's degree, professional degree, or doctoral degree)"))
educ0 <- table(data$Demo_Education , data$condition)
educ <- cbind(100*educ0[,1]/503, 100*educ0[,2]/501, 100*educ0[,3]/495, 100*educ0[,4]/501 ,100*educ0[,5]/503)

kable(educ,
      col.names = c("Education", "Control", "Statement Balance", "Slider High", "Slider Low", "Table"),
      #      "latex",
      digits = c(2, 2, 2, 2, 2,2))

data$edu1 <-  ifelse(data$Demo_Education == "Less than a high school diploma or equivalent", 1, 0)
chisq.test(table(data$condition, data$edu1))
data$edu2 <-  ifelse(data$Demo_Education == "High school graduate", 1, 0)
chisq.test(table(data$condition, data$edu2))
data$edu3 <-  ifelse(data$Demo_Education == "Some college but no degree", 1, 0)
chisq.test(table(data$condition, data$edu3))
data$edu4 <-  ifelse(data$Demo_Education == "Bachelor's degree", 1, 0)
chisq.test(table(data$condition, data$edu4))
data$edu5 <-  ifelse(data$Demo_Education == "Advanced degree (Master's degree, professional degree, or doctoral degree)", 1, 0)
chisq.test(table(data$condition, data$edu5))

fl <- subset(data, is.na(data$FL)==FALSE) %>%
  group_by(condition) %>%
  summarise_at(vars(FL), list(mean,sd))

anova_fl <- aov(FL ~ condition, data)
summary(anova_fl)

### Table A.2: Logit regression models for repayment outcomes

get_robust_se_logit <- function(glm_obj) {
  coeftest_obj <- coeftest(glm_obj, vcov = vcovHC(glm_obj, type = "HC0"))
  se <- coeftest_obj[, "Std. Error"]
  return(se)
}

logit01 <- glm(Min ~ condition, data = data, family = binomial(link = "logit"))
logit02 <- glm(Partial ~ condition, data = data, family = binomial(link = "logit"))
logit03 <- glm(Total ~ condition, data = data, family = binomial(link = "logit"))

stargazer(logit01, logit02, logit03,
          type = "text", 
          style = "default", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          se = list(
            get_robust_se_logit(logit01),
            get_robust_se_logit(logit02),
            get_robust_se_logit(logit03)
          ))

### Table A.3: Linear regression models to examine heterogeneous treatment effects, using full payment as dependent variable.

data_LC <- subset(data, is.na(data$FL)==FALSE) # Financial literacy
model1_LC <- lm(Total ~ condition * FL, data = data_LC)
coeftest(model1_LC, vcov = vcovHC(model1_LC, type = "HC1"))

data_LF <- subset(data, is.na(data$LFees)==FALSE) # Late Fees
model1_LF <- lm(Total ~ condition * LFees, data = data_LF)
coeftest(model1_LF, vcov = vcovHC(model1_LF, type = "HC1"))

data_DA <- subset(data, !(data$DA == "NA")) # Debt Aversion
model1_DA <- lm(Total ~ condition * DA , data = data_DA)
coeftest(model1_DA, vcov = vcovHC(model1_DA, type = "HC1"))

data$male <- ifelse(data$Sex == "Male", 1, 0) # Male 
model1_male <- lm(Total ~ condition * male, data = data)
coeftest(model1_male, vcov = vcovHC(model1_male, type = "HC1"))

model1_abc1 <- lm(Total ~ condition * ses, data = data) # Income
coeftest(model1_abc1, vcov = vcovHC(model1_abc1, type = "HC1"))

select <- dplyr::select

extract_model <- function(model) {
  model_summary <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  
  result <- data.frame(
    term = rownames(model_summary),
    estimate = model_summary[,1],
    std.error = model_summary[,2],
    p.value = model_summary[,4],
    stringsAsFactors = FALSE
  ) %>% 
    mutate(
      term = gsub("conditionSB", "Statement Balance", term),
      term = gsub("conditionSliderHigh", "Slider-High", term),
      term = gsub("conditionSliderLow", "Slider-Low", term),
      term = gsub("conditionTable", "Reference Table", term),
      term = gsub("FL", "X", term),
      term = gsub("LFees", "X", term),
      term = gsub("DA", "X", term),
      term = gsub("male", "X", term),
      term = gsub("ses", "X", term),
      term = gsub(":", " × ", term),
      formatted = sprintf("%.3f%s", estimate, 
                          ifelse(p.value < 0.01, "***",
                                 ifelse(p.value < 0.05, "**",
                                        ifelse(p.value < 0.1, "*", "")))),
      std.error = sprintf("(%.3f)", std.error)
    )
  
  return(result)
}

results <- list(
  "Financial Literacy (1)" = extract_model(model1_LC),
  "Late Fees (2)" = extract_model(model1_LF),
  "Debt Aversion (3)" = extract_model(model1_DA),
  "Gender (Male) (4)" = extract_model(model1_male),
  "SES (5)" = extract_model(model1_abc1)
)

create_table <- function(results) {
  terms_to_keep <- c(
    "Statement Balance", "Slider-High", "Slider-Low", "Reference Table", 
    "X", "Statement Balance × X", "Slider-High × X", "Slider-Low × X", "Reference Table × X"
  )
  
  final_table <- data.frame(Term = terms_to_keep, stringsAsFactors = FALSE)
  
  for (model_name in names(results)) {
    model_df <- results[[model_name]] %>% 
      filter(term %in% terms_to_keep) %>% 
      select(term, formatted, std.error)  
    
    final_table <- final_table %>% 
      left_join(model_df, by = c("Term" = "term"))
    
    colnames(final_table)[(ncol(final_table)-1):ncol(final_table)] <- 
      paste0(model_name, c("_coef", "_se"))
  }
  
  return(final_table)
}

regression_table <- create_table(results)

regression_table[is.na(regression_table)] <- ""

coef_table <- select(regression_table, "Term", "Financial Literacy (1)_coef", "Late Fees (2)_coef", "Debt Aversion (3)_coef", "Gender (Male) (4)_coef", "SES (5)_coef")
kable(coef_table)

se_table <- select(regression_table, "Term", "Financial Literacy (1)_se", "Late Fees (2)_se", "Debt Aversion (3)_se", "Gender (Male) (4)_se", "SES (5)_se")
kable(se_table)

### Table A.4: Linear regression models for high reference point and interactivity.

dataN <- data
dataN$din <- ifelse(dataN$condition %in% c("SliderHigh", "SliderLow") ,1,0)
dataN$high <- ifelse(dataN$condition %in% c("SliderHigh", "SB") ,1,0)

d2_payment <- lm(share ~ din + high , data = dataN)
d2_min <- lm(Min ~ din + high , data = dataN)
d2_partial <- lm(Partial ~ din + high , data = dataN)
d2_total <- lm(Total ~ din + high , data = dataN)

coeftest(d2_payment, vcov = vcovHC(d2_payment, type = "HC1"))
coeftest(d2_min, vcov = vcovHC(d2_min, type = "HC1"))
coeftest(d2_partial, vcov = vcovHC(d2_partial, type = "HC1"))
coeftest(d2_total, vcov = vcovHC(d2_total, type = "HC1"))

stargazer(d2_payment, d2_min, d2_partial, d2_total, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")
