library(tidyverse)
library(magrittr)
library(broom)
library(modelr)
library(GGally)
library(car)


bike_data <- day_data
summary(bike_data)
str(bike_data)

# set categorical variables as factors
bike_data <- bike_data %>%
  mutate(season = as.factor(season),
         mnth = as.factor(mnth),
         weekday = as.factor(weekday),
         workingday = as.factor(workingday),
         weathersit = as.factor(weathersit))

train_set <- bike_data %>% filter(yr == 0) 
test_set <- bike_data %>% filter(yr == 1) 
  

# Data exploration
ggplot(train_set, aes(x = season, y = cnt, fill = season)) +
  geom_boxplot() +
  labs(title = "Boxplot for season") +
  xlab("season") +
  ylab("count")
  
ggplot(train_set, aes(x = mnth, y = cnt, fill = mnth)) +
  geom_boxplot() +
  labs(title = "Boxplot for month") +
  xlab("month") +
  ylab("count")

ggplot(train_set, aes(x = weekday, y = cnt, fill = weekday)) +
  geom_boxplot() +
  labs(title = "Boxplot for weekday") +
  xlab("weekday") +
  ylab("count")

ggplot(train_set, aes(x = workingday, y = cnt, fill = workingday)) +
  geom_boxplot() +
  labs(title = "Boxplot for workingday") +
  xlab("workingday") +
  ylab("count")

ggplot(train_set, aes(x = weathersit, y = cnt, fill = weathersit)) +
  geom_boxplot() +
  labs(title = "Boxplot for weather situation") +
  xlab("weather situation") +
  ylab("count")

test_set %>%
  select(temp, hum, windspeed, cnt) %>%
  ggpairs()

# backward elimination

model1<-lm(cnt~season+mnth+weekday+workingday+weathersit+temp+hum+windspeed,
           data=train_set)
summary(model1)
# weekday2 has highest pval = 0.266 (pval > alpha_rem = 15%)
# since it's dummy variable remove weekday

model2<-lm(cnt~season+mnth+workingday+weathersit+temp+hum+windspeed,
           data=train_set)
summary(model2)
# workingday1 has highest pval = 0.265 (pval > alpha_rem = 15%)
# since it's dummy variable remove workingday

model3<-lm(cnt~season+mnth+weathersit+temp+hum+windspeed, data=train_set)
summary(model3)
# mnth2 has highest pval = 0.225 (pval > alpha_rem = 15%)
# since it's dummy variable remove mnth

model4 <- lm(cnt~season+weathersit+temp+hum+windspeed, data=train_set)
summary(model4)
# hum has highest pval = 0.002 (pval < alpha_rem = 15%)
# Therefore, cannot remove.

# Final model.
final_model <- lm(cnt~season+weathersit+temp+hum+windspeed, data=train_set)
summary(final_model)

# component level statistics
final_model %>% tidy()

# model level statistics
glance(final_model)

# observational level statistics
model_fitresid <- augment(final_model)
model_fitresid

# fitted vs residuals
ggplot(model_fitresid, aes(x=.fitted, y=.resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Residuals vs Fitted values plot") +
  xlab("fitted values") +
  ylab("residuals")

# Normality of residuals
# histogram
ggplot(model_fitresid,
       aes(x=.resid)) +
  geom_histogram(color = "white") +
  labs(title = "Histogram of residuals") +
  xlab("residuals")

# QQ plot
ggplot(model_fitresid,
       aes(sample=.resid)) +
  stat_qq() + 
  stat_qq_line() +
  labs(title = "Q-Q plot")

# shapiro test
shapiro.test(model_fitresid$.resid)
# p-value < alpha
# residuals are not normal

# checking for multicollinearity
car::vif(final_model)
# all vif less than 10
# no multicollinearity

# identify influential observations
library(lindia)
gg_cooksd(final_model)

# remove influential observations
df1 <- train_set %>%
  filter(instant!=26,instant!=69,instant!=86,instant!=87,instant!=89,instant!=90,
         instant!=120,instant!=151,instant!=160,instant!=185,instant!=203,instant!=204,
         instant!=205,instant!=239,instant!=249,instant!=259,instant!=260,instant!=266,
         instant!=286,instant!=328,instant!=341)

new_model <- lm(cnt~season+weathersit+temp+hum+windspeed, data=df1)
summary(new_model)

# observational level statistics
model_fitresid_new <- augment(new_model)
model_fitresid_new

# fitted vs residuals
ggplot(model_fitresid_new, aes(x=.fitted, y=.resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Residuals vs Fitted values plot") +
  xlab("fitted values") +
  ylab("residuals")

# Normality of residuals
# histogram
ggplot(model_fitresid_new,
       aes(x=.resid)) +
  geom_histogram(color = "white") +
  labs(title = "Histogram of residuals") +
  xlab("residuals")

# QQ plot
ggplot(model_fitresid_new,
       aes(sample=.resid)) +
  stat_qq() + 
  stat_qq_line() +
  labs(title = "Q-Q plot")

# shapiro test
shapiro.test(model_fitresid_new$.resid)
# p-value > alpha
# residuals are normal

# checking for multicollinearity
car::vif(new_model)
# all vif less than 10
# no multicollinearity

# checking for heteroscedasticity
# graphical identification
ggplot(model_fitresid_new, aes(x=.fitted, y=abs(.resid))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Absolute Residuals vs Fitted values plot") +
  xlab("fitted values") +
  ylab("abs(residuals)")

# temperature
ggplot(model_fitresid_new, aes(x=temp, y=abs(.resid))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Absolute Residuals vs temperature plot") +
  xlab("x") +
  ylab("abs(residuals)")

# humidity
ggplot(model_fitresid_new, aes(x=hum, y=abs(.resid))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Absolute Residuals vs humidity plot") +
  xlab("x") +
  ylab("abs(residuals)")

# windspeed
ggplot(model_fitresid_new, aes(x=windspeed, y=abs(.resid))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Absolute Residuals vs windspeed plot") +
  xlab("x") +
  ylab("abs(residuals)")

# BPG test
library(olsrr)
bpg_test_model <- lm(cnt~temp, data=df1)
summary(bpg_test_model)
ols_test_breusch_pagan(bpg_test_model, rhs = TRUE, multiple = TRUE)

# Autocorrelation
count <- ts(df1$cnt)
acf(count, 100)
pacf(count, 100)
diff_count <- diff(count, 1)
acf(diff_count, 100)
pacf(diff_count, 100)

library(lmtest)
bgtest(cnt ~ season+weathersit+temp+hum+windspeed, order=6, data=df1)

# prediction ability
library(modelr)
test_set %>%
  add_predictions(final_model) %>%
  summarise(MSPR= mean((cnt - pred)^2))








