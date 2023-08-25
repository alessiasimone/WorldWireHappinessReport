library(countrycode)
library(dplyr)
library(GGally)
library(visdat)
library(skimr)
library(DataExplorer)
library(corrplot)
library(lattice)
library(labelled)   # labeling data
library(rstatix)    # summary statistics
library(ggpubr)     # convenient summary statistics and plots
library(car)        # useful for anova/wald test
library(Epi)        # easy getting CI for model coef/pred
library(lme4)       # linear mixed-effects models
library(lmerTest)   # test for linear mixed-effects models
library(emmeans)    # marginal means
library(multcomp)   # CI for linear combinations of model coef
library(geepack)    # generalized estimating equations
library(ggeffects)  # marginal effects, adjusted predictions
library(gt)         # nice tables
library(tidyverse)  # for everything (data manipulation, visualization, coding, and more)

# PREPROCESSING
## Adding continent
df <- data %>%
  mutate(continent_code = countrycode(Country.name, "country.name", "continent")) %>%
  mutate(continent = case_when(
    continent_code == "AF" ~ "Africa",
    continent_code == "AS" ~ "Asia",
    continent_code == "EU" ~ "Europe",
    continent_code == "NA" ~ "North America",
    continent_code == "OC" ~ "Oceania",
    continent_code == "SA" ~ "South America",
    continent_code == "AN" ~ "Antarctica",
    TRUE ~ "Unknown"
  ))


df <- df[2:12]
df$continent_code <- as.factor(df$continent_code)
df <- na.omit(df)

##Scaling
scaled_df <- scale(df[, c('Life.Ladder', 'Log.GDP.per.capita', 'Social.support', 'Healthy.life.expectancy.at.birth', 'Freedom.to.make.life.choices', 'Generosity', 'Perceptions.of.corruption', 'Positive.affect', 'Negative.affect')])
scaled_df <- data.frame(scaled_df)
scaled_df <- cbind(scaled_df, continent = df$continent_code, year = df$year)

## Train and Test set splitting

test <- scaled_df %>%
  filter(year == 2022)

indices <- as.numeric(rownames(test))
train <- scaled_df[-indices, ]

#EDA
skim(train)
hist(train$Life.Ladder)
plot_bar(train, ggtheme=theme_bw(), title='Distribution of continents')
boxplot(train[1:9])
ggpairs(train, aes(colour=continent), lower = list(continuous = wrap("points", size=0.6)), upper = list(continuous = wrap("cor", size = 2)))
R <- cor(train[1:9])
corrplot(R, method='color')

xyplot(Life.Ladder~year | continent, 
       data=train, 
       panel=function(x,y){
         panel.xyplot(x, y)
         panel.lmline(x,y)
       }, as.table=T)

ggplot(train, aes(year, Life.Ladder)) +
  geom_line(aes(group = factor(continent))) +
  geom_smooth() +
  facet_grid(~ continent) 

#Linear Mixed Effects
model <- lmer(Life.Ladder~ . - continent - year + (1 + year|continent), data=train)
summary(model)

##check assumption
hist(resid(model))
qqnorm(resid(model))
qqline(resid(model))
plot(resid(model) ~ fitted(model))

#TESTING
test$predicted <- predict(model, newdata = test)
plot(test$Life.Ladder, test$predicted)

actual <- test$Life.Ladder
predicted <- test$predicted
mean_actual <- mean(actual)
ss_res <- sum((actual - predicted)^2)
ss_tot <- sum((actual - mean_actual)^2)
r_squared <- 1 - (ss_res / ss_tot)
cat("R-squared:", r_squared, "\n")

