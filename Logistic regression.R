#### SET UP & CLEAN DATA ####
## SET UP ##
#load libraries
library(tidyverse)
library(randomForest)
library(caret)
library(pROC)
library(MASS)
library(lmtest)
library(regclass)


#load and view data
data <- read_csv("NOAA.csv")
str(data)
nrow(data)
ncol(data)
sum(is.na(data))

## CLEAN DATA ##
#convert to factors (and order factors)
data$Bleaching <- as.factor(data$Bleaching)
data$Ocean <- as.factor(data$Ocean)
data$Storms <- as.factor(data$Storms)

data$HumanImpact <- as.factor(data$HumanImpact)
levels(data$HumanImpact)
data$HumanImpact <- factor(data$HumanImpact, levels = c("none", "low", "moderate", "high"), ordered = TRUE)

data$Siltation <- as.factor(data$Siltation)
levels(data$Siltation)
data$Siltation <- factor(data$Siltation, levels = c("never", "occasionally", "often", "always"), ordered = TRUE)

data$Dynamite <- as.factor(data$Dynamite)
levels(data$Dynamite)
data$Dynamite <- factor(data$Dynamite, levels = c("none", "low", "moderate", "high"), ordered = TRUE)

data$Poison <- as.factor(data$Poison)
levels(data$Poison)
data$Poison <- factor(data$Poison, levels = c("none", "low", "moderate", "high"), ordered = TRUE)

data$Sewage <- as.factor(data$Sewage)
levels(data$Sewage)
data$Sewage <- factor(data$Sewage, levels = c("none", "low", "moderate", "high"), ordered = TRUE)

data$Industrial <- as.factor(data$Industrial)
levels(data$Industrial)
data$Industrial <- factor(data$Industrial, levels = c("none", "low", "moderate", "high"), ordered = TRUE)

data$Commercial <- as.factor(data$Commercial)
levels(data$Commercial)
data$Commercial <- factor(data$Commercial, levels = c("none", "low", "moderate", "high"), ordered = TRUE)

unique(data$Bleaching)
unique(data$Ocean)
unique(data$Year)
unique(data$Depth)
unique(data$Storms)
unique(data$HumanImpact)
unique(data$Siltation)


#### FULL DATASET - VISUALISATIONS ####

plot(data$Bleaching)

#plotting Year and bleaching
data %>% ggplot(aes(Year)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations Per Year", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting ocean & bleaching
data %>% ggplot(aes(Ocean)) +
  geom_bar(aes(fill = Bleaching))

#plotting depth & bleaching
data %>% filter(Depth <=15) %>% ggplot(aes(Depth)) +
  geom_bar(aes(fill = Bleaching))

#plotting storms & bleaching
data %>% ggplot(aes(Storms)) +
  geom_bar(aes(fill = Bleaching))

#plotting human impact & bleaching
data %>% ggplot(aes(HumanImpact)) +
  geom_bar(aes(fill = Bleaching))

#plotting siltation & bleaching
data %>% ggplot(aes(Siltation)) +
  geom_bar(aes(fill = Bleaching))

#plotting dynamite & bleaching
data %>% ggplot(aes(Dynamite)) +
  geom_bar(aes(fill = Bleaching))

#plotting poison & bleaching
data %>% ggplot(aes(Poison)) +
  geom_bar(aes(fill = Bleaching)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
  
#plotting sewage & bleaching
data %>% ggplot(aes(Sewage)) +
  geom_bar(aes(fill = Bleaching))

#plotting industrial & bleaching
data %>% ggplot(aes(Industrial)) +
  geom_bar(aes(fill = Bleaching))

#plotting commercial & bleaching
data %>% ggplot(aes(Commercial)) +
  geom_bar(aes(fill = Bleaching))

#### FULL DATASET - MODELS ####

#split into test and train sets
set.seed(123)
training_data_indexes <- createDataPartition(data$Bleaching, p = 0.7, list = FALSE)
training_data <- data[training_data_indexes,]
testing_data <- data[-training_data_indexes,]

# GLM #
glm_model = glm(Bleaching ~ ., family = binomial(logit), data = training_data)
summary(glm_model)

exp(glm_model$coefficients)

#probabilities and predictions
testing_data$probability <- predict(glm_model, newdata = testing_data[,-1], type = "response")
testing_data$prediction <- "0"
testing_data[testing_data$probability >=0.2, "prediction"] = "1"          #0.2 chosen as threshold

#confusion matrix
glm_cfm <- table(predicted = testing_data$prediction, true = testing_data$Bleaching)
glm_cfm

#Accuracy
glm_accuracy <- (glm_cfm[2,2]+glm_cfm[1,1])/sum(glm_cfm)
glm_accuracy
#Precision
glm_precision <- glm_cfm[2,2]/(glm_cfm[2,2]+glm_cfm[2,1])
glm_precision
#Recall
glm_recall <- glm_cfm[2,2]/(glm_cfm[2,2]+glm_cfm[1,2])
glm_recall
#f1
glm_f1 <- 2*(glm_precision*glm_recall/(glm_precision+glm_recall))
glm_f1

#AUC
glm_predictions <- as.data.frame(predict(glm_model, testing_data, type = "response"))
plot.roc(testing_data$Bleaching, glm_predictions[,1], print.auc=TRUE)

#variable importance
glm_imp <- varImp(step_glm, scale=FALSE)
glm_imp

# RANDOM FOREST #
rf_model <- randomForest(Bleaching ~., data = training_data, ntree = 200, mtry = 8, importance = TRUE, proximity = TRUE)
rf_model
varImpPlot(rf_model)


#### FILTERED DATASET - VISUALISATIONS ####

#filter to include only years that experienced bleaching
filtered_data <- data %>% filter(Year >= 1998 & Year <= 2003)
str(filtered_data)

plot(filtered_data$Bleaching)

## BAR PLOTS WITH COUNT ON Y AXIS ##
#plotting Year and bleaching
filtered_data %>% ggplot(aes(Year)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations Per Year", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting ocean & bleaching
filtered_data %>% ggplot(aes(Ocean)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations By Ocean", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting depth & bleaching
filtered_data %>% filter(Depth <=15) %>% ggplot(aes(Depth)) +
  geom_bar(aes(fill = Bleaching)) +
  scale_x_binned(n.breaks = 14) +
  labs(title = "Counts of Observations By Depth", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting storms & bleaching
filtered_data %>% ggplot(aes(Storms)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations By Storm Presence", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting human impact & bleaching
filtered_data %>% ggplot(aes(HumanImpact)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations By Level of Human Impact", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting siltation & bleaching
filtered_data %>% ggplot(aes(Siltation)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations By Amount of Siltation", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting dynamite & bleaching
filtered_data %>% ggplot(aes(Dynamite)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations By Level of Dynamite Fishing", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting poison & bleaching
filtered_data %>% ggplot(aes(Poison)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations By Level of Poison Fishing", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting sewage & bleaching
filtered_data %>% ggplot(aes(Sewage)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations By Level of Sewage", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting industrial & bleaching
filtered_data %>% ggplot(aes(Industrial)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations By Level of Industrial Pollution", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting commercial & bleaching
filtered_data %>% ggplot(aes(Commercial)) +
  geom_bar(aes(fill = Bleaching)) +
  labs(title = "Counts of Observations By Level of Commercial Fishing", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")


## BAR PLOTS WITH PROPORTION ON Y AXIS ##
#plotting Year and bleaching
filtered_data %>% ggplot(aes(Year)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  labs(title = "Proportion of Observations Per Year", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting ocean & bleaching
filtered_data %>% ggplot(aes(Ocean)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  labs(title = "Proportion of Observations By Ocean", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting depth & bleaching
filtered_data %>% filter(Depth <=15) %>% ggplot(aes(Depth)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  scale_x_binned(n.breaks = 14) +
  labs(title = "Proportion of Observations By Depth", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting storms & bleaching
filtered_data %>% ggplot(aes(Storms)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  labs(title = "Proportion of Observations By Storm Presence", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting human impact & bleaching
filtered_data %>% ggplot(aes(HumanImpact)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  labs(title = "Proportion of Observations By Level of Human Impact", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting siltation & bleaching
filtered_data %>% ggplot(aes(Siltation)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  labs(title = "Proportion of Observations By Amount of Siltation", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting dynamite & bleaching
filtered_data %>% ggplot(aes(Dynamite)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  labs(title = "Proportion of Observations By Level of Dynamite Fishing", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting poison & bleaching
filtered_data %>% ggplot(aes(Poison)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  labs(title = "Proportion of Observations By Level of Poison Fishing", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting sewage & bleaching
filtered_data %>% ggplot(aes(Sewage)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  labs(title = "Proportion of Observations By Level of Sewage", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting industrial & bleaching
filtered_data %>% ggplot(aes(Industrial)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  labs(title = "Proportion of Observations By Level of Industrial Pollution", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")

#plotting commercial & bleaching
filtered_data %>% ggplot(aes(Commercial)) +
  geom_bar(position = "fill", aes(fill = Bleaching)) +
  labs(y = "Proportion") +
  labs(title = "Proportion of Observations By Level of Commercial Fishing", 
       subtitle = "Coloured by Bleaching Status") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1")


#### FILTERED DATASET - MODELS ####
#split into test and train sets
set.seed(123)
training_data_indexes <- createDataPartition(filtered_data$Bleaching, p = 0.7, list = FALSE)
training_data <- filtered_data[training_data_indexes,]
testing_data <- filtered_data[-training_data_indexes,]

# GLM #
glm1 <- glm(Bleaching ~ ., family = binomial(logit), data = training_data)
summary(glm1)

# Testing different model selection methods #
#backwards elimination
model_drop <- drop1(glm1, test = "F")
model_drop

#forwards selection
add1(glm1, scope = ~.^2, test = "Chisq")

#backward stepwise using AIC
stepAIC(glm1, direction = "backward")

#best model found by ^ - backwards AIC = 930.6
step_aic_glm <- glm(formula = Bleaching ~ Year + Storms + HumanImpact + Siltation + 
                  Dynamite + Poison + Sewage + Commercial, family = binomial(logit), data = training_data)
summary(step_aic_glm)

#stepwise selection
step(glm1, scope = ~ Ocean + Depth + Year + Storms + HumanImpact + Siltation + 
       Dynamite + Poison + Sewage + Commercial + Industrial, direction = "forward")



step(glm1, direction = "backward")

#best model found by ^
step_glm <- glm(formula = Bleaching ~ Year + Storms + HumanImpact + Siltation + 
                  Dynamite + Poison + Sewage + Commercial, family = binomial(logit), data = training_data)
summary(step_glm)


### Probabilities and predictions ###
testing_data$probability <- predict(step_glm, newdata = testing_data[,-1], type = "response")
testing_data$prediction <- "0"
testing_data[testing_data$probability >=0.5, "prediction"] = "1"      #0.2 chosen as threshold

#confusion matrix
glm_cfm <- table(predicted = testing_data$prediction, true = testing_data$Bleaching)
glm_cfm
#to get counts of predicted and true
#table(testing_data$Bleaching)
#table(testing_data$prediction)

#Accuracy
glm_accuracy <- (glm_cfm[2,2]+glm_cfm[1,1])/sum(glm_cfm)
glm_accuracy
#Precision
glm_precision <- glm_cfm[2,2]/(glm_cfm[2,2]+glm_cfm[2,1])
glm_precision
#Recall
glm_recall <- glm_cfm[2,2]/(glm_cfm[2,2]+glm_cfm[1,2])
glm_recall
#f1
glm_f1 <- 2*(glm_precision*glm_recall/(glm_precision+glm_recall))
glm_f1

#AUC
glm_predictions <- as.data.frame(predict(step_glm, testing_data, type = "response"))
#using test$default and predictions (type="prob") for test test ([,2] is Y column)
plot.roc(testing_data$Bleaching, glm_predictions[,1], print.auc=TRUE)

#variable importance
glm_imp <- varImp(step_glm, scale=FALSE)
glm_imp

prop.table(table(filtered_data$Bleaching))

#TRY UPSAMPLING
set.seed(111)

trainup <- upSample(x = training_data[,-ncol(training_data)], y = training_data$Bleaching)
table(trainup$Class)

trainup_glm <- glm(formula = Bleaching ~ Year + Storms + HumanImpact + Siltation + 
                     Dynamite + Poison, family = binomial(logit), 
                   data = trainup)
summary(trainup_glm)

waldtest(trainup_glm)
anova(trainup_glm, test = "Chisq")


#get exponential of model coefficients
exp(trainup_glm$coefficients)

#get confidence intervals
confint(trainup_glm)

#get exponential of coefficients (to interpret as odds ratios)
round(as.numeric(format(exp(coef(trainup_glm)), scientific = FALSE)),5)
format(exp(coef(trainup_glm)), scientific = FALSE)


### Probabilities and predictions ###
testing_data$probability <- predict(trainup_glm, newdata = testing_data[,-1], type = "response")
testing_data$prediction <- "0"
testing_data[testing_data$probability >=0.5, "prediction"] = "1"          #0.5 chosen as threshold

#confusion matrix
glm_cfm <- table(predicted = testing_data$prediction, true = testing_data$Bleaching)
glm_cfm


#Accuracy
glm_accuracy <- (glm_cfm[2,2]+glm_cfm[1,1])/sum(glm_cfm)
glm_accuracy
#Precision
glm_precision <- glm_cfm[2,2]/(glm_cfm[2,2]+glm_cfm[2,1])
glm_precision
#Recall
glm_recall <- glm_cfm[2,2]/(glm_cfm[2,2]+glm_cfm[1,2])
glm_recall
#specificity
#glm_specificity <- glm_cfm[1,1]/(glm_cfm[1,1]+glm_cfm[2,1])
#glm_specificity
#f1
glm_f1 <- 2*(glm_precision*glm_recall/(glm_precision+glm_recall))
glm_f1

#AUC
glm_predictions <- as.data.frame(predict(trainup_glm, testing_data, type = "response"))
#using test$default and predictions (type="prob") for test test ([,2] is Y column)
plot.roc(testing_data$Bleaching, glm_predictions[,1], print.auc=TRUE, 
        ylab = "Sensitivity (Recall)")

#variable importance
glm_imp <- varImp(trainup_glm, scale=FALSE)
glm_imp


#get VIF to test for multicolinearity
VIF(trainup_glm)

#get probabilities from model using formula
probabilities <- exp(glm_predictions)/(1+exp(glm_predictions))
probabilities



#### TRYING A RANDOM FOREST MODEL ####
rf_model <- randomForest(Bleaching ~ Year + Storms + HumanImpact + Siltation + Dynamite + 
                           Poison + Commercial, data = training_data,
                         ntree = 200, mtry = 8, importance = TRUE, proximity = TRUE)
rf_model
varImpPlot(rf_model)


#check class - check that keep.forest = TRUE worked
class(rf_model)


# Model summary
summary(rf_model)
# Objects returned from the model 
names(rf_model)


