library(randomForest)
library(MASS)
library(cvAUC)

load("final_data.rda")
data <- data.frame(new_data)

summary(data)

nrow(subset(data, label == 1))/nrow(data)
nrow(subset(data, label == 0))/nrow(data)

data$label = as.factor(data$label)

test = subset(data, id == 14)
train = subset(data, id != 14)
nrow(data)
nrow(test)
nrow(train)

colnames(train)

personal = colnames(train)[3:6]
wrist_acc = colnames(train)[7:8]
chest_acc = colnames(train)[24:25]
wrist_bvp = colnames(train)[9:11]
wrist_eda = colnames(train)[12:17]
wrist_temp = colnames(train)[18:23]
wrist_physio = colnames(train)[9:23]
chest_ecg = colnames(train)[26:28]
chest_eda = colnames(train)[29:34]
chest_emg = colnames(train)[35:37]
chest_resp = colnames(train)[38:40]
chest_temp = colnames(train)[41:46]
chest_physio = colnames(train)[26:46]
all_wrist = colnames(train)[7:23]
all_chest = colnames(train)[24:46]
all_physio = colnames(train)[c(9:23,26:46)]
all_modalities = colnames(train)[c(7:46)]

predictor_vars <- c("personal", "wrist_acc", "chest_acc", "wrist_bvp", 
                    "wrist_eda", "wrist_temp", "wrist_physio", "chest_ecg", 
                    "chest_eda", "chest_emg", "chest_resp", "chest_temp", 
                    "chest_physio", "all_wrist", "all_chest", "all_physio", 
                    "all_modalities")

train_sample = train
test_sample = test

rf <- function(train_sample, test_sample, predictors){
  set.seed(1)
  model_rf <- randomForest(as.formula(paste("label ~ ", paste(predictors, collapse = ' + '))), ntree = 500, importance = TRUE, data = train_sample)
  predict_rf <- predict(model_rf, test_sample)
  cat("Accuracy is", mean(test_sample$label == predict_rf)*100, "% \n")
  cat("AUROC is", AUC(as.numeric(as.character(predict_rf)), as.numeric(as.character(test_sample$label))), "\n \n")
  if (mean(test_sample$label == predict_rf) == 1){
    df <- data.frame(importance(model_rf, type = 1))
    print(df)
    cat('\n')
  }
}

print_baseline <- function(test_sample){
  predict_rf <- rep(0,nrow(test_sample))
  cat("Accuracy is", mean(test_sample$label == predict_rf)*100, "% \n")
  cat("AUROC is", AUC(as.numeric(as.character(predict_rf)), as.numeric(as.character(test_sample$label))), "\n \n")
}

print_baseline(test_sample)

for (i in 1:length(predictor_vars)){
  cat("Predictors: ", predictor_vars[i], "\n")
  rf(train_sample, test_sample, eval(parse(text = predictor_vars[i])))
}

for (i in 2:length(predictor_vars)){
  cat("Predictors: personal +", predictor_vars[i], "\n")
  rf(train_sample, test_sample, c(eval(parse(text = predictor_vars[1])), eval(parse(text = predictor_vars[i]))))
}

LDA <- function(train_sample, test_sample, predictors){
  model_lda <- lda(as.formula(paste("label ~ ", paste(predictors, collapse = ' + '))), data = train_sample)
  predict_lda <- predict(model_lda, test_sample)[[1]]
  cat("Accuracy is", mean(test_sample$label == predict_lda)*100, "% \n")
  cat("AUROC is", AUC(as.numeric(as.character(predict_lda)), as.numeric(as.character(test_sample$label))), "\n \n")
}

for (i in 1:length(predictor_vars)){
  cat("Predictors: ", predictor_vars[i], "\n")
  LDA(train_sample, test_sample, eval(parse(text = predictor_vars[i])))
}

for (i in 2:length(predictor_vars)){
  cat("Predictors: personal +", predictor_vars[i], "\n")
  LDA(train_sample, test_sample, c(eval(parse(text = predictor_vars[1])), eval(parse(text = predictor_vars[i]))))
}

logistic <- function(train_sample, test_sample, predictors){
  model_logistic <- glm(as.formula(paste("label ~ ", paste(predictors, collapse = ' + '))), family=binomial(link='logit'), data = train_sample)
  predict_logistic <- predict(model_logistic, test_sample)
  predict_logistic <- ifelse(predict_logistic > 0.5,1,0)
  cat("Accuracy is", mean(test_sample$label == predict_logistic)*100, "% \n")
  cat("AUROC is", AUC(as.numeric(as.character(predict_logistic)), as.numeric(as.character(test_sample$label))), "\n \n")
}

for (i in 1:length(predictor_vars)){
  cat("Predictors: ", predictor_vars[i], "\n")
  logistic(train_sample, test_sample, eval(parse(text = predictor_vars[i])))
}

for (i in 2:length(predictor_vars)){
  cat("Predictors: personal +", predictor_vars[i], "\n")
  logistic(train_sample, test_sample, c(eval(parse(text = predictor_vars[1])), eval(parse(text = predictor_vars[i]))))
}






































