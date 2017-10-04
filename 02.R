source('01.R')

# Remove variables that have near zero variance

nzv_cols <- nearZeroVar(x = X_train)
names(X_train)[nzv_cols]

X_train <- data.table(X_train)
X_train <- X_train[,(nzv_cols) := NULL, ]

X_test <- data.table(X_test)
X_test <- X_test[,(nzv_cols) := NULL, ]

# Create dummy variables 

dummy <- dummyVars(~ employment_status_description + occupation + borrower_state + investment_type_description, data = X_train, sep = "_", fullRank = TRUE)

train_dummies_df <- data.frame(predict(dummy, X_train))
train_no_dummies_df <- select(X_train, -employment_status_description, -occupation, -borrower_state, -investment_type_description)
X_train_dummies <- cbind(train_no_dummies_df, train_dummies_df)

test_dummies_df <- data.frame(predict(dummy, X_test))
test_no_dummies_df <- select(X_test, -employment_status_description, -occupation, -borrower_state, -investment_type_description)
X_test_dummies <- cbind(test_no_dummies_df, test_dummies_df)

# Recursive Feature Elimination - WARNING takes 2 days to run.

y_train$loan_status <- factor(y_train$loan_status, levels = c('paid', 'default'))
y_test$loan_status <- factor(y_test$loan_status, levels = c('paid', 'default'))

new_lr_func <- caretFuncs
new_lr_func$summary <- twoClassSummary

rfe_ctrl <- rfeControl(functions = new_lr_func, 
                       method = 'repeatedcv', 
                       number = 5, 
                       repeats = 1, 
                       verbose = TRUE, 
                       saveDetails = TRUE)

set.seed(1)
system.time(glmProfile <- rfe(x = X_train_dummies, 
                              y = y_train$loan_status, 
                              rfeControl = rfe_ctrl, 
                              sizes = 1:ncol(X_train_dummies), 
                              method = "glm", 
                              metric = "ROC", 
                              preProc = c("center", "scale", "medianImpute"), 
                              trControl = trainControl(method = "cv", 
                                                       verboseIter = FALSE,
                                                       classProbs = TRUE)))


saveRDS(glmProfile, '/mnt/372CF7A40F31F1C0/Jash/Prosper rfe - caret/glm_iteration_1.rds')
glmProfile <- readRDS('/mnt/372CF7A40F31F1C0/Jash/Prosper rfe - caret/glm_iteration_1.rds')

# write.csv(glmProfile$optVariables, 'rfe-variables.csv')
# write.csv(names(X_train_dummies), 'original_variables.csv')


glm_pred_test_df <- predict(glmProfile, newdata = X_test_dummies)

caret::confusionMatrix(data = factor(y_test$loan_status, levels = c('paid', 'default')), reference = glm_pred_test_df$pred, positive = 'default', mode = 'prec_recall')

# Baseline Accuracy: 0.785 (if all loans are classified as paid)
# Accuracy: 0.7892
# Precision: 0.07943
# Recall: 0.55943

roc(response = y_test$loan_status, predictor=glm_pred_test_df$default)$auc # AUC of 0.7091
