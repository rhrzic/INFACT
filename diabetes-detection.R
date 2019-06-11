# Packages

require(caret)

# Currently using the Heart Disease UCI (https://archive.ics.uci.edu/ml/datasets/Heart+Disease) as a standin

df <- read.csv("data/heart.csv")
df$target[df$target == 1] <- "yes"
df$target[df$target == 0] <- "no"
df$target <- factor(df$target, levels = c("yes", "no"))

# I reserve 10% of dataset for out of sample test

split <- createDataPartition(df$target, p = 0.9, list=FALSE)

df_train <- df[split,]
df_test <- df[-split,]

#Variable selection + sensitivity analyses via bootstrap



# Fitting models
# Note: CV is done in a single step and are defined in fitControl. 
# Here I selected 3 repeats of 5-fold cross-validation and ROC for evaluation.

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

# We fit a logistic regression, a support vector machine and a decision tree model. 

set.seed(1)

logreg <- train(target ~ .,
            data = df_train, 
            method = "glm",
            preProc = "range",
            trControl = fitControl,
            metric = "ROC")

set.seed(1)

svm <- train(target ~ .,
             data = df_train, 
             method = "svmRadial",
             preProc = "range",
             trControl = fitControl,
             tuneLength = 10,
             metric = "ROC")

set.seed(1)

tree <- train(target ~ .,
             data = df_train, 
             method = "rpart",
             preProc = "range",
             trControl = fitControl,
             tuneLength = 10,
             metric = "ROC")

# Confusion matrix for each model

confusionMatrix(data = predict(logreg, df_test), 
                reference = df_test$target, 
                positive = "yes")

confusionMatrix(data = predict(svm, df_test), 
                reference = df_test$target, 
                positive = "yes")

confusionMatrix(data = predict(tree, df_test), 
                reference = df_test$target, 
                positive = "yes")

# The results are then printed and graphed (I chose ROC as the key metric)


results = resamples(list(LR = logreg,
                         SVM = svm,
                         CT = tree))

summary(results)

dotplot(results, metric = "ROC")

#Logistic regression performs best for ROC and Sensitivity