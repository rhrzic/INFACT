# Packages

require(caret)

# Currently using the Heart Disease UCI (https://archive.ics.uci.edu/ml/datasets/Heart+Disease) as a standin

df <- read.csv("data/heart.csv")
df$target[df$target == 1] <- "yes"
df$target[df$target == 0] <- "no"
df$target <- factor(df$target, levels = c("yes", "no"))

# Fitting models
# Note: CV is done in a single step and are defined in fitControl. 
# Here I selected 3 repeats of 5-fold cross-validation and ROC for evaluation.

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

set.seed(1)

# We fit a logistic regression, a support vector machine and a decision tree model. 

logreg <- train(target ~ .,
            data = df, 
            method = "glm",
            preProc = "range",
            trControl = fitControl,
            metric = "ROC")

svm <- train(target ~ .,
             data = df, 
             method = "svmRadial",
             preProc = "range",
             trControl = fitControl,
             tuneLength = 10,
             metric = "ROC")

tree <- train(target ~ .,
             data = df, 
             method = "rpart",
             preProc = "range",
             trControl = fitControl,
             tuneLength = 10,
             metric = "ROC")


results = resamples(list(LR = logreg,
                         SVM = svm,
                         CT = tree))

# The results are then printed and graphed (I chose ROC as the key metric)

summary(results)

dotplot(results, metric = "ROC")

#Logistic regression performs best for ROC and Sensitivity