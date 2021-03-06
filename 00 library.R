checkInstallLoad <- function(libName) {
  if(!require(libName, character.only=TRUE)) {
    install.packages(libName)
    require(libName, character.only=TRUE)
  }
}

checkInstallLoad("arules")
checkInstallLoad("ROCR")
checkInstallLoad("nnet")
checkInstallLoad("car")
checkInstallLoad("Ckmeans.1d.dp")

checkInstallLoad("data.table")
checkInstallLoad("Matrix")
checkInstallLoad("YaleToolkit")
checkInstallLoad("Amelia")
checkInstallLoad("Metrics")
checkInstallLoad("plyr")
checkInstallLoad("dplyr")
checkInstallLoad("stringr")
checkInstallLoad("lubridate")
checkInstallLoad("ggplot2")
checkInstallLoad("plot3D")
checkInstallLoad("pROC")
checkInstallLoad("caret")
checkInstallLoad("caretEnsemble")
checkInstallLoad("e1071")
checkInstallLoad("randomForest")
checkInstallLoad("xgboost")
checkInstallLoad("rpart")
checkInstallLoad("C50")
checkInstallLoad("adabag")
checkInstallLoad("gbm")
checkInstallLoad("corrplot")
checkInstallLoad('DataCombine')
checkInstallLoad('caTools')
checkInstallLoad('tidyverse')
checkInstallLoad('dummies')

rm(checkInstallLoad)

print("Loading libraries complete.")