ccfraud <- read.csv('creditcard.csv')
ccfraud$Class <- as.factor(ccfraud$Class)
levels(ccfraud$Class) <- c("Genuine", "Fraud")
dim(ccfraud)

## [1] 284807 31

ftable(ccfraud$Class)
##Genuine  Fraud
##                
##   284315    492
##The Mice package was used to inspect if we have any missing values. There were none.

data(ccfraud, package = "VIM")
## Show incomplete cases
sleep[!complete.cases(ccfraud),]
library(mice)

#md.pattern(ccfraud)

##There are no missing data in the dataset. ## Plots on each of the variables

cn <- colnames(ccfraud[,1:30])
print(cn)
## [1] "Time" "V1" "V2" "V3" "V4" "V5" "V6"
## [8] "V7" "V8" "V9" "V10" "V11" "V12" "V13"
## [15] "V14" "V15" "V16" "V17" "V18" "V19" "V20"
## [22] "V21" "V22" "V23" "V24" "V25" "V26" "V27"
## [29] "V28" "Amount"

vars = paste("V", 1:30, sep="")
par(mfrow=c(29,4))
library(ggplot2)
for( var in cn){
  p <- ggplot(ccfraud, aes_string(x =  var, 
                                  fill = ccfraud$Class, colour = ccfraud$Class, alpha = 0.2)) +
    geom_density() + xlab(paste(var)) + 
    ylab("Density") +
    ggtitle(paste(var, "Density funcion"))
  print(p)
}

## Correlation Plot This is not really important here as input data is already a PCA tranformation.

library(corrplot)
cp <- corrplot(cor(ccfraud[, 1:30])) 
##par("mar")
##par(mar=c(1,1,1,1))
##{plot.new(); dev.off()}
##dev.off()

library(caret)
## Loading required package: lattice
set.seed(123)

index <- createDataPartition(ccfraud$Class, p = 0.7, list = FALSE)
train_data <- ccfraud[index, ]
test_data  <- ccfraud[-index, ]
ftable(train_data$Class)
##  Genuine  Fraud
##                
##   199021    345
ftable(test_data$Class)
##  Genuine Fraud
##               
##    85294   147
MiscFactors <- c()
pcafactors <-paste("V", 1:28, sep="")
formula = reformulate(termlabels = c(MiscFactors,pcafactors), response = 'Class')
print (formula)
## Class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + 
##     V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + 
##     V22 + V23 + V24 + V25 + V26 + V27 + V28
ControlParamteres <- trainControl(method = "cv",
                                  number = 10,
                                  savePredictions = TRUE,
                                  classProbs = TRUE,
                                  verboseIter = TRUE
)
str(train_data)
## 'data.frame':    199366 obs. of  31 variables:
##  $ Time  : num  0 0 1 1 2 2 4 7 7 10 ...
##  $ V1    : num  -1.36 1.192 -1.358 -0.966 -1.158 ...
##  $ V2    : num  -0.0728 0.2662 -1.3402 -0.1852 0.8777 ...
##  $ V3    : num  2.536 0.166 1.773 1.793 1.549 ...
##  $ V4    : num  1.378 0.448 0.38 -0.863 0.403 ...
##  $ V5    : num  -0.3383 0.06 -0.5032 -0.0103 -0.4072 ...
##  $ V6    : num  0.4624 -0.0824 1.8005 1.2472 0.0959 ...
##  $ V7    : num  0.2396 -0.0788 0.7915 0.2376 0.5929 ...
##  $ V8    : num  0.0987 0.0851 0.2477 0.3774 -0.2705 ...
##  $ V9    : num  0.364 -0.255 -1.515 -1.387 0.818 ...
##  $ V10   : num  0.0908 -0.167 0.2076 -0.055 0.7531 ...
##  $ V11   : num  -0.552 1.613 0.625 -0.226 -0.823 ...
##  $ V12   : num  -0.6178 1.0652 0.0661 0.1782 0.5382 ...
##  $ V13   : num  -0.991 0.489 0.717 0.508 1.346 ...
##  $ V14   : num  -0.311 -0.144 -0.166 -0.288 -1.12 ...
##  $ V15   : num  1.468 0.636 2.346 -0.631 0.175 ...
##  $ V16   : num  -0.47 0.464 -2.89 -1.06 -0.451 ...
##  $ V17   : num  0.208 -0.115 1.11 -0.684 -0.237 ...
##  $ V18   : num  0.0258 -0.1834 -0.1214 1.9658 -0.0382 ...
##  $ V19   : num  0.404 -0.146 -2.262 -1.233 0.803 ...
##  $ V20   : num  0.2514 -0.0691 0.525 -0.208 0.4085 ...
##  $ V21   : num  -0.01831 -0.22578 0.248 -0.1083 -0.00943 ...
##  $ V22   : num  0.27784 -0.63867 0.77168 0.00527 0.79828 ...
##  $ V23   : num  -0.11 0.101 0.909 -0.19 -0.137 ...
##  $ V24   : num  0.0669 -0.3398 -0.6893 -1.1756 0.1413 ...
##  $ V25   : num  0.129 0.167 -0.328 0.647 -0.206 ...
##  $ V26   : num  -0.189 0.126 -0.139 -0.222 0.502 ...
##  $ V27   : num  0.13356 -0.00898 -0.05535 0.06272 0.21942 ...
##  $ V28   : num  -0.0211 0.0147 -0.0598 0.0615 0.2152 ...
##  $ Amount: num  149.62 2.69 378.66 123.5 69.99 ...
##  $ Class : Factor w/ 2 levels "Genuine","Fraud": 1 1 1 1 1 1 1 1 1 1 ...
model.glm <- train(formula, data = train_data,method = "glm", family="binomial", trControl = ControlParamteres)
## + Fold01: parameter=none 
## - Fold01: parameter=none 
## + Fold02: parameter=none 
## - Fold02: parameter=none 
## + Fold03: parameter=none
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
## - Fold03: parameter=none 
## + Fold04: parameter=none 
## - Fold04: parameter=none 
## + Fold05: parameter=none 
## - Fold05: parameter=none 
## + Fold06: parameter=none 
## - Fold06: parameter=none 
## + Fold07: parameter=none 
## - Fold07: parameter=none 
## + Fold08: parameter=none 
## - Fold08: parameter=none 
## + Fold09: parameter=none 
## - Fold09: parameter=none 
## + Fold10: parameter=none 
## - Fold10: parameter=none 
## Aggregating results
## Fitting final model on full training set
exp(coef(model.glm$finalModel))
##  (Intercept)           V1           V2           V3           V4 
## 0.0001958784 1.0465226361 0.9692444177 1.0153588236 1.9545622788 
##           V5           V6           V7           V8           V9 
## 1.0040275509 0.8729683757 0.9622179694 0.8190839236 0.8090811225 
##          V10          V11          V12          V13          V14 
## 0.4658158788 1.0298687876 0.9696172856 0.7447382118 0.5935635399 
##          V15          V16          V17          V18          V19 
## 0.9115946086 0.8742182886 1.0436825514 0.9773124043 1.0100473879 
##          V20          V21          V22          V23          V24 
## 0.7444748231 1.4329545639 1.5347401880 0.8957445857 0.9727523021 
##          V25          V26          V27          V28 
## 0.8206291918 1.1151834700 0.5184477350 0.7885349948

pred <- predict(model.glm, newdata=test_data)
accuracy <- table(pred, test_data[,"Class"])
sum(diag(accuracy))/sum(accuracy)
## [1] 0.9990754
pred = predict(model.glm, newdata=test_data)
confusionMatrix(data=pred, test_data$Class)

mtry <- sqrt(ncol(train_data))

tuneGrid=expand.grid(.mtry=mtry)

ControlParamteres <- trainControl(method = "cv",
                                  number = 10,
                                  savePredictions = TRUE,
                                  classProbs = TRUE,
                                  verboseIter = TRUE
)
model.rf <- train(formula, data = train_data,method = "rf", family="binomial", metric="Accuracy",  trControl = ControlParamteres, tuneGrid=tuneGrid)
pred <- predict(model.rf, newdata=test_data)
accuracy <- table(pred, test_data[,"Class"])
print(accuracy)
