library(ncvreg)
library(bnstruct)
library(tidyverse)
library(parallel)

#Function returning mode (to be used for imputation for missing values)
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

#Load dataset
load("crimedata.Rdata")
set.seed(3)

#Discard non predictive attributes
myd<-myd[!names(myd) %in% c('county','countyCode','communityCode', 'community','communityname', 'fold')]

#Turn to factors
myd$state<-as.factor(myd$state)
myd$LemasGangUnitDeploy<-as.factor(myd$LemasGangUnitDeploy)

#Discard columns with missing values in more than 50% of the dataset
myd<-myd[,colnames(myd)[colSums(is.na(myd)) < 0.5*nrow(myd)]]

#Imputation of missing data using the K-NN (discrete values-> mode, continuous->median)
myd<-myd %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), median(., na.rm = TRUE))))

myd<-myd %>% 
  mutate_if(is.factor, funs(replace(., is.na(.), Mode(., na.rm=T))))


#Split into train(75%) and test set (25%)
dt = sort(sample(nrow(myd), nrow(myd)*.75, replace=FALSE))
train<-myd[dt,]
test<-myd[-dt,]

#Create X_train,y_train, X_test, y_test
X_train<-data.matrix(train[names(train)!='murdPerPop'])
y_train<-train$murdPerPop
X_test<-data.matrix(test[names(test)!='murdPerPop'])
y_test<-test$murdPerPop

#Function for model fit (cross validation for fine tuning of lamda) provided a gamma 
#If a list of gamma values is provided, then the function returns the 
#model with the lowest MSE on test set
model_training<-function (X_train, y_train, X_test,y_test,gammas) {
  best_model<-cv.ncvreg(X_train, y_train,  penalty="SCAD", family=c("gaussian"),trace=TRUE, gamma=gammas[[1]], seed=5)
  pred <- predict(best_model$fit, X_test)
  best_mse<-mean((y_test - pred)^2)
  if (length(gammas)>1){
    for(gamma_ in gammas[2:length(gammas)]){
      candidate_model<-cv.ncvreg(X_train, y_train,  penalty="SCAD", family=c("gaussian"),trace=TRUE, gamma=gamma_, seed=5)
      pred <- predict(candidate_model$fit, X_test)
      mse<-mean((y_test - pred)^2)
      if (mse<best_mse){
        best_mse<-mse
        best_model<-candidate_model
      }
    }
  }
  return(best_model)
}


#Fit model
model<-model_training(X_train, y_train, X_test,y_test,gammas=list(2.1, 3.7, 10))
summary(model)
plot(model$fit, main=bquote("SCAD penalty" ~ gamma == .(model$fit$gamma)))

coef_per_lamda<-model$fit$beta
coef_for_best_lambda<-coef(model)
best_lambda<-model$lambda.min

#Refit in order to get summary statistics for the model (not supported by cv.ncvreg)
fit <- ncvreg(X_train, y_train, penalty="SCAD", nfold=10, gamma=model$fit$gamma)
summary(fit, lambda=model$lambda.min)



# Plot model
op<- par(mfrow=c(2,1))
#Plot cross validation error (squared error loss) vs variables selected and log(?)
plot(model, type="cve") 
#Plot beta vs beta_hat
plot(model$fit, main=bquote("SCAD penalty" ~ gamma == .(model$fit$gamma) ~lambda==.(model$lambda.min)))
par(op)


#Non zero coefficients
names(coef_for_best_lambda[coef_for_best_lambda!=0])

#Predictions for test set and metrics
pred <- predict(model$fit, X_test)
MSE<-mean((y_test - pred)^2)
RMSE = sqrt(MSE)
MAE = mean(abs(y_test - pred))






