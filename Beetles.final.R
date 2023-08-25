btl <- read.csv("PineBeetle2.csv", header = TRUE)
btl$Infested <- ifelse(btl$Infested=="Yes", 1, 0)
library(bestglm)
library(MASS)
library(lmtest)

#explore data
par(mfrow=(c(2,3)))
scatter.smooth(btl$January, btl$Infested, xlab = "January", ylab = "Infested")
scatter.smooth(btl$August_max, btl$Infested, xlab = "August_max", ylab = "Infested")
scatter.smooth( btl$Slope, btl$Infested, xlab = "Slope", ylab = "Infested")
scatter.smooth( btl$Elev, btl$Infested, xlab = "Elev", ylab = "Infested")
scatter.smooth( btl$Precip, btl$Infested, xlab = "Precip", ylab = "Infested")

cor(btl$January, btl$Infested)
cor(btl$August_max, btl$Infested)
cor( btl$Slope, btl$Infested)
cor( btl$Elev, btl$Infested)
cor( btl$Precip, btl$Infested)


#find most significant covariates
bests <- bestglm(btl, family = binomial, method = "exhaustive", IC="BIC")
b.llr <- bests$BestModel
summary(b.llr)
exp(b.llr$coefficients)
bests$Subsets
par(1)
plot(bests$Subsets$BIC,type="b",pch=19,xlab="Number of Variables", ylab="BIC", xlim = c(1,20))


#create confidence intervals
beta.conf <- confint(b.llr)              
multiple.conf <- exp(confint(b.llr))     
percentage.conf <- 100*(multiple.conf-1)


#classification
pred.probs <- predict.glm(b.llr,type="response")
thresh <- seq(0,1,length=100)
misclass <- rep(NA,length=length(thresh))
for(i in 1:length(thresh)) {
  #If probability greater than threshold then 1 else 0
  my.classification <- ifelse(pred.probs > thresh[i],1,0)
  # calculate the pct where my classification not eq truth
  misclass[i] <- mean(my.classification!=btl$Infested)
}
#Find threshold which minimizes miclassification
c <- thresh[which.min(misclass)]
c
plot(misclass~thresh, typ= "l")
abline(v=c)

#measure of how well model fits all data
pred.class <- pred.probs > c
true.class <- btl$Infested
table(pred.class,true.class)
addmargins(table(pred.class,true.class))

pseudoR <- 1-(b.llr$deviance/b.llr$null.deviance)



#cross-validation
n.cv <- 200
sens <- rep(NA,n.cv)
spec <- rep(NA,n.cv)
ppv <- rep(NA,n.cv)
npv <- rep(NA,n.cv)
cutoff <- c

for(cv in 1:n.cv){
  ## Separate into test and training sets
  obs.test <- sample(1:2310,2310*.1)
  train.set <- btl[obs.test,]
  test.set <- btl[-obs.test,]
  ## Fit best model to training set
  train.model <- glm(Infested ~ January + August_max + Precip + NC + SE ,data=train.set)
  ## Use fitted model to predict test set
  pred.probs <- predict.glm(train.model,newdata=test.set,
                            type="response") #response gives probabilities
  ## Classify according to threshold
  test.class <- ifelse(pred.probs>cutoff,1,0)
  ## Create a confusion matrix
  conf.mat <- addmargins(table(factor(test.set$Infested,levels=c(0,1)),
                               factor(test.class,levels=c(0,1))))
  ## Pull off sensitivity, specificity, PPV and NPV
  ## using bracket notation
  sens[cv] <- conf.mat[1,1]/conf.mat[1,3]
  spec[cv] <- conf.mat[2,2]/conf.mat[2,3]
  ppv[cv] <- conf.mat[1,1]/conf.mat[3,1]
  npv[cv] <- conf.mat[2,2]/conf.mat[3,2]
} #End for loop

mean(sens)
mean(spec)
mean(ppv)
mean(npv)

#prediction
new.df <- data.frame(January= c(-13.98,-17.80, -17.27, -12.52, -15.99, -11.97, -15.75, -16.19, -17.87, -12.44),
                     August_max= c( 15.89, 18.07, 16.74, 18.06, 18.23, 15.81, 16.85, 16.51, 17.84, 16.96),
                     Precip= c(771.13, 788.54, 677.63, 522.77, 732.32, 615.96, 805.90, 714.57, 740.50,801.22),
                     NC="No", SE="Yes")

pred.prob <- predict.glm(b.llr,newdata=new.df,type="response")

mydf <- data.frame(pred.prob)
rownames(mydf) <- c(2018, 2019,2020,2021,2022, 2023,2024,2025,2026,2027)
colnames(mydf) <- "Prediction Probabilites"


