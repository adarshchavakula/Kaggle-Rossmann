
# All derived features
# Predict the sales with some experiments

wd <- '~/Analytics/Rossmann'
setwd(wd)
set.seed(22)
makeCV <- function(x,n){
  y <- 1:nrow(x)
  ts_id <- array(0,nrow(x))
  ts_id[which(x$Year==2015 & x$Month>=6)] <- 1
  xts <-x[which(x$Year==2015 & x$Month>=6),] 
  xtr <-x[which(x$Year2015 & x$Month>=6)] 
  return(list(xtr,xts))
}

xtrain <- read.csv('train_feats4.csv')

xtrain <- xtrain[which(xtrain$Sales>0),]
xtrain$DayOfWeek <- as.factor(xtrain$DayOfWeek)
xtrain$Promo <- as.factor(xtrain$Promo)
xtrain$Promo2 <- as.factor(xtrain$Promo2)
xtrain$Month <- as.factor(xtrain$Month)
xtrain$StateHoliday <- as.factor(xtrain$StateHoliday)
xtrain$SchoolHoliday <- as.factor(xtrain$SchoolHoliday)
xtrain$StoreType<- as.factor(xtrain$StoreType)
xtrain$Assortment <- as.factor(xtrain$Assortment)
xtrain$IsComp1km <- as.factor(xtrain$IsComp1km)
xtrain$CompositeType <- as.factor(xtrain$CompositeType)

xtrain$High <- as.factor(xtrain$High)
xtrain$Low <- as.factor(xtrain$Low)

xtrain$kfactor <- xtrain$Sales/xtrain$Med
#x <-makeCV(xtrain,300000)
#xtr <- x[[1]]
#xts <- x[[2]]
#xtr <- xtrain[xtrain$Ye]
#rm(x)
require(gbm)
set.seed(91089)
gbr <- gbm(Sales~
             DayOfWeek+Promo+Month+StateHoliday+IsComp1km+
             SchoolHoliday+Promo2+High+Low+Variation+
             CompositeType+Med,
           distribution = "gaussian",
           data = xtrain,
           var.monotone = NULL,
           n.trees = 200,
           interaction.depth = 6,
           n.minobsinnode = 100,
           shrinkage = 0.1,
           bag.fraction = 0.9,
           train.fraction = 1.0,
           cv.folds=0,
           keep.data = TRUE,
           verbose = TRUE,
           class.stratify.cv=NULL,
           n.cores = NULL)

#zp <- predict(gbr, xtest,n.trees = gbr$n.trees)*xtest$HMs
#z <- as.array(xts$Sales)
#score <- sqrt(sum(((z-zp)/z)^2)/length(z))
#score





#-------------------------------------------------------
#rm(xtrain)
xtest <- read.csv('test_feats4.csv')
xtest$DayOfWeek <- as.factor(xtest$DayOfWeek)
xtest$Promo <- as.factor(xtest$Promo)
xtest$Promo2 <- as.factor(xtest$Promo2)
xtest$Month <- as.factor(xtest$Month)
xtest$StateHoliday <- as.factor(xtest$StateHoliday)
xtest$SchoolHoliday <- as.factor(xtest$SchoolHoliday)
xtest$StoreType<- as.factor(xtest$StoreType)
xtest$Assortment <- as.factor(xtest$Assortment)
xtest$IsComp1km <- as.factor(xtest$IsComp1km)
xtest$CompositeType <- as.factor(xtest$CompositeType)
xtest$High <- as.factor(xtest$High)
xtest$Low <- as.factor(xtest$Low)

zp <- predict(gbr, xtest,n.trees = gbr$n.trees)
sub <- read.csv('sample_submission.csv')
sub$Sales <- zp
write.table(sub,'submission_GBM_new2.csv',row.names=FALSE,sep=',')

