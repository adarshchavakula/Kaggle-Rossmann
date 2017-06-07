wd <- '~/Analytics/Rossmann'
setwd(wd)
train <- read.csv('train.csv')
train <- train[which(train$Open==1),]
store <- read.csv('store.csv')
st <- 1115
st_id <- 1:1115
med <- array(0,dim=st)

sales_p <- data.frame('sid'=st_id,'day1'=med,
                      'day2'=med,'day3'=med,'day4'=med,
                      'day5'=med,'day6'=med,'day7'=med)

sales_np <- data.frame('sid'=st_id,'day1'=med,
                       'day2'=med,'day3'=med,'day4'=med,
                       'day5'=med,'day6'=med,'day7'=med)

for(s in 1:st){
  x <- train[which(train$Store==s),]
  for(d in 1:7){
    xd <- x[which(x$DayOfWeek==d),]
    xdp <- xd[which(xd$Promo==1),]
    xdnp <- xd[which(xd$Promo==0),]
    sales1 <- as.vector(xdp$Sales)
    sales2 <- as.vector(xdnp$Sales)
    sales_p[s,(d+1)] <- median(sales1)
    sales_np[s,(d+1)] <- median(sales2)
  }
}

sales_p[is.na(sales_p)] = 0
sales_np[is.na(sales_np)] = 0
rm(train)

test <- read.csv('test.csv')
preds <- array(0,nrow(test))

for(t in 1:nrow(test)){
  if(test$Promo[t]==1){
    preds[t] <- sales_p[test$Store[t],(test$DayOfWeek[t]+1)]
  }
  else{
    preds[t] <- sales_np[test$Store[t],(test$DayOfWeek[t]+1)]
  }
}

sub <- read.csv('sample_submission.csv')
sub$Sales <- preds
write.table(sub,'submission_Median1.csv',row.names=FALSE,sep=',')
