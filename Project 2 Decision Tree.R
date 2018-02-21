
# this downloads and unzips the dataset
 temp <- tempfile()
 download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip",temp, mode="wb")
 unzip(temp, "bank-full.csv")
 unlink(temp)
#setwd("~/taoj@mail.gvsu.edu/gvsu/course/CIS678/Bank Marketing by Decision Tree/data/bank")
bank <- read.table("bank-full.csv", sep=";", header=T)

library(data.tree)
# check the pure, the class should be last column
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

# check the entropoy``
Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

# cal gain
InformationGain <- function( tble ) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

# retrun p of majority  in a given vector
predict.outcome <- function(outcome.vector){
  # Predict class based on outcome.vector
  
  # Determine possible outcomes and percentage occurrence
  outcomes <- unique(outcome.vector)
  outcome.counts <- rep(0, length(outcomes))
  names(outcome.counts) <- as.character(outcomes)
  # count num for each element in vecotr
  for(item in names(outcome.counts)){
    outcome.counts[item] <- sum(outcome.vector == item)
  }
  # calc P
  outcome.counts <- outcome.counts / sum(outcome.counts)
  # return prediction based on max outcome counts
  max.index <- which(outcome.counts == max(outcome.counts))
  return(list('predict' = names(outcome.counts)[max.index][1],
              'margin'  = max(outcome.counts)))
}
# find the initial node
rootNode = function(data){
  ig <- sapply(colnames(data)[-ncol(data)], 
               function(x) InformationGain(
                 table(data[,x], data[,ncol(data)])
               )
  )
  feature <- names(ig)[ig == max(ig)][1]
  return(feature)
}

# train model
TrainID3 <- function(node, data, thredhold, purity) {
  node$obsCount <- nrow(data)
  # set the purity to nodes
  node$purity <- predict.outcome(data[,ncol(data)])$margin
  #if the data-set is pure, then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature
    child <- node$AddChild(unique(data[,ncol(data)]))
    child$purity <- 1
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  }
  # no attribute can be used for spliting or less then threshold num
  else if(ncol(data)==1 | nrow(data)<thredhold | node$purity>purity)
  {
    predict <- predict.outcome(data[,ncol(data)])
    child <- node$AddChild(predict$predict)
    child$purity <- predict$margin
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''

  }
  else {
    #choose the feature with the highest information gain
    ig <- sapply(colnames(data)[-ncol(data)], 
                    function(x) InformationGain(
                      table(data[,x], data[,ncol(data)])
                    )
    )
    feature <- names(ig)[ig == max(ig)][1]
    
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    childObs <- split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value
      # child <- node$AddChild(paste(feature,":",names(childObs)[i]))
      child <- node$AddChild(names(childObs)[i])
      #if(class(childObs[[i]])=="factor"){childObs[[i]] <-as.data.frame(childObs[[i]])}
      childObs[[i]] <-as.data.frame(childObs[[i]])
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]], thredhold,purity)
    }
  }
}

# prediction
Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) {
    return (tree$children[[1]]$name)}
  child <- tree$children[[as.character(features[tree$feature][[1]])]]
  if(is.null(child)){
    return ("unknow")}
  return ( Predict(child, features))
}

# automatic bin
binconti <- function(df, conti.name, class.name){
  subdf <- df[,c(conti.name,class.name)]
  # sort by continus numbers ascedently
  subdf <- subdf[order(subdf[,conti.name],subdf[,class.name]),]
  # find the split point
  temp <- subdf[1, class.name] # save previous point
  enthropy.orgi <- Entropy(table(subdf[,class.name]))
  rownum <- nrow(subdf)
  # record gain and split point
  gain <- NULL
  splitpoint <- NULL
  for(i in 2: rownum){
    if(temp != subdf[i, class.name]){
      # calculate inforgain
      enthropy1 <- Entropy(table(subdf[1:i-1,class.name]))
      enthropy2 <- Entropy(table(subdf[i:rownum,class.name]))
      gain <- append(gain,enthropy.orgi - ((i-1)/rownum)*enthropy1 - ((rownum-i+1)/rownum)*enthropy2)
      splitpoint <- append(splitpoint, subdf[i,conti.name])
      temp <- subdf[i,class.name] # change to new class
    }
  }
  point <- cbind(splitpoint, gain)
  return (point)
}

# start to prepare date
#bank <- read.table("bank-full.csv", sep=";", header=T)
# bin for age
# banktemp <-bank
# bank <-banktemp

# bank$age <- cut(bank$age, breaks=c(-Inf, 20,30, 40,50,60,Inf), 
#                 labels=c("~20","21~30","31~40","41~50","51~60","6~"))
# bank$age <- cut(bank$age, breaks=c(-Inf, 30,55,Inf), 
#                 labels=c("~30","31~60","55~"))
# 
# partition.table = table(bank$age,bank$y)
# row.sums = as.vector(rowSums(partition.table))
# prob = partition.table / row.sums
# mosaicplot(partition.table , shade = T, xlab = "age", ylab = "y", main = "Mosaic Plot")

# check for point to split continuous variables at
p<-binconti(bank, "age","y")
plot(p)
bank$age = cut(bank$age, breaks=c(-Inf, 61, Inf), labels=c("<61",">=61"))
summary(bank$age)

# bin for balance
p<-binconti(bank, "balance","y")
plot(p)

summary(bank$balance)
bank$balance <- cut(bank$balance, breaks=c(-Inf, 72,Inf),
                    labels=c("<72",">=72"))
summary(bank$balance)

partition.table = table(bank$balance,bank$y)
row.sums = as.vector(rowSums(partition.table))
prob = partition.table / row.sums
mosaicplot(partition.table , shade = T, xlab = "balance", ylab = "y", main = "Mosaic Plot")

# bin for pday
table(bank$pdays)  # too many nosie, skip
p<-binconti(bank, "pdays","y")
plot(p)

# start to build tree
# plot correlation matrix to determine related attributes
pairs(~y+month+contact+duration+pdays, data=bank)
pairs(~y+day+campaign+previous, data=bank)

# delete unrelated attribute
MyData = subset(bank,select = -c(month,contact,duration,pdays,day,campaign,previous))

# split to training and test data
MyData <- MyData[sample(1:nrow(MyData)),]
MyTest <- MyData[40001:45211,]
MyTrain <- MyData[1:40000,]
library(ROSE)

# oversampling training data
MyTrain <- ovun.sample(y ~ ., data = MyTrain, method = "both",N = 40000, p=0.5, seed=1)$data

MyTrain <- MyTrain[sample(1:nrow(MyData)),]

MyTrain <- MyTrain[MyTrain$y %in% c("yes","no"),]

# cross-validation
library(caret)

# set the sample thredhold and purity threhold
threshold.sample <- c(2000)
threshold.purity <- c(0.9)
# set error matrix for recording validation error
error.matrix <- matrix(rep(0,length(threshold.purity)*length(threshold.sample)),
                           nrow=length(threshold.sample),ncol=length(threshold.purity))
colnames(error.matrix) <- threshold.purity
rownames(error.matrix) <- threshold.sample
for(s in 1:length(threshold.sample)){
  for(p in 1:length(threshold.purity)){
    set.seed(1)
    # set fold 10
    fold = 10
    idx <- createFolds(c(1:dim(MyTrain)[1]), k=fold)
    error <- rep(0,fold)
    # k fold
    for (i in 1:fold){
      # split training and validation datale
      learn    <- MyTrain[-idx[[i]], ]
      x.valid    <- MyTrain[idx[[i]], ]
      y.valid    <- MyTrain[idx[[i]], ncol(MyTrain)]
      # traning
      bankNode = rootNode(bank)
      tree <- Node$new(bankNode)
      TrainID3(tree, learn,threshold.sample[s],threshold.purity[p])
      # validation
      result <- rep(0,nrow(x.valid))
      for(row in 1:nrow(x.valid)){
        result[row]  <-  Predict(tree,x.valid[row,])
      }
      error[i] <- mean(result!=y.valid)
    }
    error.matrix[s,p] <- mean(error)
  }
}
#View(error.matrix)
plot(tree)

# test
# the test data is not over sampling
MyTest.x <- MyTest[,-ncol(MyTest)]
MyTest.y <- MyTest[,ncol(MyTest)]
result.test <- rep(0,nrow(MyTest.x))
for(row in 1:nrow(MyTest.x)){
  result.test[row]  <-  Predict(tree,MyTest.x[row,])
}
error.test <- mean(result.test!=MyTest.y)
print(error.test)


# plot errors
library(ggplot2)
thresh.sample = c(100,1000,1500,2000,2500,5000,7000,10000,20000,30000,40000)
validation.error = c(.9914,.7640,.3451,.3017,.3532,.3282,.3280,.3706,.3885,.3996,.4979)
errordf = data.frame(thresh.sample, validation.error)

ggplot(data=errordf, aes(x=thresh.sample, y=validation.error)) + geom_line() + geom_point()
