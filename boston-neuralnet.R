install.packages("MASS")
library(MASS)
head(Boston)
str(Boston)

#Checking for Null values
any(is.na(Boston))

data<-Boston

maxs<-apply(data,2,max)
mins<-apply(data,2,min)
maxs
mins

#Normalize our Data
scaled.data<-scale(data,center = mins,scale=maxs-mins)
scaled<-as.data.frame(scaled.data)
head(scaled)
head(Boston)


#Splitting the dataset into training and testing dataset
library(caTools)
split<-sample.split(scaled$medv,SplitRatio = 0.7)
train<-subset(scaled,split==T)
test<-subset(scaled,split==F)


#Installing neuralnet for neural network
install.packages('neuralnet')
library(neuralnet)


n<-names(train)
n

# Paste together
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
f


#Training the neuralnet
nn<-neuralnet(f,data=train,hidden=c(5,3),linear.output = T)
plot(nn)


#Getting the predictions
predicted.nn.values<-compute(nn,test[1:13])
str(predicted.nn.values)


true.predictions<-predicted.nn.values$net.result*(max(data$medv)-min(data$medv))+min(data$medv)


#Convert the test data
test.r<-(test$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn<-sum((test.r-true.predictions)^2)/nrow(test)
MSE.nn


error.df<-data.frame(test.r,true.predictions)
head(error.df)

#Plotting true predictions against the actual test values
library(ggplot2)
ggplot(error.df,aes(x=test.r,y=true.predictions)) + geom_point()+
  stat_smooth()
