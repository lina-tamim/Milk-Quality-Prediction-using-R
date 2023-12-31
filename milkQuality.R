library(outliers)
install.packages("outliers")
library(dplyr)
install.packages("dplyr")
library(magrittr)
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
install.packages("party")
library(party)
install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)
library(ggplot2)
library(lubridate)
library(tidyverse)
install.packages("cowplot")
library(cowplot)
install.packages("DataCombine")
install.packages("stargazer")
install.packages("dyn")
install.packages("lmtest")
install.packages("forecast")
install.packages("gridExtra")
install.packages("factoextra")
install.packages("NbClust")
library(scales) 
library(DataCombine)
library(stargazer)
library(sandwich)
library(dyn)
library(lmtest)
library(data.table)
library(tidyr)
library(readr)
library(stringr)
library(forecast)
library(gridExtra)
library(factoextra)
library(cluster)
library(NbClust)




# 1-preprocessing part: 
#change type of Taste , Odor , Fat , Turbidity , color into factor 
milknew$Taste <-as.factor(milknew$Taste)
milknew$Odor <-as.factor(milknew$Odor)
milknew$Fat <-as.factor(milknew$Fat)
milknew$Turbidity <-as.factor(milknew$Turbidity)
milknew$Colour <-as.factor(milknew$Colour)


#check missing value:
sum(is.na(milknew))
#five number summary 
summary(milknew)

#plots
#1-box plot
boxplot(Tempature~milknew , data = milknew)
boxplot(pH~milknew , data = milknew)

#2-histogram
hist(milknew$pH)
hist(milknew$Temprature)

#3-piechart
# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(value*100), "%")), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
geeks <- c(429, 374, 256 )
labels <- c("Low", "Medium", "High")

piepercent<- round(100 * geeks / sum(geeks), 1)

# Plot the chart.
pie(geeks, labels = piepercent, main = "Grade pie chart")

legend("topright", c("Low", "Medium", "High"), cex = 0.5)

#4-scatter plot

input <- milknew[,c('Temprature','pH')]
# Plot the chart for cars with weight between 2.5 to 5 and mileage between 15 and 30.
plot(x = input$Colour,y = input$pH,
     xlab = "pH",
     ylab = "Temprature",
     xlim = c(3,9.5),
     ylim = c(30,90),		 
     main = "Colour and pH"
)
x <- c(milknew$pH)
y <- c(milknew$Temprature)
plot(x, y, main="Observation of milk", xlab="ph", ylab="temp")



#A-data cleaning 

#check missing value 
sum(is.na(milknew))

#detect outlires and remove it 
View(milknew)
dim(milknew)

outlierpH = outlier(milknew$pH , logical = TRUE)
sum(outlierpH)
FindoutlierpH = which(outlierpH==TRUE , arr.ind = TRUE)
milknew= milknew [-FindoutlierpH,]
nrow(milknew)
outlierTemprature = outlier(milknew$Temprature , logical = TRUE)
sum(outlierTemprature)
FindooutlierTemprature = which(outlierTemprature==TRUE , arr.ind = TRUE)
milknew= milknew [-FindooutlierTemprature,]
nrow(milknew)

# after removing outliers
dim(milknew)  
str(milknew)
summary(milknew)


#B-data cleaning

#normailzation 
dataWithoutNormalization <- milknew
print(dataWithoutNormalization)
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
milknew$Temprature<-normalize(dataWithoutNormalization$Temprature)
print(milknew)


#encoding:
# we alreadychange type of Taste , Odor , Fat , Turbidity , color into factor 

milknew$Grade = factor(milknew$Grade , levels = c("low" , "medium" , "high") , labels=c(1,2,3))

#---------------------PART2


#classification 

#1- split data(70%-30%)
set.seed(1234)
ind<-sample(2,nrow(milknew),replace=TRUE,prob=c(0.7,0.3))
trainData<-milknew[ind==1,]
testData<-milknew[ind==2,]
myFormula<- Grade~pH+Temprature+Taste+Odor+Fat+Turbidity+Colour
milknew_ctree<-ctree(myFormula, data=trainData)
table(predict(milknew_ctree), trainData$Grade)

#tree
print(milknew_ctree)
plot(milknew_ctree)
plot(milknew_ctree , type="simple")

#B-test data
testPred <-predict(milknew_ctree , newdata=testData)
#####c-Evaluat model ###
table(testPred , testData$Grade)
results <-confusionMatrix (testPred , testData$Grade)
acc<- results$overall ["Accuracy"]*100
as.table(results)
as.matrix(results)
as.matrix(results , what ="overall")
as.matrix(results , what ="classes")
print(results)

#2- split data(80%-20%)
set.seed(1234)
ind<-sample(2,nrow(milknew),replace=TRUE,prob=c(0.8,0.2))
trainData<-milknew[ind==1,]
testData<-milknew[ind==2,]
myFormula<- Grade~pH+Temprature+Taste+Odor+Fat+Turbidity+Colour
milknew_ctree<-ctree(myFormula, data=trainData)
table(predict(milknew_ctree), trainData$Grade)

#tree2
print(milknew_ctree)
plot(milknew_ctree)
plot(milknew_ctree , type="simple")

#B-test data(80-20)
testPred <-predict(milknew_ctree , newdata=testData)

#####c-Evaluat model ###
table(testPred , testData$Grade)
results <-confusionMatrix (testPred , testData$Grade)
acc<- results$overall ["Accuracy"]*100
as.table(results)
as.matrix(results)
as.matrix(results , what ="overall")
as.matrix(results , what ="classes")
print(results)

#3- split data(60%-40%)
set.seed(1234)
ind<-sample(2,nrow(milknew),replace=TRUE,prob=c(0.6,0.4))
trainData<-milknew[ind==1,]
testData<-milknew[ind==2,]
myFormula<- Grade~pH+Temprature+Taste+Odor+Fat+Turbidity+Colour
milknew_ctree<-ctree(myFormula, data=trainData)
table(predict(milknew_ctree), trainData$Grade)
#tree3
print(milknew_ctree)
plot(milknew_ctree)
plot(milknew_ctree , type="simple")

#B-test data(60-40)
testPred <-predict(milknew_ctree , newdata=testData)

#####c-Evaluat model ###

table(testPred , testData$Grade)
results <-confusionMatrix (testPred , testData$Grade)
acc<- results$overall ["Accuracy"]*100
as.table(results)
as.matrix(results)
as.matrix(results , what ="overall")
as.matrix(results , what ="classes")
print(results)



#clustring 

milknew1<-milknew
milknew1$Grade<-NULL



milknew1$Taste<-as.numeric(as.character(milknew1$Taste))
milknew1$Odor<-as.numeric(as.character(milknew1$Odor))
milknew1$Fat<-as.numeric(as.character(milknew1$Fat))
milknew1$Turbidity<-as.numeric(as.character(milknew1$Turbidity))
milknew1$Colour<-as.numeric(as.character(milknew1$Colour))



set.seed(1234)

milknew2<-scale(milknew1)

kmeans.result<-kmeans(milknew2,2)
kmeans.result
fviz_cluster(kmeans.result, data=milknew2)
sil<-silhouette(kmeans.result$cluster, dist(milknew2))
fviz_silhouette(sil)

kmeans.result2<-kmeans(milknew2,3)
kmeans.result2
fviz_cluster(kmeans.result2, data=milknew2)
sil<-silhouette(kmeans.result2$cluster, dist(milknew2))
fviz_silhouette(sil)

kmeans.result3<-kmeans(milknew2,9)
kmeans.result3
fviz_cluster(kmeans.result3, data=milknew2)
sil<-silhouette(kmeans.result3$cluster, dist(milknew2))
fviz_silhouette(sil)




fviz_nbclust(milknew2, kmeans, method = "silhouette")+ labs(subtitle = "silhouette method")


install.packages("GGally")
library(GGally)

install.packages("plotly")
library(plotly)

input<-milknew2[,1:7]
milknew2<-as.data.frame(milknew2)
milknew2$cluster<-as.factor(kmeans.result$cluster)
p<- ggparcoord(data=milknew2, columns = c(1,2,3,4,5,6,7), groupColumn = "cluster", scale="std")
ggplotly(p)


write.csv(milknew, file="milknew.csv")









