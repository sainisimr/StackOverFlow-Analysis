library(shiny)
library(ggplot2)
library(arules)
library(gdata)
library(e1071)
library(arulesViz)
library(ggplot2)
library(taRifx)

server=function(input, output) {

  origdata <- read.csv(file="st.csv", header = T, sep=",")
  displaydata <- read.csv(file="stackoverflowdataset.csv", header = T, sep=",")
  #print(abc)
  #service for data navigation tab  
  output$contents = renderDataTable({
    displaydata
  })
  
  
  #service for Frequency
  subset<-origdata[c(11,16)]
  
  subset<-subset[!duplicated(subset[c(1,2)]),]
  
  aggSubSet<-split(subset$Tags,subset$LastEditorDisplayName)
  #print(aggSubSet)
  trns<-as(aggSubSet,"transactions")

  Rules<-apriori(trns,parameter=list(supp=0.05,conf=0.6,target="rules",minlen=2))
  Rules1<-apriori(trns,parameter=list(supp=0.05,conf=0.6,target="rules",minlen=2))
  #print(Rules)
  ItemSet<-inspect(Rules[1:151])

   output$Itemcontents<- renderDataTable({
     ItemSet[c(1,2,3,5)]
   })
   #service for MFI
   #graph sub tab apriori-graph
   output$aprioriGraph<- renderPlot({
     plot(Rules[1:100], method="graph", control=list(type="items"))
   })
   
   #graph sub tab apriori-parachod
   output$aprioriGraph1<- renderPlot({
     plot(Rules1[1:100], method="paracoord", control=list(reorder=TRUE))
   })
   
   #service for MFI
   fetchTopLevel <- reactive({
     topLevel <- input$top_level
     #print(aggSubSet)
     #write.csv(aggSubSet, file ="mfi.csv")
     itemFrequencyPlot(trns, topN = topLevel, type="absolute", popCol = "black",
                       xlab="Most Frequent Item (MFI)",ylab="Frequency",
                       main=paste("Top",topLevel,"Frequent Items",sep=" "))
   })
   
   #service for MFI
   output$ItemGraph <- renderPlot({
     topLevel <- fetchTopLevel()
     topLevel
   })
   
   
   ##logistic model
   posts <- read.csv("st.csv", header=TRUE, stringsAsFactors = F)
   posts$Tags = as.factor(posts$Tags)
   posts$AnswerCount = as.numeric(posts$AnswerCount)
   posts$OwnerUserId = as.numeric(posts$OwnerUserId)
   
   posts$Score = as.numeric(posts$Score)
   posts$CommentCount = as.numeric(posts$CommentCount)
   posts$FavoriteCount = as.numeric(posts$FavoriteCount)
   posts$ViewCount = as.numeric(posts$ViewCount)
   
   train.data <- posts[-which(posts$Important == "unknown"),c(1,2,3,6,8,15,16,17,18,13)]
   test.data <- posts[which(posts$Important == "unknown"),c(1,2,3,6,8,15,16,17,18,13)]
   
  
   
   train.data$rank[which(train.data$AcceptedAnswerId == "Yes" & (train.data$ViewCount == 100 & train.data$AnswerCount > 15
                                                                 & train.data$CommentCount == 10 & train.data$FavoriteCount == 2000) | (train.data$ViewCount == 100 &
                                                                                                                                          train.data$CommentCount == 10 ) 
                         | (train.data$ViewCount > 10 & train.data$AnswerCount > 30 ) | (train.data$ViewCount < 50 & train.data$AnswerCount > 0
                                                                                         & train.data$AnswerCount < 30 ))] = 1
   
   train.data$rank[-which(train.data$AcceptedAnswerId == "Yes" & (train.data$ViewCount == 100 & train.data$AnswerCount > 15
                                                                  & train.data$CommentCount == 10 & train.data$FavoriteCount == 2000) | (train.data$ViewCount == 100 &
                                                                                                                                           train.data$CommentCount == 10 ) 
                          | (train.data$ViewCount > 10 & train.data$AnswerCount > 30 ) | (train.data$ViewCount < 50 & train.data$AnswerCount > 0
                                                                                          & train.data$AnswerCount < 30 ))] = 0
   
   
   ##logistic model
   model.logit = glm(rank~ Tags +AnswerCount+CommentCount+FavoriteCount+ViewCount+Score+OwnerUserId, data = train.data, family = "binomial")
   ind = which(test.data$Tags %in% train.data$Tags == TRUE)
   test.data = test.data[ind,]
   test.data$rankpred = predict(model.logit, digit = 2, newdata = test.data, type = "response")
   print(test.data$rankpred)
   #test.data$rankpred <- round(test.data$rankpred, digits = 6)
   test.data = test.data[order(test.data$rankpred, decreasing = TRUE),]
   test.data <- na.omit(test.data)
   #test.data <- test.data[!is.na(test.data)]
   output$rankPredictContents<- renderDataTable({
     test.data[c(1,2,3,4,5,6,7,8,9,11)]
   })
   write.csv(test.data, file ="success_prob1.csv")
   output$logisticGraph<- renderPlot({
   ggplot(test.data, aes(x=Tags, y=rankpred)) + geom_point() + 
     stat_smooth( method="glm", se=FALSE)
   })
   
   #SVM data
   mydata <- read.csv(file="svm.csv", header = T, sep=",")
   
   svmSubset<-mydata[c(5,15,16,17)]
   
   svmSubset <- svmSubset[svmSubset$Tags =="c++",]
   
   svmSubset<-svmSubset[c(1,2,4)]
   
   
   svmSubset$Total.Cost <- svmSubset$Score
   svmSubset$Post.Year <- as.numeric(format(as.Date(svmSubset$CreationDate,"%m/%d/%Y"),"%Y"))
   svmSubset$Post.Date <- as.numeric(format(as.Date(svmSubset$CreationDate,"%m/%d/%Y"),"%m"))
   
   names(svmSubset)[6] <- "PostMonth"
   
   names(svmSubset)[2] <- "PostCount"
   
   names(svmSubset)[3] <- "Title"
   for(i in 1:length(svmSubset$Total.Cost))
   {
     med <- median(svmSubset$Total.Cost)
     if((svmSubset$Total.Cost[i])>med)
       svmSubset$Post.Val[i]="High"
     else
       svmSubset$Post.Val[i]="Low"
   }
   
   svmSubset2 <- svmSubset[c(7,6,2,3)]
   #print(svmSubset)
   processData <- write.csv(svmSubset2, file= "newdata.csv")
   #processData<-read.csv(file="newdata.csv",header=T,sep="," )
   cleanData <- read.csv(file="newdata.csv",header=T,sep=",")
   cleanData <- cleanData[c(2,3,4)]
   #print(cleanData)
   #model<-svm(Order.Val ~ .,data=cleanData)
   model<-svm(Post.Val ~ .,data=cleanData,kernel="polynomial",
              degree=3,coef0=0.045,cost=1.3,tolerance=0.008)
   
   plot(model,cleanData)
   
 
   
   #Create an index
   index<-1:nrow(cleanData)
   

   testindex<-sample(index,trunc(length(index)*25/100))
   
   #Segregate the testdataset and trainingdataset using the testindex
   testset<-cleanData[testindex,]
   trainingset<-cleanData[-testindex,]
   
   output$svmGraph<- renderPlot({
     plot(model,trainingset)
   })
   
 
}
