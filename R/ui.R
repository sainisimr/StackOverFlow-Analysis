

library(shiny)
library(Cairo)



shinyUI=navbarPage("StackOverFlow Analysis",
                   
#UI for Data Section in UI                   
  tabPanel("Data",
           dataTableOutput('contents'),
           hr(),
           
           list(basicPage(
          renderDataTable('contents')
        ))),

#UI for Logistic Regression in UI                   
tabPanel("Logitic Regression",
         dataTableOutput('rankPredictContents'),
         hr(),
         
         list(basicPage(
           renderDataTable('contents')
         ))),

#UI for Logistic Graph
tabPanel("Logistic Graph",basicPage(plotOutput("logisticGraph"))),

#UI for Apriori Analysis
tabPanel("Frequency",
         dataTableOutput('Itemcontents'),
         hr(),
         
         list(basicPage(
           renderDataTable('Itemcontents')
         ))
         ),

tabPanel("Fractional Count",plotOutput("aprioriGraph")),
tabPanel("Parallel Coordinates",plotOutput("aprioriGraph1")),
  
#UI for Most Frequent Item
  tabPanel("Most Frequent Tag",
           fluidPage(
             titlePanel("Most Frequent Tag"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "top_level",label = "Top Items",min=1,
                             max = 10,value=10), width = 4),
                 mainPanel(plotOutput("ItemGraph"))
               )
             )
           ),
  tabPanel("Behaviour",basicPage(plotOutput("svmGraph")))
    

)