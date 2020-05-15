#Loading the library
library(shiny)
library(ggplot2)
library("ggrepel")
#writeLines("Please Select the Dataset...")


setwd("E:/NEU/Classes/Fall 2019/Comm Visualization/Final Project Dataset")

mydata <- read.csv("E:/NEU/Classes/Fall 2019/Comm Visualization/Final Project Dataset/2015_Street_Tree_Census_-_Tree_Data.csv")

#View(mydata)


# Defining UI for application   
ui <- fluidPage(
  # Application title
  titlePanel("Statistics for street trees"),
  
  #Input to the plots
  mainPanel(
    width = 12,
    
    splitLayout(
      #Setting widths for input selections
      cellWidths = c("100%"),
      
      checkboxGroupInput(
        inputId = "borough",
        label = "Select Borugh:",
        choices = unique(mydata$borough),
        inline = TRUE,
        selected = ""
      )
      # selectInput(
      #   size = 3,
      #   selectize = FALSE,
      #   inputId = "borough",
      #   label = "Select Borugh:",
      #   choices = unique(mydata$borough),
      #   selected = "All"
      # )
      
    ),
    #Plotting barplots in the UI
    splitLayout(
      cellWidths = c("50%", "50%"),
      plotOutput("Plot1"),
      #uiOutput("dynamic"),
      plotOutput("Plot2")
    ),
    
    splitLayout(
      cellWidths = c("50%", "50%"),
      plotOutput("Plot3"),
      plotOutput("Plot4")
    )
    
  )
)

#mydata[mydata[,'borough']== 'Queens', ]

# Defining server logic required for plotting

server <- function(input, output) {
  
  #mydata<-mydata[mydata[,'borough']== input$borough, ]
  output$Plot1 <- renderPlot({
    
    if(is.null(input$borough)== F){
    mydata<-mydata[mydata[,'borough']== input$borough, ]}
    ggplot(data=mydata,aes(x=tree_dbh))+geom_histogram(fill="#00AFBB",stat ="count",binwidth=1)+
      coord_flip()+ggtitle("NYC Tree Diameter")+xlim(0,50)+ylab("Tree_count")+xlab("Diameter")+ theme_bw()
    
  })
  
  
  output$Plot2 <- renderPlot({
    
    if(is.null(input$borough)== F){
      mydata<-mydata[mydata[,'borough']== input$borough, ]}
    options(scipen=99999)
    ggplot(data=mydata,aes(x=health))+geom_histogram(fill="#CC79A7",stat ="count")+
      ggtitle("NYC Tree Health")+ylab("Tree_count")+ theme_bw()
  })
  
  output$Plot3 <- renderPlot({
    
    rownames(mydata)
    if(is.null(input$borough)== F){
      mydata<-mydata[mydata[,'borough']== input$borough, ]}
    ggplot(data=mydata,aes(x=status))+geom_histogram(fill="#FC4E07",stat ="count")+
      ggtitle("NYC Tree Status")+ylab("Tree_count")+ theme_bw()
    })
  
  output$Plot4 <- renderPlot({
    
    if(is.null(input$borough)== F){
      mydata<-mydata[mydata[,'borough']== input$borough, ]}
    ggplot(data=mydata,aes(x=user_type))+geom_histogram(fill="#E7B800",stat ="count")+
      ggtitle("User Category")+ylab("Tree_count")+ theme_bw()
  })
  
  # output$dynamic <- renderUI({
  #   req(input$plot_hover) 
  #   verbatimTextOutput("vals")
  # })
  # 
  # output$vals <- renderPrint({
  #   hover <- input$plot_hover 
  #   # print(str(hover)) # list
  #   y <- 1 #nearPoints(mydata, input$plot_hover)[input$var_y]
  #   req(nrow(y) != 0)
  #   y
  # })
}

# Running the application
shinyApp(ui = ui, server = server)