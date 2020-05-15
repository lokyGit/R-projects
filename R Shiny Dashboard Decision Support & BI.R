####################################
####  R Shiny Dashboard Decision Support & BI - Lokesh.R ###
####################################

#install.packages("tidyverse")
library("tidyverse")
library("dplyr")
#install.packages("corrplot")
library(corrplot)
#install.packages("ggpubr")
library("ggpubr")
library("shiny")
library("ggplot2")
library("tidyverse")



#Desinging the UI of the R shiny app

ui <- navbarPage("Boston Property Trends Over The Past Two Centuries",
               
        
                 
                 tabPanel("Find out the variable Dependencies here!!",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput(inputId = "outlierCheck",
                                            label = "Check this, if you want to remove the outliers!!",
                                            value = TRUE)
                            ),
                            mainPanel(
                              tabsetPanel( 
                                tabPanel("Building value dominates the property cost", plotOutput("CorrPlot")),
                                tabPanel("The figures", tableOutput("Corr_matrix"))
                              )
                            )
                          )
                 ),
                 
                 
                 tabPanel("Check how Residential Condition effects the properety value",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "R_OVRALL_CND", 
                                          label = "Residential Condition",
                                          choices = unique(noOutlier$R_OVRALL_CND),
                                          selected = "A"
                              ),
                              
                              hr(),
                              
                              checkboxInput(inputId = "outlierCheckRC",
                                            label = "Check this, if you want to remove the outliers!!",
                                            value = TRUE)
                            ),
                            mainPanel(
                              tabsetPanel( 
                                tabPanel("Effect of gross area on the property value", plotOutput("PointPlotCon")),
                            tabPanel("How building value effects the total property value",plotOutput("liregpoint"))
                                
                              )
                            )
                          )
                 ),
                 
                 
                 tabPanel("Does AC units effect the cost of the property?",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("R_AC", "Air conditioning type",
                                           c(
                                             "Central A/C"="C", 
                                             "Ductless"="D",
                                             "None" = "N"),
                                           selected = "N"
                              ),
                              
                              hr(),
                              
                              selectInput(inputId = "R_BLDG_STYL", 
                                          label = "Residential Building Style",
                                          choices = unique(noOutlier$R_BLDG_STYL),
                                          selected = "CV"
                              )
                              
                            ),
                           
                            
                            mainPanel(
                              
                              tabsetPanel( 
                              
                                tabPanel("Check how building effects the property value",plotOutput("barPlotBuildStyle")) ,
                                tabPanel("Role of Ac unit on Property value", plotOutput("barPlotAc"))
                                
                              )
                            )
                          )
                 ),
               
              
                 
                 tabPanel("5 bedroom units became cheaper in early 2000's",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput(inputId = "R_BDRMS",
                                          label = "Total number of bedrooms in the structure",
                                          min = 0,
                                          max = 18,
                                          value = 5,
                                          step = 1),
                              hr(),
                              
                              checkboxInput(inputId = "outlierCheck1",
                                            label = "Check this, if you want to Smoother to the plot",
                                            value = TRUE)
                            ),
                            mainPanel(
                              plotOutput("LinePlot")
                            )
                          )
                 )
                 
                 
                 
)

############################ Server Logic ################################


wd = "//Users//lokeshpalacharla//Library//Mobile Documents//com~apple~CloudDocs//NEU//Classes//Fall 2019//Decision Support and Business Intelligence//Week 3//Property_assessment"
setwd(wd)

prop_access =  read.csv("fy19fullpropassess.csv",header = T,na.strings=c("","NA"))


#Preparing dataFrame with focus variables

required_data =  prop_access[,c("PID", "ST_NAME", "ZIPCODE","LU","AV_LAND","AV_BLDG",
                                "AV_TOTAL","YR_BUILT","YR_REMOD","GROSS_AREA","LIVING_AREA",
                                "NUM_FLOORS","R_BLDG_STYL","R_TOTAL_RMS","R_BDRMS","R_KITCH","R_HEAT_TYP",
                                "R_AC","R_EXT_CND","R_OVRALL_CND","R_INT_CND",
                                "S_NUM_BLDG","S_BLDG_STYL","S_UNIT_RES","S_UNIT_COM","S_UNIT_RC","U_TOT_RMS",
                                "U_BDRMS","U_HEAT_TYP","U_AC","U_INT_CND")];


#Defining the server logic for the application

server <- function(input, output, session) {

  options(
    
    scipen = 9999
  )


#Correlation test for the Total assessment values and Year built variables
corr_av_yrbuilt =  cor.test(required_data$AV_TOTAL, required_data$YR_BUILT, method = "pearson")
corr_av_yrbuilt$estimate
#Preparing data for coorelation matrix and correlation plots



#correlation matrix
output$Corr_matrix <- renderTable({
  
  
  Outcorr_data = required_data[, c("PID","AV_LAND","AV_BLDG",
                            "AV_TOTAL","YR_BUILT","YR_REMOD","GROSS_AREA","LIVING_AREA","NUM_FLOORS")];
  
  OutbostonCorrmat = cor(Outcorr_data, use = "complete.obs")
  Bos_Corr_matrix = round(OutbostonCorrmat,2)
  
  
  
if(input$outlierCheck){
  
corr_data = noOutlier[, c("PID","AV_LAND","AV_BLDG",
                            "AV_TOTAL","YR_BUILT","YR_REMOD","GROSS_AREA","LIVING_AREA","NUM_FLOORS")];
  
bostonCorrmat = cor(corr_data, use = "complete.obs")
Bos_Corr_matrix = round(bostonCorrmat,2)

}
print(Bos_Corr_matrix)
})


#correlation plot
output$CorrPlot <- renderPlot({
  
  Outcorr_data = required_data[, c("PID","AV_LAND","AV_BLDG",
                                   "AV_TOTAL","YR_BUILT","YR_REMOD","GROSS_AREA","LIVING_AREA","NUM_FLOORS")];
  
  OutbostonCorrmat = cor(Outcorr_data, use = "complete.obs")
  OutBos_Corr_matrix = round(OutbostonCorrmat,2)
  CorPlot = corrplot(OutBos_Corr_matrix, type = "full", order = "hclust", 
           tl.col = "black", tl.srt = 45)
  
  if(input$outlierCheck){
    
    corr_data = noOutlier[, c("PID","AV_LAND","AV_BLDG",
                              "AV_TOTAL","YR_BUILT","YR_REMOD","GROSS_AREA","LIVING_AREA","NUM_FLOORS")];
    
    bostonCorrmat = cor(corr_data, use = "complete.obs")
    Bos_Corr_matrix = round(bostonCorrmat,2)

    CorPlot = corrplot(Bos_Corr_matrix, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45)
    
  }
  
  print(CorPlot)
  
})



#plot between NUM_FLOORS, AV_TOTAL based on the AC unit

output$barPlotAc <- renderPlot({

noOutlier = required_data %>%
  filter(AV_TOTAL < 700000000) %>%filter(!is.na(R_OVRALL_CND))%>% filter(!is.na(R_BLDG_STYL))%>% filter(!is.na(R_AC))%>% 
  select(PID:U_INT_CND)

noOutBar <-  noOutlier[noOutlier[,'R_AC'] %in% input$R_AC, ]
NoutBarData <- noOutBar[noOutBar[,'R_BLDG_STYL'] %in% input$R_BLDG_STYL, ]

ggplot(NoutBarData, aes(NUM_FLOORS, AV_TOTAL)) +
  geom_bar(stat = "identity", aes(col = NUM_FLOORS , width = 0.25)) +
  scale_y_continuous(labels = paste0(c(400, 800, 1200,1600,2200,2600,3000,3400), "K"),
                     breaks = 10^6 * c(400, 800, 1200,1600,2200,2600,3000,3400))+
  theme_bw()+
  labs(x = "Number of floors", y = "Total Accessment Value")
})

output$barPlotBuildStyle <- renderPlot({

#plot between TOTAL_RMS, AV_BLDG based on the building style
noOutlier = required_data %>%
  filter(AV_TOTAL < 700000000) %>%filter(!is.na(R_OVRALL_CND))%>% filter(!is.na(R_BLDG_STYL))%>% 
  select(PID:U_INT_CND)

noOutBar <-  noOutlier[noOutlier[,'R_AC'] %in% input$R_AC, ]
NoutBarData <- noOutBar[noOutlier[,'R_BLDG_STYL'] %in% input$R_BLDG_STYL, ]

ggplot(data=NoutBarData,aes(x=R_TOTAL_RMS ,y=AV_BLDG))+ 
  geom_bar(stat="identity", aes(color = R_BLDG_STYL, width = 0.5)) + 
  labs(x = "Total rooms in a structure", y = "Total Building accessment Value") + 
  theme_bw()+
  scale_y_continuous(labels = paste0(c(400, 800, 1200,1600,2200,2600,3000,3400), "K"),
                     breaks = 10^6 * c(400, 800, 1200,1600,2200,2600,3000,3400))
})


output$PointPlotCon <- renderPlot({

#plot on how building and total value changes based on the condition of the buildings
  
  OutPoint <-  required_data[required_data[,'R_OVRALL_CND'] %in% input$R_OVRALL_CND, ]  
  
  plotpoint =ggplot(OutPoint, aes(AV_TOTAL, GROSS_AREA)) +
    geom_point(aes(color = R_OVRALL_CND))+
    theme_bw()+
    scale_y_continuous(labels = paste0(c(50,100,150,200), "K"),
                       breaks = 10^5* c(50,100,150,200))+
    scale_x_continuous(labels = paste0(c(50,100,150,200), "K"),
                       breaks = 10^5* c(50,100,150,200))+
    labs(x = "Total Accessment Value", y = "Gross Area")+
    theme(legend.title  = element_text(size = 8, colour = "gray57"),
          legend.text =  element_text(colour = "darkslategray4"))
  
 
  
if(input$outlierCheckRC){
    
noOutlier = required_data %>%
  filter(AV_TOTAL < 700000000) %>%filter(!is.na(R_OVRALL_CND))%>% filter(!is.na(R_BLDG_STYL))%>% 
  select(PID:U_INT_CND)


noOutPoint <-  noOutlier[noOutlier[,'R_OVRALL_CND'] %in% input$R_OVRALL_CND, ]

plotpoint = ggplot(noOutPoint, aes(AV_TOTAL, GROSS_AREA)) +
  geom_point(aes(color = R_OVRALL_CND))+
  theme_bw()+ stat_smooth(method = lm)+
  scale_y_continuous(labels = paste0(c(50,100,150,200), "K"),
                     breaks = 10^5* c(50,100,150,200))+
  scale_x_continuous(labels = paste0(c(50,100,150,200), "K"),
                     breaks = 10^5* c(50,100,150,200))+
  labs(x = "Total Accessment Value", y = "Gross Area")+
  theme(legend.title  = element_text(size = 8, colour = "gray57"),
        legend.text =  element_text(colour = "darkslategray4"))

}

  print(plotpoint)
})




output$liregpoint <- renderPlot({
  #plot between AV TOTAL, AV BLDG with linear regression
  plotgraph = ggplot(required_data, aes(AV_TOTAL, AV_BLDG)) +
    geom_point() +
    stat_smooth(method = lm)+
    scale_y_continuous(labels = paste0(c(100,200,300,400,500,600,700,800), "K"),
                       breaks = 10^6* c(100,200,300,400,500,600,700, 800))+
    scale_x_continuous(labels = paste0(c(100,200,300,400,500,600,700,800), "K"),
                       breaks = 10^6* c(100,200,300,400,500,600,700, 800))+
    theme_bw()+
    labs(x = "Total Accessment Value", y = "Building accessement value")
  
  
  
  if(input$outlierCheckRC){
    
    noOutlierpoint <-  noOutlier[noOutlier[,'R_OVRALL_CND'] %in% input$R_OVRALL_CND, ]
    
    plotgraph = ggplot(noOutlierpoint, aes(AV_TOTAL, AV_BLDG)) +
      geom_point() +
      stat_smooth(method = lm)+
      scale_y_continuous(labels = paste0(c(100,200,300,400,500,600,700,800), "K"),
                         breaks = 10^6* c(100,200,300,400,500,600,700, 800))+
      scale_x_continuous(labels = paste0(c(100,200,300,400,500,600,700,800), "K"),
                         breaks = 10^6* c(100,200,300,400,500,600,700, 800))+
      theme_bw()+
      labs(x = "Total Accessment Value", y = "Building accessement value")
    
  }
  print(plotgraph)
  
})

#Line plot between Year built and Total accessement value
output$LinePlot <- renderPlot({
  
  ScatterData <-  required_data[required_data[,'R_BDRMS'] %in% input$R_BDRMS, ] 
  
  LinegraphData <-  aggregate(data = ScatterData,
                              AV_TOTAL~R_BDRMS+YR_BUILT
                              , FUN=mean)
  
  lineGraph = ggplot(LinegraphData, aes(x= YR_BUILT, y= AV_TOTAL, group = R_BDRMS)) +
    geom_line(aes(color = R_BDRMS)) +
    scale_y_continuous(labels = paste0(c(100,200,300,400,500,600,800), "K"),
                       breaks = 10^4 * c(100,200,300,400,500,600,800))+
    theme_bw()+
    labs(x = "Year Built", y = "Total Accessment Value")
  
 
  #handling outliers
  
  if(input$outlierCheck1){
    
    noOutlier = required_data %>%
      filter(AV_TOTAL < 700000000) %>%filter(!is.na(R_OVRALL_CND))%>% filter(!is.na(R_BLDG_STYL))%>% 
      select(PID:U_INT_CND)
    
    LinegraphData <-  aggregate(data = noOutlier,
                                AV_TOTAL~R_BDRMS+YR_BUILT
                                , FUN=mean)
    
    ScatterDataNoUt <-  LinegraphData[LinegraphData[,'R_BDRMS'] %in% input$R_BDRMS, ]
    
    lineGraph = ggplot(ScatterDataNoUt, aes(YR_BUILT, AV_TOTAL)) +
      geom_line(aes(color = R_BDRMS))+
      scale_y_continuous(labels = paste0(c(100,200,300,400,500,600,800), "K"),
                         breaks = 10^4 * c(100,200,300,400,500,600,800))+
      theme_bw()+
      labs(x = "Year Built", y = "Total Accessment Value")+
      geom_smooth()
    
  }
  
  print(lineGraph)
  
})





}

#Calling the R Shiny Application

shinyApp(ui = ui, server = server)
