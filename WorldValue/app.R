library(shiny)
library(shinydashboard)
library(esquisse)
library(psych)
library(tidyverse)
library(ggplot2)
library(purrr)
library(plotrix)
library(ggridges)
library(tigerstats)
library(lattice)
library(mosaic)
library(hrbrthemes)
data.new<-read.csv("/Users/heguo/Desktop/615/MA-615-Homework/WorldValue/final_data_shiny.csv")
data.new<-data.new[,-257]
data.new$V97..Private.vs.state.ownership.of.business<-as.integer(data.new$V97..Private.vs.state.ownership.of.business)
tab6<-data.new %>% count(V97..Private.vs.state.ownership.of.business)


ui <- dashboardPage(
  dashboardHeader(title = "World Value Survey"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Intro", icon = icon("dashboard")),
      menuItem("Factor Analysis", tabName = "FDA", icon = icon("th")),
      menuItem("Figure", tabName = "EDA", icon = icon("th")),
      menuItem("Conclusion", tabName = "Con", icon = icon("th")),
      menuItem("Reference", tabName = "Refer", icon = icon("th"))
    )## sidebarMenu
  ),#Sidebar
  dashboardBody(
    tabItems(
      tabItem(tabName = "Intro",
              fluidRow(column(12,
                              box(width=50,solidHeader = TRUE, status = "primary",
                                  title="Introduction",
                                  h4("The World Value Survey (WVS) is a global social scientist research project 
                                     focus on the changing values and their influence on personal and social life. 
                                     The questions in WVS focus on: Support for democracy, 
                                     gender equality, culture value, belief toward gender, family and religion, opinions of poverty, education, health, social tolerance and trust, attitude toward social institutions, 
                                     culture difference between society, issue of justice, moral principles, risk, national security 
                                     and global governance, attitude toward minorities, subjective of well-being. 
                                      Current study chooses the data from China in 2013.",
                                     size = 10,style = "font-family: 'Arial'," )
                              )#box
              )#column
                
              )# fluidRow      
              ),#tabItemIntro
      tabItem(tabName = "FDA",
              fluidRow(column(12,
                              box(width=50,solidHeader = TRUE, status = "primary",
                                  title="Factor Analysis",
                                  h4("From the description of WVS, those questions can be divided into several categories. 
                                  It indicates that there are some variables which are highly corelated with other variables.  
                                     This study applies data factor analysis into data analyzing process.   ",
                                     size = 10,style = "font-family: 'Arial'," )
                              )#box
              )#column
              ),# fluidRow 
              plotOutput("fdaPlot")
              ),#tabItemFDA
      tabItem(tabName = "EDA",
              fluidRow(column(12,
                              box(width=50,solidHeader = TRUE, status = "primary",
                                  title="Lollipop Plot",
                                  h4("This pie plot shows that people's agreement about private ownership vs Government  ownership",
                                     size = 10,style = "font-family: 'Arial'," )
                              )#box
              )#column
              ),# fluidRow
              plotOutput("lollipopplot")
              ),#tabItemFigure
      tabItem(tabName = "Con",
              fluidRow(column(12,
                              box(width=50,solidHeader = TRUE, status = "primary",
                                  title="Conclusion",
                                  h4("This investigation has shown that family is important to people in China.   
                                     People in China admit that the one of main goal of their life is to make their parents proud.   
                                     Almost everyone in China does not belong to any religion group.  
                                     Males in China with high education background tend to believe that men should be hire when jobs are scarce.  
                                     When Chinese people have more interested in politics, they tend to believe men can be a better political leader.  
                                     People in China do not have a clear opinion about the business ownership structure in China, they tend to stay at a balance point.  
                                     Almost everyone in China are proud to be Chinese, and there is no difference between people from different social class.  
                                     This survey have a scientific design, so it could represent the true population value by analyzing sample. ",
                                     size = 10,style = "font-family: 'Arial'," )
                              )#box
              )#column
              )# fluidRow
              ),#Con
      tabItem(tabName = "Refer",
              fluidRow(column(12,
                              box(width=50,solidHeader = TRUE, status = "primary",
                                  title="Reference",
                                  h4("Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014. World Values Survey: Round Six - Country-Pooled Datafile Version: http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp. Madrid: JD Systems Institute.",
                                     size = 5,style = "font-family: 'Arial'," )
                              )#box
              )#column
              )
              )#Refer
    )#tabItems
  )#body
)

server <- function(input, output) { 
  output$fdaPlot <- renderPlot({
    parallel <- fa.parallel(data.new, fm = 'minres', fa = 'fa')
  })
  output$lollipopplot<- renderPlot({
    ggplot(tab6, aes(x=V97..Private.vs.state.ownership.of.business, y=n)) +
      geom_segment( aes(x=V97..Private.vs.state.ownership.of.business, xend=V97..Private.vs.state.ownership.of.business, y=0, yend=n), color="grey") +
      geom_point( color="#E9967A", size=8) +
      theme_light() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      ggtitle("Private ownership vs Government  ownership") +
      ylab("Count")+xlab("Attude Toward Ownership")+theme_ipsum()
  })
  }#server

shinyApp(ui, server)


# Run the application 
shinyApp(ui = ui, server = server)
