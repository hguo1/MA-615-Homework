library(ggmap)
library(ggplot2)
library(maptools)
library(maps)
library(sp)
library(shiny)
library(graphicsQC)
library(shinydashboard)
mapWorld <- map_data("world")
map1<-ggplot(mapWorld, aes(x=long, y=lat, group=group))+
    geom_polygon(fill="white", color="black") +
    coord_map(xlim=c(-180,180), ylim=c(-60, 90))
map2 <- map1 + coord_map("cylindrical",xlim=c(-180,180), ylim=c(-60, 90))
map3 <- map1 + coord_map("mercator",xlim=c(-180,180), ylim=c(-60, 90))
map4<- map1 + coord_map("sinusoidal", xlim=c(-180,180), ylim=c(-60, 90))
map5<- map1 + coord_map("gnomonic", xlim=c(-180,180), ylim=c(-60, 90))
map6<- map1 + coord_map("rectangular", parameters = 0, xlim=c(-180,180), ylim=c(-60, 90))
map7<-map1 + coord_map("cylequalarea", parameters = 0, xlim=c(-180,180), ylim=c(-60, 90))


uif<-fluidPage(
    selectInput("projections", "Projections:",
                c( "orginal"="map1",
                  "cylindrical" = "map2",
                  "mercator" = "map3",
                  "sinusoidal" = "map4",
                  "gnomonic" = "map5",
                  "rectangular" = "map6",
                  "cylequalarea" = "map7")),
    plotOutput("map")
)

server <- function(input, output) { 
    output$map<- renderPlot({
        if(input$projections=="map1"){
            map1
        }else if(input$projections=="map2"){
            map2
        }else if(input$projections=="map3"){
            map3
        }else if(input$projections=="map4"){
            map4
        }else if(input$projections=="map5"){
            map5
        }else if(input$projections=="map6"){
            map6
        }else {
            map7
        }
        
    })
}

shinyApp(uif, server)



