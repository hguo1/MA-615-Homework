library(yelpr)
library(leaflet)
library(ggmosaic)
library(dplyr)
library(ggthemes)
library(ggmap)
library(tidyverse) 
library(cluster)    
library(factoextra)
library(shinythemes)
library(shiny)
library(plotly)
library(tidyverse)
library(tidytext)
library(knitr)
library(scales)
library(janeaustenr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(reshape2)
library(highcharter)
library(shinycssloaders)
library(shinyWidgets)
############################Map API############################
devtools::install_github('OmaymaS/yelpr')
client.id <- 'CYnB6kSW2xzzFpNRSHjrEQ'
client.key <- 'tGos9a9uRI6VWuBkK1EcYdNXIa1pWFOUMRoziue6b6hVoaT1aWp0EXLrO-buK_RYxK_3mgepSdxx5T5xlMstACpRR-eIRDXd4-zfr7a7EEqO3xb5JZxJswSvQ73eXXYx'
location.ma<-"Boston, MA"
limit <- 50
yelp.ma<-business_search(client.key,location.ma,limit)
business.ma<-yelp.ma$businesses
coordinates.ma<-business.ma$coordinates
coordinates.ma$name<-business.ma$name
boston <- leaflet(coordinates.ma) %>%
  addTiles() %>%  
  addMarkers(~longitude, ~latitude, popup=~name)
location.nyc<-"New York, NY"
limit <- 50
yelp.nyc<-business_search(client.key,location.nyc,limit)
business.nyc<-yelp.nyc$businesses
coordinates.nyc<-business.nyc$coordinates
coordinates.nyc$name<-business.nyc$name
nyc <- leaflet(coordinates.nyc) %>%
  addTiles() %>%  
  addMarkers(~longitude, ~latitude, popup=~name)

#################cluster group############################################################
yelp.business<-read.csv("business.csv")
cluster.B<-yelp.business %>%
  select(business_id,stars, review_count)
rownames(cluster.B) <- c()
rownames(cluster.B) <- cluster.B[,1]
cluster.B <- cluster.B[,-1]
cluster.B<-na.omit(cluster.B)
k2 <- kmeans(cluster.B, centers = 4, nstart = 25)
cluster = k2$cluster
yelp.business$cluster=cluster
yelp.business$cluster=as.factor(yelp.business$cluster)
yelp.business.C1<-subset(yelp.business,yelp.business$cluster==1)
yelp.business.C2<-subset(yelp.business,yelp.business$cluster==2)
yelp.business.C3<-subset(yelp.business,yelp.business$cluster==3)
yelp.business.C4<-subset(yelp.business,yelp.business$cluster==4)
##################################Stars############################################################
yelp.photo<-read.csv("photo.csv")
yelp.photo$business_id<-as.character(yelp.photo$business_id)
yelp.business$business_id<-as.character(yelp.business$business_id)
business.photo<-left_join(yelp.business,yelp.photo, by = "business_id")
business.photo$has.photo[is.na(business.photo$photo_id)==TRUE]<-"Doesn't have Photo"
business.photo$has.photo[is.na(business.photo$photo_id)==FALSE]<-"Has Photo"
business.photo$stars<-as.factor(business.photo$stars)
business.photo$has.photo<-as.factor(business.photo$has.photo)
Yelp.Lable<-business.photo%>%select(stars,label)
Yelp.Lable<-na.omit(Yelp.Lable)
##################################Text Mining############################################################
review <- read.csv("review.csv")
review$Rate[review$stars>4]<-"High"
review$Rate[review$stars>=3 & review$stars<=4 ]<-"Mid"
review$Rate[review$stars<3]<-"Low"
re.mining<-data.frame(review$Rate,review$text)
re.mining$review.text<-as.character(re.mining$review.text)
T.mining <-re.mining %>%group_by(review.Rate)%>%mutate(linenumber = row_number())%>%unnest_tokens(word, review.text) 
T.mining_sentiment <- T.mining %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(review.Rate, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
bing_word_counts <- T.mining %>%
  inner_join(get_sentiments("bing"),by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
##################################Freq Fitted############################################################
Rate_words <- re.mining %>%
  unnest_tokens(word, review.text) %>%
  count(review.Rate, word, sort = TRUE)

total_words <- Rate_words %>% 
  group_by(review.Rate) %>% 
  summarize(total = sum(n))
book_words<-na.omit(book_words)
book_words <- left_join(Rate_words, total_words)
freq_by_rank <- book_words %>% 
  group_by(review.Rate) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)
#########################################Consecutive Words#####################################################
mining_bigrams <- re.mining %>%
  unnest_tokens(bigram,review.text , token = "ngrams", n = 2)

bigrams_separated <- mining_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

mining_tf_idf <- bigrams_united %>%
  count(review.Rate, bigram) %>%
  bind_tf_idf(bigram, review.Rate, n) %>%
  arrange(desc(tf_idf))


#####################################################UI#####################################################

ui <- fluidPage(theme = shinytheme("slate"),
                navbarPage("Yelp Analysis",
                 tabPanel("Map from API",
                          fluidRow(column(4,wellPanel(radioButtons("apiMap", "Choose a city:",
                                                                   c("New York" = "nyc",
                                                                     "Boston" = "bos")))),
                                   column(8,wellPanel(addSpinner(leafletOutput("mapPlot"), spin = "cube", color = "#999999"))))
                          
                          
                          ),
                 tabPanel("Cluster Business",
                          fluidRow(column(4,wellPanel(radioButtons("ClusterMap", "Choose a Cluster Group:",
                                                                   c("Cluster 1" = "c1",
                                                                     "Cluster 2" = "c2",
                                                                     "Cluster 3" = "c3",
                                                                     "Cluster 4" = "c4")))),
                                   column(8,wellPanel(addSpinner(plotOutput("clusterPlot"), spin = "cube", color = "#999999"))))),
                 tabPanel("Stars & Photo",
                          fluidRow(column(12,wellPanel(addSpinner(plotlyOutput("stars1"), spin = "cube", color = "#999999"),
                                                       br(),
                                                       addSpinner(plotlyOutput("stars2"), spin = "cube", color = "#999999"))))),
                 
                 navbarMenu("Yelp Reviews",
                            tabPanel("Review",
                                     fluidRow(wellPanel(h2("This study analysis the review from Yelp users."))),
                                     fluidRow(wellPanel(addSpinner(plotOutput("cloud"), spin = "cube", color = "#999999")))),
                            tabPanel("Sentiment",
                                     wellPanel(h4("sentiment vs Rate Categories."),
                                               addSpinner(plotlyOutput("textS"), spin = "cube", color = "#999999"),
                                               br(),
                                               h4("Words Contirbutes"),
                                               addSpinner(plotlyOutput("textS2"), spin = "cube", color = "#999999"))),
                            tabPanel("Consecutive Words",wellPanel(h3("The study compute the tf_idf for bigrams across Rate category. tf_idf return how important a phrase in review text of users in Yelp.  
                                                                      The following plot visualized the tf_idf within each Rate category."),
                                                                   addSpinner(plotOutput("tf_idf"), spin = "cube", color = "#999999") )
                                     )),
                 tabPanel("Frequence Words",
                          wellPanel(h3("The plot shows that  the distribution of n/total for each Rate category"),
                                    addSpinner(plotOutput("freqW"), spin = "cube", color = "#999999")
                                    ))
)#navbarPage end 
)#fluidPage end



server <- function(input, output) {

  output$mapPlot <- renderLeaflet({
    if(input$apiMap=="nyc"){
      nyc
    }else{
      boston
    }
  })
  output$clusterPlot <- renderPlot({
    if(input$ClusterMap == "c1"){
      qmplot(longitude, latitude, data =yelp.business.C1)
    }else if(input$ClusterMap == "c2"){
      qmplot(longitude, latitude, data =yelp.business.C2)
    }else if(input$ClusterMap == "c3"){
      qmplot(longitude, latitude, data =yelp.business.C3)
    }else{
      qmplot(longitude, latitude, data =yelp.business.C4)
    }
  })
  output$stars1 <- renderPlotly({
    ggplotly( ggplot(data = business.photo) +
                geom_mosaic(aes(x = product(stars), fill=has.photo))   + 
                scale_fill_manual(values=c( "#9999CC", "#66CC99"))+theme(text=element_text(size=16))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                labs(x = "Stars for Each Business")+  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()))
  })
  output$stars2 <- renderPlotly({
    ggplotly( ggplot(data = Yelp.Lable) +
                geom_mosaic(aes(x = product(stars), fill=label)) +
                labs(x = "Stars for Each Business") +theme(text=element_text(size=16))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+ 
                scale_fill_viridis_d(option="plasma")+
                scale_color_viridis_d(option="plasma")) 
  })
  output$textS <- renderPlotly({
    ggplotly(ggplot(T.mining_sentiment, aes(index, sentiment, fill = review.Rate)) +
               geom_col(show.legend = FALSE) +
               facet_wrap(~review.Rate, ncol = 2, scales = "free_x"))
  })
  output$textS2<-renderPlotly({
    ggplotly(bing_word_counts %>%
               group_by(sentiment) %>%
               top_n(10) %>%
               ungroup() %>%
               mutate(word = reorder(word, n)) %>%
               ggplot(aes(word, n, fill = sentiment)) +
               geom_col(show.legend = FALSE) +
               facet_wrap(~sentiment, scales = "free_y") +
               labs(y = "Contribution to sentiment",
                    x = NULL) +
               coord_flip())
  })
  output$cloud<-renderPlot({
    T.mining %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
  })
 output$freqW<-renderPlot({
   ggplot(book_words, aes(n/total, fill = review.Rate)) +
     geom_histogram(show.legend = FALSE) +
     xlim(NA, 0.0009) +
     facet_wrap(~review.Rate, ncol = 2, scales = "free_y")
 })
 output$tf_idf<-renderPlot({
   mining_tf_idf %>%
     group_by(review.Rate) %>%
     top_n(10) %>%
     ungroup() %>%
     mutate(bigram = reorder(bigram, tf_idf)) %>%
     ggplot(aes(bigram, tf_idf, fill = review.Rate)) +
     geom_col(show.legend = FALSE) +
     facet_wrap(~review.Rate, scales = "free_y") +
     labs(y = "tf_idf",
          x = NULL) +
     coord_flip()
 })
}

# Run the application 
shinyApp(ui = ui, server = server)
