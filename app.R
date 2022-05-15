#llibraries
library(shiny)
library(wordcloud2)
library(colourpicker)
library(ggflags)
library(ggplot2)
library(leaflet)
library(networkD3)
library(ggimage)

ui <- fluidPage(
  h1("Eurovision Songs"),
  h4("Daniel Ponce"),
  # Contenidor
  tabsetPanel(
    # Create a "Word cloud" tab
    tabPanel(
      title = "Word cloud",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "source",
            label = "Cancons:",
            choices = c(
              "Totes les cancons" = "all",
              "Cancons guanyadores" = "winners"
            )
          ),
          hr(),
          numericInput("num", "Quantitat de paraules (min. 3)",
                       value = 100, min = 3
          ),
          hr(),
          colourInput("col", "Color de fons", value = "white")
        ),
        mainPanel(
          wordcloud2Output("cloud"),
          br(),
          br()
        )
      )
    ),
    tabPanel(
      title = "Ranking",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "seleccio",
            label = "Seleccio:",
            choices = c(
              "Totes els anys (1975-2019)" = "resum",
              "Un any concret" = "fix"
            )
          ),
          hr(),
          conditionalPanel(
            condition = "input.seleccio == 'fix'",
            selectInput(
              inputId = "any",
              label = "Any",
              choices = c(1975:2019),
              multiple = FALSE,
              selected = "1975"
            )
          )
        ),
        mainPanel(
          plotOutput("barchart", width = "60%", height = "800px")
        )
      )
      
    ),
    tabPanel(
      title = "Votacions",
      sidebarLayout(
        sidebarPanel(
          selectInput(
              inputId = "fixed_year",
              label = "Any",
              choices = c(1975:2019),
              multiple = FALSE,
              selected = "1975"
          ),
          hr(),
          selectInput(
              inputId = "pais",
              label = "Pais que dona vots",
              choices = c("Tots", "Albania", "Andorra", "Armenia", "Australia", 
                          "Austria", "Azerbaijan", "Belarus", "Belgium",
                          "Bosnia & Herzegovina", "Bulgaria", "Croatia",
                          "Cyprus", "Czech Republic", "Denmark", "Estonia",
                          "Finland", "France", "Georgia", "Germany", "Greece",
                          "Hungary", "Iceland", "Ireland", "Israel", "Italy",
                          "Latvia", "Lithuania", "Luxembourg", "Malta", 
                          "Moldova", "Monaco", "Montenegro", "Morocco", 
                          "North Macedonia", "Norway", "Poland", "Portugal",
                          "Romania", "Russia", "San Marino", "Serbia", 
                          "Serbia & Montenegro", "Slovakia", "Slovenia", 
                          "Spain", "Sweden", "Switzerland","The Netherlands",
                          "Turkey", "Ukraine", "United Kingdom", "Yugoslavia"),
              multiple = FALSE,
              selected = "Tots"
          ) 
        ),
          
        mainPanel(
          simpleNetworkOutput(
            "simple", width = "100%", height = "700px"
          )
        )
      )
    ),
    tabPanel(
      title = "Best Friends",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "friends_type",
            label = "Seleccio:",
            choices = c(
              "Millors amics" = "bff",
              "Pitjors amics" = "wff",
              "Escollir paisos" = "free"
            )
          ),
          hr(),
          conditionalPanel(
            condition = "input.friends_type =='bff' | input.friends_type =='wff'",
            numericInput("n_friends", "Quantitat de relacions inicials (min. 1)",
                         value = 5, min = 1
            )
          ),
          conditionalPanel(
            condition = "input.friends_type == 'free'",
            selectInput(
              inputId = "pais1",
              label = "Pais 1",
              choices = c("Albania", "Andorra", "Armenia", "Australia", 
                          "Austria", "Azerbaijan", "Belarus", "Belgium",
                          "Bosnia & Herzegovina", "Bulgaria", "Croatia",
                          "Cyprus", "Czech Republic", "Denmark", "Estonia",
                          "Finland", "France", "Georgia", "Germany", "Greece",
                          "Hungary", "Iceland", "Ireland", "Israel", "Italy",
                          "Latvia", "Lithuania", "Luxembourg", "Malta", 
                          "Moldova", "Monaco", "Montenegro", "Morocco", 
                          "North Macedonia", "Norway", "Poland", "Portugal",
                          "Romania", "Russia", "San Marino", "Serbia", 
                          "Serbia & Montenegro", "Slovakia", "Slovenia", 
                          "Spain", "Sweden", "Switzerland","The Netherlands",
                          "Turkey", "Ukraine", "United Kingdom", "Yugoslavia"),
              multiple = FALSE,
              selected = "Albania"
            )
          ),
          conditionalPanel(
            condition = "input.friends_type == 'free'",
            selectInput(
              inputId = "pais2",
              label = "Pais 2",
              choices = c("Albania", "Andorra", "Armenia", "Australia", 
                          "Austria", "Azerbaijan", "Belarus", "Belgium",
                          "Bosnia & Herzegovina", "Bulgaria", "Croatia",
                          "Cyprus", "Czech Republic", "Denmark", "Estonia",
                          "Finland", "France", "Georgia", "Germany", "Greece",
                          "Hungary", "Iceland", "Ireland", "Israel", "Italy",
                          "Latvia", "Lithuania", "Luxembourg", "Malta", 
                          "Moldova", "Monaco", "Montenegro", "Morocco", 
                          "North Macedonia", "Norway", "Poland", "Portugal",
                          "Romania", "Russia", "San Marino", "Serbia", 
                          "Serbia & Montenegro", "Slovakia", "Slovenia", 
                          "Spain", "Sweden", "Switzerland","The Netherlands",
                          "Turkey", "Ukraine", "United Kingdom", "Yugoslavia"),
              multiple = FALSE,
              selected = "Andorra"
            )
          )
        ),
        mainPanel(
          leafletOutput("mymap")
        )
      )
    ),
    # Panell about
    tabPanel(
      title = "About",
      br(),
      "Aquesta app ha estat creada amb ", HTML('<a href="https://cran.r-project.org/">R</a>') ," com a practica de lassignatura Visualitzacio de 
      Dades del master de Ciencia de Dades (maig 2022) de la ", HTML('<a href="https://uoc.edu">Universitat Oberta de Catalunya</a>'),
      br(),
      br(),
      em("Autor: "), "Daniel Ponce",
      br(),
      em("Dades: "), br(),
      HTML("<ul><li>Cancons participants a Eurovision des de 1975 fins el 2021. Dataset de <a href='https://www.kaggle.com/datasets/minitree/eurovision-song-lyrics'>Kaggle</a></li>
      <li>Votacions per pais de totes les edicions entre 1975 i 2019. Dataset de <a href='https://www.kaggle.com/datasets/imanolrecioerquicia/eurovision-song-contest-voting'>Kaggle</a></li>
      </ul>"),
      em("Llicencia: "),HTML('<a href="https://creativecommons.org/licenses/by-nc-sa/4.0/">CC BY-NC-SA 4.0</a>'),
      br(),
      em("Publicacio: "),HTML('<a href="http://www.shinyapps.io/">Shinyapps</a>'),
      br()
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "all") {
      data <- read.csv("all.csv")
    } else {
      data <- read.csv("winners.csv")
    }
    return(data)
  })
  
  create_wordcloud <- function(data, num_words = 100, background = "white") {
    
    # Nombre de paraules
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # top n
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    wordcloud2(data, backgroundColor = background)
  }
  output$cloud <- renderWordcloud2({
    
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })
  
  data_source2 <- reactive({
    if (input$seleccio == "resum") {
      data2 <- read.csv("paisos.csv")
      colnames(data2) <- c("to", "points", "any", "iso2")
    } else {
      data2 <- read.csv("votes.csv")
      data2 <- data2[data2$year==input$any,]
    }
    return(data2)
  })
  
  any_especific <- reactive({
    if (input$seleccio == "resum") {
      calc <- F
    } else {
      calc <- T
    }
    return(calc)
  })
  
  create_barchart <- function(data2, calculate = F) {
    
    if(calculate){
      margin <- max(data2$points)*0.05
      xlabel <- "Punts"
    } else {
      margin <- 0.5
      xlabel <- "Quantitat de triomfs"
    }
    
    votes_bar <- data2 %>% ggplot(aes(x=reorder(to, points), y=points)) +
      geom_flag(y = -margin,aes(image = iso2)) +
      geom_bar(stat="identity")
    votes_bar +
      coord_flip()+
      expand_limits(y = -margin) +
      labs(x = "Paisos", y = xlabel)
  }
  
  output$barchart <- renderPlot({
    
    create_barchart(data_source2(), any_especific())
    
  })
  
  data_source3 <- reactive({
    data3 <- read.csv("votes_all_years.csv")
    data3 <- data3[data3$year == input$fixed_year,]
    if (input$pais != "Tots"){
        data3 <- data3[data3$from == input$pais,]
    }
    return(data3)
  })
  
  create_graph <- function(data3) {
    if(nrow(data3)>1){
      simpleNetwork(data3, height="100px", width="100px",        
                  Source = 2,                 
                  Target = 3,                 
                  linkDistance = 10,          
                  charge = -900,      
                  fontSize = 14,
                  fontFamily = "serif",
                  linkColour = "#666", 
                  nodeColour = "#69b3a2",
                  opacity = 0.9,    
                  zoom = T
    )
    }
  }
  output$simple <- renderSimpleNetwork({
    create_graph(data_source3())
  })
  
  
  data_source4 <- reactive({
    combUni <- read.csv("combUni.csv")
    if (input$friends_type == "bff"){
      bestFriends <- head(combUni[order(combUni$points, -abs(combUni$diff), 
                                        decreasing = T),], input$n_friends)
      # Troba els complementaris
      for(i in 1:nrow(bestFriends)){
        add <- rownames(combUni[combUni$Var1==bestFriends$Var2[i] & combUni$Var2==bestFriends$Var1[i],])
        if(!(add %in% rownames(bestFriends))){
          bestFriends <- rbind(bestFriends,combUni[add,])
        }
      }
      
      # Agrupa text per als repetits
      repes <- names(which(table(bestFriends$Var1)>1))
      for(i in 1:length(repes)){
        bestFriends$text[bestFriends$Var1==repes[i]] <- paste(
          bestFriends$text[bestFriends$Var1==repes[i]], collapse=" <br> ")
      }
      
      return(bestFriends)
    } else if (input$friends_type == "wff"){
      worstFriends <- head(combUni[order(combUni$points, -abs(combUni$diff)
                                        , decreasing = F),], input$n_friends)
      # Troba els complementaris
      for(i in 1:nrow(worstFriends)){
        add <- rownames(combUni[combUni$Var1==worstFriends$Var2[i] & combUni$Var2==worstFriends$Var1[i],])
        if(!(add %in% rownames(worstFriends))){
          worstFriends <- rbind(worstFriends,combUni[add,])
        }
      }
      
      # Agrupa text per als repetits
      repes <- names(which(table(worstFriends$Var1)>1))
      for(i in 1:length(repes)){
        worstFriends$text[worstFriends$Var1==repes[i]] <- paste(
          worstFriends$text[worstFriends$Var1==repes[i]], collapse=" <br> ")
      }
      
      return(worstFriends)
    } else {
      friends <- combUni[combUni$Var1==input$pais1 & combUni$Var2==input$pais2,]
      # Troba els complementaris
      for(i in 1:nrow(friends)){
        add <- rownames(combUni[combUni$Var1==friends$Var2[i] & combUni$Var2==friends$Var1[i],])
        if(!(add %in% rownames(friends))){
          friends <- rbind(friends,combUni[add,])
        }
      }
      
      return(friends)
    }
  })
  
  create_map <- function(data4) {
    if(nrow(data4)>1){
      map = leaflet(data4) %>% addTiles()
      map <- map %>% addMarkers(~Var1_long,~Var1_lat, popup=~text)
      for(i in 1:nrow(data4)){
        map <- addPolylines(map, lat = as.numeric(data4[i, c("Var1_lat", "Var2_lat")]), 
                           lng = as.numeric(data4[i, c("Var1_long", "Var2_long")]))
      }
      map
    }
  }
  
  output$mymap <- renderLeaflet({
    create_map(data_source4())
  })
}

shinyApp(ui = ui, server = server)