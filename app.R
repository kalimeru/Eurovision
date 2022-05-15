#llibraries
library(shiny)
library(wordcloud2)
library(colourpicker)

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
          colourInput("col", "Color de fons", value = "white"),
          hr(),
          HTML('<p>Data Visualization, universitat <a href="https://uoc.edu">UOC</a></p>')
        ),
        mainPanel(
          wordcloud2Output("cloud"),
          br(),
          br()
        )
      )
    ),
    # Panell about
    tabPanel(
      title = "About",
      br(),
      "Aquesta app ha estat creada amb ", HTML('<a href="https://cran.r-project.org/">R</a>') ," com a practica de lassignatura Visualitzacio de 
      Dades del master de Ciencia de Dades (maig 2022)",
      br(),
      br(),
      em("Autor: "), "Daniel Ponce",
      br(),
      em("Dades: "),HTML('Compren totes les cancons participants al festival Eurovision
                         des de 1975 fins al 2021 i es pot trobar a <a href="https://www.kaggle.com/datasets/minitree/eurovision-song-lyrics">Kaggle</a>'),
      br(),
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
}

shinyApp(ui = ui, server = server)