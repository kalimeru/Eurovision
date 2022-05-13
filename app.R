#llibraries
library(shiny)
library(wordcloud2)
library(colourpicker)

ui <- fluidPage(
  h1("Eurovision Word Cloud"),
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
            label = "Songs:",
            choices = c(
              "All Songs" = "all",
              "Winner Songs" = "winners"
            )
          ),
          hr(),
          numericInput("num", "Quantitat de paraules (min. 3)",
                       value = 100, min = 3
          ),
          hr(),
          colourInput("col", "Color de fons", value = "white"),
          hr(),
          HTML('<p>Pràctica de Visualització de Dades de la <a href="https://uoc.edu">UOC</a></p>')
        ),
        mainPanel(
          wordcloud2Output("cloud"),
          br(),
          br()
        )
      )
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