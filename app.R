#llibraries
library(rjson)
library(tm)
library(shiny)
library(wordcloud2)
library(colourpicker)

clean_data <- function(input){
  corpus <- Corpus(VectorSource(input))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "'s", "'m", "'re", "'ll", "ooh", "-", "'ve"))
  tdm <- as.matrix(TermDocumentMatrix(corpus))
  data <- sort(rowSums(tdm), decreasing = TRUE)
  data <- data.frame(word = names(data), freq = as.numeric(data))
  return(data)
}

# Input
songs <- fromJSON(file="eurovision-lyrics.json")

# Extreure lletra en anglès de la llista
lyrics_all <- sapply(songs, function(x) if (x$`Lyrics translation` == "English"){
  x$Lyrics
} else {
  x$`Lyrics translation`
})

# Guardem tot en una variable
all <- paste(lyrics_all, collapse = " ")
all <- gsub("\n", " ", all)
all <- clean_data(all)

# Extreure lletres cancons guanyadores
songs_winners <- Filter(function(x) x$Pl. == 1, songs)
lyrics_win <- sapply(songs_winners, function(x) if (x$`Lyrics translation` == "English" ){
  x$Lyrics
} else {
  x$`Lyrics translation`
})

winners <- paste(lyrics_win, collapse = " ")
winners <- gsub("\n", " ", winners)
winners <- clean_data(winners)

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
            label = "Cançons:",
            choices = c(
              "Totes les cançons" = "all",
              "Cançons guanyadores" = "winners"
            )
          ),
          hr(),
          numericInput("num", "Quantitat de paraules (mínim 3)",
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
      data = all
    } else {
      data = winners
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