#llibraries
library(rjson)
library(tm)

clean_data <- function(input){
  corpus <- Corpus(VectorSource(input))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_contractions = TRUE)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
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

# Neteja
all <- gsub("\n", " ", all)
all <- gsub("'", "'", all)
all <- gsub("'", "'", all)
all <- clean_data(all)
treure <- c("'ll","oh.","-")
all <- all[-which(all$word %in% treure),]

# Extreure lletres cancons guanyadores
songs_winners <- Filter(function(x) x$Pl. == 1, songs)
lyrics_win <- sapply(songs_winners, function(x) if (x$`Lyrics translation` == "English" ){
  x$Lyrics
} else {
  x$`Lyrics translation`
})

winners <- paste(lyrics_win, collapse = " ")
winners <- gsub("\n", " ", winners)
winners <- gsub("'", "'", winners)
winners <- gsub("'", "'", winners)
winners <- clean_data(winners)
winners <- winners[-which(winners$word %in% treure),]

write.csv(all, "all.csv", row.names = FALSE)
write.csv(winners, "winners.csv", row.names = FALSE)

