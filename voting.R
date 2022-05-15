#devtools::install_github('rensa/ggflags')
library(readxl)
library(dplyr)
library(rlist)
library(ggflags)
library(countrycode)
library(ggplot2)
library(ggimage)
library(ggraph)
library(igraph)
library(leaflet)

# Data anys 1975-2019
df <- read_excel("eurovision_song_contest_voting_dataset.xlsx", sheet="Data")
colnames(df) <- c("year", "final", "edition", "jury", "from", "to", "points", "duplicate")
df$final <- as.factor(df$final)
df$edition <- as.factor(df$edition)
df$jury <- as.factor(df$jury)
df$from <- as.factor(df$from)
df$to <- as.factor(df$to)

# clean up Macedonia i typo Netherands
df$from[df$from == "F.Y.R. Macedonia" | df$from == "Macedonia"] = "North Macedonia"
df$to[df$to == "F.Y.R. Macedonia" | df$to == "Macedonia"] = "North Macedonia"
df$from[df$from == "The Netherands"] = "The Netherlands"
df$to[df$to == "The Netherands"] = "The Netherlands"

# Vots no nuls
dfNoNuls <- as.data.frame(df[df$points > 0,])
dfNoNuls$from <- droplevels(dfNoNuls$from)
dfNoNuls$to <- droplevels(dfNoNuls$to)
levels(dfNoNuls$to) <- levels(dfNoNuls$from)

# Vots del jurat agregats per any
votes <- df[df$final == "f",] %>% 
  group_by(year, edition, to) %>%
  summarise(points = sum(points)) %>%
  ungroup %>%
  as.data.frame()

# Guanyadors
win <- votes %>%
  group_by(year) %>%
  filter(points == max(points, na.rm=TRUE)) %>%
  ungroup %>%
  as.data.frame()

win$pos = 1

win <- win %>%
  group_by(to) %>%
  mutate(acc = cumsum(pos)) %>%
  ungroup %>%
  as.data.frame()

win <- win[order(win$acc, decreasing = T),]

# Quantitat de triomfs per pais
paisos <- as.data.frame(unique(win$to))
colnames(paisos) <- "pais"
paisos$acc <- 0
paisos$acc <- sapply(paisos$pais, function(x) {
  mateix_pais <- win[win$to == x,]
  return(max(mateix_pais$acc))
  })

paisos$any <- sapply(paisos$pais, function(x){
    mateix_pais <- win[win$to == x,]
    any <- paste(mateix_pais$year, sep = ", ")
    return(any)
    })
paisos$any <- sapply(paisos$any, function(x) toString(x))

# Afegim iso code
paisos$iso2 <- countrycode(paisos$pais, "country.name", "iso2c")
paisos$iso2[paisos$pais == "Yugoslavia"] <- "YU"
paisos <- paisos[order(paisos$acc, decreasing=T),]
votes$iso2 <- countrycode(votes$to, "country.name", "iso2c") 

# Comparació país-país
paisos <- unique(dfNoNuls$from)
comb <- expand.grid(paisos, paisos)
repes <- which(comb$Var1 == comb$Var2)
combUni <- comb[-repes,]
combUni$count <- 0

# Comptar votacions
for(i in 1:nrow(combUni)) {
  combUni$count[i] = nrow(dfNoNuls[
    dfNoNuls$from == combUni$Var1[i] & dfNoNuls$to == combUni$Var2[i]
    ,])
}

# Comptar punts votacions
combUni$points <- 0
for(i in 1:nrow(combUni)) {
  combUni$points[i] = sum(dfNoNuls$points[
    dfNoNuls$from == combUni$Var1[i] & dfNoNuls$to == combUni$Var2[i]
    ])
}

# Diferencies vots donats/rebuts
for(i in 1:nrow(combUni)) {
  combUni$diff[i] = combUni$points[i] - combUni$points[combUni$Var1==combUni$Var2[i] & combUni$Var2==combUni$Var1[i]]
}

# Total vots
votes_all_years <- dfNoNuls[,c("year","from", "to", "points")]

# Afegim coordenades
coords <- read.csv("UID_ISO_FIPS_LookUp_Table.csv")
coords <- coords[coords$Province_State=="",]
combUni$Var1_iso2 <- countrycode(combUni$Var1, "country.name", "iso2c")
combUni$Var2_iso2 <- countrycode(combUni$Var2, "country.name", "iso2c")
noLoc <- which(is.na(combUni$Var1_iso2) | is.na(combUni$Var2_iso2))
combUni <- combUni[-noLoc,]
combUni$Var1_lat <- sapply(combUni$Var1_iso2, function(x) coords$Lat[match(x, coords$iso2)])
combUni$Var1_long <- sapply(combUni$Var1_iso2, function(x) coords$'Long_'[match(x, coords$iso2)])
combUni$Var2_lat <- sapply(combUni$Var2_iso2, function(x) coords$Lat[match(x, coords$iso2)])
combUni$Var2_long <- sapply(combUni$Var2_iso2, function(x) coords$'Long_'[match(x, coords$iso2)])
combUni$text <- paste(combUni$Var1, "->", combUni$Var2, ": ", combUni$points, " punts", sep="")

# Output
write.csv(votes, "votes.csv", row.names = FALSE)
write.csv(win, "win.csv", row.names = FALSE)
write.csv(paisos, "paisos.csv", row.names = FALSE)
write.csv(votes_all_years, "votes_all_years.csv", row.names = FALSE)
write.csv(combUni, "combUni.csv", row.names = FALSE)