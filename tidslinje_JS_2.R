#tidslinjeJS
#Online-versjon er avpublisert.
# https://cdn.knightlab.com/libs/timeline3/latest/embed/index.html?source=1wcriDSOqanaExyFvhPKRZ0dL9u8oYsIzQmkWrXamY_A&font=Default&lang=no&initial_zoom=2&height=650

library(ggplot2)
library(scales)
library(lubridate)
library(readxl)
library(RColorBrewer)
library(viridisLite)
library(jsonlite)
library(reticulate)


df <- read_xlsx("tidslinjedata.xlsx", col_types = c("text", "date", "text", "text", "text", "skip", "text", "text", "text", "skip", "text", "text","text", "text","text"))

#Ta vekk datoer uten sikker bekreftelse
df <- df[!is.na(df$Filnavn),]

names_tldf <- c("Year", "Month","Day", "Time", "End Year", "End Month", "End Day","End Time", "Display Date", "Headline", "Text", "Media","Media Credit", "Media Caption", "Media Thumbnail", "Type", "Group","Background")

#Ta vekk filendelse fra filene
filer <- df$Filnavn
filer <- unlist(strsplit(filer, "\\."))
filer <- filer[!nchar(filer)==3]
df$Filnavn <- filer

year <- year(df$Dato)
month <- month(df$Dato)
day <- day(df$Dato)

starttid <- unlist(df[,4])
starttid <- as.numeric(starttid)
t <- floor(starttid * 24)
t <- as.character(t)
t <- ifelse(nchar(t) == 1, paste0("0", t), t)

m <- floor((starttid * 24 - floor(starttid * 24)) * 60)
m <- as.character(m)
m <- ifelse(nchar(m) == 1, paste0("0", m), m)


time <- paste0(t,":",m,":","00")
time <- gsub("NA:NA:00", "08:00:00", time)

sluttid <- unlist(df[,6])
sluttid <- as.numeric(sluttid)
t2 <- floor(sluttid * 24)
t2 <- as.character(t2)
t2 <- ifelse(nchar(t2) == 1, paste0("0", t2), t2)

m2 <- floor((sluttid * 24 - floor(sluttid * 24)) * 60)
m2 <- as.character(m2)
m2 <- ifelse(nchar(m2) == 1, paste0("0", m2), m2)


endtime <- paste0(t2,":",m2,":","00")
endtime <- gsub("NA:NA:00", "23:00:00", endtime)

headline <- df[,5]

text <- paste0("Han er i ", df$Oppholdsland, ". ", df$Kommentarer, " Fil: ", df$Filnavn)
text <- gsub("NA ", "", text)

#Lage kolonne med url til media
#Gammel kode:
#media <- df$bildeurl
#media[is.na(media)] <- ""
ant <- nrow(df)

lagurl <- function(x) {
  end <- ifelse(df[x,10] == "klokke", ".mp4", ".jpg")
  first <- ifelse(df[x,10] == "klokke", "video/", "bilder/")
  urlen <- paste0(first,df[x,1],end)
  return(urlen)
}

sjekk.celle <- function(x) {
  celle <- df[x,11]
  celle <- ifelse(is.na(celle), "", celle)
  celle <- ifelse(celle == "ja", lagurl(x), celle)
  return(celle)
}

bildeurl <- unlist(sapply(1:ant, function(x) sjekk.celle(x)))
df$bildeurl <- bildeurl

timeline <- data.frame(Year=year, Month=month, Day=day, Time=time, EndYear = year, End=month, Endday=day, Endtime=endtime, Displaydate="", Headline=headline, Text=text, Media=bildeurl, Media_Credit="", Media_Caption="", Media_Thumbnail="", Type="", Group=df$Oppholdsland, Background="#b2aff8")
names(timeline) <- names_tldf

write.table(timeline, "timeline.csv", row.names=FALSE, col.names=TRUE, sep=",")
