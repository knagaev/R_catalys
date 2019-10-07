library(dplyr)
library(tidyr)
library(ggplot2)
library(LaF)
library(stringr)
library(tm)
library(ngram)
library(grid)
library(gridExtra)
library(qdap)
library(readr)

library(XML)
z <- unz("PatentDocument.zip", filename = "PatentDocument.xml") 
# doc <- xmlParse(file = z)
doc <- xmlParse("PatentDocument.xml")
df <- xmlToDataFrame(
  getNodeSet(doc, "//claims[@lang='en']"),
  colClasses=c("character"),
  stringsAsFactors = F)


data <- read.table(unz("Patent Document 0001488729.zip", filename = "/PatentDocument.xml") 
                   # nrows=1, 
                   # header=F, 
                   #quote="\"", 
                   #sep=","
                  #fill = TRUE
                  #,fileEncoding = "UTF-8"
                  
                   )
data <- readLines(con = z, skipNul = TRUE)



con <- unz("Coursera-SwiftKey.zip", "final/en_US/en_US.blogs.txt")
con2 <- unz("Coursera-SwiftKey.zip", "final/en_US/en_US.news.txt")
con3 <- unz("Coursera-SwiftKey.zip", "final/en_US/en_US.twitter.txt")
NoLinesblogs <- length(readLines(con))
NoLinesnews <- length(readLines(con2))
NoLinestwitter <- length(readLines(con3))
close(con)
close(con2)
close(con3)

library(xml2)

z <- unz("Patent Document 0001488729.zip", filename = "PatentDocument.xml") 

x <- read_file(z)

doc <- xmlParse(x, asText=TRUE, fullNamespaceInfo = FALSE)
df <- xmlToDataFrame(
  getNodeSet(doc, "//claims[@lang='en' or @lang='ru']"),
  colClasses=c("character"),
  stringsAsFactors = F)


