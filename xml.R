library(XML)
z <- unz("Patent Document 0001488729.zip", filename = "PatentDocument.xml", 'r') 
# doc <- xmlParse(file = z)
doc <- xmlParse("PatentDocument.xml")
df <- xmlToDataFrame(
  getNodeSet(doc, "//claims[@lang='en']"),
  colClasses=c("character"),
  stringsAsFactors = F)
