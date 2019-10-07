library(xml2)
library(readr)
library(tibble)
library(dplyr)
library(purrr)

get_contents <- function(doc, tag_name, lang = "en")
{
  paste(
    xml_text(
      xml_find_all(doc, paste0("//", tag_name, "[@tsip:lang='", lang, "']//text()", sep = ""))
    ), 
    collapse = " ")
}

patent_path <- "./Рефераты и формулы/Русские DWPI патенты/"

patent_files <- dir(patent_path)
n_files <- length(patent_files)
#n_files <- 20 # пока ограничил 20 файлами

v_num <- numeric(n_files)
v_filename <- character(n_files)

v_titleEnhanced_en <- character(n_files)
v_abstractEnhanced_en <- character(n_files)
v_abstractDocumentation_en <- character(n_files)

for(i in seq(n_files))
{
  print(patent_files[i])
  
  #zip_name <- "Patent Document 0000474124.zip"  
  zip_name <- paste0(patent_path, patent_files[i])
  doc <- xml_ns_strip(read_xml(zip_name, filename = "PatentDocument.xml")) 
  
  v_num[i] <- i
  v_filename[i] <- patent_files[i]
  
  v_titleEnhanced_en[i] <- get_contents(doc, "titleEnhanced", "en")
  v_abstractEnhanced_en[i] <- get_contents(doc, "abstractEnhanced", "en")
  v_abstractDocumentation_en[i] <- get_contents(doc, "abstractDocumentation", "en")
  
}

df_patents <- data.frame(v_num, v_filename, 
                         v_titleEnhanced_en, 
                         v_abstractEnhanced_en,
                         v_abstractDocumentation_en,
                         stringsAsFactors=FALSE)

write.table(df_patents, "df_patents.csv", fileEncoding = "UTF-8")

# titleEnhanced
# abstractEnhanced
# abstractDocumentation

df_lda <- df_patents[df_patents$v_titleEnhanced_en != "", ]


library(RTextTools)
library(topicmodels)
library(tm)

matrix <- create_matrix(cbind(as.vector(df_lda$v_titleEnhanced_en),
                              as.vector(df_lda$v_abstractEnhanced_en)), 
                        language="english", 
                        removeNumbers=TRUE, 
                        stemWords=TRUE, 
                        weighting=weightTf,
                        minDocFreq = 1,
                        maxDocFreq = 15
                        )

#k <- length(unique(data$Topic.Code))
k <- 3

lda <- LDA(matrix, k)

sort(p$terms[1, ], decreasing = TRUE)[1:10]

sorted_terms <- apply(p$terms, 1, sort, decreasing = TRUE)

p <- posterior(lda)

terms(lda)
topics(lda)
