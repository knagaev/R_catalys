library(topicmodels)


psdocs$title <- str_replace_all(psdocs$title, '[[:punct:]]',' ')
psdocs$description <- str_replace_all(psdocs$description, '[[:punct:]]',' ')
psdocs$claims <- str_replace_all(psdocs$claims, '[[:punct:]]',' ')
psdocs$abstract <- str_replace_all(psdocs$abstract, '[[:punct:]]',' ')

sc <- SimpleCorpus(VectorSource(rbind(as.vector(psdocs$title),
                                      as.vector(psdocs$description),
                                      as.vector(psdocs$claims),
                                      as.vector(psdocs$abstract))))

scanner <- function(x) strsplit(x," ")

dtm <- DocumentTermMatrix(sc,
                          control = list(removePunctuation = TRUE,
                                         removeNumbers = TRUE,
                                         stopwords = TRUE))



Encoding(psdocs$title)  <- "UTF-8"
Encoding(psdocs$description)  <- "UTF-8"
Encoding(psdocs$claims)  <- "UTF-8"
Encoding(psdocs$abstract)  <- "UTF-8"

wordCorpus <- Corpus(DataframeSource(data.frame(psdocs[2:4])))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, removeNumbers)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, content_transformer(stemDocument), "russian")
dtm <- TermDocumentMatrix(wordCorpus)

ap_lda <- LDA(dtm, k = 10, control = list(seed = 1234))

terms(ap_lda)
topics(ap_lda)

BigramTokenizer <-
  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(wordCorpus, control = list(tokenize = BigramTokenizer))
inspect(removeSparseTerms(tdm[, 1:10], 0.7))

sc <- tm_map(sc, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))

psdocs %>% 
  filter(str_detect(title, "aотнотносительное"))

stemDocument(psdocs$title[1], language = "english")
