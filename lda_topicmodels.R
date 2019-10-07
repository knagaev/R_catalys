library(tidyverse)
library(tm)
library(slam)


#sc <- SimpleCorpus(VectorSource(rbind(as.vector(psdocs$title), as.vector(psdocs$description), as.vector(psdocs$claims), as.vector(psdocs$abstract))))
#scanner <- function(x) strsplit(x," ")
#dtm <- DocumentTermMatrix(sc, control = list(removePunctuation = TRUE, removeNumbers = TRUE, stopwords = TRUE))

#Encoding(psdocs$title)  <- "UTF-8"
#Encoding(psdocs$description)  <- "UTF-8"
#Encoding(psdocs$claims)  <- "UTF-8"
#Encoding(psdocs$abstract)  <- "UTF-8"
text_cols <- c("title", "description", "claims", "abstract")

df_corpus <- psdocs %>%
  transmute(id = id, 
            text_contents = paste(title, description, claims, abstract, sep = ' '))


# заменяем пунктуацию на пробел
df_corpus$text_contents <- gsub("[[:punct:]]", " ", df_corpus$text_contents)

#df_corpus <- column_to_rownames(df_corpus, var = "id")

myReader <- readTabular(mapping=list(content="text_contents", id="id"))
wordCorpus <- Corpus(DataframeSource(df_corpus), readerControl=list(reader=myReader))

#wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, removeNumbers)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("russian"))
#wordCorpus <- tm_map(wordCorpus, content_transformer(stemDocument), "russian")

dtm <- DocumentTermMatrix(wordCorpus, control = list(stemming = FALSE, stopwords = FALSE, minWordLength = 3,
                                                   removeNumbers = FALSE, removePunctuation = FALSE))
dim(dtm)

dtms <- removeSparseTerms(dtm, sparse=0.97)
dtmr <- DocumentTermMatrix(wordCorpus, control=list(wordLengths=c(3, 20), bounds = list(global = c(5,45))))

lda_dtm <- dtms

summary(col_sums(lda_dtm))

term_tfidf <- tapply(lda_dtm$v/row_sums(lda_dtm)[lda_dtm$i], lda_dtm$j, mean) * log2(nDocs(lda_dtm)/col_sums(lda_dtm > 0))

summary(term_tfidf)

lda_dtm <- lda_dtm[, term_tfidf >= 0.05]
lda_dtm <- lda_dtm[row_sums(lda_dtm) > 0, ]
summary(col_sums(lda_dtm))
dim(lda_dtm)

k <- 7
SEED <- 2010
jss_TM <-
  list(VEM = LDA(lda_dtm, k = k, control = list(seed = SEED)),
        VEM_fixed = LDA(lda_dtm, k = k,
                        control = list(estimate.alpha = FALSE, seed = SEED)),
        Gibbs = LDA(lda_dtm, k = k, method = "Gibbs",
                        control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
        CTM = CTM(lda_dtm, k = k,
                        control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))))


sapply(jss_TM[1:2], slot, "alpha")
sapply(jss_TM, function(x) mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))

# топики в документах
topics(jss_TM[["VEM"]], 3)

vem_terms <- topicmodels::terms(jss_TM[["CTM"]], 10)
vem_terms[,]

ap_lda <- topicmodels::LDA(dtm, k = 5, control = list(alpha = 0.1))

terms <- terms(ap_lda, 5)
topics <- topicmodels::topics(ap_lda)

terms

BigramTokenizer <-
  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm <- TermDocumentMatrix(wordCorpus, control = list(tokenize = BigramTokenizer))
inspect(removeSparseTerms(tdm[, 1:10], 0.7))

sc <- tm_map(sc, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))

psdocs %>% 
  filter(str_detect(title, "aотнотносительное"))

stemDocument(psdocs$title[1], language = "english")
