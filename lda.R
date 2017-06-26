library(RTextTools)
library(topicmodels)
library(tm)

data(NYTimes)
data <- NYTimes[sample(1:3100,size=1000,replace=FALSE),]

matrix <- create_matrix(cbind(as.vector(data$Title),
                              as.vector(data$Subject)), 
                        language="english", 
                        removeNumbers=TRUE, 
                        stemWords=TRUE, 
                        weighting=weightTf)

k <- length(unique(data$Topic.Code))
lda <- LDA(matrix, k)

terms(lda)
topics(lda)



matrix <- create_matrix(cbind(as.vector(psdocs$title),
                              as.vector(psdocs$description),
                              as.vector(psdocs$claims),
                              as.vector(psdocs$abstract)), 
                        language="russian", 
                        removeNumbers=TRUE, 
                        stemWords=TRUE, 
                        weighting=weightTf)

matrix <- create_matrix(as.vector(psdocs$title), 
                        language="english", 
                        removeNumbers=TRUE, 
                        stemWords=FALSE, 
                        weighting=weightTf)

k <- 10
lda <- LDA(matrix, k)

terms(lda)
topics(lda)
