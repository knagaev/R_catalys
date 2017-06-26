library(wordVectors)
library(magrittr)

if (!file.exists("cookbooks.zip")) {
  download.file("http://archive.lib.msu.edu/dinfo/feedingamerica/cookbook_text.zip","cookbooks.zip")
}
unzip("cookbooks.zip",exdir="cookbooks")

if (!file.exists("cookbooks.txt")) prep_word2vec(origin="cookbooks",destination="cookbooks.txt",lowercase=T,bundle_ngrams=2)

if (!file.exists("cookbook_vectors.bin")) {model = train_word2vec("cookbooks.txt","cookbook_vectors.bin",vectors=200,threads=4,window=12,iter=5,negative_samples=0)} else model = read.vectors("cookbook_vectors.bin")
