###########################################################################################################################
# Section I. Text mining case study
#1.	Study this Text mining case study	
# https://rstudio-pubs-static.s3.amazonaws.com/97469_c56e716bf5ee49cc84db0ff6d5d2ddaa.html
# Then, conduct your own Text Analysis on the textmining.zip data set provided in Canvas and provide explanation of 
# results.  Pl. make sure to include these sections:
#Explore your data 
##Word Frequency
##Plot Word Frequencies

#Relationships between Terms 
##Term Correlations
##Word Clouds!

#Clustering by Term Similarity 
##Hierarchal Clustering
##K-means clustering


# Sources:
# (1) http://handsondatascience.com/TextMiningO.pdf
# (2) https://rstudio-pubs-static.s3.amazonaws.com/97469_c56e716bf5ee49cc84db0ff6d5d2ddaa.html
# (3) file:///C:/Users/enwosu/Downloads/Case%20Study%20Text%20Mining%20in%20R.mht
###########################################################################################################################

###################################################################################################################################
# The main package to load and manipulate in this work is tm package. 
###################################################################################################################################
#remove.packages()
install.packages("tm")
install.packages("qdap") # Quantitative discourse analysis of transcripts. 
install.packages("qdapDictionaries") 
install.packages("dplyr") # Data wrangling, pipe operator %>%(). 
install.packages("RColorBrewer") # Generate palette of colours for plots. 
install.packages("ggplot2") # Plot word frequencies. 
install.packages("scales") # Include commas in numbers. 
#install.packages("Rgraphviz") # Correlation plots.
install.packages("magrittr")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("cluster")
install.packages("biclust")
install.packages("igraph")
install.packages("caret")

#install.packages("caret", dependencies = c("Depends", "Suggests"))
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
install.packages("Rcampf", repos = "http://datacube.wu.ac.at/", type="source")



library(tm) # Framework for text mining. 
library(qdap) # Quantitative discourse analysis of transcripts. 
library(qdapDictionaries) 
library(dplyr) # Data wrangling, pipe operator %>%(). 
library(RColorBrewer) # Generate palette of colours for plots. 
library(ggplot2) # Plot word frequencies. 
library(scales) # Include commas in numbers. 
library(Rgraphviz) # Correlation plots.
library(magrittr) # magrittr provides a series of aliases which can be more pleasant to use when composing chains using the %>% operator.
library(caret)
library(SnowballC)
library(wordcloud)
library(cluster)
library(fpc)
library(Rcampf)
library(biclust)
library(igraph)

# There are a variety of sources supported by tm. We can use getSources() to list them. 
getSources()

# In addition to di???erent kinds of sources of documents, our documents for text analysis 
# will come in many di???erent formats. A variety are supported by tm:
getReaders()

#Needed <-c("tm", "qdap", "qdapDictionaries", "dplyr", "RColorBrewer"
#           , "ggplot2", "scales", "magrittr", "magrittr"
#           , "SnowballC", "wordcloud", "cluster", "caret", "biclust", "igraph")
#install.packages(Needed, dependencise = TRUE)



###################################################################################################################################
# Build a corpus, which is a collection of text documents
###################################################################################################################################

dirCorpus <- DirSource("C:/Users/Desktop/UW_Course/ML_2017/ML-310/HW3/TextMining", encoding = "UTF-8")
#In order to get the right number of lines, you need to use VCVorpus and not Corpus to read the data
# VCorpus in tm refers to "Volatile" corpus which means that the corpus is stored in memory and would be destroyed when the R object containing it is destroyed.
corpus <- VCorpus(dirCorpus, readerControl = list(reader = readPlain, language = "en"))

###################################################################################################################################
# Exploring the corpus to ensure that data has been loaded properly and as we expect. 
###################################################################################################################################
# List of all files and associated path
dirCorpus

# Meta data for the entire corpus
corpus

# Corpus class
class(dirCorpus)

# For basic statistics of the corpus we can only use
summary(corpus)

# Basic statistics on the first document. 
summary(corpus[[1]]$content)

#Print the first 1 documents. print() gives a concise overview
print(corpus[1])

#Inspect the first 1 documents. inspect() gives more details
inspect(corpus[1])

# See meta data for document 1
meta(corpus[[1]])

meta(corpus)
# Individual documents can be accessed via [[, either via the position in the corpus, or via their identifier.
meta(corpus[[1]], "id")

# Proof that a file position in a corpus and the file name can be used to reference the same document
identical(corpus[[1]], corpus[["BeyondEntitiesAndRelationships.txt"]])

# See the content of document 3 and 4
lapply(corpus[1:2], as.character)

viewDocs <- function(d, n) {d %>% extract2(n) %>% as.character() %>% writeLines()} 
viewDocs(corpus, 1)

###########################################################################################################################
###########################################################################################################################
# DEEP DIVE INTO THE ANALYSIS OF A SINGLE DOCUMENT
###########################################################################################################################
###########################################################################################################################


###########################################################################################################################
# To obtain statistics of the document, we don't extract all the document, instead we sample with the next fuction,
###########################################################################################################################

# This can be used for a big file. Instead of extracting the entire file, you can sample it and get stats for the sample



# no_text =  the dicument number int he list of documents. Below, no_text=1 refers to the first document
# 
# n_muestra = 40 (muestra =  sample in Spanish). We are saying sample 40 lines from the document. 
# The document must have more than 40 lines. "summary(corpus[[1]]$content)" gives you the numebr 
# of lines i.e. 80 in this case. 40 < 80
#
#
# p_partition = 0.7 implies that you should partition the data into 70-30
#
#


# Sample the document text. Here, we use the first document in the folder, with a sample of 40 
# and a partition of 70-30.
sample_text <- function(no_text = 1, n_muestra = 40, p_partition = 0.7){
  
  # Get the number of row in a document of the corpus
  nrows_news <- as.numeric(summary(corpus[[no_text]]$content)[1])
  
  # Set the value of the sample and use replace = F so that you do not repeat lines
  id_muestra <- sort(sample(x = 1:nrows_news, size = n_muestra, replace = F))
  
  muestra <- corpus[[no_text]]$content[id_muestra]
  
  # Create a partition of the sample
  idPartition <- createDataPartition(y = id_muestra, p = p_partition, list = F)
  muestraTrain<- paste(muestra[idPartition], collapse = " ")
  muestraTest<- paste(muestra[-idPartition], collapse = " ")
  
  output <- list(CmuestraTrain = Corpus(VectorSource(muestraTrain)), 
                 CmuestrTest = Corpus(VectorSource(muestraTest)))
  
  output
}

corpus_partition <- sample_text(no_text = 1, n_muestra = 40, p_partition = 0.7)

# nw you can get a summary stats of the sample
summary(corpus_partition)

###########################################################################################################################
# Once we have a corpus we typically want to modify the documents in it, e.g., stemming, stopword 
# removal, et cetera. In tm, all this functionality is subsumed into the concept of a transformation. 
###########################################################################################################################

# The basic transforms are all available within tm includes:
getTransformations()

# We can list the stop words
length(stopwords("english"))
stopwords("english")

# Function to apply transformations
cleaning_text <- function(doc){
  # If we had a list of bad words we wanted to remove, we could 
  # (1) Change the function signature to function(doc, dir_badwords)
  # (2) include it in this function with the following line "bad_words <- readLines(dir_badwords)"
  # (3) Call it as: clean_sample <- cleaning_text(corpus_partition[[2]], 
  #                   "~/Documents/datasciencecoursera/Capstone Project/week1/bad_words.txt")
  
  
  #To create a custom transformation we make use of content transformer() to create a function to achieve the 
  # transformation, and then apply it to the corpus using tm map()
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  toEliminate <- content_transformer(function(x, pattern) gsub(pattern, "", x, perl = T))
  
  # Do the cleaning in the text. The function tm map() is used to apply one of transformations across 
  # all documents within a corpus
  
  # Conversion to lower case by:
  doc <- tm_map(doc, content_transformer(tolower))
  # Remove numbers
  doc <- tm_map(doc, removeNumbers)
  # Remove paragraph demacations
  doc <- tm_map(doc, toEliminate, "\\p{P}")
  # Remove punctuations
  doc <- tm_map(doc, removePunctuation)
  # Stemming is done by
  doc <- tm_map(doc, stemDocument)
  # Replace http(s)://www.com with a space
  doc <- tm_map(doc, toSpace, "/|@|-|https?://|www|com")
  # Removal of stopwords by
  doc <- tm_map(doc, removeWords, stopwords("english"))
  # Remove bad words
  #doc <- tm_map(doc, removeWords, bad_words)
  #Extra whitespace is eliminated by:
  doc <- tm_map(doc, stripWhitespace)
  
  doc
}


clean_sample <- cleaning_text(corpus_partition[[2]])


###########################################################################################################################
# For tokenization of the text, we use the next part of the text. We obtain the tokens and bigrams for the 
# exploratory analysis.
###########################################################################################################################

tokens <- strsplit(clean_sample[[1]]$content, split = " ", fixed = T)[[1]]
bitokens <- ngrams(strsplit(clean_sample[[1]]$content, split = " ", fixed = T)[[1]], 2)
bitokens <- lapply(bitokens, paste, collapse = " ")
bitokens <- do.call(rbind.data.frame, bitokens)

# We obtain the frequencies of the tokens, bigrams and trigrams
one_word <- data.frame(table(tokens))
two_word <- data.frame(table(bitokens))

sort_tokens <- one_word[order(one_word$Freq, decreasing = TRUE), ]
sort_bitokens <- two_word[order(two_word$Freq, decreasing = TRUE), ]

###########################################################################################################################
# Exploratory Analysis
###########################################################################################################################

# We can see the most frequently words in the sample with
head(sort_tokens)

# With the next code, we can see the distritution of word by their first letter.
group_by_letter <- data.frame(table(substr(one_word$tokens, 1, 1)))
c <- ggplot(group_by_letter, aes(group_by_letter$Var1, group_by_letter$Freq))
c + geom_bar(stat = "identity") + labs(title = "Frequency by first letter", x = "Letter", y = "Frequency")

# This can see in a better way with a cloud word
wordcloud(sort_tokens$tokens, 
          sort_tokens$Freq, 
          random.order = F, 
          scale=c(3.5,.5),
          max.words = 30, 
          colors = brewer.pal(6, "GnBu"))

# We can create a dictionary with the data.frame one_word and explore some facts about the sample.
dictionary <- one_word 
dictionary[, 3] <- cumsum(one_word$Freq)
dictionary[, 4] <- dictionary[, 3]/sum(one_word$Freq)
colnames(dictionary) <- c("word", "freq", "cum_freq", "quant")

# For example, the number of words in the sample and the unique word are
tail(dictionary$cum_freq, 1) # Number of words in the sample

length(unique(dictionary$word)) # Number of unique words

# For some basic quantiles we can see that, we need this words to cover the dictionary
quantile(c(0, dictionary[, 3]))


###########################################################################################################################
###########################################################################################################################
# DEEP DIVE INTO THE ANALYSIS OF ALL THE DOCUMENTS
###########################################################################################################################
###########################################################################################################################



###########################################################################################################################
# Clean the entire corpus by applying the following transformations
###########################################################################################################################

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
toEliminate <- content_transformer(function(x, pattern) gsub(pattern, "", x, perl = T))

# Do the cleaning in the text. The function tm map() is used to apply one of transformations across 
# all documents within a corpus

# Conversion to lower case by:
corpus <- tm_map(corpus, content_transformer(tolower))
# Remove numbers
corpus <- tm_map(corpus, removeNumbers)
# Remove paragraph demacations
corpus <- tm_map(corpus, toEliminate, "\\p{P}")
# Remove punctuations
corpus <- tm_map(corpus, removePunctuation)
# Stemming is done by
corpus <- tm_map(corpus, stemDocument)
# Replace http(s)://www.com with a space
corpus <- tm_map(corpus, toSpace, "/|@|-|https?://|www|com")

corpus <- tm_map(corpus, toSpace, "/|@|\\|") 

# Removal of stopwords by
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Remove bad words
#doc <- tm_map(doc, removeWords, bad_words)
#Extra whitespace is eliminated by:
corpus <- tm_map(corpus, stripWhitespace)

# Be sure to use the following script once you have completed preprocessing.This tells R to treat 
# your preprocessed documents as text documents.
#corpus <- tm_map(corpus, PlainTextDocument)   
###########################################################################################################################
# Creating a Document Term Matrix
###########################################################################################################################
# A document term matrix is simply a matrix with documents as the rows and terms as the columns and a count of the 
# frequency of words as the cells of the matrix
dtm <- DocumentTermMatrix(corpus)

###########################################################################################################################
# View the term matrix
###########################################################################################################################
dtm

# We can see that there are 30 documents
inspect(dtm)
inspect(dtm[1:5, 1000:1005])

# The document term matrix is in fact quite sparse (that is, mostly empty) and so it is actually stored in a much
# more compact representation internally
class(dtm)
dim(dtm)

# The transpose is created using TermDocumentMatrix()
tdm <- TermDocumentMatrix(corpus) 
tdm
###########################################################################################################################
# Explore the term matrix
###########################################################################################################################
# We can obtain the term frequencies as a vector by converting the document term matrix into a matrix and summing the 
# column counts

# Let's organize terms by their frequency
freq <- colSums(as.matrix(dtm)) 
length(freq)

# By ordering the frequencies we can list the most frequent terms and the least frequent terms
ord <- order(freq)

###########################################################################################################################
# Word Frequency
###########################################################################################################################

# Least frequent terms. These terms appear just once and are probably not really terms that are of interest to us. 
freq[head(ord)]
# Most frequent terms. These terms are much more likely to be of interest to us. 
freq[tail(ord)]

###########################################################################################################################
# Distribution of Term Frequencies
###########################################################################################################################

# Frequency of frequencies. We can see here that there are 1693 terms that occur just once.

head(table(freq), 15)

tail(table(freq), 15)

###########################################################################################################################
# Removing Sparse Terms
###########################################################################################################################

# We are often not interested in infrequent terms in our documents.
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.1) 
dim(dtms)

# Compare before and after the sparse term is removed
inspect(dtm)
inspect(dtms)

freq_dtm <- colSums(as.matrix(dtm))
freq_dtm
freq_dtms <- colSums(as.matrix(dtms))
freq_dtms


###########################################################################################################################
# For a less, fine-grained look at term freqency we can view a table of the terms we selected when we removed sparse terms
###########################################################################################################################
freq <- colSums(as.matrix(dtms))   
freq

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)  

###########################################################################################################################
# Identifying Frequent Items and Associations
###########################################################################################################################

# One thing we often to ???rst do is to get an idea of the most frequent terms in the corpus. Let's limit the output to 
# those terms that occur at least 100 times

findFreqTerms(dtm, lowfreq=100)

# Yet another way to do this
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

# We can also ???nd associations with a word, specifying a correlation limit
# If two words always appear together then the correlation would be 1.0 and if they never appear together the correlation 
# would be 0.0. Thus the correlation is a measure of how closely associated the words are in the corpus.


#If you have a term in mind that you have found to be particularly meaningful to your analysis, then you may find it helpful to identify the words that most highly correlate with that term
findAssocs(dtm, c("ascustom" , "acustom"), corlimit=0.98) # specifying a correlation limit of 0.98 

findAssocs(dtm, "data", corlimit=0.6) # specifying a correlation limit of 0.6


###########################################################################################################################
# Correlations Plots 
###########################################################################################################################
plot(dtm, terms=findFreqTerms(dtm, lowfreq=10)[1:50], corThreshold=0.5)


###########################################################################################################################
# Plotting Word Frequencies
###########################################################################################################################
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE) 
head(freq, 14)


wf <- data.frame(word=names(freq), freq=freq) 
head(wf)

# Here, I set the frequency to > 150 words. As a result, we can plot the frequency of those words that occur at least 150 times in the corpus
subset(wf, freq>150) %>% ggplot(aes(word, freq)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

###########################################################################################################################
# Word cloud
###########################################################################################################################

# Plot words that occur at least 25 times.
set.seed(142)   
wordcloud(names(freq), freq, min.freq=25)

# Plot the 100 most frequently used words.
set.seed(142)   
wordcloud(names(freq), freq, max.words=100) 

# Add some color and plot words occurring at least 20 times.
# We can change the range of font sizes used in the plot using 
# the scale= option. By default the most frequent words have a scale of 4 and the least have a scale of 0.5.
set.seed(142)   
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))  

# Plot the 100 most frequently occurring words.
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2) 


###########################################################################################################################
# Clustering by Term Similarity
###########################################################################################################################
# To do this well, you should always first remove a lot of the uninteresting or infrequent words. 
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)  

###########################################################################################################################
# Hierarchical Clustering
###########################################################################################################################

# First calculate distance between words & then cluster them according to similarity.
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d, method="ward.D")  
fit  

plot(fit, hang=-1)   

# To get a better idea of where the groups are in the dendrogram, you can also ask R to help identify the clusters. 
# Here, I have arbitrarily chosen to look at five clusters, as indicated by the red boxes.
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   

###########################################################################################################################
# K-Means Clustering
###########################################################################################################################

# The k-means clustering method will attempt to cluster words into a specified number of groups (in 
# this case 2), such that the sum of squared distances between individual words and one of the group
# centers.    
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

# Let try another value for K. Let K = 5
kfit <- kmeans(d, 5)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=5, lines=0)  