#CSV files may be read into R via the command
#example<-read.csv("C:\\file.csv")
#Notice the need for the double \\, since \ is a special R character
#For our example, we will load the file NTSB.RData, and use the data frame
#NTSBcsv

library(tm) #Load the tm library
library(wordcloud) #the wordcloud package offers a nice visualization function
#install.packages("irlba")
library(irlba) # for efficient SVD
#to install Rstem, open an R session and select Packages > Install packages
library(SnowballC)
library(Matrix)
library(data.table)
library(Rgraphviz)
library(graph)

#dtm.to.Matrix converts a sparse DTM to a sparse matrix without
#creating a dense matrix in the process
dtm.to.Matrix<- function(dtm){
  m <- sparseMatrix(i = dtm$i, j = dtm$j, x = dtm$v, dims = c(dtm$nrow, dtm$ncol))
  rownames(m)<-dtm$dimnames$Docs
  colnames(m)<-dtm$dimnames$Terms
  return(m)
}

load("//dom1/Core/Dept/GED/SSBA/Group/GSS/STATS/EliseUMBC/Tutorial/App-1/NTSB.RData")

#create a "corpus", a single object which contains all of the reports as elements
NTSB<-Corpus(VectorSource(NTSBcsv$narr_cause))

#transformations
#the tm_map(corpus,function) command simply applies "function" to each
#document of "corpus"
NTSB<-tm_map(NTSB, stripWhitespace) #remove extra whitespace
NTSB<-tm_map(NTSB, removePunctuation) #remove punctuation
NTSB<-tm_map(NTSB, tolower) #convert all characters to lower case
NTSB<-tm_map(NTSB, removeNumbers) #remove numbers

myStopWords <- c(stopwords('english'), "available", "via") #add stop words, 'available' and 'via'
#remove words from stopwords
# myStopWords <- setdiff(myStopWords, c("economy", "apps"))
NTSB <- tm_map(NTSB, removeWords, myStopWords)#remove common "stopwords"
NTSB <- tm_map(NTSB, stemDocument, "english") #stem the document

#construct the document term matrix. The rows contain documents and the columns #contain terms
#we start by considering a Term Frequency matrix
NTSBdtm<- DocumentTermMatrix(NTSB,control=list(weighting= weightTf))

#remove terms that are absent from at least 99% of documents
NTSBdtm<-removeSparseTerms(NTSBdtm, 0.99)

#view the first 10x10 block of the document term matrix
#inspect(NTSBdtm[1:10,1:10])

#sort all terms in the document term matrix by frequency
as.matrix(sort(colSums(as.matrix(NTSBdtm)),decreasing=TRUE))

#remove common words that do not carry much information
NTSB<- tm_map(NTSB, removeWords, c("factor","result","airplan","aircraft","pilot"))

#rebuild the document term matrix
NTSBdtm<- DocumentTermMatrix(NTSB,control=list(weighting= weightTf))
NTSBdtm<- removeSparseTerms(NTSBdtm, 0.99)
 
#create a new corpus containing only documents corresponding
#to fatal accidents
NTSB_fatal <- NTSB[NTSBcsv$injury_level=="FATL"]
#create a new document term matrix
NTSBdtm_fatal<-DocumentTermMatrix(NTSB_fatal,control = list(weighting= weightTf))
NTSBdtm_fatal<-removeSparseTerms(NTSBdtm_fatal, 0.99)

#create a new document term matrix with Inverse Document Frequency
NTSBdtmIDF<-DocumentTermMatrix(NTSB,control = list(weighting= weightTfIdf))
svd.v<-irlba(dtm.to.Matrix(NTSBdtmIDF),nu=0,nv=20)$v
# dtm.projection contains the "best possible" 20 column summary of the DTM 
dtm.projection<-as.matrix(dtm.to.Matrix(NTSBdtmIDF)%*%svd.v)
# Name the columns SVD1 - SVD20
colnames(dtm.projection)<-paste("SVD",1:ncol(dtm.projection),sep="")

#k-means clustering to find clusters. 
#it is better to use the transformed matrix via the singular value decomposition
#nstart indicates that the algorithm is run 10 times using different
#randomized starting points
set.seed(0)
KMeans <- kmeans(dtm.projection, centers=50,nstart=10)

#creates a vector of 0's and 1's, with 1's corresponding to fatal accidents
fatal<-as.numeric(NTSBcsv$injury_level=="FATL")

#creates a table showing how many fatal accidents are in each cluster
tab<-table(KMeans$cluster,fatal)

#Network of Terms  
freq.terms <- findFreqTerms(NTSBdtm, 500) #find terms that appear at least 500 times
plot(NTSBdtm, term=freq.terms, corThreshold=0.1, weighting=T)
