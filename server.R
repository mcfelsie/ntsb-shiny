library(shiny)

shinyServer(function(input, output) {
  
  output$freq <- renderPlot({
    findFreqTerms(NTSBdtm, 150) #find terms that appear at least n times
    v <- sort(colSums(as.matrix(NTSBdtm)), decreasing=TRUE) 
    p <-head(v, as.numeric(input$n)) #show top N words in order of abundance   
    par(las=1)
    barplot(p, horiz=TRUE, col='skyblue')
  })
  
   output$assoc <- renderPrint({
    term <- input$term
    findAssocs(NTSBdtm, term, as.numeric(input$corr)) #find words that have correlation of at least corr with "term"
  })
     
   #print a wordcloud 
   wordcloud_rep <- repeatable(wordcloud)
   
   output$cloud <- renderPlot({
   v <- colSums(dtm.to.Matrix(NTSBdtm))
   names(v)<-dimnames(NTSBdtm)$Terms
   v<-sort(v,decreasing=TRUE)
   wordcloud_rep(names(v),v,max.words=100,random.order=FALSE,colors=brewer.pal(8,"Dark2"))
   })
   
   # Generate a summary of the data
   output$svd <- renderPlot({
       
   svd_plot<-function(DTM){
   #accepts a document term matrix from package tm as input
   #plots first two SVD row and column vectors
   result<-irlba(dtm.to.Matrix(DTM),nu=50,nv=50)
   UD<-result$u[,1:2]%*%diag(result$d[1:2])
   DV<-diag(result$d[1:2])%*%t(result$v[,1:2])
   plot(result$d^2/sum(result$d^2),main="Scree Plot:\n 50 largest singular values",xlab="Component",ylab="Singular Value /% Explained",type="b")
   #x11()#new window
   #plot(UD,type="n",xlab="",ylab="")
   #text(UD, rownames(DTM), cex = 0.6)
   #x11()#new window
   plot(t(DV),type="n",xlab="",ylab="")
   text(t(DV), colnames(DTM), cex = 0.6)
   }
     
   svd_plot(NTSBdtm)
     
   }, height = 400, width = 400)
   
   output$fatal <- renderPlot({
         
     #creates a table showing how many fatal accidents are in each cluster
     tab<-table(fatal, KMeans$cluster)
     #the percent of fatal accidents in each cluster
     tab[,2]/rowSums(tab)
     cbind(tab,round(tab[,2]/rowSums(tab),2))
     dimnames(tab)$fatal <- c("Nonfatal", "Fatal")
     barplot(tab, main="Fatal vs Nonfatal by Cluster", xlab="Cluster", ylab="Number of Reports", col=c("gray86", "darkred"), legend =rownames(tab))
   }, height = 400, width = 600)
   
   output$cluster <- renderPlot({
      NTSBc<-NTSB[KMeans$cluster==as.numeric(input$clusters)]
      NTSBcdtm<-DocumentTermMatrix(NTSBc,control = list(weighting= weightTf))
      v <- colSums(dtm.to.Matrix(NTSBcdtm))
      names(v)<-dimnames(NTSBcdtm)$Terms
      v<-sort(v,decreasing=TRUE)
      wordcloud(names(v),v,max.words=100,random.order=FALSE,colors=brewer.pal(8,"Dark2"))
   })
  
   output$table <- renderDataTable({
      setnames(NTSBcsv, c(4, 13, 14, 22, 26), c("Event Date", "Nearest City", "State/Province", "Fatality", "Narrative"))
      NTSBcsv[,c(4, 13, 14, 22, 26)]
   })
   
   output$network <- renderPlot( { #Network of Terms  
     freq.terms <- findFreqTerms(NTSBdtm, 600) #find terms that appear at least 700 times
     plot(NTSBdtm, term=freq.terms, corThreshold=0.1, weighting=T)
   })
  
})
