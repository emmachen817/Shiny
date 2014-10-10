rm(list=ls())
library(shiny)
library(shinyIncubator)
library(tm)
library(wordcloud)
library(cluster)
library(Rwordseg)
library(tmcn)
library(slam)
library(rCharts)
library(rHighcharts)
library(ape)
#*#story,
pard.corpus <- Corpus(DirSource("C:\\Users\\11\\Desktop\\parttime-master"), list(language = NA))
pard.corpus <- tm_map(pard.corpus, removePunctuation)
pard.corpus <- tm_map(pard.corpus, removeNumbers)
pard.corpus <- tm_map(pard.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})
parinsertedwords <- c("¤u§@®É¶¡","¤u§@«Ý¹J","¤u§@¤º®e","³Ò°òªk","¤u§@¦aÂI","Ápµ¸¸ê°T")
parwords<-toTrad(iconv(parinsertedwords, "big5", "UTF-8"), TRUE)
insertWords(parwords)
pard.corpus <- tm_map(pard.corpus, segmentCN)
pard.corpus <- Corpus(VectorSource(pard.corpus))
myStopWords <- c(stopwordsCN(), "½s¿è", "®É¶¡", "¼ÐÃD", "µo«H", "¹ê·~", "§@ªÌ","c")
pard.corpus <- tm_map(pard.corpus, removeWords, myStopWords)
partdm <- DocumentTermMatrix(pard.corpus,control = list(wordLengths=c(2,5),removeNumbers = TRUE,
                                                        removePunctuation = list(preserve_intra_word_dashes = FALSE),
                                                        weighting = weightTf,encoding = "UTF-8"))
partable<- col_sums(partdm)
#*#¹q¼v,
mod.corpus <- Corpus(DirSource("C:\\Users\\11\\Desktop\\movie-master"), list(language = NA))

mod.corpus <- tm_map(mod.corpus, removePunctuation)
mod.corpus <- tm_map(mod.corpus, removeNumbers)
mod.corpus <- tm_map(mod.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})
moinsertedwords <- c("µl¤l­^¶¯","¬P»Ú®ÄÀ³","¦w®R¨©º¸","²¾°Ê°g®c","°§¾j¹CÀ¸","¿ûÅK¤H","ªi¦è³Ç§J´Ë","¾¤©ú¦A°_","§Úªº¿ûÅK¦Ñª¨","­»´ä¥J","ª÷°¨¼ú","¤Ñ¥~©_ÂÝ","µl¤l­^¶¯2"
                     ,"Å]ªk¤½¥D","°j¸ô±þ¤â","»jµï¤H2","¾÷¾¹¾ÔÄµ","²¸ÁD®q¯D¦å¾Ô","¤Ñ±Ò¥|ÃM¤h","¾uÅ\¨ÏªÌ","¯D¦å¥ô°È2","µL¼Ä¥¨ÃTVS¥v«eÆsÀs","¤»ºÖ³ß¨Æ","­ð§BªêÂI¬î­»"
                     ,"¥½¥@­°Á{","ÄéÄx°ª¤â","·s¾|¦Bªá:«Ä¤lªº¤ÑªÅ","¥Û¤¤©Ç","­¹¤Hµï","¯}®×¤Ñ¤~¦÷§Q²¤:¯u®L¤èµ{¦¡","¤ô©³Åå»î","¯S°È¦æ¤£¦æ","·¥¼Ö¥@¬É","¿f¤Ñ¹L®ü","±Ï¤õ­^¶¯"
                     ,"³Â»¶¤k¤ý","¸¨¤é±þ¯«","±d¤D¨f§J°­«Î¨Æ¥ó2","ªk®ü-¥Õ³D¶Ç»¡","ªá¤ìÄõ","¦n¤Í¸U¸UºÎ","½¿½»«L:¶Â·tÃM¤hÂk¨Ó(¤U)","¤@¸ô¨ì©³-²æ½u»R¨k","¥þ­±¬ð³ò","¤d¦~íL«Í¤ý"
                     ,"°¨§J¥Õ","¦Ê¸Uºë¥ý¥Í","¹p¾^¾Ôª¯","«ã¾Ô¤Ñ¯«","©Þ¤@±øªe","¤Ö·Ý·í¤j§L","¿à®a¤ý¦Ñ¤­","¯Ã¬ùÂå±¡","¬¡«Í¦aº»","°fÂà¤ýµP","¤`©R¤§®{","¸­°Ý2","®a¦³ø¦¨Æ"
                     ,"°l±þ¤ñº¸","§N¦å®«±N","¥@¬É²Ä¤@³Á¤è","§N¦å®«±N","§k¨â¤U¥´¨âºj","îî¶ÂµL¬É:¬P»Úª§ÅQ¾Ô","ÄéÄx°ª¤â","­ð¤s¤j¦a¾_","íL«Í¥ý¥Í","¨ÈªG¥X¥ô°È","µ´©R²×µ²¯¸4"
                     ,"©ÒÃ¹ªù¶Ç©_","¦N¬P°ª·Ó","¼P³q¼P³q§Úªº¤H¥Í","¸t°«¤h¬P¥Ú:¸t°ì¶Ç»¡","°­§@¨q3","¾K«á¤j¤V¤Ò")
mowords<-toTrad(iconv(moinsertedwords, "big5", "UTF-8"), TRUE)
insertWords(mowords)
mod.corpus <- tm_map(mod.corpus, segmentCN)
mod.corpus <- Corpus(VectorSource(mod.corpus))
myStopWords <- c(stopwordsCN(), "½s¿è", "®É¶¡", "¼ÐÃD", "µo«H", "¹ê·~", "§@ªÌ","c","¹q¼v","±À","¦]¬°","³o­Ó","³o","¬Ý¨ì","¤j®a","Ä±±o","¨ä¹ê","¤w¸g","¨S¦³","¤@­Ó","ºô§}")
mod.corpus <- tm_map(mod.corpus, removeWords, myStopWords)
# Define server logic for random distribution application
shinyServer(function(input, output) {
  output$plot<-renderPlot({ 
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data<-read.table(inFile$datapath,colClasses="character")
    insertedwords <- c("°ê¤ý","¤k¨à","¤k§Å®v","¤½¥D","¤ý¦Z","¤ý¤l")
    insertWords(toTrad(iconv(insertedwords, "big5", "UTF-8"), TRUE))
    word<- segmentCN(data$V1)
    word<-Corpus(VectorSource(word))
    myStopWords <- c(stopwordsCN(), "ªº","¤F","¬O","¤£","¦o","¥L","§Ú","¦b","§A","³o","¤]","¨º","±o")
    d.corpus <- tm_map(word, removeWords, myStopWords)
    d.vec <- sapply(d.corpus, paste, collapse = " ")
    d.vec <- unique(d.vec)
    d.corpus1 <- Corpus(VectorSource(d.vec))
    dtm <- DocumentTermMatrix(d.corpus1, 
                              control = list(wordLengths=c(2, 5),removeNumbers = TRUE,
                                             removePunctuation = list(preserve_intra_word_dashes = FALSE),
                                             weighting = weightTf,encoding = "UTF-8"))
    table1 <- col_sums(dtm)
    wordsDf <- data.frame(WORD = names(table1), FREQ = as.vector(table1), stringsAsFactors = FALSE)
    wordcloud(wordsDf$WORD,wordsDf$FREQ,min.freq = input$freq, max.words=input$max,scale=c(10,.5),
              colors=rainbow(length(wordsDf$FREQ)))
    datasetInput <- reactive({table1})
    output$wf <- downloadHandler( filename = function() {paste("wordfreq", ".csv", sep='') },
                                  content = function(file) {write.csv(datasetInput(), file) } )
    output$baplot<-renderChart({
      a <- rHighcharts:::Chart$new()
      a$chart(type = "bar")
      a$plotOptions(column = list(stacking = "normal"))
      a$title(text = "µüÀWªø±ø¹Ï") 
      a$yAxis(title = list(text = "ÀW²v")) 
      x <- as.data.frame(wordsDf) 
      a$xAxis(categories = wordsDf$WORD) 
      a$data(x)
      return(a)})
    output$clplot<-renderPlot({ tdm <- TermDocumentMatrix(d.corpus1, control = list(wordLengths=c(2, 5),removeNumbers = TRUE,
                                                                                    removePunctuation = list(preserve_intra_word_dashes = FALSE),
                                                                                    weighting = weightTf,encoding = "UTF-8"))
                                data <- as.data.frame(inspect(tdm))
                                data.scale <- scale(data)
                                d <- dist(data.scale, method = "euclidean")
                                fit <- hclust(d,method="ward")
                                mypal = c("#556270", "*#4ECDC4,", "*#1B676B,", "*#FF6B6B,", "*#C44D58,")
                                clus5 = cutree(fit, k=input$cn)
                                op = par(bg = "*#E8DDCB,")
                                plot(as.phylo(fit), type = "fan", tip.color = mypal[clus5], label.offset = 1, 
                                     cex =input$pz, col = "red")
                                datasetInput <- reactive({cluster<-cutree(fit, k=input$cn) 
                                                          datacluster<-cbind(data$rownames, cbind(cluster))})
                                output$dc <- downloadHandler( filename = function() {paste("datacluster", ".csv", sep='') },
                                                              content = function(file) {write.csv(datasetInput(), file) } )
    }) 
  })
  output$ptplot <- renderPlot({
    if (input$individual_obs) {
      partdm <- DocumentTermMatrix(pard.corpus,control = list(wordLengths=c(2,Inf),removeNumbers = TRUE,
                                                              removePunctuation = list(preserve_intra_word_dashes = FALSE),
                                                              weighting = weightTf,encoding = "UTF-8"))
      partable<- col_sums(partdm)
      d <- data.frame(WORD = names(partable), FREQ = as.vector(partable), stringsAsFactors = FALSE)
      wordcloud(d$WORD,d$FREQ,min.freq = input$freq1, max.words=input$max1,scale=c(10,.5),
                colors=rainbow(length(d$FREQ)))
    }
    if (input$density) {
      motdm <- DocumentTermMatrix(mod.corpus,control = list(wordLengths=c(2,Inf),removeNumbers = TRUE,
                                                            removePunctuation = list(preserve_intra_word_dashes = FALSE),
                                                            weighting = weightTf,encoding = "UTF-8"))
      motable<- col_sums(motdm)
      d <- data.frame(WORD = names(motable), FREQ = as.vector(motable), stringsAsFactors = FALSE)
      wordcloud(d$WORD,d$FREQ,min.freq = input$freq1, max.words=input$max1,scale=c(10,.5),
                colors=rainbow(length(d$FREQ)))
    }
    output$table <- renderDataTable({
      if (input$density) { 
        moss<-data.frame( findAssocs(motdm, input$dd, 0.5) )
        moa<-rownames(moss)
        mob<-moss[,1]
        moc<-cbind(moa,mob)
        colnames(moc)=c("¦rµü", "¬ÛÃö©Ê")
        finall<-moc
      }
      if (input$individual_obs) {
        parss<-data.frame( findAssocs(partdm , input$dd, 0.5) )
        para<-rownames(parss)
        parb<-parss[,1]
        parc<-cbind(para,parb)
        colnames(parc)=c("¦rµü", "¬ÛÃö©Ê")
        finall<-parc
      }
      finall
    })})
})