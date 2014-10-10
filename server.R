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
parinsertedwords <- c("�u�@�ɶ�","�u�@�ݹJ","�u�@���e","�Ұ�k","�u�@�a�I","�p����T")
parwords<-toTrad(iconv(parinsertedwords, "big5", "UTF-8"), TRUE)
insertWords(parwords)
pard.corpus <- tm_map(pard.corpus, segmentCN)
pard.corpus <- Corpus(VectorSource(pard.corpus))
myStopWords <- c(stopwordsCN(), "�s��", "�ɶ�", "���D", "�o�H", "��~", "�@��","c")
pard.corpus <- tm_map(pard.corpus, removeWords, myStopWords)
partdm <- DocumentTermMatrix(pard.corpus,control = list(wordLengths=c(2,5),removeNumbers = TRUE,
                                                        removePunctuation = list(preserve_intra_word_dashes = FALSE),
                                                        weighting = weightTf,encoding = "UTF-8"))
partable<- col_sums(partdm)
#*#�q�v,
mod.corpus <- Corpus(DirSource("C:\\Users\\11\\Desktop\\movie-master"), list(language = NA))

mod.corpus <- tm_map(mod.corpus, removePunctuation)
mod.corpus <- tm_map(mod.corpus, removeNumbers)
mod.corpus <- tm_map(mod.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})
moinsertedwords <- c("�l�l�^��","�P�ڮ���","�w�R����","���ʰg�c","���j�C��","���K�H","�i��ǧJ��","�����A�_","�ڪ����K�Ѫ�","����J","������","�ѥ~�_��","�l�l�^��2"
                     ,"�]�k���D","�j������","�j��H2","������ĵ","���D�q�D���","�ѱҥ|�M�h","�u�\�Ϫ�","�D�����2","�L�ĥ��TVS�v�e�s�s","���ֳߨ�","��B���I�"
                     ,"���@���{","���x����","�s�|�B��:�Ĥl���Ѫ�","�ۤ���","���H��","�}�פѤ~���Q��:�u�L��{��","�������","�S�Ȧ椣��","���֥@��","�f�ѹL��","�Ϥ��^��"
                     ,"�»��k��","�������","�d�D�f�J���Ψƥ�2","�k��-�ճD�ǻ�","�����","�n�͸U�U��","�����L:�·t�M�h�k��(�U)","�@���쩳-��u�R�k","�������","�d�~�L�ͤ�"
                     ,"���J��","�ʸU�����","�p�^�Ԫ�","��Ԥѯ�","�ޤ@���e","�ַݷ��j�L","��a���Ѥ�","�ì��屡","���ͦa��","�f����P","�`�R���{","����2","�a������"
                     ,"�l����","�N�宫�N","�@�ɲĤ@����","�N�宫�N","�k��U����j","��µL��:�P�ڪ��Q��","���x����","��s�j�a�_","�L�ͥ���","�ȪG�X����","���R�׵���4"
                     ,"��ù���ǩ_","�N�P����","�P�q�P�q�ڪ��H��","�t���h�P��:�t��ǻ�","���@�q3","�K��j�V��")
mowords<-toTrad(iconv(moinsertedwords, "big5", "UTF-8"), TRUE)
insertWords(mowords)
mod.corpus <- tm_map(mod.corpus, segmentCN)
mod.corpus <- Corpus(VectorSource(mod.corpus))
myStopWords <- c(stopwordsCN(), "�s��", "�ɶ�", "���D", "�o�H", "��~", "�@��","c","�q�v","��","�]��","�o��","�o","�ݨ�","�j�a","ı�o","���","�w�g","�S��","�@��","���}")
mod.corpus <- tm_map(mod.corpus, removeWords, myStopWords)
# Define server logic for random distribution application
shinyServer(function(input, output) {
  output$plot<-renderPlot({ 
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data<-read.table(inFile$datapath,colClasses="character")
    insertedwords <- c("���","�k��","�k�Ův","���D","���Z","���l")
    insertWords(toTrad(iconv(insertedwords, "big5", "UTF-8"), TRUE))
    word<- segmentCN(data$V1)
    word<-Corpus(VectorSource(word))
    myStopWords <- c(stopwordsCN(), "��","�F","�O","��","�o","�L","��","�b","�A","�o","�]","��","�o")
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
      a$title(text = "���W������") 
      a$yAxis(title = list(text = "�W�v")) 
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
        colnames(moc)=c("�r��", "������")
        finall<-moc
      }
      if (input$individual_obs) {
        parss<-data.frame( findAssocs(partdm , input$dd, 0.5) )
        para<-rownames(parss)
        parb<-parss[,1]
        parc<-cbind(para,parb)
        colnames(parc)=c("�r��", "������")
        finall<-parc
      }
      finall
    })})
})