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
parinsertedwords <- c("工作時間","工作待遇","工作內容","勞基法","工作地點","聯絡資訊")
parwords<-toTrad(iconv(parinsertedwords, "big5", "UTF-8"), TRUE)
insertWords(parwords)
pard.corpus <- tm_map(pard.corpus, segmentCN)
pard.corpus <- Corpus(VectorSource(pard.corpus))
myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者","c")
pard.corpus <- tm_map(pard.corpus, removeWords, myStopWords)
partdm <- DocumentTermMatrix(pard.corpus,control = list(wordLengths=c(2,5),removeNumbers = TRUE,
                                                        removePunctuation = list(preserve_intra_word_dashes = FALSE),
                                                        weighting = weightTf,encoding = "UTF-8"))
partable<- col_sums(partdm)
#*#電影,
mod.corpus <- Corpus(DirSource("C:\\Users\\11\\Desktop\\movie-master"), list(language = NA))

mod.corpus <- tm_map(mod.corpus, removePunctuation)
mod.corpus <- tm_map(mod.corpus, removeNumbers)
mod.corpus <- tm_map(mod.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})
moinsertedwords <- c("痞子英雄","星際效應","安娜貝爾","移動迷宮","飢餓遊戲","鋼鐵人","波西傑克森","黎明再起","我的鋼鐵老爸","香港仔","金馬獎","天外奇蹟","痞子英雄2"
                     ,"魔法公主","迴路殺手","蜘蛛人2","機器戰警","硫磺島浴血戰","天啟四騎士","骷髏使者","浴血任務2","無敵巨鯊VS史前鱷龍","六福喜事","唐伯虎點秋香"
                     ,"末世降臨","灌籃高手","新魯冰花:孩子的天空","石中怪","食人蛛","破案天才伽利略:真夏方程式","水底驚魂","特務行不行","極樂世界","瞞天過海","救火英雄"
                     ,"麻辣女王","落日殺神","康乃狄克鬼屋事件2","法海-白蛇傳說","花木蘭","好友萬萬睡","蝙蝠俠:黑暗騎士歸來(下)","一路到底-脫線舞男","全面突圍","千年殭屍王"
                     ,"馬克白","百萬精先生","雷霆戰狗","怒戰天神","拔一條河","少爺當大兵","賴家王老五","紐約醫情","活屍地獄","逆轉王牌","亡命之徒","葉問2","家有囍事"
                     ,"追殺比爾","冷血悍將","世界第一麥方","冷血悍將","吻兩下打兩槍","闇黑無界:星際爭霸戰","灌籃高手","唐山大地震","殭屍先生","亞果出任務","絕命終結站4"
                     ,"所羅門傳奇","吉星高照","噗通噗通我的人生","聖鬥士星矢:聖域傳說","鬼作秀3","醉後大丈夫")
mowords<-toTrad(iconv(moinsertedwords, "big5", "UTF-8"), TRUE)
insertWords(mowords)
mod.corpus <- tm_map(mod.corpus, segmentCN)
mod.corpus <- Corpus(VectorSource(mod.corpus))
myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者","c","電影","推","因為","這個","這","看到","大家","覺得","其實","已經","沒有","一個","網址")
mod.corpus <- tm_map(mod.corpus, removeWords, myStopWords)
# Define server logic for random distribution application
shinyServer(function(input, output) {
  output$plot<-renderPlot({ 
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data<-read.table(inFile$datapath,colClasses="character")
    insertedwords <- c("國王","女兒","女巫師","公主","王后","王子")
    insertWords(toTrad(iconv(insertedwords, "big5", "UTF-8"), TRUE))
    word<- segmentCN(data$V1)
    word<-Corpus(VectorSource(word))
    myStopWords <- c(stopwordsCN(), "的","了","是","不","她","他","我","在","你","這","也","那","得")
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
      a$title(text = "詞頻長條圖") 
      a$yAxis(title = list(text = "頻率")) 
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
        colnames(moc)=c("字詞", "相關性")
        finall<-moc
      }
      if (input$individual_obs) {
        parss<-data.frame( findAssocs(partdm , input$dd, 0.5) )
        para<-rownames(parss)
        parb<-parss[,1]
        parc<-cbind(para,parb)
        colnames(parc)=c("字詞", "相關性")
        finall<-parc
      }
      finall
    })})
})