library(quanteda)
library(tm)

find_phrase <- function(ngram,phrase){
    ngram[grepl(paste0("^",phrase), ngram[,1]),]
}

take_words<-function(sentence,n){
    sentence_t <-tokenize(as.character(sentence),removePunct = TRUE)
    l<-length(sentence_t[[1]])
    if (l<n) {n=l}
    phrase<- sentence_t[[1]][(l-n+1):l]
    if (n == 5) { phrase = paste(phrase[1],phrase[2],phrase[3],phrase[4],phrase[5])}
    if (n == 4) { phrase = paste(phrase[1],phrase[2],phrase[3],phrase[4])}
    if (n == 3) { phrase = paste(phrase[1],phrase[2],phrase[3])}
    if (n == 2) { phrase = paste(phrase[1],phrase[2])}
    phrase
    #   for (i in seq(n)) {
    #       phrase<-paste(phrase[i])  
    #   }
}


find_suggestion <-function(ngram,phrase){
    ngram[grepl(paste0("^",phrase," "), ngram[,1]),]
}


take_lastWord <- function(phrase){
    listWords<-take_words(phrase,1)
}

# final_suggestion <- function(suggestion,m){
#     if (nrow(suggestion)<m) { m <- nrow(suggestion)}
#     temp<- data.frame(rep(0,m)) 
#     names(temp) <- c("suggestion")
#     for (i in seq(m)){ temp[i,1] <-take_lastWord(suggestion[i,1]) }
#     temp
# }

final_suggestion <- function(suggestion,m){
    if (nrow(suggestion)<m) { m <- nrow(suggestion)}
    temp<- data.frame(rep(0,m),rep(0,m)) 
    names(temp) <- c("suggestion","probability")
    for (i in seq(m)){ temp[i,1] <-take_lastWord(suggestion[i,1]) 
    temp[i,2] <- round(suggestion[i,3],5)    }
    temp
}

#########
unigram <- readRDS("unigram.RDS")
bigram <- readRDS("bigram.RDS")
trigram <- readRDS("trigram.RDS")
fourgram <- readRDS("fourgram.RDS")
#########

shinyServer(function(input, output) {
    result <- data.frame("")
    names(result) <- c("suggestion")
    
    output$value <- renderPrint({ 
    nr <- input$number
    sentence <- tolower(input$text)    
    len <- length(tokenize(take_words(sentence,3))[[1]])
    if (len == 3){
        phrase <- take_words(sentence,3)
        suggestion <- find_suggestion(fourgram,phrase)
        if (nrow(suggestion) >0){ result <- final_suggestion(suggestion,nr) }
        else {
            phrase <- take_words(sentence,2)
            suggestion <- find_suggestion(trigram,phrase)
            if (nrow(suggestion) > 0) { result <- final_suggestion(suggestion,nr) }
            else {
                phrase <- take_words(sentence,1)
                suggestion <- find_suggestion(bigram,phrase)
                if (nrow(suggestion) > 0) { result <- final_suggestion(suggestion,nr) }
                else {
                    suggestion <- unigram[,1][4:8]
                    result <- final_suggestion(suggestion,nr)
                }
            }
        }
    }
    
    if (len == 2){
        phrase <- take_words(sentence,2)
        suggestion <- find_suggestion(trigram,phrase)
        if (nrow(suggestion) > 0) { result <- final_suggestion(suggestion,nr) }
        else {
            phrase <- take_words(sentence,1)
            suggestion <- find_suggestion(bigram,phrase)
            if (nrow(suggestion) > 0) { result <- final_suggestion(suggestion,nr) }
            else {
                suggestion <- unigram[,1][4:8]
                result <- final_suggestion(suggestion,nr)
            }
        }
    }
    
    if (len == 1){
        phrase <- take_words(sentence,1)
        suggestion <- find_suggestion(bigram,phrase)
        if (nrow(suggestion) > 0) { result <- final_suggestion(suggestion,nr) }
        else {
            suggestion <- unigram[4:20,1]
            suggestion <- as.data.frame(suggestion)
            result <- final_suggestion(suggestion,nr)
        }
        
    }
    
    result 
  
    })  
    
})