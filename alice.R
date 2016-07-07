library(NNLM)
fname <- "http://www.umich.edu/~umfandsf/other/ebooks/alice30.txt"
alice <- readLines(fname)
alice2 <- alice[41:3370] 
alice2 <- alice2[alice2 != ""]
alice.words <- unlist(strsplit(alice2, split ="[[:space:]]+|[[:punct:]]+"))
# 文の区切りは空白になる。これを"<ends>"に変える
#alice.words2 <- ifelse(alice.words=="", "<ends>", alice.words)
alice.words2 <- alice.words[alice.words!=""]

make.lagged.2gram=function(words, lag){
  ngram <- as.data.frame(embed(alice.words2, lag+1))
  ngram <- ngram[,c(lag+1,1)]
  ag = aggregate(rep(1, nrow(ngram)), ngram, sum)
  names(ag) <- c("V2","V1","x")
  ret <- as.matrix(xtabs(x~V2+V1, data=ag, sparse=T))
  rownames(ret) <- paste0(rownames(ret), "_", lag)
  ret
}

mat <- make.lagged.2gram(alice.words2, 1)
mat2 <- make.lagged.2gram(alice.words2, 2)
mat3 <- make.lagged.2gram(alice.words2, 3)
mat4 <- make.lagged.2gram(alice.words2, 4)

#mat = prop.table(mat, margin=2)
#mat2 = prop.table(mat2, margin=2)
#mat3 = prop.table(mat3, margin=2)
#mat4 = prop.table(mat4, margin=2)

matx <- rbind(mat4, mat3, mat2, mat)
wh = nnmf(matx, k=10)
matp = wh$W %*% wh$H
predict.next.word <- function(w, model, nword){
  w = paste0(w, "_", (length(w):1))
  p=apply(model[w,], 2, prod)
  names(sort(p, decreasing=T)[1:nword])
}
predict.sentence <- function(ws, model, n){
  for(w in ws){
    cat(w, " ")
  }
  for(i in 1:n){
    pw = predict.next.word(ws, model, 1)
    cat(pw, " ")
    ws <- c(ws[-1], pw)
  }
}
ws <- c("I","m","afraid","but")
predict.sentence(ws,matp,10)
ws <- c("Alice","was","afraid","that")
predict.sentence(ws,matp,10)
