library(NMF)
library(Matrix)
fname <- "http://www.umich.edu/~umfandsf/other/ebooks/alice30.txt"
alice <- readLines(fname)
alice2 <- alice[41:3370] 
alice2 <- alice2[alice2 != ""]
alice.words <- unlist(strsplit(alice2, split ="[[:space:]]+|[[:punct:]]+"))
# 文の区切りは空白になる。これを"<ends>"に変える
#alice.words2 <- ifelse(alice.words=="", "<ends>", alice.words)
alice.words2 <- alice.words[alice.words!=""]

ngram <- as.data.frame(embed(alice.words2, 2))
ngram <- ngram[,c(2,1)]
ag <- aggregate(rep(1, nrow(ngram)), ngram, sum)

u1 = unique(ag[,1])
u2 = unique(ag[,2])
mat = as.matrix(xtabs(~V2+V1, data=ag, sparse=T))
wh = nmf(mat, 10)
