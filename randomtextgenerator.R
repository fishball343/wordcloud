set.seed(1)
fileConn<-file("random.txt")
common_words = scan("commonwords.txt", what="", sep="\n")

string = c()
for(word in sample(common_words, 150)){
  word_list = rep(word, rpois(1, 4))
  string = c(string, word_list)
}

writeLines(string, fileConn)
close(fileConn)