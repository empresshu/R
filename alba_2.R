#¿øº» µ¥ÀÌÅÍ ÀĞ±âÀ§ÇÑ ÆĞÅ°Áö
library(readxl)
library(pillar)
#¿¬°á¸Á ±×·¡ÇÁ ±×¸®±âÀ§ÇÑ ÆĞÅ°Áö

library(igraph)
library(combinat)

install.packages("arules")

install.packages("igraph")
install.packages("combinat")
install.packages("tm")

library(tm)
#library(stringr)

#text cleaning
install.packages("qdap")
library(qdap)
#install.packages("textclean")

#library(textclean)


#¿øº»µ¥ÀÌÅÍ ÀĞ±â 
kpop<-read_excel("20180509_data_kpop_2017.xlsx")

#2017³âµµ µ¥ÀÌÅÍ ÃßÃâ  
kpop_2017<-subset(kpop,year__1 ==2017)
#ºĞ¼®ÇÒ ¼¿ ÃßÃâ 
kpop_2017<-subset(kpop_2017, select = c(Title, Description, Tags))
kpop_2017 <-c(kpop_2017[,1],kpop_2017[,2],kpop_2017[,3])
head(kpop_2017)
# Çà·ÄÀ» ¸¸µé±â À§ÇØ ¹®ÀÚ¿­ ÇÊ¤·
f_2017<-as.character(kpop_2017)



# ¾à¾î¸¦ ¿ø·¡ ´Ü¾î·Î
fl_2017 <- replace_contraction(f_2017)
# ¶ç¾î¾²±â Á¦°Å
fl_2017 <- stripWhitespace(fl_2017)°Å
# ¼Ò¹®ÀÚ·Î
fl_2017 <- tolower(fl_2017)
fl_2017 <- replace_symbol(fl_2017)
fl_2017 <- removeWords(fl_2017, stopwords("en"))
head(fl_2017, 10)
class(fl_2017)
sort(fl_2)
corp=VCorpus(VectorSource(fl_2017))
inspect(corp)


corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, PlainTextDocument)
corp <- tm_map(corp, removeWords, stopwords('en'))
class(corp)



tdm<-TermDocumentMatrix(corp)
inspect(tdm)
# tdm <- removeSparseTerms(tdm, 0.8)
N <- 1000
tdm <-findFreqTerms(tdm, N)
sort(tdm, decreasing = T)
m2 <- as.matrix(tdm)
m2


# http://replet.tistory.com/17?category=667225
# https://rpubs.com/ivan_berlocher/79849
ko.words = function(doc){
  d = str_split(doc, " ")[[1]]
  
  extracted = tolower(str_match(d, '([°¡-ÆRa-zA-Z]+)/[NVO]'))
  keyword = extracted[,2]
  keyword[!is,na(keyword)]
}


options(mc.cores=1)

cps = tm_map(cps, removeWords,c("com","www","https","http",
"http","youtube","na"))

# corpus¸¦ tdmÀ¸·Î º¯È¯ : ÃâÇöºóµµ counting
tdm<-TermDocumentMatrix(cps, control=list(tokenize=ko.words,
                                          
tdm.matrix = as.matrix(tdm)

word.count = rowSums(tdm.matrix)
word.order = order(word.count, decreasing = T)
freq.word = tdm.matrix[word.order[35:85],]
rownames(tdm.matrix)[word.order[35:85]]
qg <-qgraph(co.matrix,
            labels = rownames(co.matrix),diag = F, layout='spring', edge.color='black', vsize=log(diag(co.matrix))*1.5)
#corpus <- tm_map(corpus, removeWords, c("dont", "can", "what", "cant"))     # Æ¯Á¤ ´Ü¾î Á¦°Å - Ä³¸¯ÅÍ º¤ÅÍ·Î ÁöÁ¤

#ÃâÃ³: http://pannacotta.tistory.com/51 [Shimmering Green]



# http://replet.tistory.com/48?category=667225
apply(kpop_2017, readLines)
kpop_2017_txt <-


g<-graph.data.frame(kpop_2017,directed=FALSE)

plot(g)


close(f)
tran <- Map(extractNoun, fl_2017)
tran <- unique(tran)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x) {Filter(function(y) {nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)} )
tran <- Filter(function(x){length(x) >= 2}, tran)
names(tran) <- paste("Tr", 1:length(tran), sep="")
wordtran <- as(tran, "transactions")

#co-occurance table 
wordtab <- crossTable(wordtran)


ares <- apriori(wordtran, parameter=list(supp=0.05, conf=0.05))
inspect(ares)
rules <- labels(ares, ruleSep=" ")
rules <- sapply(rules, strsplit, " ",  USE.NAMES=F)
rulemat <- do.call("rbind", rules)

# ares <- apriori(wordtran, parameter=list(supp=0.05, conf=0.05))
# inspect(ares)
# rules <- labels(ares, ruleSep="/", setStart="", setEnd="")
# rules <- sapply(rules, strsplit, "/",  USE.NAMES=F)
# rules <- Filter(function(x){!any(x == "")},rules)
# rulemat <- do.call("rbind", rules)
# rulequality <- quality(ares)
# ruleg <- graph.edgelist(rulemat,directed=F)

#plot for important pairs 
ruleg <- graph.edgelist(rulemat[-c(1:16),],directed=F)
plot.igraph(ruleg, vertex.label=V(ruleg)$name, vertex.label.cex=0.5, vertex.size=20, layout=layout.fruchterman.reingold.grid)


#plot for all pairs
# tranpairs <- sapply(tran, function(x){t(combn(x,2))})
# sapply(tran,function(x){x })
# edgelist  <- do.call("rbind", tranpairs)
# edgelist <- unique(edgelist)
# g <- graph.edgelist(edgelist, directed=F)
# plot(g)




                                            