#원본 데이터 읽기위한 패키지
library(readxl)
library(pillar)
#연결망 그래프 그리기위한 패키지

library(igraph)
library(combinat)

install.packages("arules")

install.packages("igraph")

install.packages("combinat")
#전처리 패키지
install.packages("tm")
install.packages("slam")
library(slam)
library(tm)
#library(stringr)

#text cleaning
install.packages("qdap")
library(qdap)
library(Nippon)
#install.packages("textclean")

library(textclean)
install.packages("Nippon")

#원본데이터 읽기 
kpop<-read_excel("20180509_data_kpop_2017.xlsx")

#2017년도 데이터 추출  
kpop_2017<-subset(kpop,year__1 ==2017)
#분석할 셀 추출 
kpop_2017<-subset(kpop_2017, select = c(Title, Description, Tags))
kpop_2017 <-c(kpop_2017[,1],kpop_2017[,2],kpop_2017[,3])
head(kpop_2017)
write.table(kpop_2017,"kpop_2017.txt")


g<-graph.data.frame(kpop_2017, directed = FALSE)

plot(g)
write.
# 행렬을 만들기 위해 문자열 필ㅇ
f_2017<-as.character(kpop_2017)
write.csv
write.csv(f_2017, "f_2017.csv")



# 약어를 원래 단어로
fl_2017 <- replace_contraction(f_2017)
# 띄어쓰기 제거
fl_2017 <- stripWhitespace(fl_2017)
# 소문자로
fl_2017 <- tolower(fl_2017)
# stop words 삭제 
fl_2017 <- removeWords(fl_2017, stopwords("en"))
zen2han(fl_2017)
write.csv(fl_2017, "fl_2017.csv")
write.table(fl_2017, "fl_2017.txt")
head(fl_2017,1)
class(fl_2017)
sort(fl_2)
corp=VCorpus(VectorSource(fl_2017))
inspect(corp)


corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, PlainTextDocument)
corp <- tm_map(corp, removeWords, "")
corp <- tm_map(corp, removeWords, stopwords('en'))
class(corp)
View(corp)



tdm<-TermDocumentMatrix(corp, control = list(bounds = list(global = c(1,Inf))))
tdm

# Since the sparsity is so high, i.e. a proportion of cells with 0s/ cells with other values is too large,
# let's remove some of these low frequency terms
tdm_rm_sparse <- removeSparseTerms(tdm, 0.995)
# Print out tweets_dtm data
tdm_rm_sparse

m <- as.matrix(tdm_rm_sparse)
View(m)

adjmatrix<- m %*% t(m)
g1 <-graph.adjacency(adjmatrix ,weighted = T, mode = "undirected")
g2 <-simplify(g1)
plot(g2)

#횟수에 따라 가중치 반영하여 크기를 다르게 하자
#정점은 해당 꼭지점이고 대문자 V로 표시
#엣지는 그 꼭지점에 연결된 선으로 대문자 E로 표시
#각 정점별로 엣지수를 파악하는 명령어가 degree()
V(g2)$degree <- degree(g2)
V(g2)$label.cex <-3*(V(g2)$degree / max(V(g2)$degree))
V(g2)$size <- 10*(V(g2)$degree / max(V(g2)$degree))
E(g2)$width <- 2*(E(g2)$weight / max(E(g2)$weight))
max(E(g2)$weight)


# 매개 중심성

# 네트워크 내에서 한 점이 담당하는 매개자 혹은 중개자 역할의 정도로서 중심성을 측정하는 방법

# ex : A와 B는 C를 통해서만 관계를 맺을 수 있다면, C는 잠재적으로 다른 사람들 사이를 통제하는 문지기 역할

#      한 노드가 연결망 내의 다른 노드들 사이의 최다 경로 위에 위치하면 할 수록 매개 중심성이 높다.
btw<-betweenness(g2)
btw.score<-round(btw)+1
btw.colors<-rev(heat.colors(max(btw.score)))
V(g2)$color<-btw.colors[btw.score]





plot(g2)
