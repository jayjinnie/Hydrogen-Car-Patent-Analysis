library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(widyr)
library(arules)
library(arulesViz)
library(igraph)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(network)
library(magrittr)

ipc <- read_excel('C:/Users/olivi/Rdataframe/GreenHydrogen_Dataset.xlsx')
head(ipc)
summary(ipc)
str(ipc)
glimpse(ipc)
sum(is.na(ipc))

unique(ipc$소분류)


## 1. 소분류별 테이블 생성
# sofc
sofc <- ipc %>% 
  filter(`소분류` == 'SOFC') %>% 
  select('전체IPC')
# pemfc
pemfc <- ipc %>% 
  filter(`소분류` == 'PEMFC') %>% 
  select('전체IPC')
# mcfc
mcfc <- ipc %>% 
  filter(`소분류` == 'MCFC') %>% 
  select('전체IPC')
# aemfc
aemfc <- ipc %>% 
  filter(`소분류` == 'AEMFC') %>% 
  select('전체IPC')
# dmfc
dmfc <- ipc %>% 
  filter(`소분류` == 'DMFC') %>% 
  select('전체IPC')
# pafc
pafc <- ipc %>% 
  filter(`소분류` == 'PAFC') %>% 
  select('전체IPC')


## 2. 소분류별 Basket Transaction 생성
# sofc
write.csv(sofc, 'IPC_sofc.csv', quote=FALSE, row.names=TRUE)
sofc_txn <- read.transactions(file='IPC_sofc.csv', rm.duplicates=TRUE, format='basket', sep=',', cols=1)
print(sofc_txn)
# pemfc
write.csv(pemfc, 'IPC_pemfc.csv', quote=FALSE, row.names=TRUE)
pemfc_txn <- read.transactions(file='IPC_pemfc.csv', rm.duplicates=TRUE, format='basket', sep=',', cols=1)
print(pemfc_txn)
# mcfc
write.csv(mcfc, 'IPC_mcfc.csv', quote=FALSE, row.names=TRUE)
mcfc_txn <- read.transactions(file='IPC_mcfc.csv', rm.duplicates=TRUE, format='basket', sep=',', cols=1)
print(mcfc_txn)
# aemfc
write.csv(aemfc, 'IPC_aemfc.csv', quote=FALSE, row.names=TRUE)
aemfc_txn <- read.transactions(file='IPC_aemfc.csv', rm.duplicates=TRUE, format='basket', sep=',', cols=1)
print(aemfc_txn)
# dmfc
write.csv(dmfc, 'IPC_dmfc.csv', quote=FALSE, row.names=TRUE)
dmfc_txn <- read.transactions(file='IPC_dmfc.csv', rm.duplicates=TRUE, format='basket', sep=',', cols=1)
print(dmfc_txn)
# pafc
write.csv(pafc, 'IPC_pafc.csv', quote=FALSE, row.names=TRUE)
pafc_txn <- read.transactions(file='IPC_pafc.csv', rm.duplicates=TRUE, format='basket', sep=',', cols=1)
print(pafc_txn)


## 3. 출원인별 CPC 빈도수 확인
# pemfc
itemFrequencyPlot(pemfc_txn, topN = 20)
# sofc
itemFrequencyPlot(sofc_txn, topN = 20)
# dmfc
itemFrequencyPlot(dmfc_txn, topN = 20)
# aemfc
itemFrequencyPlot(aemfc_txn, topN = 20)
# mcfc
itemFrequencyPlot(mcfc_txn, topN = 20)
# pafc
itemFrequencyPlot(pafc_txn, topN = 20)


## 4. 출원인별 Apriori Algorithm
# pemfc
pemfc_rules <- apriori(pemfc_txn, 
                       parameter = list(minlen=3, sup=0.02, 
                                        conf=0.05, target='rules'))
print(length(pemfc_rules))
inspect(pemfc_rules[1:20])

# sofc
sofc_rules <- apriori(sofc_txn, 
                    parameter = list(minlen=3, sup=0.03, 
                                     conf=0.05, target='rules'))
print(length(sofc_rules))
inspect(sofc_rules[1:20])

# dmfc
dmfc_rules <- apriori(dmfc_txn, 
                      parameter = list(minlen=1, sup=0.03, 
                                       conf=0.05, target='rules'))
print(length(dmfc_rules))
inspect(dmfc_rules[1:20])

# aemfc
aemfc_rules <- apriori(aemfc_txn, 
                       parameter = list(minlen=1, sup=0.05, 
                                        conf=0.05, target='rules'))
print(length(aemfc_rules))
inspect(aemfc_rules[1:20])

# mcfc
mcfc_rules <- apriori(mcfc_txn, 
                      parameter = list(minlen=3, sup=0.07, 
                                       conf=0.05, target='rules'))
print(length(mcfc_rules))
inspect(mcfc_rules[1:20])



# pafc
pafc_rules <- apriori(pafc_txn, 
                      parameter = list(minlen=3, sup=0.03, 
                                       conf=0.05, target='rules'))
print(length(pafc_rules))
inspect(pafc_rules[1:20])



## 5. 출원인별 Parallel Coordinates Plot 시각화
# 선 굵기: support, 색: lift
plot(pemfc_rules[1:20], method="paracoord")
plot(sofc_rules[1:20], method="paracoord")
plot(dmfc_rules[1:20], method="paracoord")
plot(aemfc_rules[1:20], method="paracoord")
#plot(mcfc_rules[1:20], method="paracoord")
#plot(pafc_rules[1:20], method="paracoord")

plot(pemfc_rules[1:20], method="grouped")
plot(sofc_rules[1:20], method="grouped")
plot(dmfc_rules[1:20], method="grouped")
plot(aemfc_rules[1:20], method="grouped")
#plot(mcfc_rules[1:20], method="grouped")
#plot(pafc_rules[1:20], method="grouped")

plot(pemfc_rules[1:20], method="graph")
plot(sofc_rules[1:20], method="graph")
plot(dmfc_rules[1:20], method="graph")
plot(aemfc_rules[1:20], method="graph")


## 6. 출원인별 주요 연관규칙 CPC 필터링
# pemfc
ipc %>% filter(`소분류` == 'PEMFC') %>%
   filter(grepl('C25B-009/10', `전체IPC`)) %>%
   filter(grepl('H01M-008/02', `전체IPC`)) %>%
   filter(grepl('C25B-009/00', `전체IPC`)) %>%
  arrange(desc('피인용문헌수')) -> pemfc_filter
# sofc
ipc %>% filter(`소분류` == 'SOFC') %>%
  filter(grepl('H01M-008/124', `전체IPC`)) %>%
  filter(grepl('C25B-011/04', `전체IPC`)) %>%
  filter(grepl('H01M-012/06', `전체IPC`)) %>%
  arrange(desc('피인용문헌수')) -> sofc_filter
# dmfc
ipc %>% filter(`소분류` == 'DMFC') %>%
  filter(grepl('B01J-023/42', `전체IPC`)) %>%
  filter(grepl('H01M-004/92', `전체IPC`)) %>%
  filter(grepl('H01M-004/88', `전체IPC`)) %>%
  arrange(desc('피인용문헌수')) -> dmfc_filter
# aemfc
ipc %>% filter(`소분류` == 'AEMFC') %>%
  filter(grepl('C02F-001/461', `전체IPC`)) %>%
  filter(grepl('H01M-008/1051', `전체IPC`)) %>%
  arrange(desc('피인용문헌수')) -> aemfc_filter


## 7. 소분류별 주요 IPC SNA
# 1) 중심성 확인 (페이지랭크 Page Rank)
# 2) 네트워크 그래프 시각화

# pemfc
# 노드(IPC) 생성
pemfc_filter %>% tidyr::separate_rows(전체IPC, sep=',') -> df
length(unique(df$전체IPC))
df$전체IPC <- df$전체IPC %>% str_trim() # 공백제거
mtx <- df %>% pairwise_count(전체IPC, 키값, sort=T, upper=T)
mtx <- mtx %>% arrange(desc(n))
head(mtx, 20)
mtx <- mtx %>% 
  filter(n > 1)
gph <- mtx %>% graph_from_data_frame()

# 중심성 계산
pr <- gph %>%
  page.rank(directed = FALSE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank")
head(pr)
#pr <- pr[c(1:19),]
#head(pr)
#---
deg <- igraph::degree(gph, V(gph), mode='all')
V(gph)$size <- pr + 20
gph <- simplify(gph, remove.multiple=T, remove.loops=T)
par(mar=c(0,0,0,0))
# 시각화
clp <- cluster_optimal(gph)
plot(clp, gph,
     layout = layout.fruchterman.reingold,
     vertex.color = 'orange',
     vertex.label.color = 'navy',
     vertex.label.cex = .75,
     edge.arrow.size=.3,
     edge.color='orange',
     interactive=T)


# sofc
# 노드(IPC) 생성
sofc_filter %>% tidyr::separate_rows(전체IPC, sep=',') -> df
length(unique(df$전체IPC))
df$전체IPC <- df$전체IPC %>% str_trim() # 공백제거
mtx <- df %>% pairwise_count(전체IPC, 키값, sort=T, upper=T)
mtx <- mtx %>% arrange(desc(n))
head(mtx, 20)
gph <- mtx %>% graph_from_data_frame()

# 중심성 계산
pr <- gph %>%
  page.rank(directed = FALSE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank")
head(pr)
#pr <- pr[c(1:19),]
#head(pr)
#---
deg <- igraph::degree(gph, V(gph), mode='all')
V(gph)$size <- pr + 20
gph <- simplify(gph, remove.multiple=T, remove.loops=T)
par(mar=c(0,0,0,0))
# 시각화
clp <- cluster_optimal(gph)
plot(clp, gph,
     layout = layout.fruchterman.reingold,
     vertex.color = 'orange',
     vertex.label.color = 'navy',
     vertex.label.cex = .75,
     edge.arrow.size=.3,
     edge.color='orange',
     interactive=T)

# dmfc
# 노드(IPC) 생성
dmfc_filter %>% tidyr::separate_rows(전체IPC, sep=',') -> df
length(unique(df$전체IPC))
df$전체IPC <- df$전체IPC %>% str_trim() # 공백제거
mtx <- df %>% pairwise_count(전체IPC, 키값, sort=T, upper=T)
mtx <- mtx %>% arrange(desc(n))
head(mtx, 20)
gph <- mtx %>% graph_from_data_frame()

# 중심성 계산
pr <- gph %>%
  page.rank(directed = FALSE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank")
head(pr)
#pr <- pr[c(1:19),]
#head(pr)
#---
deg <- igraph::degree(gph, V(gph), mode='all')
V(gph)$size <- pr + 20
gph <- simplify(gph, remove.multiple=T, remove.loops=T)
par(mar=c(0,0,0,0))
# 시각화
clp <- cluster_optimal(gph)
plot(clp, gph,
     layout = layout.fruchterman.reingold,
     vertex.color = 'orange',
     vertex.label.color = 'navy',
     vertex.label.cex = .75,
     edge.arrow.size=.3,
     edge.color='orange',
     interactive=T)

# aemfc
# 노드(IPC) 생성
aemfc_filter %>% tidyr::separate_rows(전체IPC, sep=',') -> df
length(unique(df$전체IPC))
df$전체IPC <- df$전체IPC %>% str_trim() # 공백제거
mtx <- df %>% pairwise_count(전체IPC, 키값, sort=T, upper=T)
mtx <- mtx %>% arrange(desc(n))
head(mtx, 20)
gph <- mtx %>% graph_from_data_frame()

# 중심성 계산
pr <- gph %>%
  page.rank(directed = FALSE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank")
head(pr)
#pr <- pr[c(1:19),]
#head(pr)
#---
deg <- igraph::degree(gph, V(gph), mode='all')
V(gph)$size <- pr + 20
gph <- simplify(gph, remove.multiple=T, remove.loops=T)
par(mar=c(0,0,0,0))
# 시각화
clp <- cluster_optimal(gph)
plot(clp, gph,
     layout = layout.fruchterman.reingold,
     vertex.color = 'orange',
     vertex.label.color = 'navy',
     vertex.label.cex = .75,
     edge.arrow.size=.3,
     edge.color='orange',
     interactive=T)
