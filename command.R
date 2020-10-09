





###########################
#package list

install.packages("quanteda")
install.packages("stm")
install.packages("readtext")
install.packages("RMeCab", repos = "http://rmecab.jp/R") 
install.packages("tidyr")
install.packages("devtools")
install.packages("tm")
install.packages("broom")
install.packages("tidytext")
install.packages("ggthemes")
install.packages("furrr")
install.packages("tidyverse")
install.packages("rmdformats")
install.packages("tibble")
install.packages("clue")

library(corrplot)
library(RMeCab)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(magrittr)
library(quanteda)
library(stm)
library(devtools)
library(readxl)
library(readtext)
library(tidyr)
library(tm)
library(broom)
library(tidytext)
library(ggthemes)
library(furrr)
library(scales)


#######################################
#data


d<-read.csv("musiclist.csv", header=T, stringsAsFactors = T, fileEncoding = "cp932")






####################################################################
# processing


keitai<- docMatrixDF(d$歌詞)






d2<-d
d2<-cbind(d2, sumw)
d2$sex_w[d2$sex_w=="不明"]<- NA
d2<-na.omit(d2)

d2$歌詞 %<>% str_replace_all(pattern= "[[:punct:]]",replacement=" ")
d2$歌詞 %<>% str_replace_all(pattern= "[[:digit:]]",replacement=" ")
d2$歌詞 %<>% str_replace_all(pattern= "[[:lower:]+[:upper:]]",replacement=" ")




corp_tm <- tm::VCorpus(tm::VectorSource(d2))
corp_tm[["1"]][["content"]] %<>% iconv(from="UTF-8",to = "CP932")
toks_mecab<- corp_tm[["1"]][["content"]] %>% lapply(function(x) unlist(RMeCabC(x))) %>% as.tokens() 
mx<-dfm(toks_mecab)


sumw<- apply(mx,1, sum)
sumw<- as.data.frame(sumw)

mx2<- dfm_trim(mx,min_termfreq = 0.0001, termfreq_type = "prop")
mx2<-dfm_remove(mx2,"^([ぁ-ん|ー]){1,2}$", valuetype="regex")
mx2<-dfm_remove(mx2,"^([ァ-ヴ|ー])+$",valuetype="regex")
mx2<-dfm_remove(mx2,"^([ぁ-んァ-ヴ|ー]){1,2}$", valuetype="regex")
mx2<-dfm_remove(mx2,"^([~ ～ = × ○ ←ゞ])+$",valuetype="regex")
mx2<-dfm_remove(mx2,  c("っていう","かっか","こっち","そういう"
                        ,"どんな" ,"なにか","ちょっと"
                        ,"ながら","ちゃん","なんで","どうして","そんなに"
                        ,"ばかり","そして","とても","しまっ","みたい"
                        ,"ちゃっ","みたい","なんて","あゝ","どうぞ","ちゃんと"
                        ,"じゃん","ほんの","たって", "らしく", "なれる"
                        ,"として","うまく","せめて","ごらん","だから","けれど"
                        ,"まかせ" ,"あふれ","こんな","くれる","できる"
                        ,"ましょ","そんな","だって","だけど","だらけ"
                        ,"たとえ","ところ", "という","ください","いくつ"
                        ,"おいで","ちゃう","つない","まるで","しまう"
                        ,"それでも","つもり","かしら" ,"おくれ","そっと"
                        ,"やっぱり","なきゃ" ,"くらい","こんなに","こんなにも"
                        ,"めぐり","たった", "たかっ","よかっ","どんなに","なさい"
                        ,"しまい","でしょ","なんか","事" ,"思う","様"
                        ,"見","誰", "何","少し","的","回","もう少し"
                        ,"程","方","合っ","られる","何もかも"
                        ,"なんか","出" ,"中","強","もっと", "何故", "何処","なかっ" 
                        ,"もっと","以上","微","出来","ごめん","ありがとう"))

colnames(mx2)
mx2<-mx2[,c(-3)]
mx2@Dimnames$features
stm<-convert(mx2, to="stm")

d2<- d2[c(-59),]


stm<- c(stm, list(d2, all.names=T))


out<- prepDocuments(stm$documents, stm$vocab, stm[[4]])
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


##################################
#topic search

kResult<- searchK(out$documents, out$vocab, K=c(3,4,5,6,7,8,9,10,20,30),prevalence=~s(年代)+歌手性別+sumw+年齢効果+sex_w,
                  data=meta, init.type="Spectral")


kResult[["results"]] %>% select(K, exclus, semcoh, heldout, residual) %>%  
  unnest() %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, colour = Metric)) +
  geom_point(size=2, shape=22, colour="grey", fill="lightgreen")+
  geom_line(size = 1.5, alpha = 0.7, show.legend = F) +
  facet_wrap(~ Metric, scales = "free_y") +
  theme(plot.title = element_text(hjust = 0.5,size = 20))+
  labs(x = "K (number of topics)", y = "Value", title = "Fig.2 トピック数（K）別の各指標の推定値")


kResult$results %>%
  select(K, exclus, semcoh) %>%
  filter(K %in% 2:10) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semcoh, exclus,color=K))+
  geom_point(size = 2, alpha = 1) +
  scale_colour_identity(guide="legend")+
  theme(plot.title = element_text(hjust = 0.5,size = 20),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.position="right")+
  labs(x = "Semantic coherence", y = "Exclusivity",
       title = "Semantic coherenceとExclusivityの比較")

###################
# run stm 

poliblogSelect_c <- stm(out$documents, out$vocab, K=5, content=~sex_w, prevalence=~s(年代)+歌手性別+sumw+年齢効果+sex_w,
                               data=meta, seed=8458159, init.type="Spectral",LDAbeta=F)

poliblogSelect7topic <- stm(out$documents, out$vocab, K=7, prevalence=~s(年代)+歌手性別+sumw+年齢効果+sex_w,
                   data=meta, seed=8458159, init.type="Spectral")

poliblogSelect6topic <- stm(out$documents, out$vocab, K=6, prevalence=~s(年代)+歌手性別+sumw+年齢効果+sex_w,
                       data=meta, seed=8458159, init.type="Spectral")

poliblogSelect5topic <- stm(out$documents, out$vocab, K=5, prevalence=~s(年代)+歌手性別+sumw+年齢効果+sex_w,
                            data=meta, seed=8458159, init.type="Spectral")

poliblogSelect4topic <- stm(out$documents, out$vocab, K=4, prevalence=~s(年代)+歌手性別+sumw+年齢効果+sex_w,

                            
                                                        data=meta, seed=8458159, init.type="Spectral")

###########################################
#choose model

  modelselect <- manyTopics(out$documents, out$vocab, K=5:7, prevalence=~s(年代)+歌手性別+sumw+年齢効果+sex_w,
                               data=meta, seed=8458159, init.type="Spectral")

 
  
 exclusivity<- modelselect[["exclusivity"]] %>% unlist() 
 semanticCoherence<- modelselect[["semcoh"]] %>% unlist()
 K<- c(1,2,3,4,5,1,2,3,4,5,6,1,2,3,4,5,6,7)
 model<-c(5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,7) %>% as.factor()
 
modelc<-cbind(K,exclusivity,semanticCoherence,model)
modelc<-as.data.frame(modelc)

modelc$model %<>% as.factor()
 

####################################################################
#7topic


topic7_th <- tidy(poliblogSelect7topic)

topic7_th %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  ggtitle("?g?s?b?N") +
  theme(plot.title = element_text(hjust = 0.5,size=20)) +
  labs(x="", y = expression("?g?҂?"))


top_terms_7 <- topic7_th %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()


gamma_terms_7 <- td_gamma_7 %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms_7, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms_7 %>%
  top_n(10, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(alpha = 0.8,show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 4.5) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.5), labels = percent_format()) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(hjust = 0.5,size = 20),
        plot.subtitle = element_text(size = 13),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12))+
  labs(x = NULL, y = expression("θ: Mean P（Topic|document）"),
       title = "文書中のトピック割合")


td_gamma_7 <- tidy(poliblogSelect7topic, matrix = "gamma")


td_gamma_7 %>%
  top_n(15, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(alpha = 0.8,show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 4.5) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.5),
                     labels = percent_format()) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(hjust = 0.5,size = 20),
        plot.subtitle = element_text(size = 13),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12))+
  labs(x = NULL, y = expression("θ: Mean P（Topic|document）"),
       title = "文書中のトピック割合")

labels7<- labelTopics(poliblogSelect7topic, 1:7, n=10)



thoughts7_1 <- findThoughts(poliblogSelect7topic, texts=str_sub(out$meta$歌詞,1,50), n=30, topics=1)$docs[[1]]
plotQuote(thoughts7_1[c(1,8,10,18,21,22,23,25,27,28)], width=90, main="Topic 1")

thoughts7_2 <- findThoughts(poliblogSelect7topic, texts=str_sub(meta$歌詞,50,100), n=30, topics=2)$docs[[1]]
plotQuote(thoughts7_2[c(3,5,7,9,12,14,18,23,27,29)], width=50, main="Topic 2")

thoughts7_3 <- findThoughts(poliblogSelect7topic, texts=str_sub(out$meta$歌詞,1,50), n=30, topics=3)$docs[[1]]
plotQuote(thoughts7_3[c(9,11,12,14,15,16,17,18,23,26)], width=100, main="Topic 3")

thoughts7_4 <- findThoughts(poliblogSelect7topic, texts=str_sub(out$meta$歌詞,1,50), n=30, topics=4)$docs[[1]]
plotQuote(thoughts7_4[c(3,4,7,17,18,22,26,28,29,30)], width=150, main="Topic 4")

thoughts7_5 <- findThoughts(poliblogSelect7topic, texts=str_sub(out$meta$歌詞,150,200), n=30, topics=5)$docs[[1]]
plotQuote(thoughts7_5[c(3,4,7,8,9,11,15,20,26,28)], width=100, main="Topic 5")

thoughts7_6 <- findThoughts(poliblogSelect7topic, texts=str_sub(out$meta$歌詞,100,150), n=30, topics=6)$docs[[1]]
plotQuote(thoughts7_6[c(1,2,5,8,14,15,16,17,23,24)], width=100, main="Topic 6")

thoughts7_7 <- findThoughts(poliblogSelect7topic, texts=str_sub(out$meta$歌詞,100,150), n=30, topics=7)$docs[[1]]
plotQuote(thoughts7_7[c(1,6,11,13,16,18,20,24,25,29)], width=100, main="Topic 7")






################################
# topic regression

kaiki7.lm<- estimateEffect(1:7~時代 +歌手性別+sumw+年齢効果+sex_w, stmobj=poliblogSelect7topic, metadata=meta, 
                               uncertainty="Global")

  kaiki7.spline<- estimateEffect(1:7~s(年代) +歌手性別+sumw+年齢効果+sex_w, stmobj=poliblogSelect7topic, metadata=meta, 
                        uncertainty="Global")

  options(scipen=20) 
  summary(kaiki7.spline)
  summary(kaiki_lm)

  par(mfrow = c(2, 3))
  
  for (i in 1:7){
    plot(kaiki7.spline, "年代", method = "continuous", topics = c(i) 
         ,main =  c(str_c("Topic ", i,":"),paste(labels7$prob[i,1:7],collapse = ", ")),xlab="Year" ,ylab=expression("Expected Topic Propoton（θ）")
         ,printlegend=F )
    
  }
  
  mtext(side = 1, line= -2, outer=T, text = "Fig. J-POPのトレンド1960-2019", cex=2 )




#####################################
#topic correlation

corr<-topicCorr(poliblogSelect7topic)



  
M <- corr$cor
colnames(M)<-c("Topic1", "Topic2",  "Topic3",    "Topic4",    "Topic5", "Topic6", "Topic7")
rownames(M)<-c("Topic1", "Topic2",  "Topic3",    "Topic4",    "Topic5", "Topic6","Topic7")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag = F)


#################################################################
#topic interaction

poliblogInteraction<- stm(out$documents, out$vocab, K=7, prevalence=~s(年代)*歌手性別+sumw+年齢効果+sex_w,
                       data=meta, seed=8458159, init.type="Spectral")



prep2 <- estimateEffect(1:7 ~s(年代)*歌手性別+年齢効果+sumw+sex_w, poliblogInteraction, metadata=meta, 
                        uncertainty="None")

par(mfrow = c(3,3))

for (i in 1:7){
  
  plot(prep2, covariate="年代", model=poliblogInteraction, method="continuous", xlab="Year",ylab="Expected Topic Propotion（θ）",main =  c(str_c("Topic ", i,":"),paste(labels7$prob[i,1:7],collapse = ", ")),
       moderator="歌手性別", moderator.value="男", linecol="blue", ylim=c(0,0.50), topics=c(i),
       printlegend=F)
  plot(prep2, covariate="年代", model=poliblogInteraction, method="continuous", xlab="Year",ylab="Expected Topic Propotion（θ）",main =  c(str_c("Topic ", i,":"),paste(labels7$prob[i,1:7],collapse = ", ")),
       moderator="歌手性別", moderator.value="女", linecol="red", add=T,topics=c(i),
       printlegend=F)
  legend("topleft", c("男", "女"), lwd=2, col=c("blue", "red"))
  
}



poliblogInteraction2<- stm(out$documents, out$vocab, K=7, prevalence= ~s(年代)*sex_w+歌手性別+sumw+年齢効果,
                          data=meta, seed=8458159, init.type="Spectral")

prep3 <- estimateEffect(1:7 ~s(年代)*sex_w+歌手性別+sumw+年齢効果, poliblogInteraction2, metadata=meta, 
                        uncertainty="None",prior=1e-5)


par(mfrow = c(3, 3))

for (i in 1:7){
  
  plot(prep3, covariate="年代", model=poliblogInteraction2, method="continuous", xlab="Year",ylab="Expected Topic Propotion（θ）",main =  c(str_c("Topic ", i,":"),paste(labels7$prob[i,1:7],collapse = ", ")),
       moderator="sex_w", moderator.value="男", linecol="blue", ylim=c(0,0.50), topics=c(i),
       printlegend=F)
  plot(prep3, covariate="年代", model=poliblogInteraction2, method="continuous", xlab="Year",ylab="Expected Topic Propotion（θ）",main =  c(str_c("Topic ", i,":"),paste(labels7$prob[i,1:7],collapse = ", ")),
       moderator="sex_w", moderator.value="女", linecol= "red", add=T,topics=c(i),
       printlegend=F)
  legend("topleft", c("男", "女"), lwd=2, col=c("blue", "red"))
  
}




#################################################################################
# topic6


topic6_th <- tidy(poliblogSelect6topic)

topic6_th %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  ggtitle("トピックごとの単語割合（上位10語）") +
  theme(plot.title = element_text(hjust = 0.5,size=20)) +
  labs(x="", y = expression("（β: P(word|topic)）"))




td_gamma_6 <- tidy(poliblogSelect6topic, matrix = "gamma")

top_terms_6 <- topic6_th %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms_6 <- td_gamma_6 %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms_6, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms_6 %>%
   top_n(10, gamma) %>%
   ggplot(aes(topic, gamma, label = terms, fill = topic)) +
   geom_col(alpha = 0.8,show.legend = FALSE) +
   geom_text(hjust = 0, nudge_y = 0.0005, size = 4.5) +
    coord_flip() +
    scale_y_continuous(expand = c(0,0), limits = c(0, 0.5), labels = percent_format()) +
   theme_tufte(ticks = FALSE) +
    theme(plot.title = element_text(hjust = 0.5,size = 20),
                 plot.subtitle = element_text(size = 13),
                    axis.text.x = element_text(size=10),
                   axis.text.y = element_text(size=12))+
     labs(x = NULL, y = expression("θ: Mean P（Topic|document）"),
                 title = "文書中のトピック割合")



thoughts6_1 <- findThoughts(poliblogSelect6topic, texts=str_sub(out$meta$歌詞,1,50), n=10, topics=1)$docs[[1]]
plotQuote(thoughts6_1, width=100, main="Topic 1")

thoughts6_2 <- findThoughts(poliblogSelect6topic, texts=str_sub(meta$歌詞,1,100), n=5, topics=2)$docs[[1]]
plotQuote(thoughts6_2, width=50, main="Topic 2")

thoughts6_3 <- findThoughts(poliblogSelect6topic, texts=str_sub(out$meta$歌詞,1,100), n=5, topics=3)$docs[[1]]
plotQuote(thoughts6_3, width=50, main="Topic 3")

thoughts6_4 <- findThoughts(poliblogSelect6topic, texts=str_sub(out$meta$歌詞,1,100), n=10, topics=4)$docs[[1]]
plotQuote(thoughts6_4, width=90, main="Topic 4")

thoughts6_5 <- findThoughts(poliblogSelect6topic, texts=str_sub(out$meta$歌詞,1,70), n=25, topics=5)$docs[[1]]
plotQuote(thoughts6_5, width=100, main="Topic 5")


thoughts6_6 <- findThoughts(poliblogSelect6topic, texts=str_sub(out$meta$歌詞,1,70), n=20, topics=6)$docs[[1]]
plotQuote(thoughts6_6, width=100, main="Topic 6")



############################################################################################
# topic5


td_gamma_5 <- tidy(poliblogSelect5topic, matrix = "gamma")

top_terms_5 <- topic5_th %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms_5 <- td_gamma_5 %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms_6, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms_5 %>%
  top_n(10, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(alpha = 0.8,show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 4.5) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.5), labels = percent_format()) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(hjust = 0.5,size = 20),
        plot.subtitle = element_text(size = 13),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12))+
  labs(x = NULL, y = expression("θ: Mean P（Topic|document）"),
       title = "文書中のトピック割合")


topic5_th <- tidy(poliblogSelect5topic)

topic5_th %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  ggtitle("トピックごとの単語割合（上位10語）") +
  theme(plot.title = element_text(hjust = 0.5,size=20)) +
  labs(x="", y = expression("（β: P(word|topic)）"))


labels5<- labelTopics(poliblogSelect5topic, 1:5, n=10)

  thoughts5_1 <- findThoughts(poliblogSelect5topic, texts=str_sub(out$meta$歌詞,1,50), n=10, topics=1)$docs[[1]]
plotQuote(thoughts5_1, width=90, main="Topic 1")

thoughts5_2 <- findThoughts(poliblogSelect5topic, texts=str_sub(out$meta$歌詞,1,30), n=25, topics=2)$docs[[1]]
plotQuote(thoughts5_2, width=100, main="Topic 2")

thoughts5_3 <- findThoughts(poliblogSelect5topic, texts=str_sub(out$meta$歌詞,1,50), n=10, topics=3)$docs[[1]]
plotQuote(thoughts5_3, width=90, main="Topic 3")

thoughts5_4 <- findThoughts(poliblogSelect5topic, texts=str_sub(out$meta$歌詞,1,50), n=10, topics=4)$docs[[1]]
plotQuote(thoughts5_4, width=90, main="Topic 4")

thoughts5_5 <- findThoughts(poliblogSelect5topic, texts=str_sub(out$meta$歌詞,50,100), n=10, topics=5)$docs[[1]]
plotQuote(thoughts5_5, width=100, main="Topic 5")





##################################################################
# 4topic



topic4_th <- tidy(poliblogSelect4topic)

topic4_th %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  ggtitle("トピックごとの単語割合（上位10語）") +
  theme(plot.title = element_text(hjust = 0.5,size=20)) +
  labs(x="", y = expression("（β: P(word|topic)）"))

thoughts4_1 <- findThoughts(poliblogSelect4topic, texts=str_sub(out$meta$歌詞,1,60), n=20, topics=1)$docs[[1]]
plotQuote(thoughts4_1, width=90, main="Topic 1")

thoughts4_2 <- findThoughts(poliblogSelect4topic, texts=str_sub(meta$歌詞,1,100), n=10, topics=2)$docs[[1]]
plotQuote(thoughts4_2, width=50, main="Topic 2")

thoughts4_3 <- findThoughts(poliblogSelect4topic, texts=str_sub(out$meta$歌詞,1,100), n=10, topics=3)$docs[[1]]
plotQuote(thoughts4_3, width=70, main="Topic 3")

thoughts4_4 <- findThoughts(poliblogSelect4topic, texts=str_sub(out$meta$歌詞,1,50), n=40, topics=4)$docs[[1]]
plotQuote(thoughts4_4, width=150, main="Topic 4")










