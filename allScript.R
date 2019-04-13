## download data
file_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
file_name <- "Coursera-SwiftKey.zip" 
if (!file.exists(file_name)){
  download.file(file_url, destfile=file_name, method="curl")
  unzip(zipfile=file_name, exdir=getwd())
}

## load data and get file information
library(stringi)
library(dplyr)
library(knitr)
text_dir <- "final/en_US"
text_names <- list.files(text_dir, "txt") 
file_size <- round(file.info(dir(text_dir))[1]/1024^2, digits=2)
colnames(file_size) <- "File size (MB)"
start_time <- as.POSIXct(Sys.time())
if (exists("a")){rm(a)} 
for(i in c(1: length(text_names))){
  print(text_names[i])
  con <- file(paste(text_dir,text_names[i], sep = "/"), "rb") #use "rb" to make sure all the lines are read
  assign(text_names[i], readLines(con, encoding = "UTF-8", skipNul = T))
  close(con)
  texts <- get(text_names[i])
  df <- data.frame(text_name = text_names[i], 
                   line_length = stri_count_boundaries(texts,type="character"),
                   word_count = stri_count_words(texts),
                   stringsAsFactors = F)
  if (exists("a")) {
    a <- bind_rows(a, df)
  }else{
    a <- bind_rows(df)
  }
}
rm(texts, df)
print(difftime(Sys.time(), start_time, units="secs")) #70.89089 secs
kable(cbind(file_size, info_df <- a %>% group_by(text_name) %>%
              summarise("Num. of lines" = n(),
                        "Max. line length" = max(line_length), 
                        "Total word count" = sum(word_count),
                        "Words per line" = round(mean(word_count),2)) %>% 
              subset(select = -text_name )))

# plot histogram 
library(ggplot2)
a %>% group_by(text_name) %>%
  subset(word_count<quantile(a$word_count, 0.99)) %>%
  ggplot(aes(x = word_count, fill=text_name)) +
  geom_histogram(binwidth = 5, show.legend = FALSE)+
  facet_grid(text_name~., scales = "free_y") +
  ggtitle("Histogram of Word Counts of Each Line", subtitle = "of all data")+
  xlab("Word count per line")+
  ylab("Frequency")
rm(a)

## sampling data and show samples information
set.seed(10086)
sampling_rate = 0.01
text_len <- info_df$`Num. of lines`
samples <- rbind(data.frame(text_src = "blogs", 
                            text = en_US.blogs.txt[rbinom(text_len[1]*sampling_rate,text_len[1], 0.5)]
                            ,stringsAsFactors = F),
                 data.frame(text_src = "news", 
                            text = en_US.news.txt[rbinom(text_len[2]*sampling_rate,text_len[2], 0.5)]
                            ,stringsAsFactors = F),
                 data.frame(text_src = "twitter", 
                            text = en_US.twitter.txt[rbinom(text_len[3]*sampling_rate,text_len[3], 0.5)]
                            ,stringsAsFactors = F))
a <- data.frame(text_name = samples$text_src, 
                word_count = stri_count_words(samples$text)) 
kable(a %>% group_by("Text Source" = text_name) %>%
        summarise("Num. of lines" = n(),
                  "Total word count" = sum(word_count), 
                  "Words per line" = round(mean(word_count),2))
)
a %>% group_by(text_name) %>%
  subset(word_count<quantile(a$word_count, 0.99)) %>%
  ggplot(aes(x = word_count, fill=text_name)) +
  geom_histogram(binwidth = 5, show.legend = FALSE)+
  facet_grid(text_name~., scales = "free_y") +
  ggtitle("Histogram of Word Counts of Each Line", subtitle = "of samples")+
  xlab("Word count per line")+
  ylab("Frequency")
write.table(samples,row.names = F,"samples.txt", fileEncoding = "UTF-8")
rm(a, en_US.blogs.txt, en_US.news.txt, en_US.twitter.txt)


## tokenize samples and build N-gram models
library(tidytext)
library(tidyr)
file_url <- "https://www.freewebheaders.com/download/files/full-list-of-bad-words_csv-file_2018_07_30.zip"
file_name <- "full-list-of-bad-words_csv-file_2018_07_30" 
if (!file.exists(paste(file_name,"zip", sep = "."))){
  download.file(file_url, destfile=paste(file_name,"zip", sep = "."), method="curl")
  unzip(zipfile=paste(file_name,"zip", sep = "."), exdir=getwd())
}
profanity <- read.csv(paste(file_name, "csv", sep = "."), header=F, sep=";", strip.white=T, stringsAsFactors = F)[1]
characters <- c("\u00f1","\u00a9","\u3e64","\u00f8","\u24d5","\u24d0","\u24d2","\u24e3",
                "\ufeff","\u1e5b","\u1e63","\u1e47","\u3e30","\u00e4","\u00e7")

start_time <- as.POSIXct(Sys.time())
clean_sample <- 
  mutate(samples, text = gsub("([--:\\w?@%&+~#=]*\\.[a-z]{2,4}\\/{0,2})((?:[?&](?:\\w+)=(?:\\w+))+|[--:\\w?@%&+~#=]+)?", 
                              "",text, perl=T)) %>% #remove url and email address
  mutate(text = gsub("RT : | via|\\(via \\)" ,"",text, perl = T))%>% #remove retweet
  mutate(text = gsub(sprintf("(*UCP)(%s)", paste(characters, collapse = "|")), 
                     "", text, perl = TRUE)) %>% #remove unicode characters
  mutate(text = gsub("[[:digit:]]","",text)) %>% #remove numbers
  mutate(text = gsub("(['])|[[:punct:]]", "\\1", text)) #remove punctuation
write.table(clean_sample,row.names = F,"clean.txt")#,fileEncoding = "UTF-8")
print(difftime(Sys.time(), start_time, units="secs")) 

# single word frequency 
unigram<- clean_sample %>% 
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>% #10.02817 secs
  count(word, sort = T) %>% 
  mutate(cum = cumsum(n/sum(n)))
write.table(unigram,row.names = F,"1-gram.txt")

# generate word cloud
library(wordcloud)
with(unigram,wordcloud(word, n, max.words = 100, col = rainbow(n),random.color = T,random.order = F))

# % of words covering
a <- unigram%>%
  mutate(gp = ifelse(cum < 0.9, ifelse(cum<0.5, "50%", "90%"),"100%"))%>%
  group_by(gp)%>%
  summarise(n = cumsum(n())) %>%
  mutate(gp = ordered(gp, levels = c("50%","90%","100%"))) #%>%

#print(difftime(Sys.time(), start_time, units="secs")) 

## plot frequency
p1 <- unigram %>%
  head(20)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = n)) +
  geom_col() +
  scale_fill_gradient2(high = "firebrick1") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab(NULL) +
  ylab(NULL) +
  labs(fill = "Frequency", title = "Top 20 frequent Unigram") +
  coord_flip()

# 2-gram
bigrams <- clean_sample %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)

p2 <- bigrams %>%
  head(20)%>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = n)) +
  geom_col() +
  scale_fill_gradient2(high = "limegreen")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab(NULL) +
  ylab(NULL) +
  labs(fill = "Frequency", title = "Top 20 frequent 2-gram") +
  coord_flip()
bigrams_sep <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

write.table(bigrams_sep, row.names = F, "2-gram.txt")


## build 3-gram
trigrams <- clean_sample %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  count(trigram, sort = T)
p3 <- trigrams %>%
  head(20)%>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n, fill = n)) +
  geom_col() +
  scale_fill_gradient2(high = "dodgerblue")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab(NULL) +
  ylab(NULL) +
  labs(fill = "Frequency", title = "Top 20 frequent 3-gram") +
  coord_flip()

trigrams_sep <- trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  unite(word12, "word1", "word2", sep = " ")

write.table(trigrams_sep, row.names = F, "3-gram.txt")


## build 4-gram
quadgrams <- clean_sample %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
  count(quadgram, sort = T)
p4 <- quadgrams %>%
  head(20)%>%
  mutate(quadgram = reorder(quadgram, n)) %>%
  ggplot(aes(quadgram, n, fill = n)) +
  geom_col()+ 
  scale_fill_gradient2(high = "darkorchid")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab(NULL) +
  ylab(NULL) +
  labs(fill = "Frequency", title = "Top 20 frequent 4-gram") +
  coord_flip()

quadgrams_sep <- quadgrams %>%
  separate(quadgram, c("word1", "word2","word3","word4"), sep = " ") %>%
  unite(word123, "word1", "word2","word3", sep = " ")

write.table(quadgrams_sep, row.names = F, "4-gram.txt")

## show plots
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


## test models
kable(head(quadgrams_sep %>% subset(word123 == "thanks for the"),3))
kable(head(quadgrams_sep %>% subset(word123 == "the end of"),3))
kable(head(trigrams_sep %>% subset(word12 == "the end"),3))
kable(head(bigrams_sep %>% subset(word1 == "the"),3))

