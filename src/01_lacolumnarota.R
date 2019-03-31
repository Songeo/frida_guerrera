
### La Columna Rota 

library(ProjectTemplate)
reload.project()


# Download data ----
tbl_data <- 
  scraping_data_fun() %>% 
  download_data_fun()

# unnesting and stopwords
text_tidy <- tbl_data %>%
  unnest_tokens(word, text) %>% 
  anti_join(stopwords_df, 
            by = "word")
text_tidy

# wordcloud raw
text_tidy %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 40))


# count 
text_count_words <- text_tidy %>% 
  count(links, word)
text_count_words

# total words
total_words <- text_count_words %>% 
  group_by(links) %>% 
  summarise(total = sum(n)) %>% 
  ungroup()
total_words

# rank
text_freqrank <- text_count_words %>% 
  left_join(total_words, 
            by = "links") %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)
text_freqrank

ggplot(text_freqrank, aes(`term frequency`, 
                          fill = links)) +
  geom_histogram(show.legend = FALSE, bins = 30) +
  facet_wrap(~links, scales = "free_y")

# tf-idf
text_tfidf <- text_count_words %>%
  bind_tf_idf(word, links, n) %>% 
  ungroup 
text_tfidf

tab <- text_tfidf %>%
  filter(links != "https://www.vice.com/es_mx/article/mbk7bn/frida-guerrera-desayunar-escribir-y-sonar-con-feminicidios") %>% 
  arrange(desc(tf_idf)) %>% #.$links %>% head
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(links) %>% 
  top_n(15) %>% 
  ungroup
tab

tab %>%
  ggplot(aes(word, tf_idf, fill = links)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~links, scales = "free") +
  coord_flip()

quartz()
lapply(unique(tab$links), function(lk){
  wc <- tab %>%
    filter(links == lk) %>% 
    with(wordcloud(word, n, max.words = 100))  
  print(wc)
  "fin"
})

