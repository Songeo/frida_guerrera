


### La Columna Rota 

library(ProjectTemplate)
load.project()

library(tidytext)
library(wordcloud)


# function extracting
extract_text_fun <- function(link_vec){
  url <- paste0("https://www.vice.com", link_vec)
  html_url <- read_html(url)
  results <- html_nodes(html_url, xpath = "//*[(@id = 'V1C3')]")
  
  text <- html_text(results)[[1]] %>% 
    as.character() %>% 
    gsub(pattern = "@FridaGuerrera.*", "", .) %>% 
    gsub(pattern = ".*La columna rota", "", .) %>% 
    gsub(pattern = "FridaGuerrera", " FridaGuerrera", .) %>%
    gsub(pattern = " Villalvazo", " Villalvazo ", .) %>%
    gsub(pattern = "pm", " pm ", .) %>%
    gsub(pattern = "”|“", "", .) %>% 
    str_trim()
  
  tibble(links = url, 
         text = text)
}

pages_url_text_fun <- function(url_page){
  web_page <- read_html(url_page)
  links <- html_attr(html_nodes(web_page, "a"), "href")
  
  tbl_links <- as_tibble(links) %>% 
    filter(str_detect(value, "es_mx/article"))
  tbl_links
  
  # raw text from webpages
  text_tidy <- tbl_links %>%
    split(.$value) %>%
    map_df(extract_text_fun) 
  text_tidy  
}

# Extract links

pages_links <- c("https://www.vice.com/es_mx/topic/la-columna-rota?page=1",
                 "https://www.vice.com/es_mx/topic/la-columna-rota?page=2",
                 "https://www.vice.com/es_mx/topic/la-columna-rota?page=3",
                 "https://www.vice.com/es_mx/topic/la-columna-rota?page=4")


text_raw <- map(pages_links, pages_url_text_fun) %>% 
  bind_rows()
text_raw

# unnesting and stopwords
text_tidy <- text_raw %>%
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

