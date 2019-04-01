

stpwds_nonexists <- (list.files("cache/", pattern = "stop_words_tbl") %>% 
                       length()) == 0

if(stpwds_nonexists){
  # https://github.com/stopwords-iso/stopwords-es
  sw <- read_table("https://raw.githubusercontent.com/stopwords-iso/stopwords-es/master/stopwords-es.txt", 
             col_names = c("word"))
  
  stop_words_tbl <- 
    read_delim("docs/stopwords_countswords.txt", 
             delim = "\t", 
             col_names = c("row_num", "word")) %>% 
    dplyr::select(-row_num) %>% 
    bind_rows(sw) %>% 
    unique()
  
  rm("sw")
  save(stop_words_tbl, file = "cache/stop_words_tbl.RData")
}
