
# 1. FUNCTIONS DOWNLOAD ----

# number of pages to scrap ----
number_page_fun <- function(path_param){
  html_nodes("https://www.vice.com/es_latam/topic/la-columna-rota" %>% 
               read_html(),
             xpath = path_param) %>% 
    html_text() %>% 
    parse_number()
}

# urls per page ----
hrefs_page_fun <- function(num){
  url_page <- paste0("https://www.vice.com/es_latam/topic/la-columna-rota?page=", num) 
  href <- tbl_hrefs_pg <- 
    html_nodes(url_page %>% read_html(), 
             xpath = "//*[@id = 'V1C3']//
             a[@class='grid__wrapper__card grd-col col-12-xs col-6-m col-3-hd dsp-block-xs p-t-3-xs col-4-xl']/@href") %>% 
    html_text() 
  tibble(href) 
}

# text, header and date per url ----
text_url_fun <- function(href_url){
  html_page <- paste0("https://www.vice.com", href_url) %>% 
    read_html() 
  
  text <- html_page %>% 
    html_nodes(xpath = "//*[@id='V1C3']//div[@data-type='body-text']//p") %>% 
    html_text(trim = T) %>% 
    reduce(paste) %>% 
    as.character()
  
  date_txt <- html_page %>% 
    html_nodes(xpath = "//*[@id='V1C3']//div[@data-type='article-wrapper']//
               div[@class='article__contributions__publishdate']") %>% 
    html_text(trim = T) %>% 
    first()
  
  header <- html_page %>% 
    html_nodes(xpath = "//*[@id='V1C3']//div[@data-type='article-wrapper']//h1") %>%
    html_text(trim = T)
  
  tibble(header, date_txt, text)
}

# scraping data ----
scraping_data_fun <- function(run_scraping = T){
  
  if(run_scraping){
    message("...SCRAPING for new data...")
    
    # number of pages
    pages <- map_dbl(c("//*[@class='paginator__progress--current-page p-b-1--xs']", 
                       "//*[@class='paginator__progress--last-page p-t-1--xs']"),
                     number_page_fun)
    
    # hrefs comparison
    qry <- "SELECT DISTINCT href FROM `fridaguerrera.columnarota.TextByArticle`"
    hrefs_bgqry <- DBI::dbGetQuery(conn = DBI::dbConnect(bigquery(), 
                                                         project = "fridaguerrera"), 
                                   statement = qry) 
    
    tbl_hrefs <- map_df(first(pages):last(pages), hrefs_page_fun) %>% 
      unique()
    
    # download new data
    diff_length <- setdiff(tbl_hrefs$href, hrefs_bgqry$href) %>% length()
    
    if(diff_length > 0){
      
      message(paste("...", diff_length,"new articles..."))
      
      tbl_data <- tbl_hrefs %>% 
        anti_join(hrefs_bgqry, by = "href") %>%
        mutate(text = map(href, text_url_fun)) %>% 
        unnest()  
      
      if(nrow(tbl_data)> 0){
        
        message("...uploading new data...")
        bqr_auth()
        bqr_upload_data(projectId = "fridaguerrera" ,
                        datasetId = "columnarota", 
                        tableId = "TextByArticle", 
                        upload_data = tbl_data,
                        create = c("CREATE_IF_NEEDED"), 
                        overwrite = FALSE,
                        schema = schema_fields(tbl_data))
      }
      
    }else{
      message("...no new articles...")
    }
  }
  
  "Done"
}

# download data ----
download_data_fun <- function(status, run_download = T){
  # downloading data
  if(run_download){
    message("...DOWNLOADING text...")
    qry <- "SELECT * FROM `fridaguerrera.columnarota.TextByArticle`"
    tbl_download <- DBI::dbGetQuery(conn = DBI::dbConnect(bigquery(), 
                                                         project = "fridaguerrera"), 
                                   statement = qry) 
    return(tbl_download)
  }else{
    return(NULL)
  }
  
}





# clean data ----
time_convertion_fun <- function(col_time){
  # col_time <- tbl$time
  col_hm <- hm(col_time)
  col_hm[str_detect(col_time, "pm")] <- col_hm[str_detect(col_time, "pm")] + hours(12)
  return(col_hm)
}

clean_data_fun <- function(tbl_data){
  message("...CLEANING data...")
  tbl <- 
    tbl_data %>% 
    mutate_at(c("header", "date_txt", "text"), tolower) %>% 
    separate(date_txt, c('day', 'month', 'year', 'time'), 
             sep = " ", remove = T) %>% 
    mutate(publication_ts = paste(parse_number(day),
                                  factor(month, c("enero", "febrero", 'marzo', 
                                                  'abril', 'mayo', 'junio', 
                                                  'julio', 'agosto', 'septiembre', 
                                                  'octubre', 'noviembre', 'diciembre')) %>% 
                                    as.numeric(),
                                  parse_number(year), 
                                  time_convertion_fun(time)) %>% 
             dmy_hms) %>% 
    dplyr::select(-day, -month, -year, -time) %>% 
    mutate_at(c("text", "header"), function(col){
      col %>% 
        str_replace_all('"', "") %>% 
        str_replace_all('“', "") %>% 
        str_replace_all('”', "") 
    }) 
  
  return(tbl)
}
