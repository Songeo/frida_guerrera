
# 1. FUNCTIONS DOWNLOAD ----

# number of pages downloading ----
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
  
  date <- html_page %>% 
    html_nodes(xpath = "//*[@id='V1C3']//div[@data-type='article-wrapper']//
               div[@class='article__contributions__publishdate']") %>% 
    html_text(trim = T) %>% 
    first()
  
  header <- html_page %>% 
    html_nodes(xpath = "//*[@id='V1C3']//div[@data-type='article-wrapper']//h1") %>%
    html_text(trim = T)
  
  tibble(header, date, text)
}

# scrapping data ----
scrapping_data_fun <- function(run_dowload = T){
  
  if(run_dowload){
    message("...scrapping data...")
    
    # number of pages
    pages <- map_dbl(c("//*[@class='paginator__progress--current-page p-b-1-xs']", 
                       "//*[@class='paginator__progress--last-page p-t-1-xs']"),
                     number_page_fun)
    
    # hrefs comparison
    qry <- "SELECT DISTINCT href FROM `fridaguerrera.columnarota.TextByArticle`"
    hrefs_bgqry <- DBI::dbGetQuery(conn = DBI::dbConnect(bigquery(), 
                                                         project = "fridaguerrera"), 
                                   statement = qry) 
    
    tbl_hrefs <- map_df(first(pages):last(pages), hrefs_page_fun) %>% 
      unique()
    
    # download new data
    if((setdiff(tbl_hrefs$href, hrefs_bgqry$href) %>% length()) >0){
      tbl_data <-tbl_hrefs %>% 
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
      message("...no new data...")
    }
  }
  
  "Done"
}

# download data ----
# scrapping_data_fun <- function(run_dowload = T)