library(tidyverse)
library(rvest)
library(stringr)

########
setwd("C:/Users/inetc/OneDrive/Documents/R/Job listings")
getwd()


### get search page links
links <- paste0("https://www.reed.co.uk/jobs/data-scientist-jobs?pageno=",seq(1:32)) %>% as.data.frame(stringsAsFactors=FALSE)
#links

listings <- tibble()

### crawl through search pages to scrape job listing URLs
for (i in seq(nrow(links))) {
  
       listing_url <- read_html( links$.[i]) %>% html_nodes('.title a') %>% html_attr("href") %>% as.data.frame(stringsAsFactors=FALSE)

      listings <- rbind(listings,listing_url)

      Sys.sleep(3)

}

#listings



### join links

job_links <- paste0("https://www.reed.co.uk",listings$.) %>% as.data.frame(stringsAsFactors=FALSE)
#job_links 


## create empty frames
job_listings <- tibble()
job <- tibble()

## loop through listings and scrape info
for (i in seq(nrow(job_links))) {
  
  job_name <- read_html(job_links$.[i]) %>% 
    html_nodes("h1") %>% html_text() %>% data.frame(stringsAsFactors=FALSE)
  
  salary <- read_html(job_links$.[i]) %>%  html_nodes(".salary") %>%
    html_text() %>% data.frame(stringsAsFactors=FALSE) %>% .[1,] %>% str_trim() %>% data.frame(stringsAsFactors = FALSE)
  
  location <- read_html(job_links$.[i]) %>%  html_nodes(".location") %>%
    html_text() %>% data.frame(stringsAsFactors=FALSE) %>% .[1,] %>% str_trim() %>% data.frame(stringsAsFactors = FALSE)
  
  time <- read_html(job_links$.[i]) %>%  html_nodes(".time") %>%
    html_text() %>% data.frame(stringsAsFactors=FALSE) %>% .[1,] %>% str_trim() %>% data.frame(stringsAsFactors = FALSE)
  
  description <- read_html(job_links$.[i]) %>%  html_nodes(".description p") %>%
    html_text() %>% as.vector() %>% toString() %>% data.frame(stringsAsFactors = FALSE)
  
  description_bullets <- read_html(job_links$.[i]) %>% html_nodes(".description li") %>%
    html_text() %>% as.vector() %>% toString() %>% data.frame(stringsAsFactors=FALSE)
  
  company_posted <- read_html(job_links$.[i]) %>%  html_nodes(".posted") %>%
    html_text() %>% data.frame(stringsAsFactors=FALSE) %>% .[1,] 
  company_posted <- gsub(".*by", "", company_posted) %>% str_remove("Easy Apply") %>% str_remove("Featured") %>% str_trim() %>% as.data.frame(stringsAsFactors = FALSE) 
  
  ##bind scraped data
  job <- cbind(job_name,salary,location,time,description,
             description_bullets,company_posted) %>% as.data.frame(stringsAsFactors=FALSE)

  ## row bind with previously scrapped data
  job_listings <- rbind(job_listings,job)
  
  Sys.sleep(3)
  
}

#View output
#job_listings

## change column names
job_listings <- setNames(job_listings,c("job_title","salary","location","employment type","description",
                      "description_bulletpoints","posting_company"))

## export file
write_excel_csv(job_listings,"job listing.csv")







###x <- cbind(job_name,salary,location,time,description,
###               description_bullets,company_posted)
###
###x <- list(job_name,salary,location,time,description,
###     description_bullets,company_posted) %>% reduce(full_join)
###
###
###
###
###
###job_name <- read_html("https://www.reed.co.uk/jobs/data-scientist/39695449?source=searchResults#/jobs/data-scientist-jobs") %>% 
###  html_nodes("h1") %>% html_text() %>% data.frame(stringsAsFactors=FALSE)
###salary <- read_html("https://www.reed.co.uk/jobs/data-scientist/39695449?source=searchResults#/jobs/data-scientist-jobs") %>%  html_nodes(".salary") %>%
###  html_text() %>% data.frame(stringsAsFactors=FALSE) %>% .[1,] %>% str_trim() %>% data.frame(stringsAsFactors = FALSE)
###location <- read_html("https://www.reed.co.uk/jobs/data-scientist/39695449?source=searchResults#/jobs/data-scientist-jobs") %>%  html_nodes(".location") %>%
###  html_text() %>% data.frame(stringsAsFactors=FALSE) %>% .[1,] %>% str_trim() %>% data.frame(stringsAsFactors = FALSE)
###time <- read_html("https://www.reed.co.uk/jobs/data-scientist/39695449?source=searchResults#/jobs/data-scientist-jobs") %>%  html_nodes(".time") %>%
###  html_text() %>% data.frame(stringsAsFactors=FALSE) %>% .[1,] %>% str_trim() %>% data.frame(stringsAsFactors = FALSE)
###description <- read_html("https://www.reed.co.uk/jobs/data-scientist/39845886?source=searchResults#/jobs/data-scientist-jobs") %>%  html_nodes(".description p") %>%
###  html_text() %>% as.vector() %>% toString() %>% data.frame(stringsAsFactors = FALSE)
###description_bullets <- read_html("https://www.reed.co.uk/jobs/data-scientist/39695449?source=searchResults#/jobs/data-scientist-jobs") %>%  html_nodes(".description li") %>%
###  html_text() %>% as.vector() %>% toString() %>% data.frame(stringsAsFactors = FALSE)
###company_posted <- read_html("https://www.reed.co.uk/jobs/data-scientist/39695449?source=searchResults#/jobs/data-scientist-jobs") %>%  html_nodes(".posted") %>%
###  html_text() %>% data.frame(stringsAsFactors=FALSE) %>% .[1,] 
###company_posted <- gsub(".*by", "", company_posted) %>% str_remove("Easy Apply") %>% str_trim() %>% as.data.frame(stringsAsFactors = FALSE) 
###
###job <- cbind(job_name,salary,location,time,description,
###             description_bullets,company_posted) %>% as.data.frame(stringsAsFactors=FALSE)
###job <- setNames(job,c("job_title","salary","location","employment type","description",
###                      "description_bulletpoints","posting_company"))
###
###job_listings[i] <- job
###
