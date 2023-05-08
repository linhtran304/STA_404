## DATA PREPARATION

# Load required packages --------------------------------------------------

if (require(pacman) == F) install.packages("pacman")
pacman::p_load(rvest, gutenbergr, tidyverse, tidytext, textdata, purrr, magrittr, furrr)

## load custom function
source('custom_function.R')

# Import gutenberg data ---------------------------------------------------

meta_data = gutenberg_metadata
meta_subjects = gutenberg_subjects
authors = gutenberg_authors

# Web Scraping ------------------------------------------------------------

## Split book id -----------------------------------------------------------

sub1 = meta_data$gutenberg_id[1:10000]
sub2 = meta_data$gutenberg_id[10001:20000]
sub3 = meta_data$gutenberg_id[20001:30000]
sub4 = meta_data$gutenberg_id[30001:40000]
sub5 = meta_data$gutenberg_id[40001:50000]
sub6 = meta_data$gutenberg_id[50001:60000]
sub7 = meta_data$gutenberg_id[60001:length(meta_data$gutenberg_id)]

### Batch 1 -----------------------------------------------------------------
batch1 = tibble()

for (i in sub1) {
  book = paste0("https://gutenberg.org/ebooks/", i) |> 
    read_html() |> 
    html_elements("table") |> 
    html_table()
  
  ## Extract Published Date
  date = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "(?<!.)([A-Z][a-z]{2} [0-9]{1,2}, [0-9]{4})(?!.)"))]
  
  ## Extract downloads
  downloads = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "[0-9]+? downloads"))]
  
  temp = tibble(gutenberg_id = i, date_published = date, downloads = downloads)
  batch1 = rbind(batch1, temp)
}

save(batch1, file='group_data/batch1.RData')


### Batch 2 -----------------------------------------------------------------
batch2 = tibble()

for (i in sub2) {
  book = paste0("https://gutenberg.org/ebooks/", i) |> 
    read_html() |> 
    html_elements("table") |> 
    html_table()
  
  ## Extract Published Date
  date = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "(?<!.)([A-Z][a-z]{2} [0-9]{1,2}, [0-9]{4})(?!.)"))]
  
  ## Extract downloads
  downloads = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "[0-9]+? downloads"))]
  
  temp = tibble(gutenberg_id = i, date_published = date, downloads = downloads)
  batch2 = rbind(batch2, temp)
}

save(batch2, file='group_data/batch2.RData')

### Batch 3 -----------------------------------------------------------------
batch3 = tibble()

for (i in sub3) {
  book = paste0("https://gutenberg.org/ebooks/", i) |> 
    read_html() |> 
    html_elements("table") |> 
    html_table()
  
  ## Extract Published Date
  date = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "(?<!.)([A-Z][a-z]{2} [0-9]{1,2}, [0-9]{4})(?!.)"))]
  
  ## Extract downloads
  downloads = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "[0-9]+? downloads"))]
  
  temp = tibble(gutenberg_id = i, date_published = date, downloads = downloads)
  batch3 = rbind(batch3, temp)
}

save(batch3, file='group_data/batch3.RData')

### Batch 4 -----------------------------------------------------------------
batch4 = tibble()

for (i in sub4) {
  book = paste0("https://gutenberg.org/ebooks/", i) |> 
    read_html() |> 
    html_elements("table") |> 
    html_table()
  
  ## Extract Published Date
  date = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "(?<!.)([A-Z][a-z]{2} [0-9]{1,2}, [0-9]{4})(?!.)"))]
  
  ## Extract downloads
  downloads = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "[0-9]+? downloads"))]
  
  temp = tibble(gutenberg_id = i, date_published = date, downloads = downloads)
  batch4 = rbind(batch4, temp)
}

save(batch4, file='group_data/batch4.RData')

### Batch 5 -----------------------------------------------------------------
batch5 = tibble()

for (i in sub5) {
  book = paste0("https://gutenberg.org/ebooks/", i) |> 
    read_html() |> 
    html_elements("table") |> 
    html_table()
  
  ## Extract Published Date
  date = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "(?<!.)([A-Z][a-z]{2} [0-9]{1,2}, [0-9]{4})(?!.)"))]
  
  ## Extract downloads
  downloads = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "[0-9]+? downloads"))]
  
  temp = tibble(gutenberg_id = i, date_published = date, downloads = downloads)
  batch5 = rbind(batch5, temp)
}

save(batch5, file='group_data/batch5.RData')

### Batch 6 -----------------------------------------------------------------
batch6 = tibble()

for (i in sub6[6979:10000]) {
  book = paste0("https://gutenberg.org/ebooks/", i) |> 
    read_html() |> 
    html_elements("table") |> 
    html_table()
  
  ## Extract Published Date
  date = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "(?<!.)([A-Z][a-z]{2} [0-9]{1,2}, [0-9]{4})(?!.)"))]
  
  ## Extract downloads
  downloads = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "[0-9]+? downloads"))]
  
  temp = tibble(gutenberg_id = i, date_published = date, downloads = downloads)
  batch6= rbind(batch6, temp)
}

save(batch6, file='group_data/batch6.RData')


### Batch 7 -----------------------------------------------------------------
batch7 = tibble()

for (i in sub7) {
  book = paste0("https://gutenberg.org/ebooks/", i) |> 
    read_html() |> 
    html_elements("table") |> 
    html_table()
  
  ## Extract Published Date
  date = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "(?<!.)([A-Z][a-z]{2} [0-9]{1,2}, [0-9]{4})(?!.)"))]
  
  ## Extract downloads
  downloads = book[[2]][["X2"]][which(str_detect(book[[2]][["X2"]], pattern = "[0-9]+? downloads"))]
  
  temp = tibble(gutenberg_id = i, date_published = date, downloads = downloads)
  batch7 = rbind(batch7, temp)
}

save(batch7, file='group_data/batch7.RData')



## Merging all data --------------------------------------------------------

load('group_data/batch1.RData')
load('group_data/batch2.RData')
load('group_data/batch3.RData')
load('group_data/batch4.RData')
load('group_data/batch5.RData')
load('group_data/batch6.RData')

all_scrapped = bind_rows(batch1, 
                         batch2,
                         batch3,
                         batch4,
                         batch5,
                         batch6,
                         batch7)

all_scrapped = all_scrapped |> 
  mutate(downloads_30_days = str_sub(downloads, end=-31) |> as.numeric(),
         date_published = lubridate::mdy(date_published),
         .keep = 'unused')

save(all_scrapped, file='group_data/all_scrapped.RData')

## Getting the first 30k books to use first --------------------------------

load('group_data/batch1.RData')
load('group_data/batch2.RData')
load('group_data/batch3.RData')

first_30k_id = bind_rows(batch1_all, batch2, batch3)

first_30k_id = first_30k_id |> 
  mutate(downloads = str_sub(downloads, end=-31) |> as.numeric(),
         date_published = lubridate::mdy(date_published))

save(first_30k_id, file='group_data/first_30k_id.RData')



# Data Processing ----------------------------------------------------------

## Merge Scrapped Data
load('group_data/all_scrapped.RData')

all_data = inner_join(meta_data, all_scrapped, by="gutenberg_id")

save(all_data, file='group_data/all_data.RData')

## Merge subjects
load('group_data/all_data.RData')

subjects = meta_subjects |> 
  filter(subject_type == "lcsh") |>  ## only consider the lcsh type because it's more meaningful
  group_by(gutenberg_id) |> 
  mutate(subject_merged = paste0(subject, collapse = ","), .keep='unused') |> 
  distinct() |> 
  select(-c(subject_type))
  
all_data = inner_join(all_data, subjects, by="gutenberg_id")

## Create Genres
## using our custom function

all_data = all_data |> 
  mutate(genre = get_genres(subject_merged), .keep="unused")

save(all_data, file="group_data/all_data.RData")


## Text Downloading -----------------------------------------------------

book_w_text_id = meta_data[which(meta_data$has_text),1]

### Split book ID -----------------------------------------------------------

linh = book_w_text_id$gutenberg_id[1:17000]
allie = book_w_text_id$gutenberg_id[17001:34000]
jenn = book_w_text_id$gutenberg_id[34001:51000]
anh = book_w_text_id$gutenberg_id[51001:length(book_w_text_id$gutenberg_id)]


### Code --------------------------------------------------------------------
## This is an example
## We queried in smaller batches

allie_batch = gutenberg_download(allie) %>% filter(text != "") ## this is to remove empty lines
save(allie_batch, file="allie_batch.RData")


## Text Mining ---------------------------------------------------------------
## this is to show you the general code
## in actuality, we run this code on our assigned downloaded batches 
## (the variable names here are only representative for what we did)
 
load('text.RData') ## say we load the downloaded text

#pacman::p_load(tidyverse, tidytext, textdata, furrr, magrittr)

plan(multisession, workers = 2)

bing = text |> 
  group_by(gutenberg_id) |> 
  mutate(linenumber = row_number(),
         index = linenumber %/% if_else((max(linenumber)%/%100) > 0, max(linenumber)%/%100, 1)) |> 
  ungroup() |> 
  unnest_tokens(word, text) |> 
  mutate(word = str_extract(word, "[a-z']+")) |>  
  anti_join(stop_words, by="word") |> 
  inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") |> 
  group_by(gutenberg_id) |> 
  count(gutenberg_id, index, sentiment) |> 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(bing_overall = positive - negative) |>
  rename(bing_positive = positive, bing_negative = negative) |> 
  nest() |> 
  ungroup() |> 
  mutate(
    index = future_map(.x=data, .f=extract2, 'index'),
    bing_positive = future_map(.x=data, .f=extract2, 'bing_positive'),
    bing_negative = future_map(.x=data, .f=extract2, 'bing_negative'),
    bing_overall = future_map(.x=data, .f=extract2, 'bing_overall'),
    net_bing = future_map_dbl(.x=bing_overall, .f=sum)
  ) |> 
  select(-c(data))

save(bing, file='bing.RData')

## We then append all the sentiment output together
load(url("https://github.com/linhtran304/STA_404/raw/main/sentiment_data/allie_data_final.RData"))
load(url("https://github.com/linhtran304/STA_404/raw/main/sentiment_data/jenn_all_sentiment.RData"))
load(url("https://github.com/linhtran304/STA_404/raw/main/sentiment_data/linh_bing_all.RData"))
load(url("https://github.com/linhtran304/STA_404/raw/main/sentiment_data/merged_anh.RData"))

sentiment_data = bind_rows(allie_data,
                           bing_all,
                           jenn_all_sentiment,
                           merged_anh)

## the result can be downloaded from here
load(url("https://github.com/linhtran304/STA_404/raw/main/sentiment_data.RData"))

## and then merge the sentiment with the rest of the data

all_data_w_sentiment = left_join(all_data, sentiment_data, by="gutenberg_id")

## this is our final dataset to use for the dashboard
load(url("https://github.com/linhtran304/STA_404/raw/main/all_data_w_sentiment.RData"))



# Process Data for Dashboard ----------------------------------------------

data = all_data_w_sentiment |> 
  mutate(title = ifelse(nchar(title) > 30, 
                        paste(substr(title, 0, 30), '...'), title),
         genre = ifelse(nchar(title) > 30, 
                        paste0(substr(title, 0, 30), '...'), genre),
         author = ifelse(grepl(",", author), 
                         str_c(str_split(author, ", ", simplify = TRUE)[,2],
                               str_split(author, ", ", simplify = TRUE)[,1],
                               sep = " "), author),
         gutenberg_bookshelf = str_split(gutenberg_bookshelf, "/"),
         language = str_to_upper(language)) |>
  unnest(gutenberg_bookshelf)

bookshelf_genres = data |> 
  group_by(gutenberg_bookshelf, genre) |> 
  summarise(total_downloads = sum(downloads_30_days))

bookshelf_authors = data |> 
  group_by(gutenberg_bookshelf, gutenberg_author_id, author) |> 
  summarise(total_downloads = sum(downloads_30_days))author_summary = data |> 
  select(gutenberg_author_id, author, genre, gutenberg_id, downloads_30_days) |> 
  distinct() |> 
  group_by(gutenberg_author_id, author) |>
  summarize(total_genres = length(unique(genre)),
            total_books = length(unique(gutenberg_id)),
            total_downloads = sum(downloads_30_days))

author_summary_by_genres = data |> 
  select(gutenberg_id, gutenberg_author_id, author, genre) |> 
  distinct() |> 
  group_by(gutenberg_author_id, author, genre) |> 
  summarize(total_pub = n())

###################################
save(data, author_summary, author_summary_by_genres, 
     bookshelf_genres, bookshelf_authors,
     file = 'processed_allData_final.RData')
