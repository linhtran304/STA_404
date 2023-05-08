############# Swap here
load(url('https://github.com/linhtran304/STA_404/raw/main/fullDashboard/all_data_w_sentiment.RData'))

if(require(pacman)==F) install.packages('pacman')
pacman::p_load(tidyverse)
data = all_data_w_sentiment |> 
  mutate(title = ifelse(nchar(title) > 30, 
                             paste(substr(title, 0, 30), '...'), title),
         genre = ifelse(nchar(genre) > 30, 
                        paste0(substr(genre, 0, 30), '...'), genre),
         author = ifelse(grepl(",", author), 
                          str_c(str_split(author, ", ", simplify = TRUE)[,2],
                                str_split(author, ", ", simplify = TRUE)[,1],
                                sep = " "), author),
         gutenberg_bookshelf = str_split(gutenberg_bookshelf, "/"),
         language = str_to_upper(language)) |>
  unnest(gutenberg_bookshelf)

#############


# Bookshelf ---------------------------------------------------------------

bookshelf_genres = data |> 
  group_by(gutenberg_bookshelf, genre) |> 
  summarise(total_downloads = sum(downloads_30_days))

bookshelf_authors = data |> 
  group_by(gutenberg_bookshelf, gutenberg_author_id, author) |> 
  summarise(total_downloads = sum(downloads_30_days))


# Author ----------------------------------------------------------------

author_summary = data |> 
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
