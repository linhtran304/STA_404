############# Swap here
load(url('https://github.com/linhtran304/STA_404/raw/main/just_classics/subset_classics.RData'))
if(require(pacman)==F) install.packages('pacman')
pacman::p_load(tidyverse)
data = subset_classics |> 
  mutate(title = ifelse(nchar(title) > 70, 
                             paste(substr(title, 0, 70), '...'), title ))
#############


# Bookshelf ---------------------------------------------------------------

bookshelf_genres = data |> 
  group_by(gutenberg_bookshelf, genre) |> 
  summarise(total_downloads = sum(downloads_30_days))

bookshelf_authors = data |> 
  group_by(gutenberg_bookshelf, gutenberg_author_id, author) |> 
  summarise(total_downloads = sum(downloads_30_days))



# Genres -------------------------------------------------------------------

genres_authors = data |> 
  group_by(genre, gutenberg_author_id, author) |> 
  summarise(total_downloads = sum(downloads_30_days))


# Author ----------------------------------------------------------------

author_summary = data |> 
  group_by(gutenberg_author_id, author) |> 
  summarize(total_genres = length(unique(genre)),
            total_books = length(unique(gutenberg_id)),
            total_downloads = sum(downloads_30_days))

author_summary_by_genres = data |> 
  group_by(gutenberg_author_id, author, genre) |> 
  summarize(total_pub = n())

###################################
save(data, author_summary, author_summary_by_genres, 
     bookshelf_genres, bookshelf_authors,
     genres_authors,
     file = 'processed_classics.RData')
