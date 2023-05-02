## Download Text

# Load required packages --------------------------------------------------

if (require(pacman) == F) install.packages("pacman")
pacman::p_load(gutenbergr, tidyverse)

meta_data = gutenberg_metadata
book_w_text_id = meta_data[which(meta_data$has_text),1]

# Split book ID -----------------------------------------------------------

allie = book_w_text_id$gutenberg_id[17001:34000]
jenn = book_w_text_id$gutenberg_id[34001:51000]
anh = book_w_text_id$gutenberg_id[51001:length(book_w_text_id$gutenberg_id)]


# Code --------------------------------------------------------------------
## This is an example
## Just plug in your assigned book ids
## feel free to break the batch down to smaller size (17k at a time is a lot)

allie_batch = gutenberg_download(allie) %>% filter(text != "") ## this is to remove empty lines
save(allie_batch, file="allie_batch.RData")
