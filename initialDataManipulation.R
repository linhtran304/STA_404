install.packages("https://github.com/ropensci/gutenbergr")
library(gutenbergr)
library(WikipediR)

### Bar Chart of subjects within classics bookshelf
# Used gutenberg package to get book info
books <- gutenberg_metadata
books2 <- gutenberg_subjects
authors <- gutenberg_authors

# Joined book info together and filtered by classics only
books <- books %>%
  filter(grepl(gutenberg_bookshelf, pattern="Harvard Classics")) %>%
  left_join(books2) %>%
  left_join(authors)

# Assign subject categories to the classics books
fiction <- books %>%
  filter(grepl(subject, pattern="Fiction", ignore.case=TRUE) |
           grepl(subject, pattern="stories", ignore.case=TRUE)) %>%
  mutate(subject_category = "Fiction") %>%
  select(gutenberg_id,
            title, author, 
            gutenberg_author_id, 
            language, rights,
            subject_category) %>%
  unique()

nonfiction <- books %>%
  filter(grepl(subject, pattern = "philosophy", ignore.case=TRUE) |
           grepl(subject, pattern = "Physiology", ignore.case=TRUE) |
           grepl(subject, pattern = "Political", ignore.case=TRUE) |
           grepl(subject, pattern = "biography", ignore.case=TRUE) |
           grepl(subject, pattern = "science", ignore.case=TRUE) |
           grepl(subject, pattern = "biology", ignore.case=TRUE)) %>%
  mutate(subject_category = "NonFiction") %>%
  select(gutenberg_id,
            title, author, 
            gutenberg_author_id, 
            language, rights,
            subject_category) %>%
  unique()

drama <- books %>%
  filter(grepl(subject, pattern = "drama", ignore.case=TRUE) |
         grepl(subject, pattern = "traged", ignore.case=TRUE) | 
         grepl(subject, pattern = "Comedies", ignore.case=TRUE) ) %>%
  mutate(subject_category = "Greek Drama") %>%
  select(gutenberg_id,
            title, author, 
            gutenberg_author_id, 
            language, rights,
            subject_category) %>%
  unique()

poetry <- books %>%
  filter(grepl(subject, pattern = "poetry", ignore.case=TRUE)) %>%
  mutate(subject_category = "Poetry") %>%
  select(gutenberg_id,
            title, author, 
            gutenberg_author_id, 
            language, rights,
            subject_category) %>%
  unique()

essays <- books %>%
  filter(grepl(subject, pattern = "essay", ignore.case=TRUE) |
           grepl(subject, pattern = "Freedom", ignore.case=TRUE))  %>%
  mutate(subject_category = "Essays") %>%
  select(gutenberg_id,
            title, author, 
            gutenberg_author_id, 
            language, rights,
            subject_category) %>%
  unique()

religious <- books %>%
  filter(!grepl(subject, pattern = "fiction", ignore.case=TRUE) &
           grepl(subject, pattern = "bible", ignore.case=TRUE) |
           grepl(subject, pattern = "church", ignore.case=TRUE) |
           grepl(subject, pattern = "Christian", ignore.case=TRUE) |
           grepl(subject, pattern = "Reformation", ignore.case=TRUE) |
           grepl(subject, pattern= "Qur'an", ignore.case=TRUE)) %>%
  mutate(subject_category = "Religious") %>%
  select(gutenberg_id,
            title, author, 
            gutenberg_author_id, 
            language, rights,
            subject_category) %>%
  unique()

fairy_tales <- books %>%
  filter(grepl(subject, pattern = "fairy", ignore.case=TRUE) |
           grepl(subject, pattern = "fables", ignore.case=TRUE)) %>%
  mutate(subject_category = "Fables") %>%
  select(gutenberg_id,
            title, author, 
            gutenberg_author_id, 
            language, rights,
            subject_category) %>%
  unique()

history <- books %>%
  filter(grepl(subject, pattern = "history", ignore.case=TRUE)) %>%
  mutate(subject_category = "History") %>%
  select(gutenberg_id,
            title, author, 
            gutenberg_author_id, 
            language, rights,
            subject_category) %>%
  unique()

# Bind all subjects together
classicsList <- fiction %>% rbind(nonfiction) %>%
  rbind(drama) %>% rbind(essays) %>%
  rbind(fairy_tales) %>% rbind(history) %>%
  rbind(poetry) %>% rbind(religious)

# Plots classics list subjects as bar chart 
ggplot(classicsList) + 
  geom_bar(aes(x=subject_category)) + 
  labs(title="Harvard Classic Novels Classified by Genre\n",
       y="", x="Genre Category") + 
  theme_classic() + 
  scale_y_continuous(expand = c(0,0))


### Scatterplot of years vs classics books (each book is a point on scatterplot)
# Web scrape Wikipedia to get years of publication/written 
# Maybe even have sized dots based on how popular/how many downloads?

# Obtain author wiki link for classic novels
author_pages <- books %>% left_join(authors) %>%
  select(gutenberg_id, wikipedia) %>%
  unique() %>% drop_na()

### Box and Whisker Plot to show distribution of downloads for books within a genre
# Takes the bar chart data and joins with data about download numbers to show 
# distributionof how popular genres are 





