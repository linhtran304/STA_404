#title: "Draft of Data Manipulation/Visualizations"
#author: "Group E (Eggplant)"
#date: "2023-04-28"

library(tidyverse)
library(gutenbergr)
library(ggplot2)
if (require(pacman) == F) install.packages("pacman")
pacman::p_load(tidyverse, gutenbergr, tidytext)

############### Data Manipulation ##################
## Initial Code to generate necessary plots <- Thank you Allie!
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


############### Data Visualizations ##################
##### Plots classics list subjects as bar chart 
ggplot(classicsList) + 
  geom_bar(aes(x=subject_category)) + 
  labs(title="Harvard Classic Novels Classified by Subject\n",
       y="", x="Subject Category",
       caption="Data collected from the Gutenberg Project") + 
  theme_classic() + 
  scale_y_continuous(expand = c(0,0))


### Plots information about language of classics
lang_list <- classicsList %>%
  select(language, subject_category) %>%
  summarize(language, name = "Total")

ggplot(lang_list) +
  geom_bar(aes(x= language)) +
  theme_classic() + 
  labs(title="Harvard Classic Novels Classified by Available Languages",
       x="Language", y="",
       caption="Data collected from the Gutenberg Project") + 
  scale_y_continuous(expand = c(0,0))


##### Plots author age distribution 
### (all authors listed in the gutenberg package - not just authors of classic texts)
authors <- authors %>% 
  mutate(age=deathdate-birthdate) %>% 
  drop_na(age)

authors <- authors %>% 
  mutate(age_group=ifelse(between(age, 10, 30), "Age 10-30",
                   ifelse(between(age, 30, 50), "Age 30-50",
                   ifelse(between(age, 50, 70), "Age 50-70",
                   ifelse(between(age, 70, 90), "Age 70-90",
                   ifelse(between(age, 90, 110), "Age 90-110", "Age 110+"))))))

ggplot(authors,aes(x=age_group))+
  geom_bar()+
  theme_classic() + 
  labs(x="Age Group", y="Number of Authors", 
       title="Number of Authors by Age Group",
       caption="Data collected from the Gutenberg Project") + 
  scale_y_continuous(expand = c(0,0), labels = scales::comma)


##### Plots example of author sentiment analysis
### Example author chosen: Jane Austen
# Text Mining 
jane_austen <- gutenberg_works(author == "Austen, Jane") |> 
  gutenberg_download(meta_fields = "title")

tidy_austen <- jane_austen |> 
  group_by(title) |> 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) |> 
  unnest_tokens(word, text) |> 
  anti_join(stop_words)

tidy_austen |> 
  ungroup() |> 
  count(word, sort = TRUE)

# Sentiment Extraction
janeaustensentiment <- tidy_austen %>%
  inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") %>% 
  count(title, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

janeaustensentiment <- janeaustensentiment %>%
  arrange(desc(sentiment)) 

ggplot(janeaustensentiment, aes(x=sentiment, y=title))+
  geom_col()+
  theme_minimal() + 
  labs(x="Sentiment", y="Title", 
       title="Sentiment Analysis for the Collected Works of Jane Austen",
       caption="Data collected from the Gutenberg Project") + 
  scale_x_continuous(labels = scales::comma)


##### Box and Whisker Plot of Popularity by Subject
## Load webscraped data
load("~/Downloads/first_30k_id.RData")

## Join with classics bookshelf data
classicsList <- classicsList %>%
  left_join(first_30k_id) %>%
  filter(downloads < 30000)

ggplot(classicsList) + 
  geom_boxplot(aes(x=subject_category, y=downloads), fill='grey') +
  theme_classic() + 
  labs(x="Subject Category", y="Popularity (Number of Downloads)", 
       title="Popularity of Hardvard Classic Novels Classified by Subject",
       caption="Data collected from the Gutenberg Project") + 
  scale_y_continuous(expand = c(0,0), labels = scales::comma)

  


