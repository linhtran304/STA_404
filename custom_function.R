
get_genres = function(x){
  manuals = c("handbooks", "manuals")
  directories = c("directories")
  fiction = c("fiction", "stories", "story", "fantasy")
  nonfiction = c("philosophy", "political", "biography", "science", "biology", "statistics", "census")
  drama = c("drama", "traged", "comedies")
  poetry = c("poetry")
  essays = c("essay", "freedom")
  religious = c("bible", "church", "christian", "reformation", "qur'an")
  fairy_tales = c("fairy", "fables")
  history = c("history", "Declaration of Independence", "slavery", "constitution", "civil rights")
  folklore = c("folklore")
  
  case_when(
    grepl(x, pattern = paste0(manuals, collapse= '|'), ignore.case = T) ~ "Handbooks/Manuals",
    grepl(x, pattern = paste0(directories, collapse= '|'), ignore.case = T) ~ "Directories",
    grepl(x, pattern = paste0(history, collapse= '|'), ignore.case = T) ~ "History",
    grepl(x, pattern = paste0(fiction, collapse= '|'), ignore.case = T) ~ "Fiction",
    grepl(x, pattern = paste0(nonfiction, collapse= '|'), ignore.case = T) ~ "Non-Fiction",
    grepl(x, pattern = paste0(drama, collapse= '|'), ignore.case = T) ~ "Drama",
    grepl(x, pattern = paste0(poetry, collapse= '|'), ignore.case = T) ~ "Poetry",
    grepl(x, pattern = paste0(essays, collapse= '|'),ignore.case = T) ~ "Essays",
    grepl(x, pattern = paste0(religious, collapse= '|'), ignore.case = T) ~ "Religious",
    grepl(x, pattern = paste0(fairy_tales, collapse= '|'), ignore.case = T) ~ "Fairy Tales",
    grepl(x, pattern = paste0(folklore, collapse= '|'), ignore.case = T) ~ "Folklore",
    .default = x
  )
}


