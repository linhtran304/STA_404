pacman::p_load(tidyverse, tidytext, textdata, furrr, magrittr)

plan(multisession, workers = 2)

## Batch 3
ebert3 = jenn_batch3 |> 
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

save(ebert3, file = "JE_sentiment3.RData")

## Batch 1
ebert1 = jenn_batch1 |> 
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

save(ebert1, file = "JE_sentiment1.RData")