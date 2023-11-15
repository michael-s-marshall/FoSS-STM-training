# loading packages and data --------------------------------------------

# devtools::install_github("mikajoh/tidystm", dependencies = TRUE)
pacman::p_load(tidyverse, lubridate, quanteda, tidytext, stm, tidystm)

rm(list = ls())

df <- read_csv("housing_scopus.csv", na = c("","NA"))

# inspecting data -------------------------------------------------------------

# column names
names(df)

# NAs
df %>% 
  map_int(~sum(is.na(.)))

# variable type
df %>% map_chr(class)

# number of quantitative papers
df %>% 
  count(quant) %>% 
  mutate(percent = n / sum(n))

# creating covariates  ----------------------------------------------------

df <- df %>% 
  mutate(
    # UK author
    united_kingdom = ifelse(str_detect(Affiliations,"United Kingdom"), 1, 0),
    # US author
    united_states = ifelse(str_detect(Affiliations,"United States"), 1, 0),
    # Year centered
    year_c = Year - min(Year)
  ) %>% 
  filter(!is.na(quant) & !is.na(united_kingdom)) # removing NAs in covariates

# looking at high frequency words ---------------------------------------------

df %>% 
  unnest_tokens(word, Abstract) %>% 
  anti_join(stop_words, by = "word") %>% 
  select(word, everything()) %>% 
  count(word) %>% 
  slice_max(order_by = n, n = 30) %>% 
  ggplot(aes(x = n, y = fct_reorder(word, n))) +
  geom_col() +
  labs(y = NULL)

df %>% 
  corpus(text_field = "Abstract") %>% 
  tokens() %>% 
  kwic(pattern = "taylor", window = 10)

publishing_string <- "Published by Informa UK Limited, trading as Taylor & Francis Group.|Informa UK Limited, trading as Taylor & Francis Group.|Taylor & Francis Group"

# replotting
tidy_tokens <- df %>% 
  mutate(Abstract = str_remove_all(Abstract, publishing_string)) %>% 
  unnest_tokens(word, Abstract) %>% 
  anti_join(stop_words, by = "word") %>% 
  select(word, everything())

tidy_tokens %>% 
  count(word) %>% 
  slice_max(order_by = n, n = 30) %>% 
  ggplot(aes(x = n, y = fct_reorder(word, n))) +
  geom_col() +
  labs(y = NULL)

# tf-idf according to covariates --------------------------------------

tidy_tokens %>% 
  group_by(quant, word) %>% 
  summarise(n = n()) %>% 
  bind_tf_idf(word, quant, n) %>% 
  slice_max(order_by = tf_idf, n = 25) %>% 
  ggplot(aes(x = tf_idf, y = fct_reorder(word, tf_idf))) +
  geom_col() +
  facet_wrap(~quant, scales = "free")  +
  labs(y = NULL, title = "Quantitative papers")

tidy_tokens %>% 
  group_by(united_kingdom, word) %>% 
  summarise(n = n()) %>% 
  bind_tf_idf(word, united_kingdom, n) %>% 
  slice_max(order_by = tf_idf, n = 25) %>% 
  ggplot(aes(x = tf_idf, y = fct_reorder(word, tf_idf))) +
  geom_col() +
  facet_wrap(~united_kingdom, scales = "free")  +
  labs(y = NULL, title = "United Kingdom")

tidy_tokens %>% 
  group_by(united_states, word) %>% 
  summarise(n = n()) %>% 
  bind_tf_idf(word, united_states, n) %>% 
  slice_max(order_by = tf_idf, n = 25) %>% 
  ggplot(aes(x = tf_idf, y = fct_reorder(word, tf_idf))) +
  geom_col() +
  facet_wrap(~united_states, scales = "free")  +
  labs(y = NULL, title = "United States")

# making corpus and dfm -------------------------------------------------------

df <- df %>% 
  mutate(Abstract = str_remove_all(Abstract, publishing_string),
         Abstract = str_remove_all(Abstract, "[:digit:]|[:punct:]|\\u00AE|\\u00a9|\\u2122"))

my_stops <- c("taylor", "francis", "article", "published", "author", "uk",
              "trading", "group", "informa", "housing", "home", "house",
              "copyright", "paper", "abstract", "limited", "social", "policy")

# corpus
hs_corpus <- df %>% 
  corpus(docid_field = "EID",
         text_field = "Abstract")

# tokens 
hs_tokens <- hs_corpus %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = c(stopwords("en"), my_stops))

# compound tokens
compounds <- list(c("universal", "credit"),
                  c("welfare", "reform"),
                  c("housing", "first"),
                  c("european", "union"),
                  c("private","rented","sector"))
hs_tokens <- hs_tokens %>% 
  tokens_compound(pattern = compounds)

# document feature matrix
hs_dfm <- dfm(hs_tokens)

# stm k search -------------------------------------------------------------------

# converting to an STM object
hs_dfm_stm <- convert(hs_dfm,
                      to = "stm",
                      docvars = docvars(hs_dfm))

set.seed(123)
start_time <- Sys.time()
k_search_results <- searchK(
  documents = hs_dfm_stm$documents,
  vocab = hs_dfm_stm$vocab,
  data = hs_dfm_stm$meta,
  K = c(5, 10, 15, 20),
  prevalence = ~s(year_c) + quant + united_kingdom + united_states
)
end_time <- Sys.time()
end_time - start_time

k_search_results[["results"]] %>% 
  map_df(as_vector) %>% 
  ggplot(aes(label = as.character(K),
             x = exclus,
             y = semcoh)) +
  geom_text()

# STM --------------------------------------------------------------------

set.seed(123)
stm_k <- stm(
  documents = hs_dfm_stm$documents,
  vocab = hs_dfm_stm$vocab,
  data = hs_dfm_stm$meta,
  K = 10,
  prevalence = ~s(year_c) + quant + united_kingdom + united_states,
  init.type = "Spectral"
)

labelTopics(stm_k)
plot(stm_k)



# 1. Homeownership and tenure change
# 2. PRS, renting and housing stress
# 3. Gentrification, regeneration and urban renewal
# 4. Residential mobility and segregation
# 5. Experiences of homelessness and marginalised communities
# 6. Families and household interventions
# 7. Finance, real estate and mortgage markets
# 8. Spatial dynamics, neighbourhood change and housing submarkets

# saving data -------------------------------------------------

save(scopus, file = "working/lss/scopus.Rdata")

save(k_search_results, file = "working/stm/k_search_results.Rdata")

write.csv(stm_probs, "working/stm/20230531_stm_k_probs.csv")

save(stm_k, file = "working/stm/stm_k.Rdata")
