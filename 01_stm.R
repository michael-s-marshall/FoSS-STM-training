pacman::p_load(tidyverse, lubridate, rscopus,
               quanteda, tidytext, stm, wordcloud, widyr,
               SnowballC, text2vec, rvest)

rm(list = ls())

load("working/stm/scopus.Rdata")
load("working/stm/my_stops.Rdata")

# scraping country names from web -----------------------------------

countries_page <- read_html("https://www.worldometers.info/geography/alphabetical-list-of-countries/")
country_names <- countries_page %>%
  html_nodes(".content-inner") %>% 
  html_text() %>% 
  str_extract_all("Afghan.+") %>% 
  str_replace_all("[^-[:^punct:]]|[:digit:]", "\n") %>%
  as_tibble() %>% 
  tidytext::unnest_tokens(word, value, token = "lines") %>%
  mutate(word = str_trim(word),
         word = str_replace_all(word, "united states of america","united states")) %>% 
  filter(str_length(word) >= 1)

manuals <- tibble(
  word = c("hong kong", "taiwan", "macau")
)

country_names <- bind_rows(country_names, manuals)

# scopus data, making covariates ---------------------------------------------

scopus <- scopus %>% 
  mutate(
    country = Affiliations %>%
      str_to_lower() %>%
      str_extract(str_c(country_names$word, collapse = "|")) %>% 
      as_factor(),
    Year = parse_integer(as.character(Year)),
    year_c = Year - min(Year)
  ) %>% 
  filter(!is.na(country))

# stm ---------------------------------------------------------------

scopus_corpus <- corpus(
  scopus,
  docid_field = "EID",
  text_field = "abstract_clean"
)

quanteda_toks <- tokens(scopus_corpus, 
                        remove_numbers = T,
                        remove_punct = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(c(stopwords("en"), my_stops$word))

scopus_dfm <- dfm(quanteda_toks)

# stm k search -------------------------------------------------------------------

scopus_dfm_stm <- quanteda::convert(scopus_dfm,
                                    to = "stm",
                                    docvars = docvars(scopus_dfm))

set.seed(123)
start_time <- Sys.time()
k_search_results <- searchK(
  documents = scopus_dfm_stm$documents,
  vocab = scopus_dfm_stm$vocab,
  data = scopus_dfm_stm$meta,
  K = c(5:20),
  prevalence = ~s(Year) + country
)
end_time <- Sys.time()
end_time - start_time

k_results <- k_search_results$results %>% 
  map_df(as_vector)

scaling <- function(x){
  xmin <- min(x)
  xmax <- max(x)
  (x - xmin)/(xmax - xmin)
}

best_k <- k_results$K[which.max(scaling(k_results$exclus) +
                                  scaling(k_results$semcoh))]
best_k 

k_results %>% 
  ggplot(aes(label = as.character(K),
             x = exclus,
             y = semcoh)) +
  geom_text() +
  geom_smooth(method = "lm", se = F)

# STM --------------------------------------------------------------------

set.seed(123)
stm_k <- stm(
  documents = scopus_dfm_stm$documents,
  vocab = scopus_dfm_stm$vocab,
  data = scopus_dfm_stm$meta,
  K = best_k,
  prevalence = ~s(Year) + country,
  init.type = "Spectral"
)

labelTopics(stm_k)
plot(stm_k)

stm_probs <- bind_cols(
  scopus,
  make.dt(stm_k) %>%
    as_tibble()
)

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

