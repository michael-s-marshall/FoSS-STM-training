# loading packages and data --------------------------------------------

# devtools::install_github("mikajoh/tidystm", dependencies = TRUE)
pacman::p_load(tidyverse, lubridate, quanteda, tidytext, stm, tidystm, igraph, ggstance)

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
         Abstract = str_replace_all(Abstract, "-", " "),
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

hs_tokens %>% 
  kwic(pattern = "welfare_reform")

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
  K = c(5:20),
  prevalence = ~s(year_c) + quant + united_kingdom + united_states
)
end_time <- Sys.time()
end_time - start_time # 25 mins

# saveRDS(k_search_results, file = "k_search_results.RDS")
# k_search_results <- readRDS("k_search_results.RDS")

plot(k_search_results)

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
  K = 11,
  prevalence = ~s(year_c) + quant + united_kingdom + united_states,
  init.type = "Spectral"
)

# inspecting topics ---------------------------------------------------

# words associated with topics
labelTopics(stm_k)

# top 20 probability words
topic_words <- labelTopics(stm_k, n = 20)
topic_words[["prob"]] %>% 
  apply(1, str_c, collapse = "; ")

# top 20 frex words
topic_words[["frex"]] %>% 
  apply(1, str_c, collapse = "; ")
  
# top probability documents by topic
doc_list <- list()
for(i in 1:11){
  doc_list[[i]] <- findThoughts(stm_k, 
                                texts = df$Abstract, 
                                topics = i, 
                                n = 5)
}
doc_list[[1]]
doc_list

# tidytext of stm result - beta matrix i.e. per-term-per-topic
tidy_beta <- tidy(stm_k, matrix = "beta")

tidy_beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  ggplot(aes(x = beta, y = term)) +
  geom_col() +
  facet_wrap(~topic, scales = "free") +
  labs(y = NULL)

# tidytext of stm result - theta matrix i.e. per-document-per-topic
tidy_theta <- tidy(stm_k, matrix = "theta", document_names = df$EID)

tidy_theta %>% 
  head(20)

tidy_theta %>%
  arrange(document, topic) %>% 
  slice_head(n = 44) %>% 
  ggplot(aes(x = document, y = gamma, fill = as.factor(topic))) +
  geom_col(position = "stack", colour = "black") +
  labs(y = NULL, fill = "Topic")

# let's look at that document that is a mix of topics 1, 8 and 11
tidy_theta %>%
  arrange(document, topic) %>% 
  slice_head(n = 44) %>% 
  select(document) %>% 
  unique()

# it's about gender inequalities and mortgage arrears
df$Abstract[df$EID == "2-s2.0-0033675282"]

# naming topics ----------------------------------------------------------------

# naming topics based on researcher interpretation
topic_names <- tibble(
  topic = seq(1,11,1),
  topic_name = c(
    "Ownership and welfare regimes",
    "Housing supply and demand",
    "Low income households and poverty",
    "Regeneration and mixed communities",
    "Migration, global South and minority ethnic groups",
    "Housing submarkets and neighbourhood change",
    "Critical urban studies and inequalities",
    "Mortgage markets and borrowing",
    "Families and household interventions",
    "Social housing funding and governance",
    "Homelessness and marginalised communities"
  )
)

# summarising estimate effect -------------------------------------------

hs_effect <- estimateEffect(
  formula = ~s(year_c) + quant + united_kingdom + united_states,
  stmobj = stm_k, 
  metadata = df
  )

summary(hs_effect)

# plotting estimate effect: UK ---------------------------------------

# extracting estimate effect with tidystm
uk_effect <- extract.estimateEffect(hs_effect, covariate = "united_kingdom",
                                    model = stm_k, method = "difference",
                                    cov.value1 = "1",
                                    cov.value2 = "0") %>% 
  left_join(topic_names, by = "topic")

uk_effect

uk_effect %>% 
  ggplot(aes(x = estimate, y = fct_reorder(topic_name, topic))) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.5, 
             colour = "lightgrey") +
  geom_point(size = 2) +
  geom_linerangeh(aes(xmin = ci.lower, xmax = ci.upper)) +
  labs(x = "Not UK     ......     UK", y = NULL) +
  theme_bw() +
  theme(axis.title.x = element_text(hjust = 0.45))

# plotting estimate effect: US ------------------------------------------

us_effect <- extract.estimateEffect(hs_effect, covariate = "united_states",
                                    model = stm_k, method = "difference",
                                    cov.value1 = "1",
                                    cov.value2 = "0") %>% 
  left_join(topic_names, by = "topic")

us_effect %>% 
  ggplot(aes(x = estimate, y = fct_reorder(topic_name, topic))) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1.5, 
             colour = "lightgrey") +
  geom_point(size = 2) +
  geom_linerangeh(aes(xmin = ci.lower, xmax = ci.upper)) +
  labs(x = "Not US     ......     US", y = NULL) +
  theme_bw() +
  theme(axis.title.x = element_text(hjust = 0.30))

# plotting estimate effect: year ----------------------------------------

year_effect <- extract.estimateEffect(hs_effect, covariate = "year_c",
                                      model = stm_k, method = "pointestimate") %>% 
  left_join(topic_names, by = "topic")

head(year_effect, 30)

year_effect %>% 
  ggplot(aes(x = covariate.value, y = estimate)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = ci.lower, ymax = ci.upper), fill = "lightgrey",
              alpha = 0.5) +
  facet_wrap(~str_wrap(topic_name, width = 30)) +
  scale_x_continuous(breaks = seq(0, 20, 5),
                     labels = seq(2000, 2020 , 5)) +
  theme_bw() +
  labs(x = "Year", y = "Topic prevalence")
