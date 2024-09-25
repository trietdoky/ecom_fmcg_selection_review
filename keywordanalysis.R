# LOAD FILE

AllSearch <- import(file = "DATA/Keywords/AllSearch.csv", encoding = "UTF-8")
HubSearch <- import(file = "DATA/Keywords/HubSearch.csv", encoding = "UTF-8")


# CLEAN

AllSearchClean <- AllSearch %>%
  gather(key = "date", value = "count", -names(.)[[1]]) %>%
  clean_names("upper_camel") %>%
  mutate(SearchKeyword = toupper(str_replace_all(SearchKeyword, '[[:punct:] ]+', ' ')),
         SearchKeyword = str_replace_all(SearchKeyword, "\t","")) %>%
  group_by(SearchKeyword) %>%
  summarize(Count = sum(Count)) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  filter(!str_detect(SearchKeyword,"NONE"))

AllSearchWords <- AllSearchClean %>%
  separate(SearchKeyword, into = paste0("x", 1:15),sep = " ") %>%
  gather(key = "Delete", value = "SearchKeyword", x1:x15) %>%
  select(SearchKeyword, Count) %>%
  mutate(SearchKeyword = str_replace_all(SearchKeyword, " ", "")) %>%
  group_by(SearchKeyword) %>%
  summarize(Count = sum(Count)) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  filter(!is.na(SearchKeyword)) %>%
  filter(!str_detect(SearchKeyword,"NONE"),
         !is.na(Count))

AnnotateAllSearch <- udpipe_annotate(ud_model, x = AllSearchClean$SearchKeyword)
AllSearchProcessed <- as.data.frame(AnnotateAllSearch) %>%
  mutate_all(toupper) %>%
  filter(toupper(sentence) != 'NA') %>%
  filter(upos != 'PUNCT')

KeyCollocation <- keywords_collocation(x = AllSearchProcessed,
                                       term = "token",
                                       group = c("doc_id", "paragraph_id", "sentence_id"),
                                       ngram_max = 5) %>%
  arrange(desc(freq)) %>%
  filter(!str_detect(keyword, "SOURCE="))

KeyFreq <- data.frame(
  Keyword = c(as.character(NA)),
  Freq = c(as.numeric(NA))) %>%
  slice(-1)

for (i in 1:nrow(KeyCollocation)) {
  CheckKeyword <- KeyCollocation$keyword[[i]]
  #print(i)
  Search <- AllSearchClean %>%
    mutate(Availability = str_detect(SearchKeyword, CheckKeyword)) %>%
    group_by(Availability) %>%
    summarise(Count = sum(Count)) %>%
    ungroup() %>%
    bind_rows(data.frame(Availability = TRUE, Count = 0)) %>%
    group_by(Availability) %>%
    summarise(Count = sum(Count)) %>%
    ungroup() %>%
    filter(Availability == TRUE)

  KeyFreq <- KeyFreq %>% bind_rows(data.frame(Keyword = CheckKeyword, Freq = Search$Count[[1]]))
}

KeyFreqClean <- KeyFreq %>%
  filter(Freq > 0) %>%
  rename("SearchKeyword" = "Keyword",
         "Count" = "Freq") %>%
  bind_rows(AllSearchWords) %>%
  arrange(desc(Count))




HubSearchClean <- HubSearch %>%
  gather(key = "date", value = "count", -names(.)[[1]]) %>%
  clean_names("upper_camel") %>%
  mutate(SearchKeyword = toupper(str_replace_all(SearchKeyword, '[[:punct:] ]+', ' ')),
         SearchKeyword = str_replace_all(SearchKeyword, "\t","")) %>%
  group_by(SearchKeyword) %>%
  summarize(Count = sum(Count)) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  filter(!str_detect(SearchKeyword,"NONE"),
         !is.na(Count))


HubSearchWords <- HubSearchClean %>%
  separate(SearchKeyword, into = paste0("x", 1:15),sep = " ") %>%
  gather(key = "Delete", value = "SearchKeyword", x1:x15) %>%
  select(SearchKeyword, Count) %>%
  mutate(SearchKeyword = str_replace_all(SearchKeyword, " ", "")) %>%
  group_by(SearchKeyword) %>%
  summarize(Count = sum(Count)) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  filter(!is.na(SearchKeyword)) %>%
  filter(!str_detect(SearchKeyword,"NONE"),
         !is.na(Count))
