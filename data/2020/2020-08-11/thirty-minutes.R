# after reading the readme, my initial idea is to try and do 
# a supervised learning method to predict which character is speaking which lines.
# I will need SMLTAR! https://smltar.com/ 

library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)

# first read in data 

dat <- read_csv("data/2020/2020-08-11/avatar.csv")
View(dat)

# okay 30 minutes start now 

# first, remove scene description 

dat <- dat %>% filter(character != "Scene Description")

dat %>% count(character, sort = T) %>% 
  mutate(perc = n / sum(n)) %>% 
  print(n = 100)

# okay, clearly need to create an "other" group in character 

dat %>% 
  mutate(character2 = fct_lump_prop(character, prop = .1)) %>% 
  count(character2, sort = T) 

dat %>% 
  mutate(character2 = fct_lump_prop(character, prop = .05)) %>% 
  count(character2, sort = T)

dat %>% 
  mutate(character2 = fct_lump_prop(character, prop = .02)) %>% 
  count(character2, sort = T)

# i'm going with prop =.02 because it preserves all the dialogue
# by the characters mentioned in the readme

dat %>% 
  mutate(character2 = fct_lump_prop(character, prop = .02)) -> dat

# okay, what other variables might be useful? 
glimpse(dat)
# features: character2 (response), book, chapter_num, character_words, 

# could writer be important? maybe one particular writer writes more dialogue for 
# a particular character? 

dat %>% count(writer, sort = T) %>% print( n = 50)

# okay, make individual writer variables (0,1)
dat %>% select(book, chapter_num, writer) %>% 
  distinct() %>% 
  separate_rows(writer, sep = ",") %>%
  mutate(writer = str_trim(writer),
         writer = str_trim(str_remove_all(writer, "\\(story editor\\)")),
         writer = str_replace_all(writer, " ", "_"),
         value = 1,
         # lump together writers who didn't contribute to 2 or more eps 
         writer = fct_lump_min(writer, min = 2)) %>%
  # some writers credited twice as story editor, remove duplicates 
  distinct() %>% 
  pivot_wider(names_from = writer, values_from =value, 
              names_prefix = "wrt_", names_sep="_", values_fill = 0) -> writers 

# maybe the number of writers is also important 
dat %>% select(id, character2, book, chapter_num, character_words, writer) %>% 
  mutate(n_writers = str_count(writer, ",") + 1) %>% 
  select(-writer, -id) %>% 
  left_join(writers) -> dat2

# split into train, test

# write the recipe for the model 

# end 30 minutes 
# -------------------------------------------------------------------------

# set another timer for 30 minutes 

# split into train/test 
dat2
set.seed(34985)
av_split <- initial_split(dat2, strata = character2)
av_train <- training(av_split)
av_test <- testing(av_split)

# write the recipe 

av_train %>% count(character2, sort = T)

av_spec <- recipe(character2 ~ . , data = av_train) %>% 
  step_dummy(book) %>% 
  step_tokenize(character_words) %>% 
  step_stopwords(character_words) %>% 
  step_ngram(character_words, num_tokens = 3, min_num_tokens = 1) %>% 
  step_tokenfilter(character_words, max_tokens = 100, min_times = 5) %>% 
  step_tfidf(character_words) %>% 
  # class imbalance in response 
  step_downsample(character2 )

av_cv <- vfold_cv(av_train, v = 5)

# setup multiclass svm classification (following SMLTAR ch 7.2-7.3)
svm_spec <- svm_rbf() %>% 
  set_mode("classification") %>% 
  set_engine("liquidSVM")

# workflow 

av_svm_wf <- workflow() %>% add_recipe(av_spec) %>% add_model(svm_spec)
av_svm_wf


# fit 

av_svm_rs <- fit_resamples(
  av_svm_wf, av_cv, metrics = metric_set(accuracy), 
  control = control_resamples(save_pred = TRUE)
)

av_svm_rs

# metrics 
av_svm_rs_met <- collect_metrics(av_svm_rs)

# predictions 
av_svm_rs_pred <- collect_predictions(av_svm_rs)

# look at metrics 
av_svm_rs_met

# that is.... very terrible 

# look at where the model does well 
av_svm_rs_pred %>% 
  filter(id == "Fold1") %>% 
  conf_mat(character2, .pred_class) %>% 
  autoplot(type = "heatmap") + 
  ggtitle("Multiclass classification results") + 
  coord_fixed()
ggsave(filename = "data/2020/2020-08-11/plot1.png", width = 8, height = 8, device  = "png", units = "in")


# interesting. Azula, Iroh, Toph seem to have most concentration in the correct pred. 
# the others are really all over the place. 

# look at where the model does poorly 
av_svm_rs_pred %>% 
  filter(id == "Fold1") %>% 
  filter(.pred_class != character2) %>% 
  conf_mat(character2, .pred_class) %>% 
  autoplot(type = "heatmap") + 
  ggtitle("Multiclass classification, correct predictions removed") + 
  coord_fixed()
ggsave(filename = "data/2020/2020-08-11/plot2.png", width = 8, height = 8, device  = "png", units = "in")


# pause timer at 7:12 left. 
