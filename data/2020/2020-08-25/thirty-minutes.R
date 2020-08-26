# read in data 
library(tidyverse)

chop <- read_tsv("data/2020/2020-08-25/chopped.tsv")

glimpse(chop)

# 30 mins starts now 

# idea: which ingredients have been most used?

chop$episode_name
# okay, only the first few ep names contain ingredients. darn.

chop$episode_notes
# free text, hard to parse out ingredients quickly. 

chop$appetizer
# okay, these look like what i'm looking for. (this could have been found out by more carefully reading the README lol)

# get the ingredients by themselves 

chop %>% 
  select(season:air_date, appetizer:dessert) %>% 
  pivot_longer(appetizer:dessert, names_to = "dish", values_to = "ingredients") %>% 
  separate_rows(ingredients, sep = ",") %>% 
  mutate(ingredients = str_trim(ingredients)) -> ingreds

ingreds %>% 
  count(ingredients, sort = T) %>% 
  ggplot(aes(x = n )) + 
  geom_histogram(binwidth = 1)
# most ingredients only used once 

# is there a way to classify the type of ingredient? 
# i.e. fruit, veg, meat, seafood, etc. 
# or sweet, savory, bitter, etc? 
#----- For this next bit, I didn't push the data to GH, but I provided the links 
# ---- to the data sources for posterity. 
# USDA foundation foods data? 
# Source: https://fdc.nal.usda.gov/download-datasets.html

food_attr <- read_csv("data/2020/2020-08-25/cdc_foundation_foods/food_attribute.csv")
food_attr %>% 
  filter(!is.na(name))
food_attr %>% count(name)
# nope 

food <- read_csv("data/2020/2020-08-25/cdc_foundation_foods/food.csv")
# not sure what food_category_id is, but is a possibility. 
food %>% count(food_category_id)


food_found <- read_csv("data/2020/2020-08-25/cdc_foundation_foods/foundation_food.csv")
# nope 

read_csv("data/2020/2020-08-25/cdc_foundation_foods/food_component.csv") %>% count(name)
# nope 

read_csv("data/2020/2020-08-25/cdc_foundation_foods/input_food.csv") 

read_csv("data/2020/2020-08-25/cdc_foundation_foods/all_downloaded_table_record_counts.csv") 
read_csv("data/2020/2020-08-25/cdc_foundation_foods/sub_sample_result.csv") 
read_csv("data/2020/2020-08-25/cdc_foundation_foods/") 


# NOPE. Try again 
# Source: https://data.nal.usda.gov/dataset/composition-foods-raw-processed-prepared-usda-national-nutrient-database-standard-referen-11
read_delim("data/2020/2020-08-25/food-usda/FOOD_DES.txt", delim = "~\\^~")
# mmeh giving up. 


# dish + ingredient? 


ingreds %>% 
  count(dish, ingredients, sort = T) 

# check for other delimiters

ingreds %>% 
  count(ingredients) %>% 
  mutate(ingred_len = str_length(ingredients)) %>%
  arrange(desc(ingred_len)) %>% 
  print(n = 500)

# things also appear in parentheses as other ingredients 
# but also 
# ("Bobby's choice") appears a lot 
# (pre-cooked), (canned)
ingreds %>% 
  count(ingredients) %>% 
  filter(str_detect(ingredients, "canned")) %>% print(n = 100)
ingreds %>% 
  count(ingredients) %>% 
  filter(str_detect(ingredients, "cooked")) %>% print(n = 100)

ingreds %>% 
  mutate(ingredients = str_replace_all(ingredients, "\\(canned\\)", "canned"), 
         ingredients = str_replace_all(ingredients, "\\(pre-cooked\\)", "pre-cooked"), 
         ingredients = str_remove_all(ingredients, "\\(Bobby's choice\\)")) -> ingreds

# over 30 minutes at this point. Wasted a lot of time on looking for a food classification data set! 
ingreds %>% 
  count(ingredients) %>% 
  mutate(ingred_len = str_length(ingredients)) %>%
  arrange(desc(ingred_len)) %>% 
  print(n = 200)

# separate out the parentheses, periods also separate ingredients. 

ingreds %>% 
  separate_rows(ingredients, sep = "\\(") %>% 
  separate_rows(ingredients, sep = "\\.") %>%
  mutate(ingredients = str_trim(ingredients), 
         ingredients = str_remove_all(ingredients, "\\)")) -> ingreds

ingreds %>% 
  count(ingredients, sort = T) %>% 
  ggplot(aes(x = n)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black", size = .25) + 
  scale_x_continuous(name = "Number of Appearances by Ingredients", breaks = 1:15) +
  theme_bw() + 
  labs(y = NULL, title = "Most ingredients in CHOPPED only appear once", 
       subtitle = "Ingredient names are very specific: e.g. zucchini noodles are separate from zucchini",
       caption = "Source: github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-08-25") + 
  theme(panel.grid.minor.x = element_blank())
ggsave(filename = "data/2020/2020-08-25/plot2.png", width = 8, height = 4, device  = "png", units = "in")


ingreds %>% 
  count(ingredients, sort = T) %>% 
  filter(n > 9) %>% 
  ggplot(aes(x = reorder(ingredients,n), y = n)) + 
  geom_col() + 
  coord_flip() + 
  theme_bw() + 
  labs(x = NULL, y = "Number of Appearances")

ingreds %>% count(dish, ingredients, sort = T) %>% 
  group_by(ingredients) %>% 
  mutate(n2 = sum(n)) %>% 
  filter(n2 > 9) -> summ_ing

summ_ing %>% 
  mutate(dish = fct_inorder(dish),
         ingredients = str_to_title(ingredients)) %>% 
  ggplot(aes(x = reorder(ingredients, n2), fill = dish, y = n)) + 
  geom_col() + 
  coord_flip(expand = F) + 
  theme_bw() + 
  scale_fill_manual(name = "Dish", values = c("#FFA92D", "#E56611", "#840610")) + 
  labs(x = NULL, y = "Number of Appearances", 
       title = "The most commmon ingredients on CHOPPED are veggies,", 
       caption = "Source: github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-08-25",
       subtitle = "But fruits rule dessert!") + 
  theme(plot.title.position = "plot", legend.position = "top", 
        axis.text.y = element_text(hjust = 0), axis.ticks.y = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.grid = element_blank(), text = element_text(family = "Arial Black"), 
        plot.caption = element_text(color = "grey80"))
ggsave(filename = "data/2020/2020-08-25/plot.png", width = 6, height = 9, device  = "png", units = "in")


# summ_ing %>% 
#   mutate(dish = fct_inorder(dish),
#          ingredients = str_to_title(ingredients)) %>% 
#   ggplot(aes(x = reorder(ingredients, n), y = n, group = dish)) + 
#   geom_col() + 
#   coord_flip(expand = F) + 
#   facet_wrap(~dish, scales = "free")


