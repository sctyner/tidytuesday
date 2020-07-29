# thirty minutes start after reading in the data

pg <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

# ready, set, go! 
# goal: to make coolest viz I can in 30 minutes 

library(tidyverse)

glimpse(pg)
summary(pg)

pg %>% count(species, island, sex)


pg %>% 
  ggplot() + 
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
  facet_grid(cols = vars(sex))

pg %>% 
  ggplot() + 
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
  facet_grid(cols = vars(sex), rows = vars(island))
# island is not a good var to use in a model because gentoo only on biscoe, 
# torgersen only has adelie penguins 
pg %>% 
  ggplot() + 
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
  facet_grid(cols = vars(sex), rows = vars(year))
# see the same general pattern over the years, so year not informative

# idea: can we predict the sex of the penguins with sex NA? 

library(tidymodels)

pg %>% 
  select(-island, -year) -> pg2 

# some are missing all info besides species. remove. 
pg_test <- pg2 %>% filter(is.na(sex), !is.na(bill_length_mm)) 

pg_train <- pg2 %>% filter(!is.na(sex))

# let's do a decision tree 

pg_cv <- vfold_cv(pg_train, strata = sex, v = 5)

pg_rec <- recipe(sex ~ ., data = pg_train) %>% 
  step_dummy(species) 

tree_spec <- decision_tree(mode = "classification", 
              tree_depth = tune(), 
              cost_complexity = tune(),
              min_n = 4) %>% 
  set_engine("rpart")

param_grid <- grid_regular(tree_depth(), cost_complexity(), levels = 5)

tree_wf <- workflow() %>% 
  add_recipe(pg_rec) %>% 
  add_model(tree_spec)

set.seed(23987)
tree_res <- tune_grid(
  tree_wf, 
  resamples = pg_cv, 
  grid = param_grid, 
  control = control_grid(save_pred = T)
)

# pause timer while running. 9 mins 13 sec left 
# resume 

autoplot(tree_res)
collect_metrics(tree_res)
tree_res %>% show_best("roc_auc")
tree_res %>% show_best("accuracy")

hyperparams <- select_best(tree_res, metric = "accuracy")

# fit to full data 

f_wf <- finalize_workflow(tree_wf, hyperparams)
tree_fit <- f_wf %>% fit(pg_train)

# predict 

pg_test %>% 
  bind_cols(predict(tree_fit, new_data = pg_test)) -> pg_test2 

pg_test3 <- pg_test2 %>% select(species:body_mass_g, sex = .pred_class)

ggplot() + 
  geom_point(data = pg_train, aes(x = bill_length_mm, y = bill_depth_mm, 
                                  color = species, shape = "Train Data"), alpha = .3) + 
  geom_point(data = pg_test3, aes(x = bill_length_mm, y = bill_depth_mm, 
                                  color = species, shape = "Test Data"), size = 2.5) + 
  facet_grid(cols = vars(sex), labeller = label_both) +
  theme_bw() + 
  theme(legend.position = "top") + 
  scale_color_brewer(palette = "Set2") + 
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)")


ggplot() + 
  geom_point(data = pg_train, aes(x = bill_length_mm, y = bill_depth_mm, 
                                  color = sex, shape = "Train Data"), alpha = .3) + 
  geom_point(data = pg_test3, aes(x = bill_length_mm, y = bill_depth_mm, 
                                  color = sex, shape = 'Test Data'), size = 2.5) + 
  facet_grid(cols = vars(species), labeller = label_both) +
  theme_bw() + 
  theme(legend.position = "top") + 
  scale_color_brewer(palette = "Set2") + 
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)")

# timer stopped


# time restarted

ggplot() + 
  geom_point(data = pg_train, aes(x = flipper_length_mm, y = body_mass_g, 
                                  color = sex, shape = "Train Data"), alpha = .3) + 
  geom_point(data = pg_test3, aes(x = flipper_length_mm, y = body_mass_g,
                                  color = sex, shape = 'Test Data'), size = 2.5) + 
  facet_grid(cols = vars(species), labeller = label_both) +
  theme_bw() + 
  theme(legend.position = "top") + 
  scale_color_brewer(palette = "Set2") + 
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)")

ggplot() + 
  geom_point(data = pg_train, aes(x = flipper_length_mm, y = body_mass_g, 
                                  color = species, shape = "Train Data"), alpha = .4) + 
  geom_point(data = pg_test3, aes(x = flipper_length_mm, y = body_mass_g,
                                  color = species, shape = 'Test Data'), size = 2) + 
  facet_grid(cols = vars(sex), labeller = label_both) +
  theme_bw() + 
  theme(legend.position = "top", 
        panel.grid =  element_blank()) + 
  scale_color_brewer(name = "Species", palette = "Set2") + 
  scale_shape(name = "Data Source") + 
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)")

# variable importance 

tree_fit %>% 
  pull_workflow_fit() %>% vi()


# use 2 most important vars 
ggplot() + 
  geom_point(data = pg_train, aes(x = bill_depth_mm, y = body_mass_g, 
                                  color = species, shape = "Train Data"), alpha = .4) + 
  geom_point(data = pg_test3, aes(x = bill_depth_mm, y = body_mass_g,
                                  color = species, shape = 'Test Data'), size = 2) + 
  facet_grid(cols = vars(sex), labeller = label_both) +
  theme_bw() + 
  theme(legend.position = "top", 
        panel.grid =  element_blank()) + 
  scale_color_brewer(name = "Species", palette = "Set2") + 
  scale_shape(name = "Data Source") + 
  labs(x = "Bill Depth (mm)", y = "Body Mass (g)")
ggsave(filename = "data/2020/2020-07-28/plot1.png", width = 8, height = 4, device  = "png", units = "in")

ggplot() + 
  geom_point(data = pg_train, aes(x = bill_depth_mm, y = body_mass_g, 
                                  color = sex, shape = "Train Data"), alpha = .4) + 
  geom_point(data = pg_test3, aes(x = bill_depth_mm, y = body_mass_g,
                                  color = sex, shape = 'Test Data'), size = 2) + 
  facet_grid(cols = vars(species), labeller = label_both) +
  theme_bw() + 
  theme(legend.position = "top", 
        panel.grid =  element_blank()) + 
  scale_color_brewer(name = "Species", palette = "Set2") + 
  scale_shape(name = "Data Source") + 
  labs(x = "Bill Depth (mm)", y = "Body Mass (g)")
ggsave(filename = "data/2020/2020-07-28/plot2.png", width = 8, height = 4, device  = "png", units = "in")


# an additional 20 minutes, 20 sec. # done! 
