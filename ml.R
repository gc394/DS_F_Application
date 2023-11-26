
USArrests

pca_input_df = filtered_df %>% 
  pivot_wider(names_from = name, values_from = median_value)
  filter(
    year == '2020') %>%
  dplyr::select(all_of(-c('value_type', 'year')))

pca_recipe <- recipes::recipe(firm ~ ., data = pca_input_df)  %>%
  # we only want to sue complete rows
  recipes::step_filter_missing(
    all_numeric(),
    threshold = 0) %>%
  # normalise all columns
  recipes::step_normalize(all_numeric()) %>%
  # compute principal components
  recipes::step_pca(
    all_numeric(), 
    threshold = .95, options = list(center = TRUE)
    )

pca_prep = recipes::prep(x = pca_recipe, training = pca_input_df)

# plot this and cluster
pca_output_df = recipes::bake(object = pca_prep, pca_input_df)

ggplot2::ggplot(
  data = pca_output_df, 
  mapping = aes(
    x = PC1, 
    y = PC2,
    col = ggthemes::wsj_pal()(1)))+
  geom_point() +
  theme(legend.position = "none") +
  labs(
    title = 'Principal Component 1 against 2', 
    x = 'Principal Component 1', 
    y = 'Principal Component 2') 




# plot this
pc_importance_df = as.data.frame(summary(pca_prep$steps[[3]]$res)$importance) %>%
  dplyr::filter(row.names(.) %in% c('Cumulative Proportion', 'Proportion of Variance')) %>%
  dplyr::mutate(Variance = case_when(
    row.names(.) == 'Cumulative Proportion' ~ 'Cumulative',
    row.names(.) == 'Proportion of Variance' ~ 'Individual'
  )) %>%
  tidyr::pivot_longer(
    cols = -'Variance', 
    names_to = 'PC'
  ) %>%
  tidyr::pivot_wider(
    id_cols = 'PC', 
    names_from = 'Variance', 
    values_from = 'value'
  )
  
ggplot(pc_importance_df)  + 
  geom_line(aes(x=PC, y=Cumulative, group = 1), linewidth=0.5)+
  geom_point(aes(x=PC, y=Cumulative, color="Cumulative"), pch =16)+
  geom_col(aes(x=PC, y=Individual, fill="Individual"), colour="black") +
  geom_hline(yintercept = 0.95, linetype="dashed", color = ggthemes::wsj_pal()(3)[3]) + 
  geom_text(label = '0.95',mapping = aes(x = 'PC9', y = 0.9), vjust=-2, color = ggthemes::wsj_pal()(3)[3]) +
  labs(
    title = 'Explained Variance', 
    subtitle = 'By Principal Component', 
    x = 'Principal Components', 
    y = 'Variance') +
  scale_color_manual(name ='', values=ggthemes::wsj_pal()(1)) +
  scale_fill_manual(name ='', values=ggthemes::wsj_pal()(2)[2]) +
  theme(legend.position="right")
  

kclust <- kmeans(pca_output_df %>% dplyr::select(-firm), centers = 3)

pca_points = pca_output_df %>% 
  dplyr::select(-firm)

kclusts <- 
  tibble(k = 3:9) %>%
  mutate(
    kclust = map(k, ~kmeans(pca_points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points))
    



