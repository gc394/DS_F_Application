
# decide the variables by most variance in median value?


# we want to find the top sd firms for all size metrics and make a table for the top 15/10%
tst = df %>%
  # take the mean value
  dplyr::filter(value_type == 'median_value') %>%
  dplyr::select(-year, -value_type) %>%
  # remove duplications from value_types
  dplyr::distinct() %>%
  # find standard deviation for each firm 
  dplyr::mutate(
    across(
      .cols = where(is.numeric),
      # find the absolute sd of each of the variable columns
      .fns = ~ abs((.x - mean(.x, na.rm = T)) / mean(.x, na.rm = T)), 
      .names = '{.col}_abs_sd'),
    # normalise these columns to stop weighting bias
    across(
      .cols = ends_with('_abs_sd'),
      .fns = ~ abs((.x - mean(.x, na.rm = T)) / mean(.x, na.rm = T)), 
      .names = '{.col}_normalised')
    ) %>% 
  rowwise() %>%
  # have used select variables to determine size 
  dplyr::mutate(size_score = mean(c(nwp_m_abs_sd_normalised, net_bel_m_abs_sd_normalised, pure_gross_claims_ratio_abs_sd_normalised), na.rm= T)) %>%
  dplyr::arrange(desc(size_score)) %>%
  utils::head(10) %>%
  dplyr::select(firm, size_score, nwp_m_abs_sd_normalised, net_bel_m_abs_sd_normalised, pure_gross_claims_ratio_abs_sd_normalised)


# CORR matrix
mat = df %>%
  # take the mean value
  dplyr::filter(value_type == 'median_value') %>%
  dplyr::select(-year, -value_type, -firm) %>%
  # remove duplications from value_types
  dplyr::distinct() %>%
  cor(., method = "pearson", use = "complete.obs")




# modifed z-score
median_ad_df = df %>%
  # take the mean value
  dplyr::filter(value_type == 'time_series_value') %>%
  dplyr::select(-value_type) %>%
  # remove duplications from value_types
  dplyr::distinct() %>%
  dplyr::group_by(year) %>%
  # find standard deviation for each firm 
  dplyr::summarise(
    across(
      .cols = where(is.numeric),
      # find the absolute sd of each of the variable columns
      .fns = ~ stats::mad(.x, na.rm = T)
      )) %>%
  pivot_longer(
    cols = -year, 
    names_to = 'metric', 
    values_to = 'year_median_ad')

median_df = df %>%
  # take the mean value
  dplyr::filter(value_type == 'time_series_value') %>%
  dplyr::select(-value_type) %>%
  # remove duplications from value_types
  dplyr::distinct() %>%
  dplyr::group_by(year) %>%
  # find standard deviation for each firm 
  dplyr::summarise(
    across(
      .cols = where(is.numeric),
      # find the absolute sd of each of the variable columns
      .fns = ~ median(.x, na.rm = T)
    )) %>%
  pivot_longer(
    cols = -year, 
    names_to = 'metric', 
    values_to = 'year_median')

mean_ad_df = df %>%
  # take the mean value
  dplyr::filter(value_type == 'time_series_value') %>%
  dplyr::select(-value_type) %>%
  # remove duplications from value_types
  dplyr::distinct() %>%
  dplyr::group_by(year) %>%
  # find standard deviation for each firm 
  dplyr::summarise(
    across(
      .cols = where(is.numeric),
      # find the absolute sd of each of the variable columns
      .fns = ~ DescTools::MeanAD(.x, na.rm = T)
    )) %>%
  pivot_longer(
    cols = -year, 
    names_to = 'metric', 
    values_to = 'year_mean_ad')


#https://www.ibm.com/docs/en/cognos-analytics/11.1.0?topic=terms-modified-z-score
modified_z_df = df %>%
  # take the mean value
  dplyr::filter(value_type == 'time_series_value') %>%
  tidyr::pivot_longer(
    cols = -c('year', 'firm', 'value_type'), 
    names_to = 'metric') %>%
  dplyr::left_join(
    median_ad_df,
    by = c('year', 'metric')
  ) %>%
  dplyr::left_join(
    mean_ad_df,
    by = c('year', 'metric')
  ) %>%
  dplyr::left_join(
    median_df,
    by = c('year', 'metric')
  ) %>%
  dplyr::mutate(
    modifed_z = ifelse(year_median_ad == 0, 
                       (value - year_median)/(1.253314 * year_mean_ad),
                       (value - year_median)/(1.486 * year_median_ad))
  ) %>%
  dplyr::select(firm, year, metric, modifed_z) %>%
  tidyr::pivot_wider(
    names_from = 'metric', 
    values_from = 'modifed_z') %>%
  dplyr::group_by(firm) %>%
  dplyr::summarise(
    across(.cols = where(is.numeric),
           .fns = ~mean(.x, na.rm = T))
  ) %>% 
  rowwise() %>%
  # have used select variables to determine size 
  dplyr::mutate(size_score = mean(c(nwp_m, net_bel_m, pure_gross_claims_ratio), na.rm= T)) %>%
  dplyr::arrange(desc(size_score)) %>%
  utils::head(10) %>%
  dplyr::select(firm, size_score, nwp_m, net_bel_m, pure_gross_claims_ratio)





# we want to find the top sd firms for all size metrics and make a table for the top 15/10%
tst = df %>%
  # take the mean value
  dplyr::filter(value_type == 'median_value') %>%
  dplyr::select(-year, -value_type) %>%
  # remove duplications from value_types
  dplyr::distinct() %>%
  # dplyr::group_by(firm) %>%
  # find standard deviation for each firm 
  dplyr::summarise(
    across(
      .cols = where(is.numeric),
      # find the absolute sd of each of the variable columns
      .fns = ~ median(.x, na.rm = T), 
      .names = '{.col}_median'))
    # normalise these columns to stop weighting bias
    across(
      .cols = ends_with('_abs_sd'),
      .fns = ~ abs((.x - mean(.x, na.rm = T)) / mean(.x, na.rm = T)), 
      .names = '{.col}_normalised')
  ) %>% 
  rowwise() %>%
  # have used select variables to determine size 
  dplyr::mutate(size_score = mean(c(nwp_m_abs_sd_normalised, net_bel_m_abs_sd_normalised, pure_gross_claims_ratio_abs_sd_normalised), na.rm= T)) %>%
  dplyr::arrange(desc(size_score)) %>%
  utils::head(10) %>%
  dplyr::select(firm, size_score, nwp_m_abs_sd_normalised, net_bel_m_abs_sd_normalised, pure_gross_claims_ratio_abs_sd_normalised)


clusterings %>%
  filter(tot.withinss == min(tot.withinss))























