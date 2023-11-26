
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

mean_df = df %>%
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
      .fns = ~ mean(.x, na.rm = T)
    )) %>%
  pivot_longer(
    cols = -year, 
    names_to = 'metric', 
    values_to = 'year_mean')

df %>%
  # take the mean value
  dplyr::filter(value_type == 'time_series_value') %>%
  tidyr::pivot_longer(
    cols = -c('year', 'firm', 'value_type'), 
    names_to = 'metric') %>%
  dplyr::left_join(
    median_df,
    by = c('year', 'metric')
  ) %>%
  dplyr::left_join(
    mean_df,
    by = c('year', 'metric')
  )



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


























