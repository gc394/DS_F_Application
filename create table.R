
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



