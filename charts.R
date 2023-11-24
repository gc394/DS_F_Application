
source('load.R')
library(ggplot2)
library(ggthemes)
library(ggrepel)


metric = 'total_assets_m'
type = 'median_value'
n = 5



# find biggest median asset firms
grouped_df = df %>%
  dplyr::filter(value_type == type) %>%
  dplyr::select(all_of(c('firm', metric))) %>%
  dplyr::group_by(firm) %>%
  dplyr::summarise(m = max(.data[[metric]]))

# find the top 3 firms
topn_firms = grouped_df %>%
  dplyr::slice_max(m, n = n) %>%
  dplyr::pull('firm')

# find the top 20pct of firms
top20pct_firms = grouped_df %>%
  dplyr::slice_max(m, prop = -.80) %>%
  dplyr::pull('firm')

# produce color vectors for the top three firms
# this gives the colour values of the vector
cols = c(ggthemes::wsj_pal()(length(topn_firms)), 
         rep('black', length(unique(df$firm)) - length(topn_firms)))

# by then naming the vector we can set the color
names(cols) = c(topn_firms, unique(df$firm)[!unique(df$firm) %in% topn_firms])

# graph
ggplot2::ggplot(
  data = df %>%
    # helps to read and debug having a smaller dataset
    dplyr::select(all_of(c('firm', 'year', 'value_type', metric))) %>%
    # we want to use the time series value as this one changes over time and we only want top 20%
    # of firms to compare this to
    dplyr::filter(value_type == 'time_series_value', firm %in% top20pct_firms) %>%
    dplyr::mutate(
      alpha = case_when(
        firm %in% topn_firms ~ 1,
        .default = 0.9
        ),
      # width = case_when(
      #   firm %in% topn_firms ~ 0.00021,
      #   .default = 0.0002
      # ),
      text = case_when(
        firm %in% topn_firms & year == '2020' ~ firm,
        .default = ''
        )
    ),
  mapping = aes(
    x = year, 
    y = .data[[metric]], 
    # linewidth = width,
    group = firm,
    alpha = alpha,
    color = firm,
    label = text)
)  +
  # ggthemes::theme_fivethirtyeight() +
  geom_line() + 
  geom_label_repel(aes(label = text),
                   nudge_x = 1,
                   na.rm = TRUE) +
  scale_color_manual(
    values = cols
  ) + 
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale(),
                                  accuracy = 1)) + 
  theme(legend.position = "none") +
  labs(
    title = glue::glue('{metric} over time'),
    subtitle = glue::glue('3 largest medians highlighted'),
    x = 'Year End',
    y = case_when(
      grepl(pattern = '_m$', x = metric) ~ 'GBP (M)',
      grepl(pattern = 'ratio$', x = metric) ~ 'Ratio'
    )
    ) +
  # xlab('Year End') +
  ylab(case_when(
    grepl(pattern = '_m$', x = metric) ~ 'GBP (M)',
    grepl(pattern = 'ratio$', x = metric) ~ 'Ratio'
  ))
