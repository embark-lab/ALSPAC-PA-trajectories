

#' Transitions Plot function
#' A function that makes a transition plot of frequency of responses across age
#' @param df a data frame
#' @param var_sw a variable prefix
#' @param age ages to be included
#' @import dplyr
#' @import ggplot2
#' @import ggalluvial
#' @import rlang
#' @import ggthemes
#' @import viridis
#' @import haven
#' @import stringr
#' @return a plot of transitions over time
#' @export
#'

transition_plot <- function (df, var_sw, title, na.rm) {
var1 <- names(df |> select(starts_with(var_sw)))

df <- df

for (i in var1){
  df <- df %>%
  mutate(!!i := as_factor(!!rlang::sym(i)))
}

if (na.rm == FALSE) {
df1 <- df |>
  filter_at(all_of(var1), any_vars(!is.na(.)))
} else {
df1 <- df |>
  filter_at(all_of(var1), all_vars(!is.na(.)))
}

df1 <- df1 %>%
  group_by(across(var1)) %>%
  summarise(n = n(), .groups = 'drop') |>
  mutate(id = row_number())

dex_trans <- df1 |> gather (value, key, -n, -id)

dex_trans <- dex_trans %>%
  mutate(Age = as.numeric(str_remove(value, !!var_sw)),
         key = key)


plot1 <- ggplot(data = dex_trans, aes(x = Age, y = n,
                               stratum = key,
                               fill = key,
                               alluvium = id)) +
  geom_stratum(alpha = 0.5) +
  geom_flow() +
  theme_tufte(base_size = 18) +
  scale_fill_viridis_d(direction = 1, na.value = 'gray') +
  scale_x_continuous(breaks = c(14, 16, 18, 24)) +
  ggtitle (title) +
  theme(plot.title = element_text (size = 16, hjust = 0.5))

return(plot1)
}

