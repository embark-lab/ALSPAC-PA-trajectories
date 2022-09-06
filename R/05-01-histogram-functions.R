


#' hist_by_age_sex function
#' creates a histogram (by percent) an identified variable across age and sex
#' @param df_1 a dataframe
#' @param x_var_1 a variable for histogram
#' @param graph_title title of the graph
#' @import dplyr
#' @import ggplot2
#' @return a histogram (percentages within gender) of an identified variable across age
#' @export


hist_by_age_sex <- function(df_1, x_var_1, graph_title) {
  df_1 <- df_1
  x_var_1 <- enquo(x_var_1)
  df_2 <- frq_table_by_age_sex(df_1, !!x_var_1)
  plot_1 <- ggplot(data = df_2,
  aes(x = !!x_var_1, y = pct, fill = sex, label = sprintf('%0.1f%%', pct*100))) + #set the aesthetic features, including the labels at one decimal place
  geom_col(position = 'dodge') + #add column graph, with male and female separated
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = 0.0,    # nudge above top of bar
            size = 3)+ #set text size
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+ #set the lower and upper limits of the y axis ( 0-100 percent);
  labs(x = element_blank(), y = 'Percent (within gender)', title = graph_title) +
  scale_fill_manual(values = c('Male' = 'darkgreen', 'Female' = 'orange')) +
  facet_wrap(~age) +
  theme_bw() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), text = element_text(size = 12)) #make legend text a bit bigger
return(plot_1)

}


#' Frequency Table by Age and Sex Function
#'creates a table which summarizes responses on an identified variable across age and sex
#' @param df A dataframe/tibble
#' @param x_var A variable
#' @import haven
#' @import dplyr
#' @import tidyr
#' @return a table which summarizes responses on an identified variable across age and sex
#' @export



frq_table_by_age_sex <- function(df, x_var) {
  x_var <- enquo(x_var)
  df_1 <- df %>%
    filter(!is.na(!! x_var) & !is.na(sex)) %>%
    select (!! x_var, sex, age) %>%
    mutate (sex = as_factor(sex)) %>%
    mutate( !!x_var := as_factor(!!x_var)) %>%
    group_by(!!x_var, sex, age) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(sex, age) %>%
    mutate(pct = prop.table(n)) %>%
    ungroup()
  return(df_1)
}


frq_table_by_sex <- function(df, x_var) {
  x_var <- enquo(x_var)
  df_1 <- df %>%
    filter(!is.na(!! x_var) & !is.na(sex)) %>%
    select (!! x_var, sex) %>%
    mutate (sex = as_factor(sex)) %>%
    mutate( !!x_var := as_factor(!!x_var)) %>%
    group_by(!!x_var, sex) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(sex) %>%
    mutate(pct = prop.table(n)) %>%
    ungroup()
  return(df_1)
}
hist_by_sex <- function(df_1, x_var_1, graph_title) {
  df_1 <- df_1
  x_var_1 <- enquo(x_var_1)
  df_2 <- frq_table_by_sex(df_1, !!x_var_1)
  plot_1 <- ggplot(data = df_2,
                   aes(x = !!x_var_1, y = pct, fill = sex, label = sprintf('%0.1f%%', pct*100))) + #set the aesthetic features, including the labels at one decimal place
    geom_col(position = 'dodge') + #add column graph, with male and female separated
    geom_text(position = position_dodge(width = .9),    # move to center of bars
              vjust = 0.0,    # nudge above top of bar
              size = 4)+ #set text size
    scale_y_continuous(labels = scales::percent, limits = c(0,1))+ #set the lower and upper limits of the y axis ( 0-100 percent);
    labs(x = element_blank(), y = 'Percent (within gender)', title = graph_title) +
    scale_fill_manual(values = c('Male' = 'darkgreen', 'Female' = 'orange'))
  theme_bw() +
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), text = element_text(size = 14)) #make legend text a bit bigger
  return(plot_1)
}