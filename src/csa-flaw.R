library(tidyverse)
library(magrittr)


generate_points = function(n) {
  tibble::tibble(
    x1 = rnorm(1:n, 0, 1),
    x2 = rnorm(1:n, 0, 1)
  )
}


generate_path = function(dfx) {
  dfx %>% 
    dplyr::summarise_all(
      ~ mean(.)
    )
}


add_anomaly = function(dfx, num, power) {
  normal_df = 
    dfx %>% 
    dplyr::slice(1:(num-1))
  anomaly_df = 
    dfx %>%
    dplyr::slice(num:dplyr::n()) %>%
    '+'(power)
  dplyr::bind_rows(normal_df, anomaly_df)
}


vector_plot = function(dfx) {
  dfx %>% 
    ggplot2::geom_segment(
     data = .,
     mapping =  aes(
          x = 0,
          y = 0,
          xend = x1,
          yend = x2
    ), arrow = arrow(), color = "red"
  )
}

population_plot = function(dfx) {
  dfx %>%
    ggplot2::ggplot(aes(x = x1, y = x2)) +
    ggplot2::geom_point(alpha = 0.3) +
    xlim(-5, 5) + 
    ylim(-5, 5) +
    theme_bw()
}
