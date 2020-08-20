

get_cr = function(dfx, width = 0.5) {
    dfx %>%
    dplyr::group_by(Algorytm, Dim) %>%
    dplyr::group_map(function(x, y) {
        y %>% 
        dplyr::mutate(Cr = compute_cr(x, width))
    }) %>%
    purrr::reduce(dplyr::bind_rows)
}

compute_cr = function(dfx, width = 0.5) {
    T = 
        floor(width*nrow(dfx))
    indices = 
        nrow(dfx) - T
    q_mean =
        dfx %>%
        dplyr::pull()
   val= 
    1:(indices-1) %>%
    purrr::map(function(s) {
        0.5*log(q_mean[s+1]/q_mean[s])
    }) %>%
    purrr::reduce(sum)
    (-1/T) * val
}

sigma_plot = function(dfx) {
    dfx %>%
    ggplot2::ggplot(aes(x = t, y = sigma_value)) +
    ggplot2::geom_line(aes(col = Algorytm, linetype = Algorytm), size = 1) +
    #ggplot2::geom_point(aes(shape = Algorytm, col = Algorytm), size = 1) +
    theme_bw() + 
    ggplot2::scale_colour_brewer(palette="Dark2") +
    ggplot2::scale_y_continuous(
        trans = 'log10',
        labels = trans_format("log10", math_format(10^.x))
    ) + 
    xlab("t") +
    ylab("σ") + 
    theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 15, face = "bold"),
        legend.background=element_blank(),
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold")
    ) + 
    guides(
        linetype=guide_legend(keywidth = 3, keyheight = 3)
    )
}

value_plot = function(.data, .tupper=NA, .f=base::identity) {
  max_value = max(.data$func_val_best, .data$func_val_mean) 
  .data %>% 
    ggplot2::ggplot(aes(x = t)) +
    ggplot2::geom_point(aes(y = .f(func_val_best), shape = Algorytm, color = Algorytm), size = 1) + 
    ggplot2::geom_line(aes(y = .f(func_val_best), color = Algorytm, linetype = Algorytm), size = 2) + 
    ggplot2::scale_colour_brewer(palette="Dark2") +
    ggplot2::scale_y_continuous(
        trans = 'log10',
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", math_format(10^.x, 2))
      ) +   
    ggplot2::xlim(0, ifelse(missing(.tupper), NA, .tupper)) +
    xlab("t") +
    ylab(latex2exp::TeX("$|q_{best}|$")) + 
    theme_bw() +
    theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.background=element_blank(),
        axis.text = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold")
    )

}


run_exp = function(alg, func, x0, dim, lower = -100, upper = 100, CMA = FALSE) {
    result = 
        alg(
            par = rep(x0, dim),
            fn = func,
            lower = lower,
            upper = upper,
            if_CMA = CMA
        )
    sigma = 
        get_value(result, "sigma")
    best_val = 
        get_value(result, "bestVal")
    population =
        get_value(result, "pop")
    tibble::tibble(
        t = 1:length(sigma),
        sigma = sigma,
        bestVal = best_val,
        label = result$label,
        dim = dim
     #   pop = list(population)
    )
}

#' Parametr sigma
#'
#' @description
#' Funkcja ekstrahuje parametr sigma

extract_sigma = function(.diag) {
   .diag$sigma 
  }
        
extract_rmean = function(.diag) {
   .diag$rmean 
  }

#' Punkt środkowy populacji
#'
#' @description
#' Funkcja wylicza punkt środkowy w populacji w generacji t

extract_mean = function(.pop) {
  dims =  
    .pop %>% dim()

  1:dims[3] %>%
    purrr::map(function(gen) {
      .pop[,,gen] %>%
      apply(1, mean)
    })
}

#' Punkt najlepszy populacji
#'
#' @description
#' Funkcja ekstrahuje punkt najlepszy w populacji w generacji t

extract_best = function(.pop, .eval) {
  dims =  
    .pop %>% dim()
  1:dims[3] %>%
    purrr::map(function(gen) {
      min_index = .pop[,,gen] %>%
        apply(2, .eval) %>%
        which.min()
      .pop[, min_index, gen]
    })
}
        

#' Ewaluacja funkcji celu
#'

do_eval = function(.extract, .eval) {
  .extract %>% purrr::map_dbl(function(gen) {
    .eval(gen)
    })
}

#' Odległość euklidesowa
#' 
#' @description
#' Funkcja wylicza odległość euklidesową między dwoma zbiorami punktów

compute_distance = function(.seta, .setb) {
  require(sp)
  purrr::map2_dbl(.seta, .setb, function(x1, x2) {
    sp::spDists(t(x1), t(x2), FALSE)
    })
}

#' Odległość do optimum globalnego
#' 
#' @description
#' Funkcja wylicza odległość euklidesową między zbiorem punktów, a punktem optimum globalnego 

compute_nearness = function(.set, .min) {
  require(sp)
  .set %>% purrr::map_dbl(function(x) {
    sp::spDists(t(x), t(.min), FALSE)
    })
}

#' Pełny zbiór danych
#'
#' @description
#' Funkcja generuje zbiór wartości pochodnych liczonych na podstawie przystosowania
#' punktu środkowego oraz punktu najlepszego

generate_ds = function(.res, .mean, .best, .sigma, .func, ...) {
  tibble::tibble(
    t = 1:gen_amount(.res$diagnostic$pop),
    func_val_mean = do_eval(.mean, .func), 
    func_val_best = do_eval(.best, .func)) %>%
  dplyr::mutate(
    ratio = func_val_best/func_val_mean,
    sigma_value = .sigma)
}



gen_amount = function(x) {
  x %>%
  dim() %>%
  purrr::pluck(3)
}

do_experiment = function(methods, func, dim, x0, ...) {
  future::plan(strategy = "multicore")
  methods %>% 
    furrr::future_map(function(method) {
                 output = 
                   method(rep(x0, dim), fn = function(x) func(x), ...)
                 mean =
                    output %>% 
                   purrr::pluck("diagnostic", "pop") %>%
                   extract_mean()
                 sigma =
                   output %>% 
                   purrr::pluck("diagnostic") %>%
                   extract_sigma()
                 best = 
                   output %>%
                   purrr::pluck("diagnostic", "pop") %>%
                   extract_best(func)
                 rmean = 
                    output %>%
                    purrr::pluck("diagnostic") %>%
                    extract_rmean()   
                 label = 
                   output %>%
                   purrr::pluck("label")
                 dataset = 
                   generate_ds(output, mean, best, sigma, func) %>%
                   dplyr::mutate(
                       method = label,
                       rmean = rmean
                   )
  }) %>%
    purrr::reduce(dplyr::bind_rows)
}

get_value = function(result, value) {
    result %>%
        purrr::pluck("diagnostic", value) %>%
        as.numeric()
}
rename_algs = function(dfx) {
    dfx %>%
    dplyr::mutate(
        Algorytm = ifelse(method == "cma-es-csa", "CMA-ES-CSA", 
                      ifelse(method == "cma-es-msr", "CMA-ES-MSR", 
                            ifelse(method == "cma-es-quant", "CMA-ES-CPMF",
                                  ifelse(method == "cma-es-ja", "CMA-ES-CPEF",
                                        ifelse(method == "cma-es-expth", "CMA-ES-PPMF", 5)))))
    ) %>%
    dplyr::select(-method)
}
                      