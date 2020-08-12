FROM jupyter/datascience-notebook


RUN R -e "library(devtools)"

RUN R -e "devtools::install_version('gghighlight', '0.3.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('here', '0.1', dependencies = T, repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('furrr', '0.1.0', dependencies = T, repos = 'http://cran.r-project.org')"
