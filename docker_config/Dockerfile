# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest
# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libmagick++-dev
# install R packages required 
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('memoise', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('htmltools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('vroom', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('Rcpp', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magick', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('rapporter/pander')"
RUN R -e "remotes::install_github('dcomtois/summarytools', ref = '0-8-9')"
#RUN R -e "remotes::install_github('dcomtois/summarytools', ref = 'dev-current')"
#RUN R -e "remotes::install_github('dcomtois/summarytools', ref = 'no-x11-check')"
#RUN R -e "install.packages('cicerone', repos='http://cran.rstudio.com/')"

RUN mkdir -p /srv/CaRinDB/CaRinDB

EXPOSE 3838
# allow permission
#RUN sudo chown -R shiny:shiny /srv/CaRinDB
# run app
CMD ["R", "-e", "shiny::runApp('/srv/CaRinDB', host='0.0.0.0', port=3838)"]
