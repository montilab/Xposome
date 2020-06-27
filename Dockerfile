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
    libssh2-1-dev 
  

# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages('BiocManager', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('data.table', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggdendro', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('jsonlite', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ipc', repos='http://cran.rstudio.com/')"

RUN R -e "install.packages('~/Documents/Internship Project/K2Taxonomer-master', repos=NULL, type='source')"
RUN R -e "install.packages('~/Documents/Internship Project/GeneHive-master', repos=NULL, type='source')"
RUN R -e "install.packages('~/Documents/Internship Project/uuidtools-master', repos=NULL, type='source')"
RUN R -e "install.packages('visNetwork', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('Biobase', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('GSVA', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('GSEABase', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('limma', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dendextend', repos='http://cran.rstudio.com/')"

RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyBS', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('andrewsali/shinycssloaders')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gglot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('heatmaply', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RcolorBrewer', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('promises', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('future', repos='http://cran.rstudio.com/')"

# copy the app to the image
COPY app.R /srv/shiny-server/
COPY www /srv/shiny-server/R
COPY R /srv/shiny-server/R
COPY data /srv/shiny-server/data

# select port
EXPOSE 5785

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]

