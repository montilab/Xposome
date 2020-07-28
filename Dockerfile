# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    xtail \
    wget

# install R packages required 
RUN R -e "install.packages('BiocManager', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('data.table', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggdendro', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ipc', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('visNetwork', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dendextend', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyBS', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gglot2', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('heatmaply', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RcolorBrewer', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('promises', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('future', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# set working directory to the app
WORKDIR /srv/shiny-server/

# set a volume directory
VOLUME /srv/shiny-server/

# copy the local packages to the image
COPY K2Taxonomer-master /srv/shiny-server/K2Taxonomer-master
COPY GeneHive-master /srv/shiny-server/GeneHive-master
COPY uuidtools-master /srv/shiny-server/uuidtools-master 

# install K2taxonomer and its dependencies
RUN R -e "BiocManager::install('limma')"
RUN R -e "BiocManager::install('Biobase')"
RUN R -e "BiocManager::install('GSVA')"
RUN R -e "BiocManager::install('GSEABase')"
RUN R -e "install.packages('flexdashboard', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('conclust', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('/srv/shiny-server/K2Taxonomer-master', dependencies=TRUE, repos=NULL, type='source')"

# copy the app to the image
COPY Xposome/run_xposome_server.R /srv/shiny-server/run_xposome_server.R
COPY Xposome/app.R /srv/shiny-server/app.R
COPY Xposome/data /srv/shiny-server/data
COPY Xposome/www /srv/shiny-server/www
COPY Xposome/R /srv/shiny-server/R

# Make the ShinyApp available at port 3838
EXPOSE 3838

# copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

# allow permission
RUN ["chmod", "+rw", "/srv/shiny-server/"]
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

CMD ["/usr/bin/shiny-server.sh"]

