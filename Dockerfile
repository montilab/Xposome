# get shiny serves plus tidyverse packages image
FROM rocker/r-ver:3.6.1

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

# Download and install ShinyServer (latest version)
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb
        
# set working directory to the app
WORKDIR /srv/shiny-server/

# create a directory to store private pkgs
RUN mkdir ./private_pkgs

# copy the packages to the image directory
COPY K2Taxonomer-master ./private_pkgs
COPY GeneHive-master ./private_pkgs
COPY uuidtools-master ./private_pkgs

# install R packages required 
RUN R -e "install.packages('BiocManager', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('data.table', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggdendro', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ipc', dependencies=TRUE, repos='http://cran.rstudio.com/')"

RUN R -e "install.packages('private_pkgs/K2Taxonomer-master', dependencies=TRUE, repos=NULL, type='source')"
RUN R -e "install.packages('private_pkgs/GeneHive-master', dependencies=TRUE, repos=NULL, type='source')"
RUN R -e "install.packages('private_pkgs/uuidtools-master', dependencies=TRUE, repos=NULL, type='source')"
RUN R -e "install.packages('visNetwork', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('Biobase', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('GSVA', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('GSEABase', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('limma', dependencies=TRUE, repos='http://cran.rstudio.com/')"
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

# copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

# copy the app to the image
COPY Xposome /srv/shiny-server/

# Make the ShinyApp available at port 3838
EXPOSE 80

# copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

# allow permission
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

CMD ["/usr/bin/shiny-server.sh"]

