# Get the xposme image from docker hub
FROM montibot/xposome:latest

# Set up the maintainer information
MAINTAINER Reina Chau (lilychau999@gmail.com)
    
# Set up GeneHive credentials
COPY .Renviron /root/.Renviron
COPY .netrc /root/.netrc
COPY .bashrc /root/.bashrc
    
# Set up a volume directory
VOLUME /srv/shiny-server/   

# Set up working directory to the app
WORKDIR /srv/shiny-server/

# Define a system argument
ARG DEBIAN_FRONTEND=noninteractive

# Install system libraries of general use
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libv8-dev \
    libsodium-dev 

# Install the required R packages to run the app
RUN R -e "install.packages('data.table', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggdendro', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('httr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('password', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sodium', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('digest', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sendmailR', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ipc', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "BiocManager::install('Biobase')"
RUN R -e "BiocManager::install('GSVA')"
RUN R -e "BiocManager::install('GSEABase')"
RUN R -e "install.packages('dendextend', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyBS', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gglot2', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RcolorBrewer', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('promises', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('future', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Install uuidtools and GeneHive and its dependencies
RUN R -e "BiocManager::install('S4Vectors')"
RUN R -e "install.packages('filenamer', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('io', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rjson', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('agower/uuidtools')"
RUN R -e "devtools::install_github('agower/GeneHive')"

# Install plumber for api calls
RUN R -e "install.packages('plumber', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('unix', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Make the ShinyApp available at port 3838
EXPOSE 3838

# Copy configuration files to Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

# Allow permission
RUN ["chmod", "+rwx", "/srv/shiny-server/"]
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

CMD ["/usr/bin/shiny-server.sh"]

