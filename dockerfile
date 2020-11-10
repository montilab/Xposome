# Get shiny+tidyverse packages from rocker image
FROM rocker/shiny-verse:latest

# Set up the maintainer information
MAINTAINER Reina Chau (lilychau999@gmail.com)
    
# Set up GeneHive credentials
COPY .Renviron /root/.Renviron
COPY .netrc /root/.netrc
COPY .bashrc /root/.bashrc
    
# Set up a volume directory
VOLUME /srv/api-server/   

# Set up working directory to the app
WORKDIR /srv/api-server/

# Define a system argument
ARG DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update --allow-releaseinfo-change && apt-get install -y \
    liblapack-dev \
    libpq-dev \
	git-core \
 	libssl-dev \
 	libcurl4-gnutls-dev

# Install R packages
RUN install2.r plumber

RUN R -e "install.packages(c('tidyr', 'dplyr', 'magrittr', 'httr', 'jsonlite'), \
dependencies=TRUE, repos = 'http://cran.us.r-project.org')"

# Install uuidtools and GeneHive and its dependencies
RUN R -e "BiocManager::install('S4Vectors')"
RUN R -e "install.packages('filenamer', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('io', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rjson', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('agower/uuidtools')"
RUN R -e "devtools::install_github('agower/GeneHive')"

# Make the API available at port 3838
EXPOSE 3838

# Copy configuration files to Docker image
COPY api.sh /usr/bin/api.sh

# Allow permission
RUN ["chmod", "+rx", "/srv/api-server/"]
RUN ["chmod", "+x", "/usr/bin/api-server.sh"]

CMD ["/usr/bin/api.sh"]