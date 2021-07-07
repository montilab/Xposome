# Get shiny+tidyverse packages from rocker image
FROM montilab/xposome-genehive:latest

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


# Make the API available at port 3838
EXPOSE 3838

# Copy configuration files to Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

# Allow permission
RUN ["chmod", "+rwx", "/srv/shiny-server/"]
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

CMD ["/usr/bin/shiny-server.sh"]
