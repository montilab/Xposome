#!/bin/sh

# start shiny server
exec Rscript /srv/api-server/run_api_server.R 2>&1
