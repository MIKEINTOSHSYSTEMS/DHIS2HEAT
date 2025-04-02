#!/bin/bash
R -e "shiny::runApp('./app/app.R', port = 80, host = '0.0.0.0')"
#R -e "shiny::runApp('./app/app.R', port = 3939, host = '0.0.0.0')"

#R -e "source("UI/install.R")"