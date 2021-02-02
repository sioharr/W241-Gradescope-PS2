#!/usr/bin/env bash

apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev
deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/
apt-get update
apt-get install -y r-base

Rscript -e "install.packages('jsonlite')"
Rscript -e "install.packages('tidyverse')"
Rscript -e "install.packages('gapminder')"
Rscript -e "install.packages('data.table')"
Rscript -e "install.packages('testthat')"
Rscript -e "install.packages('data.table')"
Rscript -e "install.packages('sandwich')"
Rscript -e "install.packages('lmtest')"
Rscript -e "install.packages('ggplot2')"
Rscript -e "install.packages('knitr')"






