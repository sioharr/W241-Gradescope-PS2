#!/usr/bin/env bash

apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev

while true; do echo Y; done | apt remove r-base
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran40/'
apt update
while true; do echo Y; done | apt install r-base


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
Rscript -e "install.packages('round')"
Rscript -e "install.packages('stargazer')"





