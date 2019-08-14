#!/bin/bash

appName="correlationAnalyzeR-ShinyApp" # Needs to be same as github repo
gitLink="https://github.com/millerh1/correlationAnalyzeR-ShinyApp.git"


# Get the correct version of R
sudo apt -y install apt-transport-https software-properties-common
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
sudo apt update
sudo apt -y install r-base

# Install R packages and system dependencies
sudo apt-get -y install libmariadbclient-dev
sudo apt-get -y install libssl-dev
sudo apt-get -y install libcurl4-openssl-dev
sudo apt-get -y install libxml2-dev
R -e "install.packages(c('shinyjs','shiny','shinydashboard','shinythemes','devtools','plotly','heatmaply','ggpubr','ggplot2','Rtsne','ggrepel','ggplot2', 'DT', 'gplots', 'data.table', 'BiocManager', 'DBI', 'RMySQL','pheatmap','SuperExactTest'), repos = 'http://cran.rstudio.com/')"
R -e "BiocManager::install(c('org.Mm.eg.db', 'org.Hs.eg.db','clusterProfiler','limma'))"
R -e "install.packages('metaMA', repos = 'http://cran.rstudio.com/')"
R -e "devtools::install_github('nik01010/dashboardthemes')"
R -e "devtools::install_github('millerh1/correlationAnalyzeR')"

# Set up shiny server
R -e "install.packages('shiny', repos = 'http://cran.rstudio.com/')"
sudo apt-get -y install gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.9.923-amd64.deb
sudo gdebi shiny-server-1.5.9.923-amd64.deb

# Get Rstudio for debugging
wget https://download1.rstudio.org/desktop/bionic/amd64/rstudio-1.2.1335-amd64.deb
sudo gdebi rstudio-1.2.1335-amd64.deb
sudo apt -y install libnss3
sudo apt-get -y install libasound2
sudo apt-get -y install libglfw3-dev libgles2-mesa-dev
sudo apt-get -y install libxkbcommon-x11-dev
# To run -- exit && rstudio

# Configure shiny server
appName="correlationAnalyzeR-ShinyApp" # Needs to be same as github repo
gitLink="https://github.com/millerh1/correlationAnalyzeR-ShinyApp.git"
sudo chmod 777 /srv/shiny-server
sudo chmod 777 /etc/shiny-server
cd /srv/shiny-server
rm -rf sample-apps
git clone $gitLink
sudo chmod 777 /srv/shiny-server/$appName
cd $appName
cp /etc/shiny-server/shiny-server.conf "."
sudo chmod 777 shiny-server.conf
# Edit this document now based on https://towardsdatascience.com/how-to-host-a-r-shiny-app-on-aws-cloud-in-7-simple-steps-5595e7885722
# Check your website to see if its working -- e.g. http://ec2-34-222-48-75.us-west-2.compute.amazonaws.com:3838/correlationAnalyzeR-ShinyApp/
# If not working check the logs under /var/log/shiny-server/
