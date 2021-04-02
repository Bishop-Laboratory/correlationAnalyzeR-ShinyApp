#!/bin/bash

appName="correlationAnalyzeR-ShinyApp" # Needs to be same as github repo
gitLink="https://github.com/millerh1/correlationAnalyzeR-ShinyApp.git"

# Most of this is root-only
# sudo su

# Get the correct version of R
apt -y install apt-transport-https software-properties-common
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
apt update
apt -y install r-base

# Install R packages and system dependencies
apt-get -y install libmariadbclient-dev
apt-get -y install libssl-dev
apt-get -y install libcurl4-openssl-dev
apt-get -y install libxml2-dev
R -e "install.packages(c('shinyjs','shiny','ipc','shinymanager','shinydashboard','shinythemes','devtools','shinyWidgets','plotly','heatmaply','ggpubr','ggplot2','Rtsne','ggrepel','ggplot2', 'DT', 'gplots', 'data.table', 'BiocManager', 'DBI', 'RMySQL','pheatmap','SuperExactTest', 'pryr', 'pool', 'shinyBS', 'future', 'promises', 'benchmarkme'), repos = 'http://cran.rstudio.com/')"
R -e "BiocManager::install(c('org.Mm.eg.db', 'org.Hs.eg.db','clusterProfiler','limma', 'preprocessCore'))"
R -e "install.packages('metaMA', repos = 'http://cran.rstudio.com/')"
R -e "devtools::install_github('nik01010/dashboardthemes')"
R -e "devtools::install_github('andrewsali/shinycssloaders')"
R -e "devtools::install_github('millerh1/correlationAnalyzeR')"

# Set up shiny server
R -e "install.packages('shiny', repos = 'http://cran.rstudio.com/')"
#apt-get -y install gdebi-core
#wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.9.923-amd64.deb
#gdebi shiny-server-1.5.9.923-amd64.deb

# Get Rstudio for debugging
#wget https://download1.rstudio.org/desktop/bionic/amd64/rstudio-1.2.1335-amd64.deb
#gdebi rstudio-1.2.1335-amd64.deb
apt -y install libnss3
apt-get -y install libasound2
apt-get -y install libglfw3-dev libgles2-mesa-dev
apt-get -y install libxkbcommon-x11-dev
# To run -- exit && rstudio

# Configure shiny server
appName="correlationAnalyzeR-ShinyApp" # Needs to be same as github repo
#gitLink="https://github.com/millerh1/correlationAnalyzeR-ShinyApp.git"
#chmod 777 /srv/shiny-server
#chmod 777 /etc/shiny-server
#cd /srv/shiny-server
#rm -rf sample-apps
#git clone $gitLink
#chmod 777 /srv/shiny-server/$appName
chmod 777 $appName
cd $appName
#chmod 777 shiny-server.conf
mkdir www/tmp
chmod 777 www/tmp
# Edit this document now based on https://towardsdatascience.com/how-to-host-a-r-shiny-app-on-aws-cloud-in-7-simple-steps-5595e7885722
# Check your website to see if its working -- e.g. http://ec2-34-222-48-75.us-west-2.compute.amazonaws.com:3838/correlationAnalyzeR-ShinyApp/
# If not working check the logs under /var/log/shiny-server/
