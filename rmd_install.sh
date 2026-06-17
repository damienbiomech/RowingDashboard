apt-get update && apt-get install -y \
    sudo \
    nano \
    curl \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libv8-dev \
    gdebi-core \
    tdsodbc \
    libsodium-dev \
    libproj-dev \
    libgdal-dev \
    libglpk-dev \
    libxml2-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

curl https://packages.microsoft.com/keys/microsoft.asc | sudo tee /etc/apt/trusted.gpg.d/microsoft.asc \
  && curl https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/prod.list | sudo tee /etc/apt/sources.list.d/mssql-release.list

export ACCEPT_EULA=Y
PATH="$PATH:/opt/mssql-tools/bin"
apt-get update -y \
  && apt-get install -y --no-install-recommends --allow-unauthenticated  \
   unixodbc-dev \
   msodbcsql18 \
   mssql-tools18 \
  && install2.r odbc \
  && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

apt update;
apt install pandoc -y;
apt install libxt6 -y;


R -e "install.packages('readr')";
R -e "install.packages('dplyr')";
R -e "install.packages('httr')";
R -e "install.packages('RODBC')";
R -e "install.packages('plyr')";
R -e "install.packages('stringr')";
R -e "install.packages('rmarkdown')";
R -e "install.packages('remotes')";
R -e "install.packages('devtools')";
R -e "devtools::install_github(
  'Teamworksapp/smartabaseR',
  ref = '45b332d5b2f313858f01af4fdbd80a742b36f5cf',
  force = TRUE,
  upgrade = 'never'
)"
R -e "install.packages('mongolite')";
R -e "install.packages('DBI')";

