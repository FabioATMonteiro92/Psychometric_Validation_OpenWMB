# Start from the official Jupyter image that already has R + IRkernel support
FROM jupyter/r-notebook:latest

# become root to install system deps
USER root

# install system libraries some R packages may need
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
      libxml2-dev \
      libcurl4-openssl-dev \
      libssl-dev \
      libxt6 \
      libfontconfig1-dev \
      libfreetype6-dev \
      libpng-dev \
      libtiff5-dev \
      libjpeg-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# switch back to the notebook user (jovyan)
USER $NB_UID

# set CRAN mirror
ENV R_REPOS https://cloud.r-project.org

# install required R packages
RUN R -e "install.packages(c( \
    'here', \
    'readxl', \
    'dplyr', \
    'tidyr', \
    'tibble', \
    'knitr', \
    'rmarkdown', \
    'psych', \
    'semTools', \
    'MVN', \
    'mice', \
    'openxlsx', \
    'sjmisc', \
    'semPlot' \
), repos='https://cloud.r-project.org')"

# try to install simsem last (it sometimes pulls more deps)
RUN R -e "install.packages('simsem', repos='https://cloud.r-project.org'); TRUE"

# make sure IRkernel is available to Jupyter
RUN R -e "IRkernel::installspec(user = FALSE)"

# copy your repo contents into the image so they show up in Binder
COPY . /home/jovyan

WORKDIR /home/jovyan
