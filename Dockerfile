FROM rocker/shiny:4.3.2

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update -qq \
  && apt-get install --yes \
    curl \
    libgdal-dev \
    libproj-dev \
    libudunits2-dev \
  && rm -rf /var/lib/apt/lists/*

# Remove examples
WORKDIR /srv/shiny-server
RUN rm -rf *

# Install R dependencies
COPY --chown=shiny:shiny .Rprofile renv.lock ./
COPY --chown=shiny:shiny renv/activate.R renv/

# Update and install required system packages
RUN apt-get update && \
    apt-get install -y \
    libglpk40 \
    libglpk-dev \
    libmagick++-6.q16-8 \
    libmagick++-dev \
    imagemagick

# Install Quarto

RUN curl -L https://github.com/quarto-dev/quarto-cli/releases/download/v1.3.450/quarto-1.3.450-linux-amd64.deb -o quarto.deb
RUN sudo dpkg -i quarto.deb
RUN rm quarto.deb

# Copy RDS file 
# COPY --chown=shiny:shiny app/static/QProMS_example_dataset_p62.rds /app/static/

# change permission 
# RUN chown shiny:shiny /app/static/QProMS_example_dataset_p62.rds

# Create and set permissions for app_cache
RUN mkdir -p /srv/shiny-server/app_cache/sass && chown shiny:shiny /srv/shiny-server/app_cache/sass

# Create a temporary directory for the Sass cache
RUN mkdir -p /tmp/sass_cache && chown shiny:shiny /tmp/sass_cache

# Add Sass cache option to .Rprofile
RUN echo "options(sass.cache = '/tmp/sass_cache')" >> /srv/shiny-server/.Rprofile

RUN sudo -u shiny Rscript -e 'renv::restore(clean = TRUE)'

# Copy app
COPY --chown=shiny:shiny app.R ./
COPY --chown=shiny:shiny config.yml ./
COPY --chown=shiny:shiny rhino.yml ./
COPY --chown=shiny:shiny app app/

# Ensure shiny user can write in /srv/shiny-server
RUN chown -R shiny:shiny /srv/shiny-server
RUN chown -R shiny:shiny /srv/shiny-server/app
RUN chmod -R 755 /srv/shiny-server/app

# Copy script for loading R packages
# COPY --chown=shiny:shiny load_packages.R ./

# Load R packages
# RUN sudo -u shiny Rscript load_packages.R 

# COPY --chown=shiny:shiny docker/shiny-server.conf /etc/shiny-server/

# Start the Shiny server
CMD ["/usr/bin/shiny-server"]

USER shiny
