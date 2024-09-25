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

RUN sudo -u shiny Rscript -e 'renv::restore(clean = TRUE)'

# Install Quarto
RUN curl -L https://github.com/quarto-dev/quarto-cli/releases/download/v1.3.450/quarto-1.3.450-linux-amd64.deb -o quarto.deb
RUN sudo dpkg -i quarto.deb
RUN rm quarto.deb

# Set PATH
ENV PATH="/usr/local/bin:${PATH}"

# Copy app
COPY --chown=shiny:shiny app.R ./
COPY --chown=shiny:shiny config.yml ./
COPY --chown=shiny:shiny rhino.yml ./
COPY --chown=shiny:shiny app app/

USER shiny
