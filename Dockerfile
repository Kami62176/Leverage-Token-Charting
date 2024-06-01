# Use the rocker/shiny image as the base image
FROM rocker/shiny:latest

# Install system dependencies for the application
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libgit2-dev



# Install R packages required by the application





RUN R -e "install.packages(c('ggplot2', 'gridExtra', 'jsonlite', 'plotly', 'dplyr', 'scales', 'devtools'), repos='https://cloud.r-project.org/')"

# Copy the application code to the Docker image
COPY . /srv/shiny-server/

# Make all app files readable (solves issue when running as non-root)
RUN chmod -R 755 /srv/shiny-server

# Make sure the app is executed as a non-root user
USER shiny

EXPOSE 8180

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server')"]
