FROM rocker/shiny:4.4.2

# Install the packages used by the application.
RUN R -e "install.packages(c('shiny', 'DT', 'bslib'), repos = 'https://cloud.r-project.org')"

# Shiny Server serves applications from this directory.
WORKDIR /srv/shiny-server/no-pixel-stuff
COPY . .

# The application creates and updates CSV files at runtime.
RUN mkdir -p data && \
    chown -R shiny:shiny /srv/shiny-server/no-pixel-stuff

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
