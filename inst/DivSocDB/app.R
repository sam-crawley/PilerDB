library(DivSocDB)

logger <- log4r::create.logger()
log4r::logfile(logger) <- '/var/log/shiny-server/piler-dashboard.log'
log4r::level(logger) <- 'DEBUG'

DivSocDB::divSocApp(logger = logger)