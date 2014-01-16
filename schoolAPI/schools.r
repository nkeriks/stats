library(RCurl)
library(RJSONIO)
library(plyr)
library(maps)
library(ggmap)

get.data <- function() { 
    data.file <- 'http://www3.cde.ca.gov/researchfiles/api/apiB12tx.zip'
    message("Downloading 2012 API data file")
    download.file(data.file, 'apiB12tx.zip')
    unzip('apiB12tx.zip')
}

load.school.data <- function() {
    if (!file.exists('apib12tx.txt')) {
        get.data()
    }
    field.names <- read.fwf('field.names', c(4, 12, 10, 3), header=T, stringsAsFactors=FALSE)
    field.names$Field.Name <- sub("\\s+$", "", field.names$Field.Name)

    sc <- read.fwf('apib12tx.txt', widths=field.names$Width, 
        col.names=field.names$Field.Name,
        stringsAsFactors=FALSE, comment.char="", na.strings=c('NA', 'N/A')
        )
    # remove schools without API scores
    sc <- subset(sc, !is.na(API12B))

    # strip whitespace from character columns
    col.class <- sapply(sc, class)
    char.cols <- names(col.class[col.class == 'character'])
    for (i in char.cols) {
        sc[,i] <- gsub("^\\s+|\\s+$", "", sc[,i])
    }
    sc[sc=='N/A'] <- NA

    # Yes/No/'' -> T/F/NA
    groups <- c('AA', 'AI', 'AS', 'FI', 'HI', 'PI', 'WH', 'MR', 'SD', 'EL', 'DI')
    yes.no <- c(paste(groups, 'SIG', sep='_'), 'YR_RND')
    for (i in yes.no) {
        tmp <- sc[,i]
        sc[,i] <- tmp == 'Yes'
        sc[,i][tmp == ''] <- NA
    }

    # API targets:  A = achieved, '' = not enough people, otherwise numeric
    # map A and '' -> NA
    api.targets <- c(paste(groups, 'TARG', sep='_'), paste(groups, 'GT', sep='_'))
    for (i in api.targets) {
        nas <- sc[,i] %in% c('A', '')
        sc[,i][nas] <- NA
        sc[,i] <- as.numeric(sc[,i])
    }

    # class size
    num <- c('ACS_K3', 'ACS_46', 'ACS_CORE')
    for (i in num) {
        sc[,i] <- as.numeric(sc[,i])
    }

    # remove schools without much parental SES info (< 25%)
    # also not special ed or other types, and not < 100 students
    sc <- subset(sc, PCT_RESP > 25)
    sc <- subset(sc, SPED=='')
    sc <- subset(sc, SIZE=='')
    # schols only, not districts
    sc <- subset(sc, RTYPE=='S')

    # address for geocoding
    sc$address <- sprintf('%s, %s County CA', sc$SNAME, sc$CNAME)

    sc

}

add.lat.long <- function(sc, 
    counties=c('Alameda', 'Contra Costa', 'Solano', 
               'Napa', 'Sonoma', 'Marin', 
               'San Francisco', 'San Mateo', 'Santa Clara', 
               'Santa Cruz', 'Sacramento')) {
    # add latitude / longitude
    # easy with google maps, but 2500 query limit per day
    # so we subset to the bay area, which has about 1600 schools
    CFILE <- 'bay_area_geocode.rdat'
    if (!file.exists(CFILE)) {
        bay.area <- subset(sc, CNAME %in% counties)
        system.time(coords <- ldply(bay.area$address, geocode))
        bay.area <- cbind(bay.area, coords)
        save(bay.area, file=CFILE) 
    } else {
        load(CFILE, envir = e <- new.env())
        bay.area <- e$bay.area
    }
    # google misclassifies some schools
    bay.area <- subset(bay.area, lon < -120 & lat < 39)
    bay.area
}


make.plots <- function() {
    sc <- load.school.data()
    ba <- add.lat.long(sc)

    # probably have to deal with edge effects more reasonably than I do here (would be easy to score a API 999 school as "bad"
    mod <- lm(API12B ~ AVG_ED +PCT_AS + PCT_AI + PCT_AA + PCT_HI + PCT_FI + PCT_PI+ PCT_MR, data=ba)
    preds <- predict(mod)
    ba$residual <-  ba$API12B - preds
    cuts <- quantile(ba$residual, c(0.05, 0.95)) 
    bad <- ba$residual < cuts[1]
    good <- ba$residual > cuts[2]
    plot(ba$lon, ba$lat, pch=20, xlab='longitude', ylab='latitude', )
    points(ba$lon[bad], ba$lat[bad], pch=20, col='red')
    points(ba$lon[good], ba$lat[good], pch=20, col='green')

    tab <- rbind(table(subset(ba, good)$CNAME), 
        table(subset(ba, !good & !bad)$CNAME),
        table(subset(ba, bad)$CNAME))
    rownames(tab) <- c('good', 'middle', 'bad')

}

