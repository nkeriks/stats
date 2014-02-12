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

    # county mean APIs
    library(plotrix)
    m <- map('county', 'california', plot=FALSE)

    median.api <- tapply(sc$API12B, sc$CNAME, median)
    # wow, ca has an entire county with only ~1000 people in it south of tahoe
    # no schools? so impute to median
    median.api['Alpine'] <- median(sc$API12B)
    median.api <- median.api[order(names(median.api))]

    #state.col<-color.scale(median.api,c(1,1,0),c(0,1,1),0)
    colors <- color.gradient(c(1,1,0),c(0,1,1),0, 10)

    # this is ugly
    api.bucket <- as.numeric(cut(median.api, 10))
    labs <- levels(cut(median.api, 10))
    ints <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
            upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
    midpoints <- round(ints[,1] + .5*(ints[,2] - ints[,1]))
    map("county", "california", fill=TRUE, col=colors[api.bucket])
    legend('topright', legend=midpoints, fill=colors)


    
    # doesn't really improve prediction
    #sc$my.avg.ed <- with(sc, (NOT_HSG + 2*HSG + 3*SOME_COL + 4*COL_GRAD + 5*GRAD_SCH)*PCT_RESP/10000 )

    ba <- add.lat.long(sc)

    # just Santa Clara / San Mateo
    silicon.valley <- subset(ba, CNAME %in% c('San Mateo', 'Santa Clara'))
    # remove blank school names
    silicon.valley <- subset(silicon.valley, SNAME != '')

    library(gooleVis)
    silicon.valley$LatLong <- paste(silicon.valley$lat, ':', silicon.valley$lon, sep='')

    # use as predictors
    # * average parental education
    # * percent of parents responding
    # * ethnic percentages
    # * school type (high schools have much worse APIs)
    
    mod <- lm(API12B ~ AVG_ED + PCT_RESP + PCT_AS + PCT_AI + PCT_AA + PCT_HI + PCT_FI + PCT_PI+ PCT_MR + STYPE, data=silicon.valley)

    # probably have to deal with edge effects more reasonably than I do here (would be easy to score a API 999 school as "bad"
    #mod <- lm(API12B ~ AVG_ED + PCT_RESP + PCT_AS + PCT_AI + PCT_AA + PCT_HI + PCT_FI + PCT_PI+ PCT_MR, data=ba)
    preds <- predict(mod)
    silicon.valley$residual <-  silicon.valley$API12B - preds
    # we've essentially recapitulated the "similar schools ranking" from the API dataset 
    boxplot(silicon.valley$residual ~ as.numeric(silicon.valley$SIM_RANK)

    cuts <- quantile(silicon.valley$residual, c(0.05, 0.95)) 
    bad <- silicon.valley$residual < cuts[1]
    good <- silicon.valley$residual > cuts[2]

    silicon.valley$title <- sprintf('%s<BR>API=%s<BR>PREDICTED=%s<BR>SIM_RANK=%s', silicon.valley$SNAME, silicon.valley$API12B, round(preds), silicon.valley$SIM_RANK)


    m1 <- gvisMap(silicon.valley, "LatLong", 'title', options=list(enableScrollWheel=TRUE, showTip=TRUE, mapType='normal')) 
    m.good <- gvisMap(silicon.valley[good,], "LatLong", 'title', options=list(enableScrollWheel=TRUE, showTip=TRUE, mapType='normal')) 
    m.bad <- gvisMap(silicon.valley[bad,], "LatLong", 'title', options=list(enableScrollWheel=TRUE, showTip=TRUE, mapType='normal')) 




    plot(silicon.valley$lon, silicon.valley$lat, pch=20, xlab='longitude', ylab='latitude', )
    points(silicon.valley$lon[bad], silicon.valley$lat[bad], pch=20, col='red')
    points(silicon.valley$lon[good], silicon.valley$lat[good], pch=20, col='green')

    # by county, no longer so useful when I've only got two counties
    tab <- rbind(table(subset(silicon.valley, good)$CNAME), 
        table(subset(silicon.valley, !good & !bad)$CNAME),
        table(subset(silicon.valley, bad)$CNAME))
    rownames(tab) <- c('good', 'middle', 'bad')

}

