library(XML)
library(dplyr)

# pull in average ideology score for each justice
justices <- read.csv('http://mqscores.berkeley.edu/media/2013-2/justices.csv')

justices$code <- NULL
justices$post_med <- NULL
justices$post_025 <- NULL
justices$post_975 <- NULL
justices$justice <- NULL

justice.scores <- justices %>% 
    group_by(justiceName) %>%
    summarize(justiceScore=mean(post_mn), endYear=max(term)) %>%
    arrange(justiceScore)

# year -> list of possible justices to clerk for 
# don't know the right R idiom for this
g = group_by(justices, term)
year2j = lapply(attr(g, 'indices'), function (x) {as.character(g$justiceName[x])})
names(year2j) <- attr(g, 'labels')$term


# get clerks
clerk.url <- 'http://en.wikipedia.org/wiki/List_of_law_clerks_of_the_Supreme_Court_of_the_United_States'
tables <- readHTMLTable(clerk.url)
# pick the biggest table on the page
clerks <- tables[[which.max(sapply(tables, function(x) {n = nrow(x); ifelse(is.null(n), NA, n)}))]]
names(clerks)[7] <- 'school'
clerks$school <- as.character(clerks$school)
clerks$school[clerks$school == ""] <- NA
# remove years
clerks$school <- sub(" \\([0-9]*\\)", '', clerks$school)
# remove (LLB) 
clerks$school <- sub(" \\(LLB\\)", '', clerks$school)
# more cleaning
clerks$school[grep("admitted", clerks$school)] <- NA
clerks$school[clerks$school == "Boalt Hall"] <- "Berkeley"


# deal with matching justice names
# strip off the display text from the names
clerks$Justice <- sub("^[A-Za-z']*( I{1,2})?, [A-Z]", '', clerks$Justice)



# matching justices between two datasets
bad.jn <- as.character(unique(clerks$Justice))
good.jn <- as.character(justice.scores$justiceName)

r = sapply(good.jn, function(x) {bad.jn[grep(x, bad.jn)]})
# fix the corner cases
r$OJRoberts <- "Owen Josephus Roberts"
r$Roberts <- "John Roberts"
r$Douglas <- "William O. Douglas"
r$Marshall <- "Thurgood Marshall"
r$Black <- "Hugo Black"
r$Warren <- "Earl Warren"
r$White <-"Byron White"
r$Clark <- "Tom C. Clark"
r$Jackson  <- "Robert H. Jackson"
r$Harlan <- "John Marshall Harlan II"

# reverse key / value in the list since we're looking up by long name
s2l <- unlist(r)
l2s <- names(s2l)
names(l2s) <- s2l

clerks$justiceName <- l2s[clerks$Justice]

clerks$Seat <- NULL
clerks$Num <- NULL
clerks$Justice <- NULL
clerks$Finished <- NULL
clerks$term <- as.numeric(as.character(clerks$Started))
clerks$Started <- NULL
clerks$`Law clerk` <- NULL
clerks$`Previous clerkship` <- NULL
clerks <- clerks[complete.cases(clerks),]

z = merge(justice.scores, clerks, by='justiceName')
y = inner_join(justices, clerks, by=c('justiceName', 'term'))
group_by(y, school) %>% summarize(mn=median(post_mn), n=n()) %>% filter(n > 20) %>% arrange(mn)
group_by(z, school) %>% summarize(mn=median(justiceScore), n=n()) %>% filter(n > 20) %>% arrange(mn)

# some schools (virigina in particular) show very different scores when matching by term as well
# that is, virginia seems to not only land conservative justices, but also in particularly conservative years
# columbia has the opposite effect
# which is interesting, as there could be a measure of effect of clerk ideology on justice ideology lurking here...
# or something EM like where you don't hold either fixed.
subset(y, school=='Virginia') %>% arrange(justiceName) %>% group_by(justiceName) %>% summarise(mn=mean(post_mn))
subset(z, school=='Virginia') %>% arrange(justiceName) %>% group_by(justiceName) %>% summarise(mn=mean(justiceScore))


# bootstrap
# this is the true 
real <- group_by(y, school) %>% summarize(mn=median(post_mn), n=n()) %>% filter(n > 20) %>% arrange(mn)

library(permute)
blocks <- factor(y$term)
h1 = how(blocks=blocks)
# permute schools within each term
permute.terms <- function(y) {
    s = shuffle(1691, control=h1)
    message(s[1])
    y$school <- y$school[s]
    y
}
tmp <- do.call(rbind, lapply(1:100, function(xx) {permute.terms(y)}))
# this is the permuted dist
perm <- group_by(tmp, school) %>% summarize(mn=median(post_mn), n=n()) %>% filter(n > 2000) %>% arrange(mn)
