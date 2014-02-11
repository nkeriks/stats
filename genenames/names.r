library(lars)
# gene_word_matrix_2011 downloaded from grail download site @ broad institute
# then cut -f 1 gene_word_matrix_2011 | sort | uniq -c > genecounts
gc <- read.table('genecounts', stringsAsFactors=FALSE)
colnames(gc) <- c('count', 'entrez')
# a mapping from entrez id to gene name from somewhere
e2n <- read.table('entrez_to_name.txt', sep='\t',header=T,quote="",comment.char="", stringsAsFactors=FALSE)
colnames(e2n) <- c("HGNC", "name", "fullname", "entrez")
x <- merge(gc, e2n)
#x <- subset(x, entrez < 2e7)

y <- x['count']
rownames(y) <- x$name
dashes <- grepl('-', rownames(y))


y$name.length <- nchar(x$name)
y$entrez.low <- x$entrez < 20000
y$num.digits <- nchar(gsub("[^0-9]", "", x$name))

#families <- c('ZNF', 'SLC', 'SNORD', 'TME', 'CCDC', 'KRT', 'GPR', 'KIAA', 'OR',
#'KCNA', 'IL', 'MYO', 'CYP', 'ATP', 'RPL', 'RAB', 'TRI', 'LRR', 'RNF', 'ANK',
#'MRP', 'PPP', 'HIS', 'FOX', 'POL', 'WDR', 'HOX', 'IGF', 'TNF', "TGF", "APO", "VEG", "MAP", "CTNN", "ESR", "NFK") 
#
#for (fam in families) {
#    y[,fam] <- substr(x$name, 1, nchar(fam)) == fam
#}


y$first.letter <- substr(x$name, 1,1)

y <- subset(y, !dashes)

# first three characters of each name, with trailing digits removed (to fix the OR2A2 -> OR family)
y$family <- gsub("[0-9]$", "", substr(rownames(y), 1,3))
# only take those families with at least 20 members
family.counts <- table(y$family)
freq.family <- names(family.counts[family.counts > 20])
# replace non frequent with NAs
y$family <- factor(y$family, c('OTHER', freq.family, 'ORF'))
y$family[is.na(y$family)] <- 'OTHER'
# open reading frames
orf <- grepl('C[0-9X]+orf', rownames(y))
y$family[orf] <- 'ORF'



#KRTTRUE         -377.863    139.264  -2.713 0.006668 ** 
#LRRTRUE         -372.334    169.254  -2.200 0.027829 *  
#WDRTRUE         -356.745    220.359  -1.619 0.105479    
#RNFTRUE         -311.149    165.568  -1.879 0.060221 .  
#orfTRUE         -239.719     70.222  -3.414 0.000642 ***
#TMETRUE         -167.558    104.893  -1.597 0.110187    

#lm(count ~ num.digits + name.length + PPP + ATP + FOX + SLC + POL + CYP + IL + IGF + GPR + HIS + OR + TRI + ZNF + CCDC + SNORD, data=y)


ss <- summary(lm(log(count) ~ ., data=y))
cc <- coef(ss)[order(abs(coef(ss)[,1])),]

# dummy variables for the factors to do variable selection
# with the intercept term removed
family.matrix <- model.matrix(~ family, data=y)[,-1]
first.matrix <- model.matrix(~ first.letter, data=y)[,-1]
z <- cbind(y[c('count', 'name.length', 'num.digits', 'entrez.low')], family.matrix, first.matrix)

z$entrez.low <- NULL
# too slow
#model <- lm(log(count) ~ ., data=z)
#model.step <- step(model)

preds <- data.matrix(z[-match('count', names(z))])
#lac <- cv.lars(preds, log(z$count), trace=TRUE)
la <- lars(preds, z$count, trace=TRUE)
log.la <- lars(preds, log(z$count), trace=TRUE)
lcoef <- coef(la, s=14)
lcoef <- lcoef[lcoef != 0]

log.coef <- coef(log.la, s=14)
log.coef <- log.coef[log.coef != 0]


