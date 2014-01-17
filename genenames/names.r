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

families <- c('ZNF', 'SLC', 'SNORD', 'TME', 'CCDC', 'KRT', 'GPR', 'KIAA', 'OR',
'KCNA', 'IL', 'MYO', 'CYP', 'ATP', 'RPL', 'RAB', 'TRI', 'LRR', 'RNF', 'ANK',
'MRP', 'PPP', 'HIS', 'FOX', 'POL', 'WDR', 'HOX', 'IGF', 'TNF', "TGF", "APO", "VEG", "MAP", "CTNN", "ESR", "NFK") 

for (fam in families) {
    y[,fam] <- substr(x$name, 1, nchar(fam)) == fam
}

y$orf <- grepl('orf', x$name)

y$first.letter <- substr(x$name, 1,1)

y <- subset(y, !dashes)

y$first3 <- gsub("[0-9]$", "", substr(rownames(y), 1,3))


#KRTTRUE         -377.863    139.264  -2.713 0.006668 ** 
#LRRTRUE         -372.334    169.254  -2.200 0.027829 *  
#WDRTRUE         -356.745    220.359  -1.619 0.105479    
#RNFTRUE         -311.149    165.568  -1.879 0.060221 .  
#orfTRUE         -239.719     70.222  -3.414 0.000642 ***
#TMETRUE         -167.558    104.893  -1.597 0.110187    

lm(count ~ num.digits + name.length + PPP + ATP + FOX + SLC + POL + CYP + IL + IGF + GPR + HIS + OR + TRI + ZNF + CCDC + SNORD, data=y)


