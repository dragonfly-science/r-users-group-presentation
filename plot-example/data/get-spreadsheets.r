library(reshape2)
library(plyr)

source('../renaming.r')
source('../helper-functions.r', chdir=T)
options(stringsAsFactors=F)



###  Import
imps <- read.csv('20140228_data_on_value_of_imports_by_port.csv',
                 skip=7, as.is=T, header=F)
imps <- imps[1:(grep('^data extracted', imps[,1])-1), c(1, 5, 11)]
names(imps) <- c('location', 'value', 'volume')
imps$location <- clean.string(imps$location)
imps$value <- as.numeric(as.character(imps$value))


###  Export
exps <- read.csv('20140228_data_on_value_of_exports_by_port.csv',
                 skip=7, as.is=T, header=F)
exps <- exps[1:(grep('^data extracted', exps[,1])-1), c(1, 5, 10)]
names(exps) <- c('location', 'value', 'volume')
exps$location <- clean.string(exps$location)
exps$value <- as.numeric(as.character(exps$value))

impexp <- merge(imps, exps, by='location', all=T, suffixes=c('_imp', '_exp'))
if (any(is.na(impexp))) stop('Problem with linking')


impexp$location <- rename.loc(impexp$location)

## Keep most important ports
ie <- subset(impexp, value_imp > 1e3 | value_exp > 1e3 |
             volume_imp > 1e3 | volume_exp > 1e3)
## Normalise
ie <- melt(ie, id='location')
ie$value_volume <- ifelse(grepl('^value', ie$variable), 'value', 'volume')
ie$import_export <- ifelse(grepl('_imp$', ie$variable), 'import', 'export')
impexp <- ie[order(ie$location, ie$value_volume, ie$import_export), ]


save(impexp, file='imports-exports-year-to-date_impexp.rdata')





