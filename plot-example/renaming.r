## List for renaming ports and airports
## left: final name - right: vector of names as they appear in data sets
renamings <- list(
    'Lyttelton'            = c('Christchurch Seaport (Lyttelton)', 'Lyttelton (sea)'),
    'Christchurch Airport' = c('Christchurch Airport'),
    'Whang\u0101rei'       = c('Whangarei', 'Whangarei (sea)'),
    'Tauranga'             = c('Tauranga Seaport', 'Tauranga (sea)'),
    'Auckland'             = c('Auckland Seaport', 'Auckland (sea)'),
    'Auckland Airport'     = c('Auckland Airport'),
    'New Plymouth'         = c('New Plymouth', 'New Plymouth (sea)'),
    'Napier'               = c('Napier', 'Napier (sea)'),
    'Nelson'               = c('Nelson', 'Nelson (sea)'),
    'Picton'               = c('Picton', 'Picton (sea)'),
    'Timaru'               = c('Timaru (sea)'),
    'Wellington'           = c('Wellington Seaport', 'Wellington (sea)'),
    'Wellington Airport'   = c('Wellington Airport'),
    'Port Chalmers'        = c('Dunedin Seaport', 'Port Chalmers (sea)'),
    'Bluff'                = c('Invercargill Seaport (Bluff)', 'Bluff (sea)'),
    'Gisborne'             = c('Gisborne', 'Gisborne (sea)')
)

## Function to rename ports and airports
rename.loc <- function(loc) {
    res <- sapply(loc, function(loc1) {
        found <- grep(loc1, renamings, fixed=T, val=T)
        if (length(found) > 1) stop('Problem renaming location, several possibilities found')
        if (length(found) == 1) return(names(found)) else return(loc1)
    })
    return(res)
}


renamings.regionbd <- list(
    'Manawat\u016B-Wanganui' = c('Manawatu-Wanganui'),
    'Hawke\'s Bay'           = c('Hawkes Bay')
)

## Function to rename ports and airports
rename.regionbd <- function(loc) {
    res <- sapply(loc, function(loc1) {
        found <- grep(loc1, renamings.regionbd, fixed=T, val=T)
        if (length(found) > 1) stop('Problem renaming location, several possibilities found')
        if (length(found) == 1) return(names(found)) else return(loc1)
    })
    return(res)
}


