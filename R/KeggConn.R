#' The connector abstract class to KEGG databases.
#'
#' This is the mother class of all KEGG connectors. It defines code common to
#' all KEGG connectors.
#'
#' The constructor accepts the following arguments:
#'
#' db.name: The database name as defined in
#' http://www.kegg.jp/kegg/docs/keggapi.html.
#'
#' db.abbrev: The database abbreviated name, as defined in
#' http://www.kegg.jp/kegg/docs/keggapi.html.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector to a KEGG database
#' conn <- mybiodb$getFactory()$createConn('kegg.compound')
#'
#' # Search for an entry
#' conn$wsFind('NADPH', retfmt='parsed')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @export KeggConn
#' @exportClass KeggConn
KeggConn <- methods::setRefClass("KeggConn",
    contains="BiodbRemotedbConn",
    fields=list(
        .db.name="character",
        .db.abbrev="character"
    ),

methods=list(

initialize=function(db.name=NA_character_, db.abbrev=NA_character_, ...) {

    callSuper(...)
    .self$.abstractClass('KeggConn')

    # Set name
    if (is.null(db.name) || is.na(db.name))
        biodb::error("You must set a name for this KEGG database.")
    .self$.db.name <- db.name

    # Set abbreviation
    .self$.db.abbrev <- db.abbrev
},

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    u <- c(.self$getPropValSlot('urls', 'entry.page.url'), 'www_bget')
    p <- .self$.completeEntryId(id)
    fct <- function(x) BiodbUrl$new(url=u, params=p)$toString()

    return(vapply(id, fct, FUN.VALUE=''))
},

wsList=function(retfmt=c('plain', 'request', 'ids')) {
    ":\n\nGets the full list of entry IDs. See
    http://www.kegg.jp/kegg/docs/keggapi.html for details.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw result from the server, as a character value. 'request' will return
    the request as it would have been sent, as a BiodbRequest object. 'ids' will
    return a character vector containing entry IDs.
    \nReturned value: Depending on `retfmt`.
    "

    # Not implemented for genes database
    if (.self$.db.name == 'genes')
        return(character())

    retfmt <- match.arg(retfmt)

    # Build request
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'list', .self$.db.name)
    url <- BiodbUrl$new(url=u)
    request <- .self$makeRequest(url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Extract IDs
    if (retfmt == 'ids') {
        results <- strsplit(results, "\n")[[1]]

        if ( ! is.na(.self$.db.abbrev) && nchar(.self$.db.abbrev) > 0)
            results <- sub('^[^:]+:([^\\s]+)\\s.*$', '\\1', results, perl=TRUE)
        else
            results <- sub('^([^\\s]+)\\s.*$', '\\1', results, perl=TRUE)
    }

    return(results)
},

wsFind=function(query,
    option=c('NONE', 'formula', 'exact_mass', 'mol_weight', 'nop'),
    retfmt=c('plain', 'request', 'parsed', 'ids', 'ids.no.prefix')) {
    ":\n\nSearches for entries. See http://www.kegg.jp/kegg/docs/keggapi.html
    for details.
    \nquery: The query to send to the database web service. When searching by
    mass (i.e.: 'option' parameter set to either 'exact_mass' or 'mol_weight'),
    this query field must be set to either an exact (i.e.: 174.05) or a range
    (i.e.: '250-260').
    \noption: Set this parameter to 'NONE' for querying on fields 'ENTRY',
    'NAME', 'DESCRIPTION', 'COMPOSITION', 'DEFINITION' and 'ORTHOLOGY'. See
    http://www.kegg.jp/kegg/docs/keggapi.html for an exact list of fields that
    are searched for each database, and also for other possible values of this
    'option' paramater.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw result from the server, as a character value. 'request' will return
    the request as it would have been sent, as a BiodbRequest object. 'parsed'
    will return a data frame. 'ids' will return a character vector containing
    the IDs of the matching entries.
    \nReturned value: Depending on `retfmt`.
    "

    chk::chk_string(query)
    retfmt <- match.arg(retfmt)
    option <- match.arg(option)

    # Build request
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'find', .self$.db.name,
        query)
    if (option != 'NONE')
        u <- c(u, option) 
    request <- .self$makeRequest(method='get', url=BiodbUrl$new(url=u))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain') {

        # Parse data frame
        if (length(grep('^[[:space:]]*$', results, perl=TRUE)) == 0) {
            readtc <- textConnection(results, "r", local=TRUE)
            df <- read.table(readtc, sep="\t", quote='', stringsAsFactors=FALSE)
            close(readtc)
            results <- df
        } else {
            results <- data.frame()
        }

        if (retfmt %in% c('ids', 'ids.no.prefix')) {
            results <- if (ncol(results) > 0) results[[1]] else character()
            if ( retfmt == 'ids.no.prefix' && ! is.na(.self$.db.abbrev)
                && nchar(.self$.db.abbrev) > 0)
                results <- sub('^[^:]*:', '', results)
        }
    }

    return(results)
},

.doSearchForEntries=function(fields=NULL, max.results=0) {

    ids <- NULL

    # Search by text field 
    for (text.field in c('accession', 'name', 'composition', 'description'))
        if (text.field %in% names(fields)) {
            chk::chk_character(fields[[text.field]])
            chk::chk_length(fields[[text.field]], 1)
            if ( ! is.na(fields[[text.field]])) {
                text.ids <- .self$wsFind(fields[[text.field]],
                    retfmt='ids.no.prefix')
                if ( ! is.null(text.ids) && any( ! is.na(text.ids)))
                    ids <- (if (is.null(ids)) text.ids else
                        ids[ids %in% text.ids])
            }
        }

    # Search by mass
    for (mass.field in c('monoisotopic.mass' ,'molecular.mass')) {
        if (mass.field %in% names(fields)) {

            # Check database
            if ( ! .self$.db.name %in% c('compound', 'drug'))
                biodb::warn("KEGG %s database is not searchable by mass.",
                    .self$.db.name)

            # Call wsFind()
            rng <- do.call(Range$new, fields[[mass.field]])
            query <- paste(rng$getMin(), rng$getMax(), sep='-')
            option <- if (mass.field == 'monoisotopic.mass') 'exact_mass' else
                'mol_weight'
            mass.ids <- .self$wsFind(query=query, option=option,
                retfmt='ids.no.prefix')

            # Merge IDs
            if ( ! is.null(mass.ids) && any( ! is.na(mass.ids)))
                ids <- (if (is.null(ids)) mass.ids else ids[ids %in% mass.ids])
        }
    }

    return(ids)
},

.completeEntryId=function(id) {

    if ( ! is.na(.self$.db.abbrev) && nchar(.self$.db.abbrev) > 0)
        id <- paste(.self$.db.abbrev, id, sep=':')

    return(id)
},

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'ws.url'), 'get', x)
        BiodbUrl$new(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
},

.doGetEntryIds=function(max.results=NA_integer_) {

    # Get IDs
    ids <- .self$wsList(retfmt='ids')

    return(ids)
}

))
