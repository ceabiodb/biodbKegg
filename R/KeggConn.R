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
#' @import R6
#' @export
KeggConn <- R6::R6Class("KeggConn",
inherit=biodb::BiodbConn,

public=list(

#' @description
#' New instance initializer. Connector classes must not be instantiated
#' directly. Instead, you must use the createConn() method of the factory class.
#' The parameters of this function are for the use of subclasses.
#' @param db.name The database name as defined in
#' www.kegg.jp/kegg/docs/keggapi.html.
#' @param db.abbrev The database abbreviation as defined in
#' www.kegg.jp/kegg/docs/keggapi.html.
#' @param accession.prefix The prefix used for accession identifiers. 
#' @param ... All parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(db.name=NA_character_, db.abbrev=NA_character_,
    accession.prefix=NA_character_, ...) {

    super$initialize(...)
    biodb::abstractClass('KeggConn', self)
    chk::chk_string(db.name)

    # Set members
    private$db.name <- db.name
    private$db.abbrev <- db.abbrev
    private$accession.prefix <- accession.prefix
},

#' @description
#' Gets the full list of entry IDs. See
#' @param http //www.kegg.jp/kegg/docs/keggapi.html for details.
#' @param retfmt Use to set the format of the returned value. 'plain' will
#' return the raw result from the server, as a character value. 'request' will
#' return the request as it would have been sent, as a BiodbRequest object.
#' 'ids' will return a character vector containing entry IDs.
#' @return Depending on `retfmt`.
wsList=function(retfmt=c('plain', 'request', 'ids')) {

    # Not implemented for genes database
    if (private$db.name == 'genes')
        return(character())

    retfmt <- match.arg(retfmt)

    # Build request
    u <- c(self$getPropValSlot('urls', 'ws.url'), 'list', private$db.name)
    url <- BiodbUrl$new(url=u)
    request <- self$makeRequest(url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Extract IDs
    if (retfmt == 'ids') {
        results <- strsplit(results, "\n")[[1]]

        if ( ! is.na(private$db.abbrev) && nchar(private$db.abbrev) > 0)
            results <- sub('^[^:]+:([^\\s]+)\\s.*$', '\\1', results, perl=TRUE)
        else
            results <- sub('^([^\\s]+)\\s.*$', '\\1', results, perl=TRUE)
    }

    return(results)
},

#' @description
#' Searches for entries. See http://www.kegg.jp/kegg/docs/keggapi.html
#' for details.
#' @param query The query to send to the database web service. When searching by
#' mass (i.e. 'option' parameter set to either 'exact_mass' or 'mol_weight'),
#' this query field must be set to either an exact (i.e. 174.05) or a range
#' (i.e. '250-260').
#' @param option Set this parameter to 'NONE' for querying on fields 'ENTRY',
#' 'NAME', 'DESCRIPTION', 'COMPOSITION', 'DEFINITION' and 'ORTHOLOGY'. See
#' http //www.kegg.jp/kegg/docs/keggapi.html for an exact list of fields that
#' are searched for each database, and also for other possible values of this
#' 'option' paramater.
#' @param retfmt Use to set the format of the returned value. 'plain' will
#' return the raw result from the server, as a character value. 'request' will
#' return the request as it would have been sent, as a BiodbRequest object.
#' 'parsed' will return a data frame. 'ids' will return a character vector
#' containing the IDs of the matching entries.
#' @return Depending on `retfmt`.
wsFind=function(query, option=c('NONE', 'formula', 'exact_mass', 'mol_weight',
    'nop'), retfmt=c('plain', 'request', 'parsed', 'ids', 'ids.no.prefix')) {

    chk::chk_string(query)
    retfmt <- match.arg(retfmt)
    option <- match.arg(option)

    # Build request
    u <- c(self$getPropValSlot('urls', 'ws.url'), 'find', private$db.name,
        query)
    if (option != 'NONE')
        u <- c(u, option) 
    request <- self$makeRequest(method='get', url=BiodbUrl$new(url=u))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- self$getBiodb()$getRequestScheduler()$sendRequest(request)

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
            if ( retfmt == 'ids.no.prefix' && ! is.na(private$db.abbrev)
                && nchar(private$db.abbrev) > 0)
                results <- sub('^[^:]*:', '', results)
        }
    }

    return(results)
}
),

private=list(
    db.name=NULL,
    db.abbrev=NULL,
    accession.prefix=NULL

,doGetEntryPageUrl=function(id) {

    u <- c(self$getPropValSlot('urls', 'entry.page.url'), 'www_bget')
    p <- private$completeEntryId(id)
    fct <- function(x) BiodbUrl$new(url=u, params=p)$toString()

    return(vapply(id, fct, FUN.VALUE=''))
}

,searchByText=function(ids, fields) {

    for (text.field in c('accession', 'name', 'composition', 'description'))
        if (text.field %in% names(fields)) {
            chk::chk_character(fields[[text.field]])
            chk::chk_length(fields[[text.field]], 1)
            if ( ! is.na(fields[[text.field]])) {
                text.ids <- self$wsFind(fields[[text.field]],
                    retfmt='ids.no.prefix')
                if ( ! is.null(text.ids) && any( ! is.na(text.ids)))
                    ids <- (if (is.null(ids)) text.ids else
                        ids[ids %in% text.ids])
            }
        }

    return(ids)
}

,searchByMass=function(ids, fields) {

    for (mass.field in c('monoisotopic.mass' ,'molecular.mass')) {
        if (mass.field %in% names(fields)) {

            # Check database
            if ( ! private$db.name %in% c('compound', 'drug'))
                biodb::warn("KEGG %s database is not searchable by mass.",
                    private$db.name)

            # Call wsFind()
            rng <- do.call(Range$new, fields[[mass.field]])
            query <- paste(rng$getMin(), rng$getMax(), sep='-')
            option <- if (mass.field == 'monoisotopic.mass') 'exact_mass' else
                'mol_weight'
            mass.ids <- self$wsFind(query=query, option=option,
                retfmt='ids.no.prefix')

            # Merge IDs
            if ( ! is.null(mass.ids) && any( ! is.na(mass.ids)))
                ids <- (if (is.null(ids)) mass.ids else ids[ids %in% mass.ids])
        }
    }

    return(ids)
}

,filterOnReferences=function(ids, fields, ref.fields, max.results) {

    if (any(ref.fields %in% names(fields)) && ! is.null(ids)) {

        ref.fields <- ref.fields[ref.fields %in% names(fields)]
        biodb::logInfo0("KEGG is not searchable by field(s) ",
            paste(ref.fields, collapse=", "),
            ", but we will run locally a filtering on all possible entries.")
        filtered.ids <- character()

        # Loop on all IDs
        prg <- biodb::Progress$new(biodb=self$getBiodb(),
            msg=paste0("Filtering ", length(ids), " found entries on field(s) ",
                paste(ref.fields, collapse=", ")),
            total=length(ids))
        for (id in ids) {

            # Get entry
            entry <- self$getEntry(id)

            # Match fields
            if ( ! is.null(entry)) {
                all.fields.match <- TRUE
                for (ref.field in ref.fields)
                    if ( ! entry$hasField(ref.field) ||
                        length(grep(tolower(fields[[ref.field]]),
                            tolower(entry$getFieldValue(ref.field)),
                            fixed=TRUE)) == 0) {
                        all.fields.match <- FALSE
                        break
                    }
                if (all.fields.match)
                    filtered.ids <- c(filtered.ids, id)
            }

            # Send progress message
            prg$increment()

            # Cut if we already get enough IDs
            if (max.results > 0 && length(filtered.ids) >= max.results)
                break
        }
        ids <- filtered.ids
    }

    return(ids)
}

,doSearchForEntries=function(fields=NULL, max.results=0) {

    ids <- NULL
    ref.fields <- c('ref.title', 'ref.accession', 'ref.authors', 'ref.journal',
        'ref.doi')

    # Add wide search for fields that are not searchable with the web service,
    # a filtering will be done later on these non-searchable fields.
    if (all(names(fields) %in% ref.fields) &&
        # Test if at least one value is: NOT NULL, NOT NA, NOT EMPTY STRING
        ! all(vapply(ref.fields, is.null, FUN.VALUE=TRUE) | is.na(ref.fields)
        | (ref.fields == '')))
        fields$accession <- switch(private$db.name,
            enzyme='.',
            private$accession.prefix)

    # Search
    ids <- private$searchByText(ids=ids, fields=fields)
    ids <- private$searchByMass(ids=ids, fields=fields)
    ids <- private$filterOnReferences(ids=ids, fields=fields,
        ref.fields=ref.fields, max.results=max.results)

    return(ids)
},

completeEntryId=function(id) {

    if ( ! is.na(private$db.abbrev) && nchar(private$db.abbrev) > 0)
        id <- paste(private$db.abbrev, id, sep=':')

    return(id)
},

doGetEntryContentRequest=function(id, concatenate=TRUE) {

    fct <- function(x) {
        u <- c(self$getPropValSlot('urls', 'ws.url'), 'get', x)
        BiodbUrl$new(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
},

doGetEntryIds=function(max.results=NA_integer_) {

    # Get IDs
    ids <- self$wsList(retfmt='ids')

    return(ids)
}
))
