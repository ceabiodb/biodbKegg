#' KEGG entry abstract class.
#'
#' This is the abstract entry class for all KEGG entry classes. 
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.compound')
#'
#' # Get an entry
#' e <- conn$getEntry('C00133')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import R6
#' @export
KeggEntry=R6::R6Class("KeggEntry",
inherit=BiodbTxtEntry,


public=list(

initialize=function(...) {
    super$initialize(...)
    biodb::abstractClass('KeggEntry', self)
}
),

private=list(
getTagLines=function(tag, parsed.content) {

    lines <- character()

    # Loop on all lines of parsed content
    in.tag <- FALSE
    for (line in parsed.content) {

        # Match
        regex <- paste0(if (in.tag) "^" else paste0("^", tag), "\\s+(.*)\\s*$")
        g <- stringr::str_match(line, regex)

        # Exit loop
        if (is.na(g[1, 1]) && in.tag)
            break

        # Store line
        if ( ! is.na(g[1, 1])) {
            s <- g[1, 2]
            lines <- c(lines, s)
            in.tag <- TRUE
        }
    }

    return(lines)
},

parseMultilinesField=function(field, tag, parsed.content, strip.chars=' ',
    split.char=' ') {

    # Get tag lines
    lines <- private$getTagLines(tag=tag, parsed.content=parsed.content)

    # Split on character
    if ( ! is.na(split.char))
        lines <- unlist(strsplit(lines, paste0(split.char, "+"), perl=TRUE))

    value <- sub(paste0('[', strip.chars, ']+$'), '',
        sub(paste0('^[', strip.chars, ']+'), '', lines))

    # Set field value
    if (length(value) > 0)
        self$setFieldValue(field, value)
},

parseNames=function(parsed.content, strip.chars=' ;',
    split.char=NA_character_) {

    private$parseMultilinesField(field='name', tag='NAME',
                                parsed.content=parsed.content,
                                strip.chars=strip.chars, split.char=split.char)
},

parseOrthologyIds=function(parsed.content) {
    ids <- private$getTagLines(tag='ORTHOLOGY',
        parsed.content=parsed.content)
    if (length(ids) > 0) {
        ids <- sub('^\\s*(K[0-9]+)\\s+.*$', '\\1', ids)
        self$setFieldValue('kegg.orthology.id', ids)
    }
},

parseModuleIds=function(parsed.content) {
    module.ids <- private$getTagLines(tag='MODULE',
        parsed.content=parsed.content)
    if (length(module.ids) > 0) {
        module.ids <- sub('^\\s*[A-Za-z_]*(M[0-9]+)\\s+.*$', '\\1', module.ids)
        self$setFieldValue('kegg.module.id', module.ids)
    }
},

parsePathwayIds=function(parsed.content) {
    pathway.ids <- private$getTagLines(tag='PATHWAY',
        parsed.content=parsed.content)
    if (length(pathway.ids) > 0) {
        pathway.ids <- sub('^\\s*([^ ]+)\\s+.*$', '\\1', pathway.ids)
        self$setFieldValue('kegg.pathway.id', pathway.ids)
    }
},

parseCompoundIds=function(parsed.content) {
    compound.ids <- private$getTagLines(tag='COMPOUND',
        parsed.content=parsed.content)
    if (length(compound.ids) > 0) {
        compound.ids <- sub('^\\s*(C[0-9]+)\\s+.*$', '\\1', compound.ids)
        self$setFieldValue('kegg.compound.id', compound.ids)
    }
},

parseDbLinks=function(parsed.content) {

    abbrev_to_db <- c(
        RN='kegg.reaction.id',
        'NCBI-GeneID'='ncbi.gene.id',
        'UniProt'='uniprot.id',
        'CAS'='cas.id',
        'ExPASy - ENZYME nomenclature database'='expasy.enzyme.id',
        'ChEBI'='chebi.id',
        'LIPIDMAPS'='lipidmaps.structure.id',
        'PubChem'='ncbi.pubchem.comp.id'
    )

    # Extract DB links
    dblinks <- private$getTagLines(tag='DBLINKS', parsed.content=parsed.content)

    if (length(dblinks) > 0) {

        # Extract 
        lnks <- stringr::str_match(dblinks, '^([A-Za-z -]+): +(.+)$')
        lnks <- data.frame(db=lnks[, 2], id=lnks[, 3], stringsAsFactors=FALSE)

        # Translate db abbrev to biodb db ID
        fct <- function(x) {
            if (x %in% names(abbrev_to_db))
                abbrev_to_db[[x]]
            else
                NA_character_
        }
        lnks$dbid <- vapply(lnks$db, fct, FUN.VALUE='')

        # Remove unknown databases
        lnks <- lnks[ ! is.na(lnks$dbid), ]

        # Set fields
        for (i in seq_along(lnks[[1]]))
            self$setFieldValue(lnks[i, 'dbid'], lnks[i, 'id'])

        # Split Uniprot IDs
        if (self$hasField('uniprot.id')) {
            ids <- strsplit(self$getFieldValue('uniprot.id'),
                            ' +', perl=TRUE)[[1]]
            self$setFieldValue('uniprot.id', ids)
        }
    }
},

parseGenesIds=function(parsed.content) {

    lines <- private$getTagLines(tag='GENES', parsed.content=parsed.content)

    if (length(lines) > 0) {
        genes.ids <- character()
        m <- stringr::str_match(lines, "^\\s*([^:]+):\\s*(.*)\\s*$")
        org <- tolower(m[, 2])
        genes <- gsub('\\([^)]+\\)', '', m[, 3], perl=TRUE)
        for (i in seq_along(org)) {
            ids <- strsplit(genes[[i]], ' ')[[1]]
            fct <- function(gene) paste(org[[i]], gene, sep=':')
            genes.ids <- c(genes.ids, vapply(ids, fct, FUN.VALUE=''))
        }
        self$setFieldValue('kegg.genes.id', genes.ids)
    }
},

parseReactionIds=function(parsed.content) {

    rids <- c(private$getTagLines(tag='REACTION',
        parsed.content=parsed.content), private$getTagLines(tag='ALL_REAC',
        parsed.content=parsed.content))
    if (length(rids) > 0) {
        rids <- stringr::str_match_all(rids, '(^|[ +,])(R[0-9]+)')
        rids <- unlist(lapply(rids, function(x) x[,3]))
        self$setFieldValue('kegg.reaction.id', rids)
    }
}
))
