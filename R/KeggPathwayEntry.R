
#' KEGG Pathway entry class.
#'
#' This is the class entry for KEGG Pathway database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.pathway')
#'
#' # Get an entry
#' e <- conn$getEntry('map00053')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggEntry.R
#' @export KeggPathwayEntry
#' @exportClass KeggPathwayEntry
KeggPathwayEntry <- methods::setRefClass("KeggPathwayEntry",
    contains='KeggEntry',

methods=list(

initialize=function(...) {
    callSuper(...)
},

.makesRefToEntryRecurse=function(db, oid) {

    makes_ref <- FALSE

    if (db %in% c('kegg.compound', 'kegg.enzyme')
        && .self$hasField('kegg.module.id')) {

        # We need to check that oid is listed in at least one of the modules
        kmc <- .self$getBiodb()$getFactory()$getConn('kegg.module')
        module.ids <- .self$getFieldValue('kegg.module.id')
        makes_ref <- kmc$makesRefToEntry(module.ids, db=db, oid=oid,
            any=TRUE, recurse=TRUE)
    }

    return(makes_ref)
},

.parseFieldsStep2=function(parsed.content) {

    # Name
    .self$.parseMultilinesField(field='name',
                                tag='NAME',
                                parsed.content=parsed.content,
                                strip.chars=' ;',
                                split.char=NA_character_)

    # Class
    .self$.parseMultilinesField(field='pathway.class',
                                tag='CLASS',
                                parsed.content=parsed.content,
                                strip.chars=' ',
                                split.char=';')

    # Module IDs
    .self$.parseModuleIds(parsed.content)

    # Compound IDs
    .self$.parseCompoundIds(parsed.content)
}

))
