
#' KEGG Reaction entry class.
#'
#' This is the entry class for KEGG Reation database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.reaction')
#'
#' # Get an entry
#' e <- conn$getEntry('R00105')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggEntry.R
#' @export KeggReactionEntry
#' @exportClass KeggReactionEntry
KeggReactionEntry <- methods::setRefClass("KeggReactionEntry",
    contains='KeggEntry',

methods=list(

initialize=function(...) {

    callSuper(...)
},

.parseFieldsStep2=function(parsed.content) {

    # Name
    .self$.parseNames(parsed.content)

    # Other KEGG IDs
    .self$.parseMultilinesField(field='kegg.enzyme.id', tag='ENZYME',
                                parsed.content=parsed.content)
    .self$.parsePathwayIds(parsed.content=parsed.content)
    .self$.parseModuleIds(parsed.content)

    # Parse subtrates and products
    if (.self$hasField('equation')) {
        s <- gsub(' ', '', .self$getFieldValue('equation')) # Remove spaces
        s <- strsplit(strsplit(s, '<=>')[[1]], '\\+')
        if (length(s) == 2) {
            .self$setFieldValue('substrates', s[[1]])
            .self$setFieldValue('products', s[[2]])
        }
        else
            .self$caution('Unable to parse equation "',
                .self$getFieldValue('equation'),
                '" of KEGG reaction ', .self$getFieldValue('accession'), '.')
    }
}

))
