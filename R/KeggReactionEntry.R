
#' KEGG Reaction entry class.
#'
#' This is the entry class for KEGG Reation database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
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
#' @export
KeggReactionEntry <- R6::R6Class("KeggReactionEntry",
inherit=KeggEntry,


public=list(

initialize=function(...) {

    super$initialize(...)
}
),

private=list(
parseFieldsStep2=function(parsed.content) {

    # Name
    private$parseNames(parsed.content)

    # Other KEGG IDs
    private$parseMultilinesField(field='kegg.enzyme.id', tag='ENZYME',
                                parsed.content=parsed.content)
    private$parsePathwayIds(parsed.content=parsed.content)
    private$parseModuleIds(parsed.content)

    # Parse subtrates and products
    if (self$hasField('equation')) {
        s <- gsub(' ', '', self$getFieldValue('equation')) # Remove spaces
        s <- strsplit(strsplit(s, '<=>')[[1]], '\\+')
        if (length(s) == 2) {
            self$setFieldValue('substrates', s[[1]])
            self$setFieldValue('products', s[[2]])
        }
        else
            self$caution('Unable to parse equation "',
                self$getFieldValue('equation'),
                '" of KEGG reaction ', self$getFieldValue('accession'), '.')
    }
}
))
