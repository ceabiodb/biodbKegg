
#' The connector class to KEGG Pathway database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{BiodbFactory}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @seealso \code{\link{KeggConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.genes')
#'
#' # Get an entry
#' e <- conn$getEntry('mmu:14635')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @export KeggGenesConn
#' @exportClass KeggGenesConn
KeggGenesConn <- methods::setRefClass("KeggGenesConn",
    contains=c("KeggConn"),

methods=list(

initialize=function(...) {
    callSuper(db.name='genes', ...)
},

getPathwayIdsPerGene=function(id, org, limit=3) {
    ":\n\nGets organism pathways for each gene. This method retrieves for
    each gene the KEGG pathways of the organism in which the gene is
    involved.
    \nid: A character vector of KEGG Gene IDs.
    \norg: The organism in which to search for pathways, as a KEGG organism code
    (3-4 letters code, like 'hsa', 'mmu', ...). See
    https://www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
    organism codes.
    \nlimit: The maximum number of modules IDs to retrieve for each gene.
    Set to 0 to disable.
    \nReturned value: A named list of KEGG pathway ID vectors, where the names
    of the list are the gene IDs."

    pathways <- list()

    fac <- .self$getBiodb()$getFactory()

    # Loop on all gene ids
    prg <- biodb::Progress$new(biodb=.self$getBiodb(),
                               msg='Retrieving pathways of genes.',
                               total=length(id))
    for (gene.id in id) {

        pws <- NULL

        # Send progress message
        prg$increment()

        # Get gene entry
        gene <- .self$getEntry(gene.id)
        if (is.null(gene))
            next

        # Does this gene have a list of pathways?
        if (gene$hasField('kegg.pathway.id')) {

            # Get pathways
            pws <- gene$getFieldValue('kegg.pathway.id')

            # Convert them to specified organism
            kegg.path.conn <- fac$getConn('kegg.pathway')
            pws <- kegg.path.conn$convertToOrgPathways(pws, org=org)
        }

        # Record found pathways
        if ( ! is.null(pws)) {
            if (limit > 0 && length(pws) > limit)
                pws <- pws[seq_len(limit)]
            pathways[[gene.id]] <- pws
        }
    }

    return(pathways)
},
getPathwayIds=function(id, org) {
    ":\n\nGets organism pathways. This method retrieves KEGG pathways of the
    specified organism in which the genes are involved.
    \nid: A character vector of KEGG Genes IDs.
    \norg: The organism in which to search for pathways, as a KEGG organism code
    (3-4 letters code, like 'hsa', 'mmu', ...). See
    https://www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
    organism codes.
    \nReturned value: A vector of KEGG pathway IDs.
    "

    pathways <- .self$getPathwayIdsPerGene(id=id, org=org)
    pathways <- unique(unlist(pathways, use.names=FALSE))

    return(pathways)
}

))
