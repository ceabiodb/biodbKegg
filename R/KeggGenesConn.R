
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
#' mybiodb <- biodb::newInst()
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
#' @import R6
#' @include KeggConn.R
#' @export
KeggGenesConn <- R6::R6Class("KeggGenesConn",
inherit=KeggConn,


public=list(

#' @description
#' New instance initializer. Connector classes must not be instantiated
#' directly. Instead, you must use the createConn() method of the factory class.
#' @param ... All parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {
    super$initialize(db.name='genes', ...)
},

#' @description
#' Gets organism pathways for each gene. This method retrieves for
#'     each gene the KEGG pathways of the organism in which the gene is
#'     involved.
#' @param id A character vector of KEGG Gene IDs.
#' @param org The organism in which to search for pathways, as a KEGG organism code
#'     (3-4 letters code, like 'hsa', 'mmu', ...). See
#' @param https //www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
#'     organism codes.
#' @param limit The maximum number of modules IDs to retrieve for each gene.
#'     Set to 0 to disable.
#' @return A named list of KEGG pathway ID vectors, where the names
#'     of the list are the gene IDs."
getPathwayIdsPerGene=function(id, org, limit=3) {
    pathways <- list()

    fac <- self$getBiodb()$getFactory()

    # Loop on all gene ids
    prg <- biodb::Progress$new(biodb=self$getBiodb(),
                               msg='Retrieving pathways of genes.',
                               total=length(id))
    for (gene.id in id) {

        pws <- NULL

        # Send progress message
        prg$increment()

        # Get gene entry
        gene <- self$getEntry(gene.id)
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

#' @description
#' Gets organism pathways. This method retrieves KEGG pathways of the
#'     specified organism in which the genes are involved.
#' @param id A character vector of KEGG Genes IDs.
#' @param org The organism in which to search for pathways, as a KEGG organism code
#'     (3-4 letters code, like 'hsa', 'mmu', ...). See
#' @param https //www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
#'     organism codes.
#' @return A vector of KEGG pathway IDs.
getPathwayIds=function(id, org) {

    pathways <- self$getPathwayIdsPerGene(id=id, org=org)
    pathways <- unique(unlist(pathways, use.names=FALSE))

    return(pathways)
}
),

private=list(
))
