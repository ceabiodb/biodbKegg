test.kegg.genes.getPathwayIdsPerGene <- function(conn) {
    id <- 'mmu:14635'
	ids <- conn$getPathwayIdsPerGene(id, org='mmu')
	testthat::expect_is(ids, 'list')
	testthat::expect_true(id %in% names(ids))
	testthat::expect_is(ids[[id]], 'character')
	testthat::expect_true(length(ids[[id]]) > 0)
    print(ids)
}

test.kegg.genes.getPathwayIds <- function(conn) {
    id <- 'mmu:14635'
    ids <- conn$getPathwayIds(id, org='mmu')
    testthat::expect_is(ids, 'character')
    testthat::expect_true(length(ids) > 0)
    print(ids)
}

# Main
################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='kegg_genes_test.log', ack=TRUE)

# Load package definitions
file <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(file)

# Set context
biodb::setTestContext(biodb, "Test Kegg Genes connector.")

# Create connector
conn <- biodb$getFactory()$createConn('kegg.genes')

# Run tests
biodb::runGenericTests(conn)
biodb::testThat('getPathwayIdsPerGene() works correctly.',
                test.kegg.genes.getPathwayIdsPerGene, conn=conn)
biodb::testThat('getPathwayIds() works correctly.',
                test.kegg.genes.getPathwayIds, conn=conn)

# Terminate Biodb
biodb$terminate()
