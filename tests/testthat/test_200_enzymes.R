
test.kegg.enzyme.getPathwayIds = function(conn) {
    c = '1.2.1.3'
    ids = conn$getPathwayIds(c, 'mmu')
    testthat::expect_is(ids, 'character')
    testthat::expect_true(length(ids) > 0)
}

test.kegg.issue_340 <- function(conn) {
    
    # Check that the right pathways are returned by getPathwaysIds()
    enz <- '1.14.14.1'
    org <- 'mmu'
    
    # Get pathways
    pws <- conn$getPathwayIds(enz, org = org)
    
    # Define wrong & right pathways to check
    wrong_pws <- c('mmu04913', 'mmu00627', 'mmu01110', 'mmu01120')
    right_pws <- c(
        'mmu00071',
        'mmu00140',
        'mmu00232',
        'mmu00380',
        'mmu00590',
        'mmu00591',
        'mmu00830',
        'mmu00980',
        'mmu00982',
        'mmu01100')
    
    # Check
    testthat::expect_false(any(wrong_pws %in% pws))
    testthat::expect_true(all(right_pws %in% pws))
}

test.addInfo <- function(conn) {

    x <- data.frame(ids=c('C06178', 'C01771'), col2=c(1, 4))
    z <- data.frame()

    y <- conn$addInfo(x, id.col='ids', org='mmu')
    testthat::expect_identical(c('ids', 'col2', 'kegg.enzyme.id',
                                 'kegg.reaction.id', 'kegg.pathway.id',
                                 'kegg.pathway.name',
                                 'kegg.pathway.pathway.class', 'kegg.module.id',
                                 'kegg.module.name'), colnames(y))
    testthat::expect_equal(nrow(y), 2)
    testthat::expect_false(any(is.na(y[1, ])))
    testthat::expect_true(all(is.na(y[2, c('kegg.pathway.id',
                                           'kegg.pathway.name',
                                           'kegg.pathway.pathway.class')])))

    # Second test
    x2 <- data.frame(ids=c('C06144', 'C06178', 'C02659'))
    y2 <- conn$addInfo(x2, id.col='ids', org='mmu')
}

# Set context
biodb::testContext("Test Kegg Enzyme connector.")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
file <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(file)

# Create connector
conn <- biodb$getFactory()$createConn('kegg.enzyme')

# Run tests
biodb::runGenericTests(conn)
biodb::testThat('getPathwayIds() works correctly.',
          test.kegg.enzyme.getPathwayIds, conn=conn)
biodb::testThat('issue 340 is corrected.',
          test.kegg.issue_340, conn=conn)

# Terminate Biodb
biodb$terminate()
