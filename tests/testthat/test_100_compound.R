test.kegg.compound.wsList <- function(conn) {

	results <- conn$wsList(retfmt='ids')
	testthat::expect_is(results, 'character')
	testthat::expect_true(length(results) > 100)
}

test.kegg.compound.wsFind <- function(db) {

	results <- db$wsFind(query = 'NADPH')
	testthat::expect_true( ! is.null(results))
	testthat::expect_true( ! is.na(results))
	testthat::expect_true(is.character(results))
	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)
	testthat::expect_true(nrow(df) > 1)
	testthat::expect_true(df[1, 1] == 'cpd:C00005')

	df.2 <- db$wsFind(query = 'NADPH', retfmt = 'parsed') 
	testthat::expect_is(df.2, 'data.frame')
	testthat::expect_true(identical(df, df.2))

	ids <- db$wsFind(query = 'NADPH', retfmt = 'ids') 
	testthat::expect_is(ids, 'character')
	testthat::expect_true(identical(ids, df[[1]]))
	testthat::expect_true(ids[[1]] == 'cpd:C00005')
}

test.kegg.compound.wsFindExactMass <- function(db) {

	# Test single mass
	results <- db$wsFindExactMass(mass=174.05)
	expect_true( ! is.null(results))
	expect_true( ! is.na(results))
	expect_true(is.character(results))
	readtc <- textConnection(results, "r", local=TRUE)
	df <- read.table(readtc, sep="\t", quote='', stringsAsFactors=FALSE)
	expect_true(nrow(df) > 1)

	# Test data frame
	df.2 <- db$wsFindExactMass(mass=174.05, retfmt='parsed')
	expect_true(identical(df, df.2))

	# Test IDs
	ids <- db$wsFindExactMass(mass=174.05, retfmt='ids')
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(identical(df[[1]], ids))

	# Test mass range
	ids <- db$wsFindExactMass(mass.min=174, mass.max=174.35, retfmt='ids')
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(length(ids) > 1)
}

test.kegg.compound.wsFindMolecularWeight <- function(db) {

	# Test single mass
	results <- db$wsFindMolecularWeight(mass = 300)
	expect_true( ! is.null(results))
	expect_true( ! is.na(results))
	expect_true(is.character(results))
	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)
	expect_true(nrow(df) > 1)

	# Test data frame
	df.2 <- db$wsFindMolecularWeight(mass = 300, retfmt = 'parsed')
	expect_true(identical(df, df.2))

	# Test IDs
	ids <- db$wsFindMolecularWeight(mass = 300, retfmt = 'ids')
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(identical(df[[1]], ids))

	# Test mass range
	ids <- db$wsFindMolecularWeight(mass.min = 300, mass.max = 310, retfmt = 'ids')
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(length(ids) > 1)
}

test.kegg.compound.getPathwayIds_issue_333_20190507 <- function(conn) {

	# Compound
	c <- 'C05402'

	# Pathways returned that contain this compound in their compound list.
	goodpws <- c('mmu00052', 'mmu01100', 'mmu02010')

	# Pathways linked through gene that do not contain this compound in their compound list.
	wrongpws <- c("mmu00561", "mmu00600", "mmu00603", "mmu04142")

	# Get all pathways
	ids <- conn$getPathwayIds(c, 'mmu')
	testthat::expect_is(ids, 'character')
	testthat::expect_true(length(ids) > 0)
	testthat::expect_true(any(goodpws %in% ids))
	testthat::expect_false(any(wrongpws %in% ids))
}

test.kegg.compound.getPathwayIds_issue_338_20190517 <- function(conn) {

	# Compound
	atp <- 'C00002'
	adp <- 'C00008'

	# Pathway that should be found (among others)
	pw <- 'mmu00190'

	# Get all pathways
	for (comp in c(adp, atp)) {
		ids <- conn$getPathwayIds(comp, 'mmu')
		testthat::expect_is(ids, 'character')
		testthat::expect_true(length(ids) > 0)
		testthat::expect_true(pw %in% ids)
	}
}

test.kegg.compound.getPathwayIds <- function(conn) {
	comp <- 'C00134'
	ids <- conn$getPathwayIds(comp, 'mmu')
	testthat::expect_is(ids, 'character')
	testthat::expect_true(length(ids) > 0)
}

test.kegg.compound.getPathwayIdsPerCompound <- function(conn) {
	c = 'C00134'
	ids = conn$getPathwayIdsPerCompound(c, 'mmu')
	testthat::expect_is(ids, 'list')
	testthat::expect_true(c %in% names(ids))
	testthat::expect_is(ids[[c]], 'character')
	testthat::expect_true(length(ids[[c]]) > 0)
}

test.addInfo <- function(conn) {

    x <- data.frame(ids=c('C06178', 'C01771', 'C06144'), col2=c(1, 4, 8))
    z <- data.frame()

    y <- conn$addInfo(x, id.col='ids', org='mmu')
    testthat::expect_identical(c('ids', 'col2', 'kegg.enzyme.id',
                                 'kegg.reaction.id', 'kegg.pathway.id',
                                 'kegg.pathway.name',
                                 'kegg.pathway.pathway.class', 'kegg.module.id',
                                 'kegg.module.name'), colnames(y))
    testthat::expect_equal(nrow(y), 3)
    testthat::expect_false(any(is.na(y[1, ])))
    testthat::expect_true(all(is.na(y[2, c('kegg.pathway.id',
                                           'kegg.pathway.name',
                                           'kegg.pathway.pathway.class')])))
    testthat::expect_false(any(is.na(y[3, ])))

    # Second test
    x2 <- data.frame(ids=c('C06144', 'C06178', 'C02659'))
    y2 <- conn$addInfo(x2, id.col='ids', org='mmu')
}

# Set context
biodb::testContext("Test Kegg Compound connector.")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
file <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(file)

# Create connector
conn <- biodb$getFactory()$createConn('kegg.compound')

# Run tests
testRefFolder <- system.file("testref", package='biodbKegg')
biodb::runGenericTests(conn, pkgName='biodbKegg', testRefFolder=testRefFolder)
biodb::testThat('wsList() works correctly.', test.kegg.compound.wsList, conn=conn)
biodb::testThat('wsFind() works correctly.', test.kegg.compound.wsFind, conn=conn)
biodb::testThat('wsFindExactMass() works correctly.', test.kegg.compound.wsFindExactMass, conn=conn)
biodb::testThat('wsFindMolecularWeight() works correctly.', test.kegg.compound.wsFindMolecularWeight, conn=conn)
biodb::testThat('getPathwayIdsPerCompound() works correctly.', test.kegg.compound.getPathwayIdsPerCompound, conn=conn)
biodb::testThat('getPathwayIds() works correctly.', test.kegg.compound.getPathwayIds, conn=conn)
biodb::testThat('getPathwayIds() issue_333 is corrected', test.kegg.compound.getPathwayIds_issue_333_20190507, conn=conn)
biodb::testThat('getPathwayIds() issue_338 is corrected', test.kegg.compound.getPathwayIds_issue_338_20190517, conn=conn)
biodb::testThat('addInfo() works correctly.', test.addInfo, conn=conn)

# Terminate Biodb
biodb$terminate()
