# Set test context
biodb::testContext("KEGG Pathway long tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('kegg.pathway')

# Run generic tests
testRefFolder <- system.file("testref", package='biodbKegg')
biodb::runGenericTests(conn, pkgName='biodbKegg', short=FALSE, long=TRUE,
    testRefFolder=testRefFolder, opt=list(max.results=1))

# Terminate Biodb
biodb$terminate()
