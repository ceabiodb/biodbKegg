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
biodb::runGenericTests(conn, short=FALSE, long=TRUE, list(max.results=1))

# Terminate Biodb
biodb$terminate()
