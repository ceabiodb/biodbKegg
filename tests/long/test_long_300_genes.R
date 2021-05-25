# Set test context
biodb::testContext("KEGG Genes long tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('kegg.genes')

# Run generic tests
biodb::runGenericTests(conn, short=FALSE, long=TRUE, list(max.results=20))

# Terminate Biodb
biodb$terminate()
