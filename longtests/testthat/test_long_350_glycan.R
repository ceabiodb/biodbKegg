# Set test context
biodb::testContext("KEGG Glycan long tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('kegg.glycan')

# Run generic tests
biodb::runGenericTests(conn, short=FALSE, long=TRUE, opt=list(max.results=3))

# Terminate Biodb
biodb$terminate()
