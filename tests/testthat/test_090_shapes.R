test.rect <- function(biodb) {

	rect = KeggRect(label = 'A', color = 'red',
		left = 10L, right = 20L, top = 5L, bottom = 40L)
	expect_is(rect, 'KeggRect')
}

test.circle <- function(biodb) {

	circle = KeggCircle(label = 'A', color = 'red',
		x = 10L, y = 20L, r = 5L)
	expect_is(circle, 'KeggCircle')
}

# Main
################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='kegg_shapes_test.log', ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(defFile)

# Set context
biodb::setTestContext(biodb, "Test shapes.")

# Run tests
biodb::testThat("We can create a rectangle object.", test.rect, biodb=biodb)
biodb::testThat("We can create a circle object.", test.circle, biodb=biodb)

# Terminate Biodb
biodb$terminate()
