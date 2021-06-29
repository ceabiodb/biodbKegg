test.rect <- function(biodb) {

	rect <- KeggRect$new(label='A', color='red', left=10L, right=20L, top=5L,
        bottom=40L)
	expect_is(rect, 'KeggRect')
}

test.circle <- function(biodb) {

	circle <- KeggCircle$new(label='A', color='red', x=10L, y=20L, r=5L)
	expect_is(circle, 'KeggCircle')
}

# Set context
biodb::testContext("Test shapes.")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbKegg')
biodb$loadDefinitions(defFile)

# Run tests
biodb::testThat("We can create a rectangle object.", test.rect, biodb=biodb)
biodb::testThat("We can create a circle object.", test.circle, biodb=biodb)

# Terminate Biodb
biodb$terminate()
