#' A class for representing a circle.
#'
#' This class represents a rectangle, used for graphical
#' representation. It is used by
#' KeggPathwayConn::extractPathwayMapShapes() method.
#'
#' Arguments to the constructor are:
#'
#' x: X coordinate of center.
#'
#' y: Y coordinate of center.
#'
#' r: Radius.
#'
#' @seealso \code{\link{KeggShape}}, \code{\link{KeggRect}}.
#'
#' @examples
#' # Create an instance
#' c1 <- KeggCircle$new(x=12, y=5, r=3)
#'
#' # Since it inherits from KeggShape, a color and a label can be set
#' c2 <- KeggCircle$new(x=12, y=5, r=3, color='blue', label='Circle 2')
#'
#' # Getting center
#' c1$getX()
#' c1$getY()
#'
#' # Getting radius
#' c1#getRadius()
#'
#' # Draw a circle on the current image
#' \donttest{
#' c1$draw()
#' }
#'
#' @import R6
#' @include KeggShape.R
#' @export
KeggCircle <- R6::R6Class('KeggCircle',
inherit=KeggShape,

public=list(

#' @description
#' Initialize new instance.
#' @param x Abscissa of the circle's center.
#' @param y Ordinate of the circle's center.
#' @param r Radius of the circle.
#' @param ... Additional parameters are passed to super class' initializer. 
#' @return Nothing.
initialize=function(x, y, r, ...) {
    super$initialize(...)
    private$x <- as.integer(x)
    private$y <- as.integer(y)
    private$r <- as.integer(r)
},

#' @description
#' Get the X coordinate.
#' @return The X coordinate.
getX=function() {
    return(private$x)
},

#' @description
#' Get the Y coordinate.
#' @return The Y coordinate.
getY=function() {
    return(private$y)
},

#' @description
#' Get the radius.
#' @return The radius.
getRadius=function() {
    return(private$r)
}
),

private=list(
    x=NULL,
    y=NULL,
    r=NULL

,doesEqual=function(other) {
    
    eq <- FALSE
    
    if (methods::is(other, "KeggCircle")) {
        eq <- private$x == other$.__enclos_env__$private$x &&
            private$y == other$.__enclos_env__$private$y &&
            private$r == other$.__enclos_env__$private$r
    }
 
    return(eq)
}

,doDraw=function() {
    # Overrides super class' method.

    symbols(x=private$x, y=private$y, circles=private$r,
        bg=self$getRgbColor(alpha=127), add=TRUE, inches=FALSE)
}
))
