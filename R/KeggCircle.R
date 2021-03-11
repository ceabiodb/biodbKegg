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
#' c1 <- KeggCircle(x=12, y=5, r=3)
#'
#' # Since it inherits from KeggShape, a color and a label can be set
#' c2 <- KeggCircle(x=12, y=5, r=3, color='blue', label='Circle 2')
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
#' @import methods
#' @include KeggShape.R
#' @export KeggCircle
#' @exportClass KeggCircle
KeggCircle <- methods::setRefClass('KeggCircle',
    contains='KeggShape',
    fields=list(.x='integer',
                .y='integer',
                .r='integer'),

methods=list(

initialize=function(x, y, r, ...) {
    callSuper(...)
    .self$.x <- as.integer(x)
    .self$.y <- as.integer(y)
    .self$.r <- as.integer(r)
},

equals=function(other) {
    # Overrides super class' method.
    
    eq <- FALSE
    
    if (methods::is(other, "KeggCircle")) {
        eq <- .self$.x == other$.x && .self$.y == other$.y &&
            .self$.r == other$.r
    }
 
    return(eq)
},

getX=function() {
    ":\n\nGet the X coordinate.
    \nReturned value: The X coordinate.
    "

    return(.self$.x)
},

getY=function() {
    ":\n\nGet the Y coordinate.
    \nReturned value: The Y coordinate.
    "

    return(.self$.y)
},

getRadius=function() {
    ":\n\nGet the radius.
    \nReturned value: The radius.
    "

    return(.self$.r)
},

draw=function() {
    # Overrides super class' method.

    symbols(x=.self$.x, y=.self$.y,
            circles=.self$.r,
            bg=.self$getRgbColor(alpha=127),
            add=TRUE, inches=FALSE)
}

))
