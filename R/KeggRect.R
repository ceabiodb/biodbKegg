#' A class for representing a rectangle.
#'
#' This class represents a rectangle, used for graphical
#' representation.
#'
#' Arguments to the constructor are:
#'
#' left: Coordinate of left border.
#'
#' right: Coordinate of right border.
#'
#' top: Coordinate of top border.
#'
#' bottom: Coordinate of bottom border.
#'
#' @seealso \code{\link{KeggShape}}, \code{\link{KeggCircle}}.
#'
#' @examples
#' # Create a rectangle instance
#' r <- KeggRect$new(left=10, top=10, bottom=20, right=30, color='yellow')
#'
#' # Draw a rectangle on current image
#' \donttest{
#' r$draw()
#' }
#'
#' @include KeggShape.R
#' @import R6
#' @export
KeggRect <- R6::R6Class('KeggRect',
inherit=KeggShape,

public=list(

#' @description
#' Initialize new instance.
#' @param left   Coordinate of rectangle's left side.
#' @param right  Coordinate of rectangle's right side.
#' @param top    Coordinate of rectangle's top side.
#' @param bottom Coordinate of rectangle's bottom side.
#' @param ... Additional parameters are passed to super class' initializer. 
#' @return Nothing.
initialize=function(left, top, bottom, right, ...) {
    super$initialize(...)
    private$left     <- as.integer(left)
    private$right    <- as.integer(right)
    private$top      <- as.integer(top)
    private$bottom   <- as.integer(bottom)
}
),

private=list(
    left=NULL,
    bottom=NULL,
    right=NULL,
    top=NULL

,doesEqual=function(other) {
    
    eq <- FALSE
    
    if (methods::is(other, "KeggRect")) {
        eq <- private$left == other$.__enclos_env__$private$left &&
            private$right == other$.__enclos_env__$private$right &&
            private$top == other$.__enclos_env__$private$top &&
            private$bottom == other$.__enclos_env__$private$bottom
    }

    return(eq)
}

,doDraw=function() {

    rect(private$left, private$bottom, private$right, private$top,
        col=self$getRgbColor(alpha=127), border=NA)
}
))
