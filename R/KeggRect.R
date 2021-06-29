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

initialize=function(left, top, bottom, right, ...) {
    super$initialize(...)
    private$left     <- as.integer(left)
    private$right    <- as.integer(right)
    private$top      <- as.integer(top)
    private$bottom   <- as.integer(bottom)
},

equals=function(other) {
    # Overrides super class' method.
    
    eq <- FALSE
    
    if (methods::is(other, "KeggRect")) {
        eq <- private$left == other$.__enclos_env__$private$left &&
            private$right == other$.__enclos_env__$private$right &&
            private$top == other$.__enclos_env__$private$top &&
            private$bottom == other$.__enclos_env__$private$bottom
    }
 
    return(eq)
},

draw=function() {
    # Overrides super class' method.

    rect(private$left, private$bottom, private$right, private$top,
        col=self$getRgbColor(alpha=127), border=NA)
}
),

private=list(
    left=NULL,
    bottom=NULL,
    right=NULL,
    top=NULL
))
