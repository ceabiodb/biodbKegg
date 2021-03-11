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
#' @include KeggShape.R
#' @export KeggRect
#' @exportClass KeggRect
#'
#' @examples
#' # Create a rectangle instance
#' r <- KeggRect(left=10, top=10, bottom=20, right=30, color='yellow')
#'
#' # Draw a rectangle on current image
#' \donttest{
#' r$draw()
#' }
#'
KeggRect <- methods::setRefClass('KeggRect',
    contains='KeggShape',
    fields=list(.left='integer',
                .bottom='integer',
                .right='integer',
                .top='integer'),

methods=list(

initialize=function(left, top, bottom, right, ...) {
    callSuper(...)
    .self$.left     <- as.integer(left)
    .self$.right    <- as.integer(right)
    .self$.top      <- as.integer(top)
    .self$.bottom   <- as.integer(bottom)
},

equals=function(other) {
    # Overrides super class' method.
    
    eq <- FALSE
    
    if (methods::is(other, "KeggRect")) {
        eq <- .self$.left == other$.left && .self$.right == other$.right &&
            .self$.top == other$.top && .self$.bottom == other$.bottom
    }
 
    return(eq)
},

draw=function() {
    # Overrides super class' method.

    rect(.self$.left, .self$.bottom, .self$.right, .self$.top,
        col=.self$getRgbColor(alpha=127), border=NA)
}

))
