#' A class for representing a shape.
#'
#' This abstract class represents a shape, used for graphical representation.
#'
#' Arguments to the constructor are:
#'
#' label: A text label to associate with the shape.
#'
#' color: A color, as a character string.
#'
#' @seealso \code{\link{KeggRect}}, \code{\link{KeggCircle}}.
#'
#' @examples
#' # Create a circle instance
#' c <- KeggCircle(x=12, y=5, r=3, label='MyCircle')
#'
#' # Create a rectangle instance
#' r <- KeggRect(left=10, top=10, bottom=20, right=30, color='yellow')
#'
#' @export KeggShape
#' @exportClass KeggShape
KeggShape <- methods::setRefClass('KeggShape',
    fields=list(.label='character',
                .color='character'),

methods=list(

initialize=function(label=NA_character_,
                    color=NA_character_) {
    .self$.label <- label
    .self$.color <- color
},

equals=function(other) {
    "\n\nTest if this shape is the same as another.
    \nother: The other shape to compare with.
    \nReturned value: TRUE or FALSE.
    "

    return(FALSE)
},

getLabel=function() {
    ":\n\nGets the label associated with this shape.
    \nReturned value: The label.
    "

    return(.self$.label)
},

getColor=function() {
    ":\n\nGets the color associated with this shape.
    \nReturned value: The color name as a string.
    "

    return(.self$.color)
},

getRgbColor=function(alpha=255) {
    ":\n\nGets the RGB color associated with this shape.
    \nalpha: The value to use for the alpha channel when building the RGB color
    object.
    \nReturned value: The color as an RGB color object.
    "

    c <- col2rgb(.self$.color)
    c <- rgb(c[1,], c[2,], c[3,], alpha, maxColorValue=255)

    return(c)
},

draw=function() {
    ":\n\nDraw the shape on the current image.
    \nReturned value: None.
    "

    invisible()
}

))
