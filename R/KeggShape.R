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
#' c <- KeggCircle$new(x=12, y=5, r=3, label='MyCircle')
#'
#' # Create a rectangle instance
#' r <- KeggRect$new(left=10, top=10, bottom=20, right=30, color='yellow')
#'
#' @import R6
#' @export
KeggShape <- R6::R6Class('KeggShape',

public=list(

#' @description
#' Initialize new instance.
#' @param label The text label to display.
#' @param color The color to use.
#' @return Nothing.
initialize=function(label=NA_character_, color=NA_character_) {
    private$label <- label
    private$color <- color
},

#' @description
#' Test if this shape is the same as another.
#' @param other The other shape to compare with.
#' @return TRUE or FALSE.
equals=function(other) {
    return(private$doesEqual(other))
},

#' @description
#' Gets the label associated with this shape.
#' @return The label.
getLabel=function() {

    return(private$label)
},

#' @description
#' Gets the color associated with this shape.
#' @return The color name as a string.
getColor=function() {

    return(private$color)
},

#' @description
#' Gets the RGB color associated with this shape.
#' @param alpha The value to use for the alpha channel when building the RGB color
#'     object.
#' @return The color as an RGB color object.
getRgbColor=function(alpha=255) {

    c <- col2rgb(private$color)
    c <- rgb(c[1,], c[2,], c[3,], alpha, maxColorValue=255)

    return(c)
},

#' @description
#' Draw the shape on the current image.
#' @return None.
draw=function() {
    private$doDraw()
    return(invisible(NULL))
}
),

private=list(
    label=NULL,
    color=NULL

,doesEqual=function(other) {
    return(FALSE)
}

,doDraw=function() {
}
))
