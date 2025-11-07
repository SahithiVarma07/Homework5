## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos)) {
    return("the lenght of val and pos should be equal")
  }
  if (length(object@length) != 1L || object@length <= 0) {
    return("length needs to be a pos integer and single.")
  }
  if (any(object@pos <= 0) || any(object@pos > object@length)) {
    return("pos needs to have valid indices")
  }
  TRUE
})


# numeric to sparse
setAs("numeric", "sparse_numeric", function(from) {
  num <- which(from != 0)
  new("sparse_numeric",
      value = from[num],
      pos = as.integer(num),
      length = as.integer(length(from)))
})

#  sparse to numeric
setAs("sparse_numeric", "numeric", function(from) {
  result <- numeric(from@length)
  result[from@pos] <- from@value
  result
})

setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))


combine_sparse <- function(x, y, op) {
  if (x@length != y@length) stop("lengths dont match")
  all_pos <- sort(unique(c(x@pos, y@pos)))
  vals <- numeric(length(all_pos))
  
  for (i in seq_along(all_pos)) {
    pos <- all_pos[i]
    val_x <- if (pos %in% x@pos) x@value[which(x@pos == pos)] else 0
    val_y <- if (pos %in% y@pos) y@value[which(y@pos == pos)] else 0
    vals[i] <- op(val_x, val_y)
  }
  
  num_idx <- which(vals != 0)
  new("sparse_numeric", 
      value = vals[num_idx], 
      pos = as.integer(all_pos[num_idx]), 
      length = x@length)
}

setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y) combine_sparse(x, y, function(a, b) a + b))

setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y) combine_sparse(x, y, function(a, b) a - b))

setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y) combine_sparse(x, y, function(a, b) a * b))

setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length) stop("Lengths must match.")
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0) return(0)
            sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
          })

setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))

setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse numeric vector of length", object@length, "\n")
  cat("non zero positions:", object@pos, "\n")
  cat("nonzero values:", object@value, "\n")
})


setMethod("plot", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            plot(x@pos, x@value, col = "green", pch = 16,
                 xlab = "Position", ylab = "Value",
                 main = "Overlap of Sparse Vectors")
            points(y@pos, y@value, col = "pink", pch = 17)
            legend("topright", legend = c("x", "y"),
                   col = c("green", "pink"), pch = c(16, 17))
          })
#added function
setGeneric("sparse_norm", function(x) standardGeneric("sparse_norm"))
setMethod("sparse_norm", "sparse_numeric", function(x) sqrt(sum(x@value^2)))

