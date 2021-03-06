# xmail - extract email addresses from strings and files
# Returns all emails found in given inputs as a named list
# @param {list.<character>} input List of plain strings, filepaths, can be mixed
# @return {list.<character>} Named list with extracted emails
xmail <- function(input=list(NULL)) {
  stopifnot(!missing(input), is.list(input))
  return(sapply(input, function(s) {
    if (file.exists(s)) {
      CON <- file(s)
      on.exit(close(CON))
      c <- paste(readLines(CON, warn=F), collapse=' ')
    } else {
      c <- paste(s, collapse=' ')
    }
    x <- list(unique(regmatches(c, gregexpr('[^\\.\\s@][^\\s@]*(?!\\.)@[^\\.\\s@]+(?:\\.[^\\.\\s@]+)*', c, perl=T))[[1]]))
    names(x) <- s
    return(x)
  }))
}