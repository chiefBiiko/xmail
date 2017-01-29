# xmail - extract email addresses from a string, file, or webpage

# Returns all emails found in given inputs as a named list
# @param {list.<character>} input List of plain strings, filepaths, URLs, can be mixed
# @return {list.<character>} Named list with extracted emails
xmail <- function(input=list(NULL)) {
  stopifnot(!missing(input), is.list(input))
  return(sapply(input, function(s) {
    if (file.exists(s)) {
      CON <- file(s)
      on.exit(close(CON))
      c <- paste(readLines(CON, warn=F), collapse=' ')
    } else if (grepl('^https?://[^\\.]+\\..+', s)) {
      CON <- url(s)
      on.exit(close(CON))
      c <- gsub('<[^>]*>', ' ', paste(readLines(CON, warn=F), collapse=' '))
    } else if (typeof(s)=='character') {
      c <- paste(s, collapse=' ')
    } else { stop('invalid input!') }
    x <- list(unique(regmatches(c, gregexpr('[^\\.\\s@][^\\s@]*(?!\\.)@[^\\.\\s@]+(?:\\.[^\\.\\s@]+)*', c, perl=T))[[1]]))
    names(x) <- s
    return(x)
  }))
}
