sv<-function (..., list = character(), file = "~/Desktop/tmp/t.RData",  
              ascii = FALSE, version = NULL, envir = parent.frame(), compress = isTRUE(!ascii), 
              compression_level, eval.promises = TRUE, precheck = TRUE) 
{
  opts <- getOption("save.defaults")
  if (missing(compress) && !is.null(opts$compress)) 
    compress <- opts$compress
  if (missing(compression_level) && !is.null(opts$compression_level)) 
    compression_level <- opts$compression_level
  if (missing(ascii) && !is.null(opts$ascii)) 
    ascii <- opts$ascii
  if (missing(version)) 
    version <- opts$version
  if (!is.null(version) && version < 2) 
    warning("Use of save versions prior to 2 is deprecated", 
            domain = NA)
  names <- as.character(substitute(list(...)))[-1L]
  if (missing(list) && !length(names)) 
    warning("nothing specified to be save()d")
  list <- c(list, names)
  if (!is.null(version) && version == 1) 
    .Internal(save(list, file, ascii, version, envir, eval.promises))
  else {
    if (precheck) {
      ok <- vapply(list, exists, NA, envir = envir)
      if (!all(ok)) {
        n <- sum(!ok)
        stop(sprintf(ngettext(n, "object %s not found", 
                              "objects %s not found"), paste(sQuote(list[!ok]), 
                                                             collapse = ", ")), domain = NA)
      }
    }
    if (is.character(file)) {
      if (!nzchar(file)) 
        stop("'file' must be non-empty string")
      if (!is.character(compress)) {
        if (!is.logical(compress)) 
          stop("'compress' must be logical or character")
        compress <- if (compress) 
          "gzip"
        else "no compression"
      }
      con <- switch(compress, bzip2 = {
        if (!missing(compression_level)) bzfile(file, 
                                                "wb", compression = compression_level) else bzfile(file, 
                                                                                                   "wb")
      }, xz = {
        if (!missing(compression_level)) xzfile(file, 
                                                "wb", compression = compression_level) else xzfile(file, 
                                                                                                   "wb", compression = 9)
      }, gzip = {
        if (!missing(compression_level)) gzfile(file, 
                                                "wb", compression = compression_level) else gzfile(file, 
                                                                                                   "wb")
      }, `no compression` = file(file, "wb"), stop(gettextf("'compress = \"%s\"' is invalid", 
                                                            compress)))
      on.exit(close(con))
    }
    else if (inherits(file, "connection")) 
      con <- file
    else stop("bad file argument")
    if (isOpen(con) && !ascii && summary(con)$text != "binary") 
      stop("can only save to a binary connection")
    .Internal(saveToConn(list, con, ascii, version, envir, 
                         eval.promises))
  }
}


ld<-function (file="~/Desktop/tmp/t.RData", envir = parent.frame(), verbose = FALSE) 
{
  if (is.character(file)) {
    con <- gzfile(file)
    on.exit(close(con))
    magic <- readChar(con, 5L, useBytes = TRUE)
    if (!length(magic)) 
      stop("empty (zero-byte) input file")
    if (!grepl("RD[ABX][2-9]\n", magic)) {
      if (grepl("RD[ABX][2-9]\r", magic)) 
        stop("input has been corrupted, with LF replaced by CR")
      warning(sprintf("file %s has magic number '%s'\n", 
                      sQuote(basename(file)), gsub("[\n\r]*", "", magic)), 
              "  ", "Use of save versions prior to 2 is deprecated", 
              domain = NA, call. = FALSE)
      return(.Internal(load(file, envir)))
    }
  }
  else if (inherits(file, "connection")) {
    con <- if (inherits(file, "gzfile") || inherits(file, 
                                                    "gzcon")) 
      file
    else gzcon(file)
  }
  else stop("bad 'file' argument")
  if (verbose) 
    cat("Loading objects:\n")
  .Internal(loadFromConn2(con, envir, verbose))
}

