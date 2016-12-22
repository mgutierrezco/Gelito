#' GelitoMain
#' Main class for Gelito

#' Constructor
#' Example for instancing: glt <- GelitoMain()
GelitoMain <- function() {

  #' Attributes
  glt = list(
    id = NA,      # ID column number
    nf = list(),  # filter columns numbers
    cc = list(),  # Criterions
    cn = list(),  # Column numbers
    dd = list(),  # Data input
    data = NA     # Data output
  )

  # Other methods

  #' Trim a string
  #' @param string: leading and trailing
  glt$trim <- function(string = "") {
    gsub("^\\s+|\\s+$", "", string)
  }

  #' Read an integer between minimum and maximum numbers
  #' @param text: text for prompt
  glt$readInteger <- function(text = "") {
    r <- NA

    # Until OK
    while (TRUE) {
      s <- readline(prompt = text)
      r <- strtoi(s)
      if (!is.na(r) && r >= 1) break # Minimum number is 1
      else message("readInteger error: input not valid for integer value")
    }

    return(r)
  }

  #' Read an list of values, separated by comma
  #' Only unique values are permitted
  #' @param text: text for prompt
  #' @param distinct: if not NA, all values must be distinct than this value
  glt$readValueList <- function(text = "", distinct = NA) {
    r <- NA

    # Until OK
    while (TRUE) {
      s <- readline(prompt = text)
      if (glt$trim(s) == "") {
        r <- NA
        break
      }

      # Crear lista
      r <- as.list(strsplit(gsub("[[:space:]]", "", s), ",")[[1]])

      # Testing for unique values
      if (length(r) == length(unique(r)))
        if (!is.na(distinct)) { # ... and distinct than "distinct"
          e <- FALSE
          for (i in r) {
            if (i == distinct) {
              message("An element of those numbers is equal to ID column number")
              break
            }
          }
        } else break
      else message("All elements must be unique")
    }

    return(r)
  }

  #' Read a list of integers
  #' @param text: text for prompt
  #' @param distinct: if not NA, all values must be distinct than this value
  glt$readIntegerList <- function(text = "", distinct = NA) {
    r <- NA

    # Until OK
    while(TRUE) {
      s <- glt$readValueList(text)

      # Check all values in list s
      allok <- TRUE
      for (i in s) {
        v <- strtoi(i)
        if (is.na(v)) allok = FALSE
        else if (!is.na(distinct))
          if (i == distinct) {
            allok <- FALSE
            break
          }
      }

      # If OK, exit with value
      if (allok) {
        r <- s
        break
      }
    }

    return(r)
  }

  #' Configure environment
  glt$inputEnvironment <- function() {

    # Read ID column number
    glt$id <- glt$readInteger(text = "ID column number: ")

    # Read filter columns numbers distinct than ID
    #glt$nf <- glt$readIntegerList(text = "Filter columns numbers (comma separated, pattern string ''): ", glt$id)

    # Read filter columns numbers
    glt$nf <- glt$readIntegerList(text = "Filter columns numbers (comma separated, pattern string ''): ")

    # Read criterion filter column
    # if criterion starts with <, <=, = (==), >=, > ó !=, operator
    # else string pattern
    glt$cc <- list()
    for (i in glt$nf) {
      t <- paste("Filtering criterion for column ", i, ": ", sep = "")
      v <- readline(prompt = t)
      glt$cc[length(glt$cc) + 1] <- v
    }

    # Read columns numbers in output
    # Column numbers must be different than others
    glt$cn <- glt$readIntegerList(text = "Output columns numbers: ")
  }

  #' Get list with environment attributes
  glt$getEnvironment <- function() {
    return(list(glt$id, glt$nf, glt$cc, glt$cn))
  }

  #' Set environment from params
  #' Attributes are setting from params values
  #' @param id ID column number
  #' @param nf filter columns numbers
  #' @param cc criterions
  #' @param cn column numbers
  glt$setEnvironment <- function(id = NA, nf = list(), cc = list(), cn = list()) {
    glt$id <- id
    glt$nf <- nf
    glt$cc <- cc
    glt$cn <- cn
  }

  #' Return string version for environment
  #' Note about criterions:
  #' - pattern string must be enclosed by character '
  glt$getEnvironmentString <- function() {
    r <- paste0("ID column number:", glt$id, "\n")
    r <- paste0(r, "Filter columns numbers:", paste(glt$nf, collapse = ","), "\n")
    n <- 0
    for (i in glt$nf) {
      n <- n + 1
      if (n == 1) r <- paste0(r, "Filtering criterions (pattern string ''):\n")
      if (length(glt$cc[n]) == 1)
        r <- paste0(r, "Column: ", i, " <- ", glt$cc[n], "\n")
      else {
        r <- paste0(r, "Column:", i, "\n")
        for (j in 1:length(glt$cc[n])) {
          r <- paste0(r, glt$cc[n][j], "\n")
        }
      }
    }
    (r <- paste0(r, "Column numbers in output:", paste(glt$cn, collapse = ","), "\n"))
  }

  #' Return string version for environment (Shiny)
  #' Note about criterions:
  #' - pattern string must be enclosed by character '
  glt$getEnvironmentShiny <- function() {
    r <- paste0("ID column number:", glt$id, "<br/>")
    r <- paste0(r, "Filter columns numbers:", paste(glt$nf, collapse = ","), "<br/>")
    n <- 0
    for (i in glt$nf) {
      n <- n + 1
      if (n == 1) r <- paste0(r, "Filtering criterions (pattern string ''):<br/>")
      if (length(glt$cc[n]) == 1)
        r <- paste0(r, "Column: ", i, " <- ", glt$cc[n], "<br/>")
      else {
        r <- paste0(r, "Column:", i, "<br/>")
        for (j in 1:length(glt$cc[n])) {
          r <- paste0(r, glt$cc[n][j], "<br/>")
        }
      }
    }
    r <- paste0(r, "Column numbers in output:", paste(glt$cn, collapse = ","), "<br/>")
    return(r)
  }

  #' Read tab file with header to console, by default
  #' @param filename: filename than contents data
  #' @param header: if header exists or not (FALSE)
  #' @param console: by default, results are not sent to console
  #' @param all: by default, one expression valid is enough. FALSE is like "and" on all expressions
  glt$readFile <- function(filename = "", header = TRUE, console = FALSE, all = FALSE) {

    # Check filename
    if (is.na(filename)) {
      message("Filename has not been specified")
      return()
    }

    if (!file.exists(filename)) {
      message("Filename does not exists")
      return()
    }

    # Check before
    if (is.na(glt$id) || length(glt$cn) == 0) {
      message("Configuration is missing")
      return()
    }

    # Check config
    uc <- c(glt$id)
    if (length(glt$nf) > 0) uc <- c(uc, glt$nf)
    if (length(glt$cn) > 0) uc <- c(uc, glt$cn)
    uq <- unique(unlist(uc))
    if (length(uq) == 0) {
      message("There is no column numbers")
      return()
    }

    # Read file into data attribute
    glt$dd <- list()

    # Read line by line
    # By default, GAF format is OK.
    c <- 0
    f <- file(filename, "r")

    message("Evaluating data file")

    if (console) message("Results will be sent to console")
    else message("Results will be not shown")

    while (TRUE) {
      l <- readLines(f, n = 1)
      if (length(l) == 0) break

      # Line number
      c <- c + 1
      if ((c == 1 && header) || c > 1) {

        # Process line
        v <- strsplit(l, "\t")[[1]]

        if (length(v) < 2) {
          message("Mimimum number of columns in data file must be 2")
          break
        }

        if (length(v) < max(strtoi(uq))) {
          message(paste("Row ", c, " has ", length(v), " columns but it needs ", max(uq)))
          break
        }

        # By default, no expressions. Then save it
        save <- FALSE

        # Analize by criterions for field
        if (length(glt$cc) > 0) { # Criterions exist
          t <- 0

          for (i in glt$cc) {
            i <- glt$trim(i) # no spaces
            t <- t + 1
            k <- strtoi(paste(glt$nf[t]))

            # Choose field to compare
            field <- glt$trim(v[k]) # no spaces

            # Create comparison for evaluating
            comp <- NA
            i <- glt$trim(i)
            if (startsWith(i, "'") && endsWith(i, "'")) {  # Pattern string

              comp <- as.vector(paste0("grep(", i, ",'", field, "')"))

            } else {  # Change operator (<,>,==,!=) adding field

              # If it is an string, add quotes
              t_field = strtoi(field)
              if (is.na(t_field)) field <- paste0("'", field, "'")

              # Add field to expression
              comp <- i
              comp <- gsub("<", paste(field, "<"), comp)
              comp <- gsub(">", paste(field, ">"), comp)
              comp <- gsub("==", paste(field, "=="), comp)
              comp <- gsub("!=", paste(field, "!="), comp)
            }

            # Evaluate expression
            ev <- eval(parse(text = comp), parent.frame())
            if (length(ev) > 0) { # continue ...
              if (all) {
                if (!ev) break
              } else {
                if (ev) {
                  save <- TRUE
                  break
                }
              }
            } else {
              if (all) break
            }
          }
        } else save <- TRUE

        # If OK ... save it
        if (save) {
          s <- list()
          for (k in glt$cn) {
            s <- append(s, glt$trim(v[strtoi(k)]))
          }

          # Save line into data
          glt$dd <- append(glt$dd, list(s))
        }

      }
    }
    close(f)

    # Sent to console, if set
    if (console) (glt$dd)
  }

  #' Write file with results
  #' @param filename: output filename
  #' @param format: output file format (txt, csv or xls)
  glt$writeFile <- function(filename = NA, format = NA) {

    # If no data, return
    if (is.na(glt$data)) {
      message("There is no data")
      return()
    }

    # Output filename
    if (is.na(filename)) {
      filename <- readline(prompt = "Output file name (path included if need): ")

      # Output file format, if filename
      if (!is.na(filename)) {
        while (TRUE) {
          format <- readline(prompt = "Output file format (txt, csv or xls): ")
          if (format == "txt" || format == "csv" || format == "xls") break
          else message("Output file format unknown")
        }
      }
    }

    if (is.na(filename) || length(filename) == 0) {
      message("Filename has not been specified")
      return()
    }
    if (is.na(format) || length(format) == 0) {
      message("File format has not been specified")
      return()
    }

    if (format == "txt") glt$writeTXTFile(filename)
    else if (format == "csv") glt$writeCSVFile(filename)
    else if (format == "xls") glt$writeXLSFile(filename)
    else message("Error in output file format")
  }

  #' Write TXT file with results in data attribute (tab delimited)
  #' @param filename: output filename
  glt$writeTXTFile <- function(filename = NA) {

    if (is.na(filename) || length(filename) == 0) {
      message("Filename has not been specified")
      return()
    }

    #if (is.na(glt$data)) {
    #  message("There is no data")
    #  return()
    #}

    # If file exists, it will be override
    unlink(filename)

    # Write data frame to tab file
    write.table(glt$data[,unlist(glt$cn)], file = filename, sep = "\t")
  }

  #' Write CSV file with results in data attribute
  #' @param filename: output filename
  glt$writeCSVFile <- function(filename = NA) {

    if (is.na(filename) || length(filename) == 0) {
      message("Filename has not been specified")
      return()
    }

    #if (is.na(glt$data)) {
    #  message("There is no data")
    #  return()
    #}

    # If file exists, it will be override
    unlink(glt$fn)

    # For all results
    write.csv(glt$data[,unlist(glt$cn)], file = filename)
  }

  #' Write XLS file with results in data attribute
  #' @param filename: output filename
  glt$writeXLSFile <- function(filename = NA) {

    if (is.na(filename) || length(filename) == 0) {
      message("Filename has not been specified")
      return()
    }

    #if (is.na(glt$data)) {
    #  message("There is no data")
    #  return()
    #}

    # Convert list to data.frame
    #ddf <- data.frame(t(sapply(glt$data[,unlist(glt$creator$cn)], `[`)))
    WriteXLS::WriteXLS(glt$data[,unlist(glt$cn)], ExcelFileName = filename, col.names = FALSE)
  }

  # Ending :-)
  glt <- list2env(glt)
  class(glt) <- "GelitoMain"
  return(glt)
}

