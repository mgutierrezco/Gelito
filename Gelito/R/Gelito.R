#' Gelito
#' Class for Gene Lists Tools

#' Constructor
#' Example for instancing: glt <- Gelito()
Gelito <- function() {

  #' Attributes
  glt = list(
    creator = GelitoMain(),  # List creator
    ll = list(),             # Data lists
    em = NA,                 # Expression matrix
    df = list(),             # Filename lists
    ul = c()                 # Lists with length > 0, last operation
  )

  # Other methods

  #' Manual input list
  #' Environment is configured by console
  #' @param filename: name of data file to read
  glt$manualInputList <- function(filename = NA) {
    glt$creator$inputEnvironment()
  }

  #' Automatic input list
  #' @param id ID column number
  #' @param nf filter columns numbers
  #' @param cc criterions
  #' @param cn column numbers
  glt$automaticInputList <- function(id = NA, nf = list(), cc = list(), cn = list()) {
    glt$creator$setEnvironment(id, nf, cc, cn)
  }

  #' Read file and append to lists
  #' @param filename: name of data file to read
  glt$readFile <- function(filename = NA, header = TRUE, console = FALSE, all = FALSE) {
    glt$creator$readFile(filename = filename, header = header, console = console, all = all)

    # Save list
    glt$ll <- append(glt$ll, list(glt$creator$dd))
    glt$df <- append(glt$df, filename)

    # Set creator data
    glt$creator$dd <- list()
  }

  #' Write file with format
  #' @param filename: name of data file to write
  glt$writeFile <- function(filename = NA, format = NA) {
    glt$creator$writeFile(filename = filename, format = format)
  }

  #' Returns lists with columns value
  #' @param lists: data lists
  #' @param ncol: number of column to create list
  glt$select <- function(lists = list(), ncol = 1) {
    if (length(lists) < 2) {
      message("Operation needs 2 lists")
      return()
    }
    if (is.na(ncol) || ncol < 1) {
      message("ncol must be positive number")
      return()
    }

    # Detect lists with length > 0
    glt$ul <- c()
    for (i in 1:length(lists)) {
      if (length(lists[[i]]) > 0) {
        glt$ul[length(glt$ul) + 1] <- i
      }
    }

    # At least there is two lists with length > 0
    if (length(glt$ul) < 2) {
      message("It needs at least 2 lists with data length > 0")
      return()
    }

    if (ncol > length(lists[[glt$ul[1]]][[1]])) {
      message("ncol must be less than number of columns in list")
      return()
    }

    # Create list with one column
    lists_local <- list()

    # For all lists in "lists"
    for (i in 1:length(glt$ul)) {
      cl <- list()
      for (j in 1:length(lists[[glt$ul[i]]])) {
        cad <- lists[[glt$ul[i]]][[j]][[ncol]]
        cl[length(cl) + 1] <- cad
      }

      # Only with data
      if (length(cl) > 1)
        lists_local[length(lists_local) + 1] <- list(cl)
    }

    return(lists_local)
  }

  #' Venn intersect. Draw diagram
  #' @param lists: list of number of list to intersect; count must be greater than 1
  #' @param ncol: intersect will be done by number of ncol
  #' @param names: columns names to show
  glt$intersectVenn <- function(lists = list(), ncol = 1, names = c()) {

    # Obtain data filtered
    lists_local <- glt$select(lists, ncol)

    # Adjust names and colors
    s_names <- c()
    for (i in 1:length(glt$ul)) {
      s_names[length(s_names) + 1] <- names[glt$ul[i]]
    }
    if (length(names) < length(lists_local)) {
      for (i in (length(names) + 1):length(lists_local))
        s_names[length[s_names] + 1] <- paste0("__", i)
    }
    colors <- rainbow(length(lists_local))

    # Draw
    venn::venn(lists_local,
               snames = s_names,
               ilabels = TRUE,
               zcolor = colors,
               transparency = 0.4,
               ellipse = FALSE,
               counts = TRUE)

  }

  #' List Intersection. By default, intersect will be done by first column in list
  #' @param lists: list of number of list to intersect; count must be greater than 0
  #' @param ncol: intersect will be done by number of ncol
  glt$intersect <- function(lists = list(), ncol = 1) {

    # Obtain data filtered
    lists_local <- glt$select(lists, ncol)

    # First list to use
    r <- lists_local[[1]]

    # Intersect rest of lists
    for (i in 2:length(lists_local)) {
      r <- intersect(r, lists_local[[i]])
    }

    # Return list intersert
    return(r)
  }

  #' List Union
  #' @param lists: list of number of list to union; count must be greater than 0
  #' @param ncol: union will be done by number of ncol
  glt$union <- function(lists = list(), ncol = 1) {

    # Obtain data filtered
    lists_local <- glt$select(lists, ncol)

    # Union lists
    r <- lists_local[[1]]  # Result is first list

    # Intersect result to rest
    for (i in 2:length(list)) {
      r <- union(r, lists_local[[i]])
    }

    # Returns list with union
    return(r)
  }

  #' Looking for shared elements in lists
  #' @param lists: data lists
  #' @param ncol: sharing will be done by number of ncol
  glt$common <- function(lists = list(), ncol = 1) {

    # Obtain data filtered
    lists_local <- glt$select(lists, ncol)

    # Results list
    r <- list()

    # Intersect all lists
    q <- c(0, 0)
    s <- lists_local[[1]]
    for (i in 2:length(lists_local)) {
      s <- intersect(s, lists_local[[i]])
    }
    a <- list(q, s)
    r[1] <- list(a) # Save

    # All instersect between lists
    for (i in 1:(length(lists_local)-1)) {
      for (j in (i+1):length(lists_local)) {
        q <- c(i, j)
        s <- intersect(lists_local[[i]], lists_local[[j]])
        a <- list(q, s)
        r[length(r) + 1] <- list(a) # Save
      }
    }

    # Returns list with intersect (shared elements)
    return(r)
  }

  #' Read expression matrix from file and set em attribute
  #' @param filename: filename with expression matrix
  glt$setMatrixFromFile <- function(filename = NA) {
    if (!is.na(filename)) {
      glt$em <- read.table(filename, sep = "\t", header = TRUE)

      # First column must be a gene name
      # Name rownames with that column
      rownames(glt$em) <- glt$em[,1]

      # Drop that column
      glt$em <- subset(glt$em, select = -1)
      message("Filename uploaded into matrix")

    } else message("Filename not specified")
  }

  #' Subsetting expression matrix with list
  #' @param vector: vector for subsetting
  glt$subset <- function(vector = c()) {

    # Update data attribute
    glt$update(glt$em[vector,])

    # Extract gene from lists for expression matrix
    return(glt$creator$data)
  }

  #' Update creator data attribute like data frame
  #' @param data: data frame with subset
  glt$update <- function(data = NA) {

    # If data class is list, convert to dataframe
    if (class(data) == "list")
       glt$creator$data <- data.frame(t(sapply(data, `[`)))
    else glt$creator$data <- data
  }

  # Ending :-)
  glt <- list2env(glt)
  class(glt) <- "Gelito"
  return(glt)
}

