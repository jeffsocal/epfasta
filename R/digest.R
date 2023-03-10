#' Proteolytic digest a read_fastad fasta list
#'
#' @description
#' `digest()` Generates peptide sequences based on *enzyme* and *partial* inputs.
#' Only works with the "list" output of the `read_fasta()` function
#'
#' @param x an epFASTA data object
#' @param ... parameters for `peptides()`
#'
#' @return a epfasta data object
#' @export
#'
#' @examples
#' library(epfasta)
#' proteins <- read_fasta("~/Local/fasta/ecoli_UniProt.fasta")
#'
#' proteins <- digest(proteins, enzyme = "[K]", partial = 2)
#'
digest <- function(
    x = NULL,
    ...
){
  check_fasta(x)
  x <- lapply(x, function(x) {
    x$peptides <- peptides(x$sequence, ...)
    return(x)
  })
  return(epfasta(x))
}


#' Get all peptides from epFASTA object as a vector
#'
#' @description
#' `get_peptides()` will return all peptides as string vector
#'
#' @param x an epFASTA data object
#'
#' @return a vector
#' @export
#'
get_peptides <- function(
    x = NULL
){
  check_fasta(x)
  x <- unlist(lapply(x, function(x) { x$peptides }))
  return(x)
}
