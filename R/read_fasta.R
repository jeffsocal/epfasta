#' The main function for parsing a fasta file
#'
#' @description
#' `read_fasta()` get the current regex
#'
#' @param fasta_path a character string of the path to the fasta formatted file
#' @param patterns a list, if not provided the default from `regex()` will be used.
#' *Note*: the first element in the regex list will define the list reference name, such
#' that with the list output, each protein can be accessed with that designation.
#' *Note*: if the patterns list is missing an explicit "sequence" element, no sequence will
#' be returned. This might be beneficial if only a few meta elements are sought.
#' @param as a character designating the output format
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' library(epfasta)
#' proteins <- read_fasta("~/Local/fasta/ecoli_UniProt.fasta")
#'
#' # using a custom supplied regex list
#' proteins <- read_fasta(fasta_path = "~/Local/fasta/ecoli_UniProt.fasta",
#'                   pattern = list(
#'                       "accession" = "sp\\|[A-Z]",
#'                       "gene_name" = "(?<=GN\\=).*?(?=\\s..\\=)"
#'                   ))
#' }
#'
read_fasta <- function(fasta_path = NULL,
                  patterns = NULL,
                  as = c('list','data.frame')
){

  as <- rlang::arg_match(as)
  if(is.null(fasta_path)) {cli::cli_abort(c("x" = "fasta_path is empty"))}
  if(is.null(patterns)) { patterns <- regex() }
  if(mode(patterns) != 'list') {cli::cli_abort(c("x" = "patterns is `{mode(patterns)}`, should be a list"))}

  cli::cli_process_start("Parsing FASTA file {basename(fasta_path)}")

  tryCatch({

    l_fasta <- readr::read_file(fasta_path)
    l_fasta <- gsub("->", "-", l_fasta)

    # read in fasta file
    l_fasta <- unlist(base::strsplit(l_fasta, ">"))
    l_fasta <- l_fasta[-1] # first in list is blank

    names(l_fasta) <- unlist(parallel::mclapply(l_fasta, extract, patterns[1]))
    l_fasta <- parallel::mclapply(l_fasta, extract, patterns)

    if(as == "data.frame") {
      l_fasta <- parallel::mclapply(l_fasta, as.data.frame)
      l_fasta <- dplyr::bind_rows(l_fasta)
    }

  }, error = function(err) {
    err = as.character(as.vector(err))
    cli::cli_process_failed()
    cli::cli_abort(err)
  })
  cli::cli_process_done()

  class(l_fasta) <- 'epfasta'

  return(l_fasta)
}
