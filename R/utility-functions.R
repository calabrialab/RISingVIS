#-----------------------------------------------------------------------------#
# Utility functions
#-----------------------------------------------------------------------------#
# This file contains some utility functions that are exported.

# known_CEM_IS
#' Returns a table containing the known control integration sites.
#'
#' This function builds a tribble which contains information
#' about the known integration sites belinging to control samples (CEMs).
#' It specifies the following fields: chromosome, integration locus,
#' strand, name (symbol) and strand of the closest gene to the IS.
#'
#' @usage known_CEM_IS()
#' @return Table of integration sites
#'
#' @export
#' 
#' @examples 
#' is <- known_CEM_IS()
#' is

known_CEM_IS <- function() {
    known_cem_is <- tibble::tribble(
        ~chr,	~integration_locus,	~strand,	~GeneName,	~GeneStrand,
        "8",	8866486,	"+",	"ERI1",	"+",
        "11",	64537168,	"-",	"SF1",	"-",
        "17",	47732339,	"-",	"SPOP",	"-",
        "2",	73762398,	"-",	"ALMS1",	"+",
        "2",	24546571,	"-",	"ITSN2",	"-",
        "17",	2032352,	"-",	"SMG6",	"-",
        "16",	28497498,	"-",	"CLN3",	"-"
    )
    return(known_cem_is)
}
