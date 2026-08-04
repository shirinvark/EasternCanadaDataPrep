#------------------------------------------------------------------------------
# Build Ownership lookup
#
# Creates the ownership lookup table.
#------------------------------------------------------------------------------

buildOwnership <- function(sim) {
  
  sim$ownershipLookup <- data.frame(
    value = c(11, 12, 13, 20, 31, 32, 33, 40, 50, 100),
    ownership = c(
      "Lands with long term volume- or area-based Crown timber dispositions",
      "Lands with short term volume- or area-based Crown timber dispositions",
      "Lands with no current Crown timber dispositions",
      "Lands legal protection status",
      "Lands held in reserve by the Federal government for military or other purposes",
      "Lands held in reserve by the Federal government under the Indian Act",
      "Lands reserved or designated restricted use by provincial or territorial government",
      "Aboriginal Lands",
      "Privately-owned lands",
      "Water"
    ),
    stringsAsFactors = FALSE
  )
  
  sim
  
}