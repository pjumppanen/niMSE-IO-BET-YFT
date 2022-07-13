# -----------------------------------------------------------------------------
# Compile TMB MPs
# -----------------------------------------------------------------------------

require(keep)
require(BuildSys)
require(ggplot2)


source('./Source/pasteOperator.R')


# -----------------------------------------------------------------------------

compileMP <- function(MP_SourcePath, MP_Name)
{
  source(MP_SourcePath)

  # make sure the MP is compiled if it is a c++ based one
  MP          <- get(MP_Name)
  BSysProject <- attr(MP, "BSysProject")
  
  if (!is.null(BSysProject))
  {
    BSysProject <- make(BSysProject)
  }
}

# -----------------------------------------------------------------------------

# path to MP code source
compileMP("./MPs/PTTMB/MPs_TMBMSY_tidied.R", "PT41F.t15.tmb")
compileMP("./MPs/PTTMB/MPs_TMBMSY_tidied.R", "PTBoB0Targ.t15")

