Require.packages <- function(list.of.packages) {
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) {
    install.packages(new.packages)
  }

  suppressPackageStartupMessages(
        x <- lapply(list.of.packages, require, character.only = TRUE)
    )
}
