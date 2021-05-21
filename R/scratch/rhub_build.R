# install.packages("rhub")

library(rhub)

validate_email()
1

cran_prep <- check_for_cran( email="joechampion@boisestate.edu" )
cran_prep$cran_summary( )
