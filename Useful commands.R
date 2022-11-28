# The packages required for writing your own package are "devtools" and "roxygen2"

# update documentation
devtools::document()

# install and attach package
#devtools::install()
#library(MyFunctions)

# installation from Github
# devtools::install_github("SOTON2947/MyFunctions",
#                         ref="master",
#                         auth_token = "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")


devtools::install_github("thezeming/MyFunctions",
                        ref="master")