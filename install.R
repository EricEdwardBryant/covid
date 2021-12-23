renv::restore()

local({
  hugo <- "0.61.0"
  if (blogdown::hugo_version() != hugo) blogdown::install_hugo(hugo, force = TRUE)
})

if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()
