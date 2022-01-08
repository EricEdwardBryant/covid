# Installs packages recorded in renv.lock
renv::restore()

# External (non-R) dependencies
local({
  # Hugo is the static website builder
  hugo <- "0.61.0"
  if (blogdown::hugo_version() != hugo) {
    blogdown::install_hugo(hugo, force = TRUE)
  }

  # PhantomJS is required to generate PNGs of gt generated html tables
  if (!webshot::is_phantomjs_installed()) {
    webshot::install_phantomjs()
  }
})


