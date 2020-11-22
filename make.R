if (file.exists("content/_index.html")) file.remove("content/_index.html")
blogdown::build_site(local = TRUE)
servr::daemon_stop()
servr::httd("public")
