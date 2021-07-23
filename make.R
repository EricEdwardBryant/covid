if (file.exists("content/_index.html")) file.remove("content/_index.html")
blogdown::stop_server()
blogdown::serve_site()
