rumba_reloader_html <- function(progress_max, progress_value){
  reloader_html <- readChar("www/reloader.html", file.info("www/reloader.html")$size)

  reloader_html <- sub("\\$progress_max", format(progress_max, digits=2, nsmall=2), reloader_html)
  reloader_html <- sub("\\$progress_value", format(progress_value, digits=2, nsmall=2), reloader_html)

  return(reloader_html)
}