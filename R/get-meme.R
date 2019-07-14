#' Read in the meme you wish to create
#'
#'
#' @param memename A character describing the meme to get. See [meme_list()]
#'
#' @examples
#' meme_get("AllTheThings")
#' @export
#' @importFrom magick image_read
#' @importFrom readr read_csv
#' @importFrom utils data
#'
#' @section To add a new meme to the data:
#' \describe{
#'    \item{First}{Add the meme to the inst/extdata folder (png)}
#'    \item{Then}{Add row to the blankmemes data `dplyr::add_row(blankmemes, filename = "...", name = "...")`}
#'    \item{Finally}{Run `usethis::use_data(blankmemes, overwrite = T)`}
#' }
#'
meme_get <- function(memename) {
  if (!is.character(memename)) {
    stop("Error: memename must be a character. See meme_list().")
  }

  idx <- which(memer::blankmemes$name == memename)
  filepath <- paste0("extdata/", memer::blankmemes$filename[idx])

  p <- image_read(system.file(filepath, package = "memer", lib.loc = NULL, mustWork = T))
  return(p)
}

#' List available memes
#'
#'
#' @examples
#' meme_list()
#' @export
meme_list <- function() {
  return(memer::blankmemes$name)
}

meme_search <- function(qry) {
    url <- glue::glue(
        "https://knowyourmeme.com/search?q={enc_qry}",
        enc_qry = URLencode(qry)
    )
    doc <- xml2::read_html(url)
    entry_box <- rvest::html_nodes(doc, "#entries")
    img_nodes <- rvest::html_nodes(entry_box, "img")
    
    meme <- rvest::html_attr(img_nodes, "title")
    url <- rvest::html_attr(img_nodes, "data-src")
    
    entry_idx <- stringr::str_detect(url, "entries")
    
    tibble::tibble(
      meme = meme[entry_idx], 
      url = url[entry_idx]
      )
}

meme_get_kym <- function(memename) {
  
  meme_url <- get_meme_url(memename)
  
  image_read(meme_url)
}

get_meme_url <- function(memename) {
  url <- glue::glue(
    "https://knowyourmeme.com/memes/{memename}",
    memename = clean_memename(memename)
  )
  
  hn <- rvest::html_node(xml2::read_html(url), css = ".wide")
  rvest::html_attr(hn, "href")
}

clean_memename <- function(memename){
  stringr::str_replace_all(
    stringr::str_to_lower(memename), 
    pattern = " ",
    replacement = "-"
  )
}