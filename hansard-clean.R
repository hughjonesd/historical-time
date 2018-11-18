
library(stringr)
library(xml2)
library(purrr)


download_zips <- function () {
  urls <- c(
    "http://www.hansard-archive.parliament.uk/Parliamentary_Debates_Vol_1_(1803)_to_Vol_41_(Feb_1820)/",
    "http://www.hansard-archive.parliament.uk/Parliamentary_Debates,_New_Series_Vol_1_(April_1820)_to_Vol_25_(July_1830)/",
    "http://www.hansard-archive.parliament.uk/Parliamentary_Debates_(3rd_Series)_Vol_1_(Oct_1830)_to_Vol_356_(August_1891)/",
    "http://www.hansard-archive.parliament.uk/Parliamentary_Debates_(4th_Series)_Vol_1_(February_1892)_to_Vol_199_(December_1908)/",
    "http://www.hansard-archive.parliament.uk/Official_Report,_House_of_Commons_(5th_Series)_Vol_1_(Jan_1909)_to_Vol_1000_(March_1981)/",
    "http://www.hansard-archive.parliament.uk/The_Official_Report,_House_of_Lords_(5th_Series)_Vol_1_(Jan_1909)_to_2004/",
    "http://www.hansard-archive.parliament.uk/The_Official_Report,_House_of_Commons_(6th_Series)_Vol_1_(March_1981)_to_2004/"
  )
  
  volnums <- c(41, 24, 356, 140, 1000, 670, 424)
  prefixes <- c("S1V", "S2V", "S3V", "S4V", "S5CV", "S5LV", "S6CV")
  parts <- list(0, 0, 0, 0, 0, 0:2, 0:2)
  parts <- map(parts, ~ paste0("P", .x))
  
  zip_files <- map2(prefixes, volnums, ~ paste0(.x, sprintf("%04.0f", 1:.y)))
  zip_files <- map2(zip_files, parts, ~ c(outer(.x, .y, paste0)))
  zip_files <- map(zip_files, paste0, ".zip")
  
  zip_urls <- map2(urls, zip_files, paste0)
  zip_urls <- flatten_chr(zip_urls)
  
  for (url in zip_urls) {
    file <- sub(".*/", "", url)
    path <- file.path("hansard", file)
    try(download.file(url, path, mode = "wb"))
  }
}


count_year_refs <- function (han_xml) {
  speeches <- xml2::xml_find_all(han_xml, 
        "//membercontribution/text()[not(self::col)]")
  speeches <- xml_text(speeches)
  # match what looks like a year; followed by a space or 
  # a .,;!?: character then a space.
  years <- stringr::str_match_all(speeches, 
        "\\s((1|2)[0-9][0-9][0-9])[\\.\\?!,;:]?\\s")
  years <- do.call(rbind, years)
  years <- as.numeric(na.omit(years[, 2]))
  
  # from 1000 to 2018 inclusive
  years <- tabulate(years, nbins = 2018)[-(1:999)]
  names(years) <- as.character(1000:2018)
  
  return(years)
}


get_hansard_year <- function (xml_doc) {
  title_page <- xml2::xml_find_all(xml_doc, "//titlepage//text()")
  title_page <- xml_text(title_page) # map_chr too slow
  title_page <- paste(title_page, collapse = "\n")
  # i = case-insensitive; x = allow whitespace; s = let . overrun lines
  year <- stringr::str_match(title_page, 
        "(?ixs) COMPRISING .*? ((1|2)[0-9][0-9][0-9])")
  year <- as.numeric(year[, 2])
  
  if (is.na(year)) {
    # gotta try the index
    index <- xml2::xml_find_all(xml_doc, "//index//text()")
    index <- sapply(index, as.character)
    index <- paste(index, collapse = "\n")
    year <- stringr::str_match(index, 
          "(?ixs) SESSION .*? ((1|2)[0-9][0-9][0-9])")
    year <- as.numeric(year[, 2])
  }
  
  if (is.na(year)) stop("Couldn't find year!")
  
  return(year)
}


unzip_file <- function (file) {
  if (file.size(file) < 10000) stop("File too small")
  res <- unzip(file, overwrite = TRUE)
  if (length(res) == 0) stop("Nothing in file")
  return(res)
}

get_data_from_file <- function (file) {
  xml_file <- try(unzip_file(file), silent = TRUE)
  if (inherits(xml_file, "try-error")) return()
  
  xml_doc <- xml2::read_xml(xml_file)
  year <- get_hansard_year(xml_doc)
  if (year == 1637 && grepl("S5CV0323P0", xml_file)) year <- 1937 # a misprint
  if (year == 1006 && grepl("S6CV0006P0", xml_file)) year <- 1980 # missing year
  if (year == 1095 && grepl("S6CV0095P0", xml_file)) year <- 1985 # missing year
  
  if (! is.numeric(year) || is.na(year) || year < 1800 || year > 2018) {
    warning(sprintf("In file %s, year was %s", xml_file, year))
  }
  year_refs <- count_year_refs(xml_doc)
  file.remove(xml_file)
  return(list(from_year = year, year_refs = year_refs))
}

get_data_from_files <- function (files) {
  # year_counts <- matrix(0, 
  #       219, 1019, 
  #       dimnames = list(
  #         from = 1800:2018,
  #         to   = 1000:2018
  #       ))
  year_counts <- readRDS("year_counts.rds")
  files_done <- readRDS("files_done.rds")
  # files_done <- setNames(rep(FALSE, length(files)), files)
  
  pb <- txtProgressBar(min = 0, max = 80)
  i <- 0
  for (file in files) {
    if (files_done[file]) next
    cat(file, "\n")
    result <- get_data_from_file(file)
    if (length(result) > 0) {
      fy <- as.character(result$from_year)
      yc <- year_counts[fy, ]
      yc <- yc + result$year_refs
      year_counts[fy, ] <- yc
    }
    files_done[file] <- TRUE
    i <- i + 1; 
    if (i > 80) {
      i <- 0
      saveRDS(year_counts, file = "year_counts.rds")
      saveRDS(files_done, file = "files_done.rds")
    }
    setTxtProgressBar(pb, i)
  }
  
  return(year_counts)
}

zip_files <- list.files("hansard", full.names = TRUE, pattern = "zip$")

year_counts <- get_data_from_files(zip_files)
saveRDS(year_counts, file = "year_counts.rds")