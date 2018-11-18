
library(stringr)
library(xml2)
library(purrr)

count_years_in_files <- function (xml_files) {
  all_years <- rep(0, 1019) # from 1000 to 2018 inclusive
  for (f in xml_files) {
    han_xml <- xml2::read_xml(f)
    speeches <- xml2::xml_find_all(han_xml, 
          "//membercontribution/text()[not(self::col)]")
    speeches <- map_chr(speeches, as.character)
    # match what looks like a year; followed by a space or 
    # a .,;!?: character then a space.
    years <- stringr::str_match_all(speeches, 
          "\\s((1|2)[0-9][0-9][0-9])[\\.\\?!,;:]?\\s")
    years <- do.call(rbind, years)
    years <- as.numeric(na.omit(years[, 2]))
    years <- tabulate(years, nbins = 2018)[-(1:999)]
    all_years <- all_years + years
  }
  
  names(all_years) <- as.character(1000:2018)
  return(all_years)
}

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