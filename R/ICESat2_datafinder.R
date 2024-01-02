PAGE_SIZE <- 2000

#' ICESat-2 ATL03 and ATL08 data finder
#'
#' @description This function finds the exact granule(s) that contain ICESat-2 ATLAS data
#' for a given region of interest and date range
#'
#' @param short_name ICESat-2 ATLAS data level short_name; Options: "ATL03", "ATL08",
#' @param ul_lat Numeric. Upper left (ul) corner coordinates, in lat
#' (decimal degrees) for the bounding box of the area of interest.
#' @param ul_lon Numeric. Upper left (ul) corner coordinates, in lon
#' (decimal degrees) for the bounding box of the area of interest.
#' @param lr_lat Numeric. Lower right (ul) corner coordinates, in lat
#' (decimal degrees) for the bounding box of the area of interest.
#' @param lr_lon Numeric. Lower right (ul) corner coordinates, in lon
#' (decimal degrees) for the bounding box of the area of interest.
#' @param version Character. The version of the ICESat-2 ATLAS product files to be
#' returned (only V005 or V006). Default "006".
#' @param daterange Vector. Date range. Specify your start and end dates
#' using ISO 8601 \[YYYY\]-\[MM\]-\[DD\]T\[hh\]:\[mm\]:\[ss\]Z. Ex.:
#' c("2019-07-01T00:00:00Z","2020-05-22T23:59:59Z"). If NULL (default),
#' the date range filter will be not applied.
#' @param cloud_hosted Logical. Flag to indicate use of cloud hosted collections.
#'
#' @return Return a vector object pointing out the path saving the downloaded
#' ICESat-2 ATLAS data within the boundary box coordinates provided
#'
#' @seealso bbox: Defined by the upper left and lower right corner coordinates,
#' in lat,lon ordering, for the bounding box of the area of interest
#' (e.g. \[ul_lat,ul_lon,lr_lat,lr_lon\]).
#'
#' This function relies on the existing CMR tool:
#' \url{https://cmr.earthdata.nasa.gov/search/site/docs/search/api.html}
#'
#' @examples
#' \donttest{
#' # ICESat-2 data finder is a web service provided by NASA
#' # usually the request takes more than 5 seconds
#'
#' # Specifying bounding box coordinates
#' ul_lat <- 42.0
#' ul_lon <- -100
#' lr_lat <- 40.0
#' lr_lon <- -96.0
#'
#' # Specifying the date range
#' daterange <- c("2019-07-01", "2020-05-22")
#'
#' # Extracting the path to ICESat-2 ATLAS data for the specified boundary box coordinates
#' ICESat-2 ATLAS02b_list <- ICESat2data_finder(
#'   short_name = "ATL08",
#'   ul_lat,
#'   ul_lon,
#'   lr_lat,
#'   lr_lon,
#'   version = "006",
#'   daterange = daterange
#' )
#' }
#' @import jsonlite curl magrittr
#' @export
ICESat2_finder <- function(short_name,
                           ul_lat,
                           ul_lon,
                           lr_lat,
                           lr_lon,
                           version = "006",
                           daterange = NULL,
                           cloud_hosted = TRUE) {
  `%>%` <- magrittr::`%>%`
  bbox <- paste(ul_lon, lr_lat, lr_lon, ul_lat, sep = ",")

  # short_name = "ATL08"
  # Get cloud hosted collection?
  collection_search_url <- sprintf(
    "https://cmr.earthdata.nasa.gov/search/collections.json?short_name=%s&version=%s&cloud_hosted=%s",
    short_name,
    version,
    cloud_hosted
  )
  response <- curl::curl_fetch_memory(collection_search_url)
  collections_json <- response$content %>% rawToChar() %>% jsonlite::parse_json()
  collections_ids <- sapply(collections_json$feed$entry, function(x) x$id)

  # Granules search url pattern
  url_format <- paste0(
    "https://cmr.earthdata.nasa.gov/search/granules.json?",
    "pretty=false&page_size=%s&short_name=%s",
    "&bounding_box=%s&version=%s%%s"
  )

  # Format request URL
  request_url <- sprintf(
    url_format,
    PAGE_SIZE,
    short_name,
    bbox,
    version
  )

  # Add temporal search if not null
  temporal_filter <- ""
  if (!is.null(daterange)) {
    temporal_filter <- sprintf("&temporal=%s,%s", daterange[1], daterange[2])
  }
  request_url <- request_url %>% sprintf(temporal_filter)

  granules_href <- c()

  granules_href <- sapply(collections_ids, function(x) fetchAllGranules(request_url, x))
  # Append fetched granules to granules_href
  # recursively, for each page (max 2000 per page)
 
  return(granules_href)
}


fetchAllGranules <- function(request_url, collection_id) {
  granules_href <- c()
  page <- 1
  totalHits <- 0
  repeat {
    response <- curl::curl_fetch_memory(paste0(
      request_url,
      "&pageNum=",
      page,
      "&collection_concept_id=",
      collection_id
    ))

    if (totalHits == 0) {
      totalHits <- as.integer(curl::parse_headers_list(response$headers)[['cmr-hits']])
    }

    result <- rawToChar(response$content) %>%
      jsonlite::parse_json()

    if (response$status_code != 200) {
      stop(paste("\n", result$errors, collapse = "\n"))
    }
    granules <- result$feed$entry

    if (length(granules) == 0) break

    hrefs <- sapply(granules, function(x) x$links[[1]]$href)

    granules_href <- c(
      granules_href,
      hrefs
    )

    if ((page * PAGE_SIZE) >= totalHits) break
    page <- page + 1
}
  return(granules_href)
}

