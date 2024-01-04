#' Download ICESat-2  data
#'
#' @description Download ICESat-2 ATL03 and ALT08 data from LP DAAC Data Pool. Users will need to enter their
#' Earth Explore login Information for downloading the data.
#'
#' @usage ICESat2data_download(filepath, outdir, overwrite, buffer_size, timeout)
#'
#' @param filepath Vector object; path to ATL03 or ALT08 data, or both (output of [`rICESat2Veg::ICESat2_finder()`])
#' @param outdir Vector object, output directory for downloading GEDI data, default [tempdir()]
#' @param overwrite logical; overwrite file if they already exists in destination, default FALSE
#' @param buffer_size integer; the size of download chunk in KB to hold in memory before writing to file, default 512.
#' @param timeout integer; connection timeout in seconds.
#'
#' @return No return value on success, on failure it will [stop()]
#' @references Credits to Cole Krehbiel. Code adapted from
#' \url{https://git.earthdata.nasa.gov/projects/LPDUR/repos/daac_data_download_r/browse/DAACDataDownload.R}
#' @examples
#' \dontrun{
#' # Set path to ICESat-2 data
#' # herein we will only download xml metedata
#' url <- c(
#'   "https://data.nsidc.earthdatacloud.nasa.gov/nsidc-cumulus-prod-protected/ATLAS/ATL08/006/2019/07/13/ATL08_20190713210015_02410406_006_02.h5",
#'   "https://data.nsidc.earthdatacloud.nasa.gov/nsidc-cumulus-prod-protected/ATLAS/ATL08/006/2019/07/15/ATL08_20190715084425_02640402_006_02.h5"
#' )
#'
#' # Set dir to download files to
#' outdir <- tempdir()
#'
#' # Create .netrc file
#' netrc <- file.path(outdir, ".netrc")
#' netrc_conn <- file(netrc)
#'
#' # Assuming login data is saved in the
#' # environmental variables NASA_USER and NASA_PASSWORD
#' writeLines(c(
#'   "machine urs.earthdata.nasa.gov",
#'   sprintf("login %s", Sys.getenv("NASA_USER")),
#'   sprintf("password %s", Sys.getenv("NASA_PASSWORD"))
#' ), netrc_conn)
#'
#' close(netrc_conn)
#'
#' #' Downloading ICEsat-2 data
#' ATLAS_dataDownload(url, outdir)
#' }
#' @import curl
#' @export
ATLAS_dataDownload <- function(url, outdir = NULL, overwrite = FALSE, buffer_size = 512, timeout = 10) {
  if (is.null(outdir)) {
    outdir <- tempdir()
  }
  stopifnotMessage(
    "outdir is not a valid path" = checkParentDir(outdir),
    "overwrite is not logical" = checkLogical(overwrite),
    "buffer_size is not an integer" = checkInteger(buffer_size)
  )
  buffer_size <- as.integer(buffer_size)
  netrc <- getNetRC(outdir)

  files <- url
  n_files <- length(files)

  # Download all files in filepath vector
  # i=1
  for (i in 1:n_files) {
    url <- files[i]

    if (is.na(url)) next
    message("------------------------------")
    message(sprintf("Downloading file %d/%d: %s", i, n_files, basename(url)))
    message("------------------------------")

    if (icesat2DownloadFile(
      url,
      outdir,
      overwrite,
      buffer_size,
      netrc,
      timeout
    ) == 0) {
      message("Finished successfully!")
    } else {
      stop(sprintf("File %s has not been downloaded properly!", basename(url)))
    }
  }
}

icesat2DownloadFile <- function(url, outdir, overwrite, buffer_size, netrc, timeout) {
  filename <- file.path(outdir, basename(url)) # Keep original filename
  if ((!overwrite) && file.exists(filename)) {
    message("Skipping this file, already downloaded!")
    return(0)
  } # SKip if already downloaded

  # Temporary to file to resume to
  resume <- paste0(filename, ".curltmp")
  if (file.exists(resume)) {
    resume_from <- file.info(resume)$size # Get current size to resume from
  } else {
    resume_from <- 0
  }

  # Connection config
  h <- curl::new_handle()
  curl::handle_setopt(
    h,
    netrc = 1,
    netrc_file = netrc,
    resume_from = resume_from,
    connecttimeout = timeout,
    followlocation = 1
  )

  tryCatch({
    fileHandle <- file(resume, open = "ab", raw = T)
    message("Connecting...")
    conn <- tryCatch(curl::curl(url, handle = h, open = "rb"), error = function(e) {
      tryCatch(curl::curl(url, handle = h, open = "rb"), error = function(e) {
        file.remove(netrc)
        stop(e)
      })
    })
    message("Connected successfully, downloading...")
    headers <- curl::parse_headers_list(rawToChar(curl::handle_data(h)$headers))
    total_size <- as.numeric(headers[["content-length"]])
    while (TRUE) {
      message(
        sprintf(
          "\rDownloading... %.2f/%.2fMB (%.2f%%)    ",
          resume_from / 1024.0 / 1024.0,
          total_size / 1024.0 / 1024.0,
          100.0 * resume_from / total_size
        ),
        appendLF = FALSE
      )
      data <- readBin(conn, what = raw(), n = 1024 * buffer_size)
      size <- length(data)
      if (size == 0) {
        break
      }
      writeBin(data, fileHandle, useBytes = T)
      resume_from <- resume_from + size
    }
    message(sprintf(
      "\rDownloading... %.2f/%.2fMB (100%%)    ",
      total_size / 1024.0 / 1024.0,
      total_size / 1024.0 / 1024.0
    ))
    close(fileHandle)
    close(conn)
    file.rename(resume, filename)
    return(0)
  }, interrupt = function(e) {
    warning("\nDownload interrupted!!!")
    try(close(conn), silent = TRUE)
    try(close(fileHandle), silent = TRUE)
  }, finally = {
    try(close(conn), silent = TRUE)
    try(close(fileHandle), silent = TRUE)
  })
  return(-1)
}

getNetRC <- function(dl_dir) {
  netrc <- file.path(dl_dir, ".netrc") # Path to netrc file
  # ------------------------------------CREATE .NETRC FILE------------------------------------------ #
  if (file.exists(netrc) == FALSE || any(grepl("urs.earthdata.nasa.gov", readLines(netrc))) == FALSE) {
    netrc_conn <- file(netrc)

    user <- Sys.getenv("NASA_USER")
    if (user == "") {
      user <- getPass::getPass(
        msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :"
      )
    }

    password <- Sys.getenv("NASA_PASSWORD")
    if (password == "") {
      password <- getPass::getPass(msg = "Enter NASA Earthdata Login Password:")
    }
    # User will be prompted for NASA Earthdata Login Username and Password below
    writeLines(c(
      "machine urs.earthdata.nasa.gov",
      sprintf("login %s", user),
      sprintf("password %s", password)
    ), netrc_conn)
    close(netrc_conn)
    message("A .netrc file with your Earthdata Login credentials was stored in the output directory ")
  }
  return(netrc)
}
