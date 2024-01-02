#' Clip ATL03 photons by Coordinates
#'
#' @description This function clips ATL03 photons attributes within a given bounding coordinates
#'
#' @usage ATL03_photons_attributes_dt_clipBox(atl03_photons_dt, xmin, xmax, ymin, ymax)
#'
#' @param atl03_photons_dt A atl03_photons_dt object (output of [atl03_photons_attributes_dt()] function).
#' An S4 object of class [rICESat2Veg::icesat2.atl03_dt]
#' @param xmin Numeric. West longitude (x) coordinate of bounding rectangle, in decimal degrees.
#' @param xmax Numeric. East longitude (x) coordinate of bounding rectangle, in decimal degrees.
#' @param ymin Numeric. South latitude (y) coordinate of bounding rectangle, in decimal degrees.
#' @param ymax Numeric. North latitude (y) coordinate of bounding rectangle, in decimal degrees.
#'
#' @return Returns an S4 object of class [rICESat2Veg::icesat2.atl03_dt]
#' containing the ATL03 photons attributes.
#'
#' @seealso \url{https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2_atl03_ATBD_r006.pdf}
#'
#' @examples
#'# Specifying the path to ATL03 file (zip file)
#'outdir = tempdir()
#'atl03_zip <- system.file("extdata",
#'                   "atl03_20220401221822_01501506_005_01.zip",
#'                   package="rICESat2Veg")
#'
#'# Unzipping ATL03 file
#'atl03_path <- unzip(atl03_zip,exdir = outdir)
#'
#'# Reading ATL03 data (h5 file)
#atl03_h5<-atl03_read(atl03_path=atl03_path)
#'
#'# Extracting ATL03 photons attributes
#'atl03_photons_dt<-ATL03_photons_attributes_dt(atl03_h5=atl03_h5)
#'
#' # Bounding rectangle coordinates
#' xmin <- -107.7
#' xmax <- -106.5
#' ymin <- 32.75
#' ymax <- 42.75
#'
#' # Clipping ATL03 photons  by boundary box extent
#'atl03_photons_dt_clip <- ATL03_photons_attributes_dt_clipBox(atl03_photons_dt, xmin, xmax, ymin, ymax)
#'
#'close(atl03_h5)
#'@import hdf5r stats
#'@export
ATL03_photons_attributes_dt_clipBox <- function(atl03_photons_dt, xmin, xmax, ymin, ymax) {

  if (!class(atl03_photons_dt)[1]=="icesat2.atl03_dt"){
    stop("atl03_photons_dt needs to be an object of class 'icesat2.atl03_dt' ")
  }

  if (any(is.na(atl03_photons_dt@dt))) {
    atl03_photons_dt<-na.omit(atl03_photons_dt@dt)
  } else {

    atl03_photons_dt<-atl03_photons_dt@dt
  }


  # xmin ymin xmax ymax
  mask <-
    atl03_photons_dt$lon_ph >= xmin &
    atl03_photons_dt$lon_ph <= xmax &
    atl03_photons_dt$lat_ph >= ymin &
    atl03_photons_dt$lat_ph <= ymax

  mask[!stats::complete.cases(mask)] <- FALSE
  mask <- (seq_along(atl03_photons_dt$lat_ph))[mask]
  newFile <- atl03_photons_dt[mask, ]

  newFile<- new("icesat2.atl03_dt", dt = newFile)

  # newFile<- new("gedi.level1b.dt", dt = level1bdt[mask,])
  return(newFile)
}
