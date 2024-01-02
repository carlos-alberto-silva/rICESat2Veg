#' Clip Joined ATL03 and ATL08 by Geometry
#'
#' @description This function clips joined ATL03 and ATL08 photon attributes within a given geometry
#'
#' @param atl03_atl08_dt  An S4 object of class [rICESat2Veg::icesat2.atl03atl08_dt] containing ATL03 and ATL08 data
#' (output of [rICESat2Veg::ATL03_ATL08_photons_attributes_dt_join()] function).
#' @param polygon Polygon. An object of class [`terra::SpatVector`],
#' which can be loaded as an ESRI shapefile using [terra::vect] function in the
#' \emph{sf} package.
#' @param split_by Polygon id. If defined, GEDI data will be clipped by each polygon using
#' the polygon id from table of attribute defined by the user
#'
#' @return Returns an S4 object of class [rICESat2Veg::icesat2.atl03atl08_dt]
#' containing the clipped ATL08 Terrain attributes.
#'
#' @examples
#'# Specifying the path to ATL03 and ATL08 file (zip file)
#'outdir = tempdir()
#'atl03_zip <- system.file("extdata",
#'                   "ATL03_20220401221822_01501506_005_01.zip",
#'                   package="rICESat2Veg")
#'
#'atl08_zip <- system.file("extdata",
#'                   "ATL08_20220401221822_01501506_005_01.zip",
#'                   package="rICESat2Veg")
#'
#'# Unzipping ATL03 file
#'atl03_path <- unzip(atl03_zip,exdir = outdir)
#'
#'# Unzipping ATL08 file
#'atl08_path <- unzip(atl08_zip,exdir = outdir)
#'
#'# Reading ATL03 data (h5 file)
#atl03_h5<-ATL08_read(atl03_path=atl03_path)
#'
#'# Reading ATL08 data (h5 file)
#atl08_h5<-ATL08_read(atl08_path=atl08_path)
#'
#'# Joining ATL03 and ATL08 photons attributes
#'atl03_atl08_dt<-ATL03_ATL08_join(atl03_h5,atl08_h5)
#'head(atl03_atl08_dt)
#'
#' # Specifying the path to shapefile
#' polygon_filepath <- system.file("extdata", "polygon.shp", package = "rICESat2Veg")
#'
#' # Reading shapefile as sf object
#'polygon <- terra::vect(polygon_filepath)
#'
#' # Clipping ATL08 terrain attributes by Geometry
#' atl03_atl08_dt_clip <- ATL03_ATL08_photons_attributes_dt_clipGeometry(atl03_atl08_dt, polygon, split_by = "FID")
#' head(atl03_atl08_dt_clip)
#'
#'close(atl03_h5)
#'close(atl08_h5)
#' @export
ATL03_ATL08_photons_attributes_dt_clipGeometry <- function(atl03_atl08_dt, polygon, split_by = "id") {

  if (!class(atl03_atl08_dt)[1]=="icesat2.atl03atl08_dt"){
    stop("atl03_atl08_dt needs to be an object of class 'icesat2.at03atl08_dt' ")
  }

  exshp <- terra::ext(polygon)

  atl03_atl08_dt <- ATL03_ATL08_photons_attributes_dt_clipBox(
    atl03_atl08_dt,
    xmin = exshp$xmin,
    xmax = exshp$xmax,
    ymin = exshp$ymin,
    ymax = exshp$ymax
  )

  if (any(is.na(atl03_atl08_dt@dt))) {
    atl03_atl08_dt<-na.omit(atl03_atl08_dt@dt)
  } else {
    atl03_atl08_dt<-atl03_atl08_dt@dt
  }

  atl03_atl08_dt$nid<-1:nrow(atl03_atl08_dt)

  if (nrow(atl03_atl08_dt) == 0) {
    print("The polygon does not overlap the ATL08 data")
  } else {
    points <- terra::vect(
      atl03_atl08_dt,
      geom = c("lon_ph", "lat_ph"),
      crs = terra::crs(polygon)
    )

    points$rowNumber <- as.integer(seq_along(points))
    pts <- terra::intersect(terra::makeValid(points), terra::makeValid(polygon))

    if (!is.null(split_by)) {
      if (any(names(polygon) == split_by)) {
        newFile <- atl03_atl08_dt[pts$nid, ]
        newFile$poly_id<-pts[[split_by]]
      } else {
        stop(paste("The", split_by, "is not included in the attribute table.
                       Please check the names in the attribute table"))
      }
    } else {

      newFile <- atl03_atl08_dt[pts$nid, ]
      #newFile <- atl03_atl08_dt[mask, ]
    }
    newFile<- new("icesat2.atl03atl08_dt", dt = newFile)
    return(newFile)
  }
}

