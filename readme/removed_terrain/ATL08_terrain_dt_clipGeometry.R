#' Clip ATL08 Terrain Attributes by Geometry
#'
#' @description This function clips ATL08 Terrain attributes within a given geometry
#'
#' @usage ATL08_terrain_dt_clipGeometry(atl08_terrain_dt, xmin, xmax, ymin, ymax)
#'
#' @param atl08_terrain_dt A atl08_terrain_dt object (output of [rICESat2Veg::ATL08_terrain_attributes_dt()] function).
#' An S4 object of class [rICESat2Veg::icesat2.atl08_dt]
#' @param polygon Polygon. An object of class [`terra::SpatVector`],
#' which can be loaded as an ESRI shapefile using [terra::vect] function in the
#' \emph{sf} package.
#' @param split_by Polygon id. If defined, GEDI data will be clipped by each polygon using
#' the polygon id from table of attribute defined by the user
#'
#' @return Returns an S4 object of class [rICESat2Veg::icesat2.atl08_dt]
#' containing the clipped ATL08 Terrain attributes.
#'
#' @examples
#' # Specifying the path to ATL08 file (zip file)
#' outdir <- tempdir()
#' atl08_zip <- system.file("extdata",
#'   "ATL08_20220401221822_01501506_005_01.zip",
#'   package = "rICESat2Veg"
#' )
#'
#' # Unzipping ATL08 file
#' atl08_path <- unzip(atl08_zip, exdir = outdir)
#'
#' # Reading ATL08 data (h5 file)
# atl08_h5<-ATL08_read(atl08_path=atl08_path)
#'
#' # Extracting ATL08-derived terrain Metrics
#' atl08_terrain_dt <- ATL08_terrain_attributes(atl08_h5 = atl08_h5)
#'
#' # Specifying the path to shapefile
#' polygon_filepath <- system.file("extdata", "polygon.shp", package = "rICESat2Veg")
#'
#' # Reading shapefile as sf object
#' polygon <- terra::vect(polygon_filepath)
#'
#' # Clipping ATL08 terrain attributes by Geometry
#' atl08_terrain_dt_clip <- ATL08_terrain_dt_clipGeometry(atl08_terrain_dt, polygon, split_by = "FID")
#'
#' hasLeaflet <- require(leaflet)
#'
#' if (hasLeaflet) {
#'   leaflet() %>%
#'     addCircleMarkers(atl08_terrain_dt_clip$longitude,
#'       atl08_terrain_dt_clip$latitude,
#'       radius = 1,
#'       opacity = 1,
#'       color = "red"
#'     ) %>%
#'     addScaleBar(options = list(imperial = FALSE)) %>%
#'     addPolygons(
#'       data = polygon, weight = 1, col = "white",
#'       opacity = 1, fillOpacity = 0
#'     ) %>%
#'     addProviderTiles(providers$Esri.WorldImagery)
#' }
#'
#' close(atl08_h5)
#' @export
ATL08_terrain_dt_clipGeometry <- function(atl08_terrain_dt, polygon, split_by = "id") {
  if (!class(atl08_terrain_dt)[1] == "icesat2.atl08_dt") {
    stop("atl08_terrain_dt needs to be an object of class 'icesat2.atl08_dt' ")
  }


  exshp <- terra::ext(polygon)

  atl08_terrain_dtdt <- ATL08_terrain_dt_clipBox(
    atl08_terrain_dt,
    xmin = exshp$xmin,
    xmax = exshp$xmax,
    ymin = exshp$ymin,
    ymax = exshp$ymax
  )

  if (any(is.na(atl08_terrain_dtdt@dt))) {
    atl08_terrain_dtdt <- na.omit(atl08_terrain_dtdt@dt)
  } else {
    atl08_terrain_dtdt <- atl08_terrain_dtdt@dt
  }

  atl08_terrain_dtdt$nid <- 1:nrow(atl08_terrain_dtdt)

  if (nrow(atl08_terrain_dtdt) == 0) {
    print("The polygon does not overlap the ATL08 data")
  } else {
    points <- terra::vect(
      atl08_terrain_dtdt,
      geom = c("longitude", "latitude"),
      crs = terra::crs(polygon)
    )

    points$rowNumber <- as.integer(seq_along(points))
    pts <- terra::intersect(terra::makeValid(points), terra::makeValid(polygon))

    if (!is.null(split_by)) {
      if (any(names(polygon) == split_by)) {
        newFile <- atl08_terrain_dtdt[pts$nid, ]
        newFile$poly_id <- pts[[split_by]]
      } else {
        stop(paste("The", split_by, "is not included in the attribute table.
                       Please check the names in the attribute table"))
      }
    } else {
      newFile <- atl08_terrain_dtdt[pts$nid, ]
      # newFile <- atl08_terrain_dtdt[mask, ]
    }
    setattr(newFile, "class", c("icesat2.atl08_dt", "data.table", "data.frame"))
    return(newFile)
  }
}
