#' Statistics of ATL08 Terrain and Canopy Attributes at grid level
#'
#' @description This function computes a series of user defined descriptive statistics within
#' each grid cell for ATL08 Terrain and Canopy Attributes
#'
#' @usage ATL08_seg_attributes_dt_gridStat(atl08_seg_att_dt, func, res)
#'
#' @param atl08_seg_att_dt  An S4 object of class [rICESat2Veg::icesat2.atl08_dt] containing ATL08 data
#' (output of [ATL08_seg_attributes_dt()] functions).
#' @param func The function to be applied for computing the defined statistics
#' @param res Spatial resolution in decimal degrees for the output SpatRast raster layer. Default is 0.5.
#'
#' @return Return a SpatRast raster layer(s) of selected ATL08 terrain and canopy attribute(s)
#'
#' @examples
#'
#' # Specifying the path to ATL08 file (zip file)
#' outdir <- tempdir()
#'
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
#' # Extracting ATL08 terrain and canopy attributes
#' atl08_seg_att_dt <- ATL08_seg_attributes_dt(atl08_h5)
#' head(atl08_seg_att_dt)
#'
#' # Computing the top h_canopy at 0.05 degree grid cell
#' max_h_canopy <- ATL08_seg_attributes_dt_gridStat(atl08_seg_att_dt, func = max(h_canopy), res = 0.5)
#'
#' plot(max_h_canopy, xlim = c(-107.5, -106.5), ylim = c(38, 39))
#'
#' # Define your own function
#' mySetOfMetrics <- function(x) {
#'   metrics <- list(
#'     min = min(x), # Min of x
#'     max = max(x), # Max of x
#'     mean = mean(x), # Mean of x
#'     sd = sd(x) # Sd of x
#'   )
#'   return(metrics)
#' }
#'
#' # Computing a series of h_canopy statistics at 0.05 degree grid cell from customized function
#' h_canopy_metrics <- ATL08_seg_attributes_dt_gridStat(atl08_seg_att_dt, func = mySetOfMetrics(h_canopy), res = 0.05)
#'
#' plot(h_canopy_metrics, xlim = c(-107.5, -106.5), ylim = c(38, 39))
#'
#' close(atl03_h5)
#' close(atl08_h5)
#' @import data.table lazyeval
#' @export
ATL08_seg_attributes_dt_gridStat <- function(atl08_seg_att_dt, func, res = 0.5) {
  if (!inherits(atl08_seg_att_dt, "icesat2.atl08_dt")) {
    stop("atl08_seg_att_dt needs to be an object of class 'icesat2.atl08_dt' ")
  }

  if (any(is.na(atl08_seg_att_dt))) {
    atl08_seg_att_dt2 <- na.omit(atl08_seg_att_dt)
  } else {
    atl08_seg_att_dt2 <- atl08_seg_att_dt
  }

  if (!nrow(atl08_seg_att_dt2) > 1) {
    stop(paste("atl08_seg_att_dt is invalid. It contain only", nrow(atl08_seg_att_dt2), "observations"))
  }


  cells <- NA

  # Add data.table operator
  `:=` <- data.table::`:=`

  call <- lazy_call(func)
  setattr(atl08_seg_att_dt2, "class", c("data.table", "data.frame"))

  vect <- terra::vect(
    atl08_seg_att_dt2,
    geom = c("longitude", "latitude"),
    crs = "epsg:4326"
  )
  layout <- terra::rast(terra::ext(vect), resolution = res, vals = NA, crs = "epsg:4326")

  atl08_seg_att_dt2[, cells := terra::cells(layout, vect)[, 2]]
  metrics <- lazy_apply_dt_call(atl08_seg_att_dt2, call, group.by = "by = cells")

  if (any(is.na(metrics))) {
    metrics <- na.omit(metrics)
  }

  n_metrics <- ncol(metrics) - 1
  bbox <- terra::ext(vect)
  output <-
    terra::rast(
      bbox,
      resolution = res,
      nlyrs = n_metrics,
      crs = "epsg:4326"
    )



  for (metric in 1:n_metrics) {
    output[[metric]][metrics$cells] <- metrics[[metric + 1]]
  }

  names(output) <- paste0(paste0(call)[2], "_", names(metrics)[-1])

  return(output)
}
