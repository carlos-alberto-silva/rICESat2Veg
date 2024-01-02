.datatable.aware <- TRUE
#' @importFrom hdf5r H5File
setRefClass("H5File")

# Base class for icesat H5 files
setClass(
  Class = "icesat2.h5",
  slots = list(h5 = "H5File")
)

#' Class for ICESat-2 ATL03
#'
#' @slot h5 Object of class [`H5File`][hdf5r::H5File-class] from `hdf5r` package containing the
#' ICESat-2 Global Geolocated Photon Data (ATL03)
#'
#' @seealso [`H5File`][hdf5r::H5File-class] in the `hdf5r` package and
#' \url{https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2_ATL03_ATBD_r006.pdf}
#'
#' @import methods
#' @export
icesat2.atl03_h5 <- setClass(
  Class = "icesat2.atl03_h5",
  contains = "icesat2.h5"
)

#' Class for ICESat-2 ATL08
#'
#' @slot h5 Object of class [`H5File`][hdf5r::H5File-class] from `hdf5r` package containing the
#' ICESat-2 Land and Vegetation Along-Track Products (ATL08)

#'
#' @seealso [`H5File`][hdf5r::H5File-class] in the `hdf5r` package and
#' \url{https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2_ATL08_ATBD_r006.pdf}
#'
#' @import methods
#' @export
icesat2.atl08_h5 <- setClass(
  Class = "icesat2.atl08_h5",
  contains = "icesat2.h5"
)


#' Dispatches the `[[` function to h5
#'
#' @param x An object of class `icesat2.atl03_h5`
#' @param path The path for the dataset which to open
#'
#' @export
`[[.icesat2.atl03_h5` <- function(x, path) {
  x@h5[[path]]
}

#' Dispatches the `[[` function to h5
#'
#'
#' @param x An object of class `icesat2.atl08_h5`
#' @param path The path for the dataset which to open
#'
#' @export
`[[.icesat2.atl08_h5` <- function(x, path) {
  x@h5[[path]]
}

#' Dispatches the `[[` function to dt
#'
#'
#' @param x An object of class `icesat2.atl03atl08_dt`
#' @param path The path for the dataset which to open
#'
#' @export
`[[.icesat2.atl03atl08_dt` <- function(x, path) {
  x@dt[[path]]
}

#' Dispatches the `@dt` function to dt
#'
#'
#' @param x An object of class `icesat2.atl03_dt`
#' @param path The path for the dataset which to open
#'
#' @export
`$.icesat2.atl03_dt` <- function(x, path) {
  x@dt[,get(path)]
}

#' Dispatches the `@dt` function to dt
#'
#'
#' @param x An object of class `icesat2.atl03atl08_dt`
#' @param path The path for the dataset which to open
#'
#' @export
`$.icesat2.atl03atl08_dt` <- function(x, path) {
  x@dt[,get(path)]
}


#' Head for ATL03 photons
#'
#' @param atl03 An object of class `icesat2.atl03_dt`
#'
#' @export
`head.icesat2.atl03_dt` <- function(x) {
  head(x@dt)
}


#' Summary for ATL03 photons
#'
#' @param atl03 An object of class `icesat2.atl03_dt`
#'
#' @export
`summary.icesat2.atl03_dt` <- function(x) {
  summary(x@dt)
}

#' Number of rows for ATL03 photons
#'
#' @param atl03 An object of class `icesat2.atl03_dt`
#'
#' @export
`nrow.icesat2.atl03_dt` <- function(x) {
  print(head(x@dt))
  nrow(x@dt)
}

#' Summary for merged ATL03 ATL08 photons
#'
#' @param atl03_atl08_dt An object of class `icesat2.atl03atl08_dt`
#' @export
`summary.icesat2.atl03atl08_dt` <- function(x) {
  summary(x@dt)
}

#' Number of rows for merged ATL03 ATL08 photons
#'
#' @param atl03_atl08_dt An object of class `icesat2.atl03atl08_dt`
#'
#' @export
`nrow.icesat2.atl08atl03_dt` <- function(x) {
  print(head(x@dt))
  nrow(x@dt)
}

#' Head for ATL08 photons
#'
#' @param atl03_atl08_dt An object of class `icesat2.atl03atl08_dt`
#' @export
`head.icesat2.atl03atl08_dt` <- function(x) {
  head(x@dt)
}


#' Get Beams for icesat h5 classes
#'
#' @param h5 An object of class [`icesat2.atl03_h5-class`] or [`icesat2.atl08_h5-class`]
#' @param ... Other parameters to dispatch to the hdf5 library
#'
#' @export
setGeneric("getBeams", function(h5, ...) {
  standardGeneric("getBeams")
})


setMethod(
  "getBeams",
  signature = c("icesat2.h5"),
  function(h5, ...) {
    groups <- h5@h5$ls()$name
    grep("gt[1-3][lr]", groups, value = TRUE)
  }
)


#' @importFrom data.table data.table
setRefClass("data.table")

#' Class for ATL08 attributes
#'
#' @seealso [`data.table`][data.table::data.table-class] in the `data.table` package and
#' \url{https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2_ATL08_ATBD_r006.pdf}
#'
#' @import methods
#' @exportClass icesat2.atl08_dt
setRefClass("icesat2.atl08_dt")

#' Class for ATL03 attributes
#'
#' @slot data.table Object of class [`data.table`][data.table::data.table-class] from `data.table` package containing the
#' Extracted ICESat-2 ATL03 attributes

#'
#' @seealso [`data.table`][data.table::data.table-class] in the `data.table` package and
#' \url{https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2_ATL03_ATBD_r006.pdf}
#'
#' @import methods
#' @export
icesat2.atl03_dt <- setClass(
  Class = "icesat2.atl03_dt",
  slots = list(dt = "data.table")
)

#' Class for joined ATL03 and ATL08 attributes
#'
#' @slot data.table Object of class [`data.table`][data.table::data.table-class]
#' from `data.table` package containing the
#' Joined ICESat-2 ATL03 and ATL08 attributes
#'
#' @seealso [`data.table`][data.table::data.table-class] in the `data.table` package and
#' \url{https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2_ATL03_ATBD_r006.pdf}
#'
#' @import methods
#' @export
icesat2.atl03atl08_dt <- setClass(
  Class = "icesat2.atl03atl08_dt",
  slots = list(dt = "data.table")
)


h5closeall <- function(con, ...) {
  try(con@h5$close_all(), silent = TRUE)
}


#' Safely closes the [`rICESat2Veg::icesat2.atl03_h5-class`]
#'
#' @description
#' Closing files will avoid locking HDF5 ATL03 files.
#'
#' @param con An object of class `icesat2.atl03_h5`
#' @param ... Inherited from base
#'
#' @export
#' @rdname close
#' @method close icesat2.atl03_h5
setMethod("close", signature = c("icesat2.atl03_h5"), h5closeall)


#' Safely closes the [`rICESat2Veg::icesat2.atl08_h5-class`]
#'
#' @description
#' Closing files will avoid locking HDF5 ATL08 files.
#'
#' @param con An object of class `icesat2.atl08_h5`
#' @param ... Inherited from base
#' @method close icesat2.atl08_h5
#' @rdname close
setMethod("close", signature = c("icesat2.atl08_h5"), h5closeall)


#' Plot photons from ATL03 and ATL08 joined products
#'
#' @description This function plots photons along track
#'
#' @param x An object of class [`rICESat2Veg::icesat2.atl03atl08_dt-class`]
#' @param y photon attribute to be plot (ph_h or h_ph)
#'@param beam Character vector indicating only one beam to process ("gt1l", "gt1r", "gt2l", "gt2r", "gt3l", "gt3r").
#'Default is "gt1r"
#' @param colors A vector containing colors for plotting noise, terrain, vegetation and top canopy photons
#' (e.g. c("gray", "#bd8421", "forestgreen", "green")
#' @param ... will be passed to the main plot
#'
#' @return No return value
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
#atl03_h5<-ATL03_read(atl03_path=atl03_path)
#'
#'# Reading ATL08 data (h5 file)
#atl08_h5<-ATL08_read(atl08_path=atl08_path)
#'
#'# Extracting ATL03 and ATL08 photons and heights
#'atl03_atl08_dt<-ATL03_ATL08_photons_attributes_dt_join(atl03_h5,atl08_h5)
#'
#'rICESat2Veg::plot(atl03_atl08_dt=atl03_atl08_dt,attribute="ph_h",
#'                 colors = c("gray", "#bd8421", "forestgreen", "green"),
#'                 pch = 16, cex = 0.5)
#'
#'close(atl03_h5)
#'close(atl08_h5)
#' @export
#' @method plot icesat2.atl03atl08_dt
#' @rdname plot
setMethod(
  f = "plot",
  signature("icesat2.atl03atl08_dt", y = "missing"),
  definition = function(x, y, beam="gt1l",
                        colors=c("gray", "#bd8421", "forestgreen", "green"),
                        xlim=NULL,
                        ylim=NULL,...) {
    if (!is(x, "icesat2.atl03atl08_dt")) {
      print("Invalid input file. It should be an object of class 'icesat2.atl03atl08_dt' ")
    } else {

      #colors <- c("gray", "#bd8421", "forestgreen", "green")
      xdt<-x@dt[x@dt$beam==beam,c("dist_ph_along",y,"classed_pc_flag"), with = FALSE]

      if (is.null(xlim)){xlim=range(xdt$dist_ph_along)}
      if (is.null(ylim)){ylim=range(xdt[,get(y)], na.rm=T)}

      mask<-xdt$dist_ph_along >= xlim[1] &
        xdt$dist_ph_along <= xlim[2] &
        xdt[,2] >= ylim[1] &
        xdt[,2] <= ylim[2]

      mask[!stats::complete.cases(mask)] <- FALSE
      mask <- (seq_along(xdt$dist_ph_along))[mask]
      newFile <- xdt[mask, ]

      colorMap <- colors[newFile$classed_pc_flag + 1]


      suppressWarnings({
          plot(x=newFile$dist_ph_along,
               y=newFile[,get(y)],
               col = colorMap,xlim=xlim, ylim=ylim, xlab="Distance along-track (m)", ylab=paste(y,"(m)"),...)
          legend("topleft", legend=c("ATL03 unclassified","ATL03 Terrain", "ATL03 Vegetation", "ATL03 Top canopy"), pch=16, col=colors, bty="n")
        })

    }
  }
)


#' Plot ATL08 photons
#'
#' @description This function plots photons along track
#'
#' @param x An object of class [`rICESat2Veg::icesat2.atl08_dt-class`]
#'@param beam Character vector indicating only one beam to process ("gt1l", "gt1r", "gt2l", "gt2r", "gt3l", "gt3r").
#'Default is "gt1r"
#' @param colors A vector containing colors for plotting noise, terrain, vegetation and top canopy photons
#' (e.g. c("gray", "#bd8421", "forestgreen", "green")
#' @param ... will be passed to the main plot
#'
#' @return No return value
#'
#' @examples
#'# Specifying the path to atl08 and ATL08 file (zip file)
#'outdir = tempdir()
#'atl08_zip <- system.file("extdata",
#'                   "atl08_20220401221822_01501506_005_01.zip",
#'                   package="rICESat2Veg")
#'
#'atl08_zip <- system.file("extdata",
#'                   "ATL08_20220401221822_01501506_005_01.zip",
#'                   package="rICESat2Veg")
#'
#'# Unzipping atl08 file
#'atl08_path <- unzip(atl08_zip,exdir = outdir)
#'
#'# Reading atl08 data (h5 file)
#atl08_h5<-atl08_read(atl08_path=atl08_path)
#'
#'# Extracting atl08 and ATL08 photons and heights
#'atl08_photons_dt<-atl08_seg_attributes_dt(atl08_h5=atl08_h5)
#'
#'rICESat2Veg::plot(atl08_photons_dt=atl08_photons_dt,
#'                 beam="gt1r",
#'                 colors = c("gray", "#bd8421", "forestgreen", "green"),
#'                 pch = 16, cex = 0.5)
#'
#'close(atl08_h5)
#'close(atl08_h5)
#' @export
#' @method plot icesat2.atl08_dt
#' @rdname plot
plot.icesat2.atl08_dt <- function(x, y, beam="gt1l",
                        colors=c("gray", "#bd8421", "forestgreen", "green"),
                        xlim=NULL,
                        ylim=NULL,...) {
    if (!is(x, "icesat2.atl08_dt")) {
      print("Invalid input file. It should be an object of class 'icesat2.atl03atl08_dt' ")
    } else {

      #colors <- c("gray", "#bd8421", "forestgreen", "green")
      xdt<-x@dt[x@dt$beam==beam,c("dist_ph_along",y,"classed_pc_flag"), with = FALSE]

      if (is.null(xlim)){xlim=range(xdt$dist_ph_along)}
      if (is.null(ylim)){ylim=range(xdt[,get(y)])}

      mask<-xdt$dist_ph_along >= xlim[1] &
        xdt$dist_ph_along <= xlim[2] &
        xdt[,2] >= ylim[1] &
        xdt[,2] <= ylim[2]

      mask[!stats::complete.cases(mask)] <- FALSE
      mask <- (seq_along(xdt$dist_ph_along))[mask]
      newFile <- xdt[mask, ]

      colorMap <- colors[newFile$classed_pc_flag + 1]

      suppressWarnings({
        plot(x=newFile$dist_ph_along,
             y=newFile[,get(y)],
             col = colorMap,xlim=xlim, ylim=ylim,...)
        legend("topleft", legend=c("Noise","Terrain", "Vegetation", "Top canopy"), pch=16, col=colors, bty="n")
      })

    }
  }



#' Plot atl03 photons
#'
#' @description This function plots photons along track
#'
#' @param x An object of class [`rICESat2Veg::icesat2.atl03_dt-class`]
#' @param beam Character vector indicating only one beam to process ("gt1l", "gt1r", "gt2l", "gt2r", "gt3l", "gt3r").
#'Default is "gt1r"
#' @param col  Color for plotting the photons. Default is "gray"
#' @param ... will be passed to the main plot
#'
#' @return No return value
#'
#' @examples
#'# Specifying the path to atl03 and atl03 file (zip file)
#'outdir = tempdir()
#'atl03_zip <- system.file("extdata",
#'                   "atl03_20220401221822_01501506_005_01.zip",
#'                   package="rICESat2Veg")
#'
#'atl03_zip <- system.file("extdata",
#'                   "atl03_20220401221822_01501506_005_01.zip",
#'                   package="rICESat2Veg")
#'
#'# Unzipping atl03 file
#'atl03_path <- unzip(atl03_zip,exdir = outdir)
#'
#'# Reading atl03 data (h5 file)
#atl03_h5<-atl03_read(atl03_path=atl03_path)
#'
#'# Extracting atl03 and atl03 photons and heights
#'atl03_photons_dt<-atl03_seg_attributes_dt(atl03_h5=atl03_h5)
#'
#'rICESat2Veg::plot(atl03_photons_dt
#'                 col = "gray",
#'                 pch = 16,
#'                 cex = 0.5)
#'
#'close(atl03_h5)
#'close(atl03_h5)
#' @export
#' @method plot icesat2.atl03_dt
#' @rdname plot
setMethod(
  f = "plot",
  signature("icesat2.atl03_dt", y = "missing"),
  definition = function(x, y, col,...) {
    if (!is(x, "icesat2.atl03_dt")) {
      print("Invalid input file. It should be an object of class 'icesat2.atl03_dt' ")
    } else {

      x<-x[x$beam==beam,]
      suppressWarnings({
        plot(x=x$dist_along, y=x$h_ph, col = col,xlab="Distance along-track (m)", ylab="Elevation (m)",...)
        legend("topleft", legend=c("Noise","Terrain", "Vegetation", "Top canopy"), pch=16, col=colors, bty="n")
      })

    }
  }
)
