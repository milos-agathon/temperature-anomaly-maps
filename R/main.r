#################################
# Map temperature anomaly with R
# Milos Popovic 2024-06-04
#################################

# 1. PACKAGES
#------------

install.packages("pacman")

pacman::p_load(
    R.utils,
    terra,
    giscoR,
    sf,
    classInt,
    tidyverse,
    tidyterra,
    colorspace
)

# 2. QUERY TEMPERATURE DATA
#---------------------------

url <- "https://data.giss.nasa.gov/pub/gistemp/gistemp1200_GHCNv4_ERSSTv5.nc.gz"

destfile <- basename(url)

download.file(
    url = url,
    destfile = destfile,
    mode = "wb"
)

R.utils::gunzip(
    destfile
)

temp_anomaly <- terra::rast(
    gsub(
        ".gz",
        "",
        destfile
    )
)

# 3. WORLD SHAPEFILE
#----------------

world_sf <- giscoR::gisco_get_countries(
    spatialtype = "BN"
)

# 4. APRIL 2024 ANOMALY
#----------------------

temp_anomaly_april2024 <- terra::subset(
    temp_anomaly,
    time(
        temp_anomaly
    ) == as.Date(
        "2024-04-15"
    )
)

terra::plot(temp_anomaly_april2024)
plot(sf::st_geometry(world_sf), add = T)

world_bb <- sf::st_union(
    sf::st_make_grid(
        sf::st_bbox(
            world_sf
        ),
        n = 100
    )
)
    
terra::plot(temp_anomaly_april2024_proj)
plot(sf::st_geometry(world_sf_proj), add = TRUE)
plot(sf::st_geometry(world_bb), add = TRUE)

temp_anomaly_april2024_proj <- terra::crop(
    temp_anomaly_april2024,
    world_bb
)

# 5. MAP APRIL 2024 ANOMALY
#--------------------------

breaks <- classInt::classIntervals(
    terra::values(
        temp_anomaly_april2024_proj
    ),
    n = 8,
    style = "pretty"
)$brks

cols <- hcl.colors(
    n = length(breaks),
    palette = "Temps"
)

theme_for_the_win <- function() {
    theme_void() +
        theme(
            legend.position = "right",
            legend.title = element_text(
                size = 20, color = "grey20"
            ),
            legend.text = element_text(
                size = 15, color = "grey20"
            ),
            plot.title = element_text(
                size = 30, color = "grey40",
                hjust = .5, vjust = -1
            ),
            plot.caption = element_text(
                size = 10, color = "grey40",
                hjust = .5, vjust = 10
            ),
            plot.margin = unit(
                c(
                    t = 0, b = 0,
                    l = 0, r = .5
                ), "lines"
            )
        )
}

robinson_proj <- "+proj=robin +over"

map1 <- ggplot() +
    tidyterra::geom_spatraster(
        data = temp_anomaly_april2024_proj
    ) +
    geom_sf(
        data = world_sf_proj,
        color = "grey20",
        linewidth = .5,
        fill = "transparent"
    ) +
    geom_sf(
        data = world_bb,
        color = "grey20",
        linewidth = .15,
        fill = "transparent"
    ) +
    # colorspace::scale_fill_binned_divergingx(
    #     name = "°C",
    #     palette = "Temps",
    #     na.value = "white",
    #     mid = 0,
    #     breaks = breaks
    # ) +
    scale_fill_gradient2(
        name = "°C",
        low = "#089392",
        mid = "#EAE29C",
        high = "#CF597E",
        midpoint = 0,
        breaks = breaks,
        labels = round(breaks, 0),
        na.value = "white"
    ) +
    coord_sf(crs = robinson_proj) +
    guides(
        fill = guide_colorbar(
            direction = "vertical",
            barheight = unit(60, units = "mm"),
            barwidth = unit(3, units = "mm"),
            title.position = "top",
            label.position = "left",
            label.hjust = 0,
            title.hjust = .5,
            byrow = FALSE
        )
    ) +
    labs(
        title = "Temperature anomaly for April 2024 (vs. average for 1950-1980)",
        caption = "Data: NASA GISS, Land-Ocean Temperature Index, ERSSTv5, 1200km smoothing"
    ) +
    theme_for_the_win()

# 6. 2023 ANOMALY
#----------------

temp_anomaly_2023 <- temp_anomaly |>
    terra::subset(
        time(temp_anomaly) >= as.Date("2023-01-15") &
            time(temp_anomaly) <= as.Date("2023-12-15")
    ) |>
    terra::app(fun = mean)

temp_anomaly_2023_proj <- terra::project(
    temp_anomaly_2023,
    robinson_proj
) |>
    terra::crop(world_bb)

breaks <- classInt::classIntervals(
    terra::values(
        temp_anomaly_2023_proj
    ),
    n = 8,
    style = "pretty"
)$brks

cols <- hcl.colors(
    n = length(breaks),
    palette = "Temps"
)

map2 <- ggplot() +
    tidyterra::geom_spatraster(
        data = temp_anomaly_2023_proj
    ) +
    geom_sf(
        data = world_sf_proj,
        color = "grey20",
        linewidth = .5,
        fill = "transparent"
    ) +
    geom_sf(
        data = world_bb,
        color = "grey20",
        linewidth = .15,
        fill = "transparent"
    ) +
    colorspace::scale_fill_binned_divergingx(
        name = "°C",
        palette = "Temps",
        na.value = "white",
        mid = 0,
        breaks = breaks
    ) +
    # scale_fill_gradient2(
    #     name = "°C",
    #     low = "#089392",
    #     mid = "#EAE29C",
    #     high = "#CF597E",
    #     midpoint = 0,
    #     breaks = breaks,
    #     labels = round(breaks, 0),
    #     na.value = "white"
    # ) +
    coord_sf(crs = robinson_proj) +
    guides(
        fill = guide_colorbar(
            direction = "vertical",
            barheight = unit(60, units = "mm"),
            barwidth = unit(3, units = "mm"),
            title.position = "top",
            label.position = "left",
            label.hjust = 0,
            title.hjust = .5,
            byrow = FALSE
        )
    ) +
    labs(
        title = "Temperature anomaly for 2023 (vs. average for 1950-1980)",
        caption = "Data: NASA GISS, Land-Ocean Temperature Index, ERSSTv5, 1200km smoothing"
    ) +
    theme_for_the_win()

# 7. ORTHOGRAPHIC PROJECTION
#---------------------------

ortho_proj <- "+proj=ortho +lat_0=32.4279 +lon_0=53.688"

temp_anomaly_april2024_ortho <- terra::project(
    temp_anomaly_april2024,
    ortho_proj
)

install.packages("mapview")

world_sf_ortho <- world_sf |>
    sf::st_transform(crs = ortho_proj) |>
    dplyr::mutate(
        num_points = mapview::npts(
            geometry,
            by_feature = TRUE
        )
    ) |>
    dplyr::filter(num_points >= 4) # should be filter

map3 <- ggplot() +
    tidyterra::geom_spatraster(
        data = temp_anomaly_april2024_ortho
    ) +
    geom_sf(
        data = world_sf_ortho,
        color = "grey20",
        linewidth = .5,
        fill = "transparent"
    ) +
    # geom_sf(
    #     data = world_bb,
    #     color = "grey20",
    #     linewidth = .15,
    #     fill = "transparent"
    # ) +
    colorspace::scale_fill_binned_divergingx(
        name = "°C",
        palette = "Temps",
        na.value = "white",
        mid = 0,
        breaks = breaks
    ) +
    # scale_fill_gradient2(
    #     name = "°C",
    #     low = "#089392",
    #     mid = "#EAE29C",
    #     high = "#CF597E",
    #     midpoint = 0,
    #     breaks = breaks,
    #     labels = round(breaks, 0),
    #     na.value = "white"
    # ) +
    guides(
        fill = guide_colorbar(
            direction = "vertical",
            barheight = unit(60, units = "mm"),
            barwidth = unit(3, units = "mm"),
            title.position = "top",
            label.position = "left",
            label.hjust = 0,
            title.hjust = .5,
            byrow = FALSE
        )
    ) +
    labs(
        title = "Temperature anomaly for April 2024 (vs. average for 1950-1980)",
        caption = "Data: NASA GISS, Land-Ocean Temperature Index, ERSSTv5, 1200km smoothing"
    ) +
    theme_for_the_win()



