library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(sf)
library(leaflet)
library(ggplot2)

#Load data 
f <- "your/data"      

# the file is \t-separated with a header row
df <- read_tsv(f, show_col_types = FALSE)     # header: type time latitude
head(df)

#Keep track points only, in my case waypoints arent needed
trk_pts <- df %>%
  filter(type == "T") %>%                     # drop waypoints (type == "W")
  mutate(timestamp = ymd_hms(time)) %>%       # parse the timestamp
  arrange(timestamp) %>%
  fill(name, .direction = "down") %>%         # propagate track name (“ACTIVE LOG 001”, …)
  group_by(name) %>%
  mutate(seg_id = cur_group_id())             # numeric id for each track

trk_pts <- trk_pts %>%
  mutate(across(c(longitude, latitude), as.numeric))

# --- rebuild LINESTRINGs safely ------------------------------------------
trk_sf <- trk_pts %>%
  group_by(seg_id) %>%           # one feature per original “ACTIVE LOG …”
  filter(n() >= 2) %>%           # st_linestring() needs at least 2 points
  summarise(
    geometry = st_sfc(
      st_linestring(as.matrix(cbind(longitude, latitude)))
    ),
    .groups = "drop"
  ) %>%
  st_as_sf(crs = 4326)           # set WGS-84



pal <- colorFactor("viridis", trk_sf$seg_id)   # reuse in both places

leaflet(trk_sf) |>
  addProviderTiles("CartoDB.Positron") |>
  addPolylines(
    color  = ~pal(seg_id),
    weight = 3, opacity = 0.9,
    popup  = ~paste("Track", seg_id),          # ← click to see this
    label  = ~paste("Track", seg_id),          # ← shows on hover
    highlightOptions = highlightOptions(       # visually emphasise on hover
      weight = 6, color = "black", bringToFront = TRUE
    )
  ) |>
  addLegend(pal = pal, values = trk_sf$seg_id, title = "Track")


keep_ids <- c() #after looking through map, add what tracks you need

trk_keep <- trk_sf |>                # your sf LINESTRING object
  filter(seg_id %in% keep_ids) 

leaflet(trk_keep) |>
  addProviderTiles("CartoDB.Positron") |>
  addPolylines(color = ~colorFactor("viridis", seg_id)(seg_id),
               weight = 3, opacity = 0.9) |>
  addLegend(pal = colorFactor("viridis", trk_sf$seg_id),
            values = trk_sf$seg_id, title = "Track")


trk_out <- trk_keep |>
  mutate(name = paste0("Track_", seg_id)) |>
  st_zm() |>                         # drop any Z/M dimensions just in case
  st_cast("MULTILINESTRING")         # GPX "tracks" layer wants MultiLine

trk_out_gpx <- trk_out |>
  select(name, geometry)          # keep only GPX-legal fields

str(trk_out_gpx)
st_write(
  trk_out_gpx,
  dsn          = "good_tracks.gpx",
  layer        = "tracks",
  driver       = "GPX",
  delete_dsn   = TRUE             # overwrite if it exists
)

leaflet(trk_out_gpx) |>
  addProviderTiles("CartoDB.Positron") |>
  addPolylines(color = ~colorFactor("viridis", seg_id)(seg_id),
               weight = 3, opacity = 0.9) |>
  addLegend(pal = colorFactor("viridis", trk_sf$seg_id),
            values = trk_sf$seg_id, title = "Track")
