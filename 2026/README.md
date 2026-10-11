# 30DayMapChallenge 2026: Transport in Uppsala

30 maps in November 2026, all about transport in Uppsala. Follows the Uppsala transport theme of the 2026 #30DayChartChallenge, and reuses its data where noted (Trafa punctuality, NVDB network length, UL ridership, SMHI emissions, Mobility Sweden registrations).

Themes and descriptions from [30daymapchallenge.com](https://30daymapchallenge.com).

## Shared data sources

🔑 = free API key or login needed.

| Source | Link | Notes |
|---|---|---|
| OpenStreetMap, Sweden | `https://download.geofabrik.de/europe/sweden-latest.osm.pbf` | Overpass for small extracts: `https://overpass-api.de/api/interpreter` |
| UL GTFS 🔑 | `https://opendata.samtrafiken.se/gtfs/ul/ul.zip?key=KEY` | [Trafiklab GTFS Regional](https://www.trafiklab.se/api/gtfs-datasets/gtfs-regional/). Static data updated daily, plus GTFS-RT |
| GTFS Sweden 3 🔑 | [Trafiklab](https://trafiklab.se/api/gtfs-datasets/gtfs-sweden) | Aggregates GTFS Regional, plus all Trafikverket passenger rail since [June 2026](https://www.trafiklab.se/news/2026/2026-06-01-trafikverket-data-gtfs-sweden-3/). Not yet every operator; [GTFS Sverige 2](https://www.trafiklab.se/api/trafiklab-apis/gtfs-sverige-2) is the only complete national feed (static, no shapes) |
| Trafikverket Lastkajen 🔑 | [trafikverket.se/lastkajen](https://www.trafikverket.se/lastkajen) | NVDB roads, GCM cycle network, rail network, speed limits, ÅDT |
| Trafikverket open API 🔑 | `https://api.trafikinfo.trafikverket.se/v2/data.json` | [Docs](https://www.trafiklab.se/api/trafiklab-apis/trafikverket/). `TrainAnnouncement` for train counts and delays |
| Lantmäteriet Geotorget 🔑 | [geotorget.lantmateriet.se](https://geotorget.lantmateriet.se) | [DTM grid 1](https://lantmateriet.se/en/geodata/geodata-products/product-list/terrain-model-download-grid-1), Laserdata skog, [Byggnad vektor](https://lantmateriet.se/en/geodata/geodata-products/product-list/building-download-vector) (footprints, no heights) |
| SCB open geodata | WFS `https://geodata.scb.se/geoserver/stat/wfs` | [DeSO 2025](https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/demografiska-statistikomraden-deso/), RegSO, tätorter, [1 km grid](https://scb.se/vara-tjanster/oppna-data/oppna-geodata/statistik-pa-rutor/) |
| SCB Statistikdatabasen | [Vehicles](https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001A), [commuting](https://www.statistikdatabasen.scb.se/goto/sv/ssd/AM0207PendlKomA04N) | Car ownership per DeSO is in the DeSO statistics package. Commuting is kommun-to-kommun only |
| Uppsala kommun geodata | [dataportal.se](https://www.dataportal.se) (search "Uppsala kommun") | WFS, SWEREF 99 18 00, CC-BY. Questions to geodata@uppsala.se. Most transport data is PDF only |

Slow sources need a request, so ask early: STRADA, noise and NO2 rasters, cycle counter series, parking zone GIS.

## Scripts

One script per day in `NN-theme/`, sharing `R/helpers.R`. Paths use `here::here("2026", ...)`, so the folder is meant to sit at `2026/` in the 30daymapchallenge project. Downloads and OSM queries are cached in `2026/data/`.

The scripts have not been run yet. They were written without network access to the data or an R install, so expect small fixes on first run.

Setup:

- Packages: tidyverse, sf, here, janitor, camcorder, osmdata, tidytransit, MetBrewer, ggrepel, patchwork, terra, h3jsr, igraph, mapgl, htmlwidgets, jsonlite, hms. r5r for days 3 and 29 also needs Java 21.
- Keys in `~/.Renviron`: `TRAFIKLAB_GTFS_REGIONAL_KEY` (all UL days) and `TRAFIKLAB_GTFS_SWEDEN_KEY` (day 13).
- Manual downloads:
  - `data/r5/uppsala.osm.pbf` for days 3 and 29, cut from Geofabrik's Sweden extract: `osmium extract -b 16.7,59.3,18.95,60.75 sweden-latest.osm.pbf -o uppsala.osm.pbf`.
  - `data/dem/*.tif` for day 5: Lantmäteriet Markhöjdmodell grid 1 tiles from Geotorget.
  - Optional `data/scb-befolkning-1km.gpkg` for day 9: SCB population per 1 km square.

| Day | Script | Map | Data |
|---|---|---|---|
| 1 | `01-points-stops.R` | Stops and stations sized by weekday departures | UL GTFS |
| 2 | `02-lines-bus-routes.R` | Bus routes, width by trips, dark | UL GTFS |
| 3 | `03-polygons-isochrones.R` | 15/30/45 min from Uppsala C by bike and by bus | UL GTFS, OSM, r5r |
| 4 | `04-clusters-stop-patterns.R` | Stops clustered by hourly departure profile | UL GTFS |
| 5 | `05-sight-domkyrka-viewshed.R` | Viewshed from the cathedral towers over cycleways | Lantmäteriet DEM, OSM |
| 6 | `06-vintage-tram.R` | The 1906–1953 tram in an old-map style | OpenHistoricalMap, OSM |
| 7 | `07-10-minute-stations.R` | Train stations in the län | OSM |
| 8 | `08-utopia-tram-built.R` | The tram under construction, drawn as if running | OSM |
| 9 | `09-urban-rural-service-gap.R` | Distance to a stop with 10+ departures, 1 km grid | UL GTFS, OSM, SCB (optional) |
| 10 | `10-prompting-only-ul-network.R` | Whole UL network by city bus, regional bus and train | UL GTFS |
| 11 | `11-sound-noise-barriers.R` | Noise barriers along loud roads and rail | OSM |
| 12 | `12-power-rail-electrification.R` | Electrified rail, traction substations, grid | OSM |
| 13 | `13-interactions-interchanges.R` | Departures and operators around each station | GTFS Sweden 3, OSM |
| 14 | `14-borgesian-bike-parking.R` | Every bike space in the centre at true size | OSM |
| 15 | `15-inside-out-bike-city.R` | Cycleways as main roads, roads as hairlines | OSM |
| 16 | `16-collaborative-osm-notes.R` | OSM notes about transport, open and fixed | OSM Notes API |
| 17 | `17-light-dark-night-buses.R` | Glowing night routes, 23:00–05:00 | UL GTFS, OSM |
| 18 | `18-null-road-tags.R` | Roads with no maxspeed or surface tag | OSM |
| 19 | `19-smell-cafes-bike-routes.R` | Cafés and bakeries by distance to cycleways | OSM |
| 20 | `20-hexagons-departures.R` | Departures per hour in H3 cells | UL GTFS, h3jsr |
| 21 | `21-openstreetmap-bus-stop-completeness.R` | UL stops with and without an OSM stop; writes the missing ones to GeoJSON | UL GTFS, OSM |
| 22 | `22-projections-mercator.R` | Kommun area and Uppsala–Stockholm distance, SWEREF vs Web Mercator | OSM |
| 23 | `23-taste-station-food.R` | Food within 400 m of each station | OSM |
| 24 | `24-network-betweenness.R` | Bus network graph, stops sized by betweenness | UL GTFS, igraph |
| 25 | `25-is-this-a-map-stringline.R` | Stringline diagram of line 1 beside its route | UL GTFS |
| 26 | `26-water-fyrisan-bridges.R` | Bridges over Fyrisån by mode | OSM |
| 27 | `27-new-tool-mapgl.R` | Interactive MapLibre map of routes and stops, saved as HTML | UL GTFS, mapgl |
| 28 | `28-feeling-waiting.R` | Average wait per stop on a calm-to-fuming scale | UL GTFS, OSM |
| 29 | `29-raster-travel-time.R` | Travel-time raster from Uppsala C, 250 m pixels | UL GTFS, OSM, r5r |
| 30 | — | Pen and paper | — |

## Days

### Nov 1 — Points
1. Every UL bus stop and railway station in Uppsala län, sized by weekday departures.
2. Bike-share, e-scooter, bike-pump points and cykelräknare as a dot map of cycling infrastructure.
3. EV charging points coloured by operator and power.

Data: GTFS `stops` + `stop_times`. OSM `amenity=bicycle_rental`, `amenity=bicycle_repair_station`, `service:bicycle:pump=yes` (on its own `amenity=compressed_air` includes car compressors), `amenity=charging_station`. Counter totals in [cykelbokslut 2023](https://www.uppsala.se/contentassets/4282024145b24b65924c402cf99e1c41/cykelbokslut_2023.pdf) and [Cykelåret 2024](https://uppsala.se/contentassets/08a716f96ad04b18898ef49625629c17/cykelaret_2024.pdf). Nobil API 🔑 `https://nobil.no/api/server/` has better power fields than OSM.

### Nov 2 — Lines
1. All UL bus routes, width by trips per day, dark background.
2. Cycle paths versus car roads in Uppsala kommun.
3. Uppsala–Gävle and Uppsala–Stockholm rail lines, width by trains per day.

Data: GTFS `shapes` + `trips`. OSM and Lastkajen NVDB GCM. Lastkajen rail network with GTFS Sweden 3 or `TrainAnnouncement` for train counts.

### Nov 3 — Polygons
1. Parking zones and fee areas in the city centre.
2. DeSO areas by share of households with a car, bivariate with bike use.
3. Isochrones from Uppsala C for 15/30/45 minutes by bike and by bus.

Data: Fee zones are described in the [2025 parking fee decision](https://www.uppsala.se/contentassets/dc16a991b5b3499392d4819fc6604368/27.-nya-parkeringstaxor-i-uppsala.pdf) with no GIS, so digitise or ask geodata@uppsala.se. SCB DeSO boundaries + DeSO car ownership. Bike use is not published per DeSO. GTFS + OSM pbf in r5r.

### Nov 4 — Clusters
1. Cyclist accident clusters with DBSCAN.
2. Bus stops clustered by departure pattern.
3. Bike parking and theft hotspots near Uppsala C.

Data: [STRADA for researchers](https://www.transportstyrelsen.se/sv/om-oss/statistik-och-analys/statistik-inom-vagtrafik/olycksstatistik/olycksdata-for-forskare/) needs an application with weeks of lead time. GTFS. OSM `amenity=bicycle_parking`. No open source supports idea 3. The police events API (`https://polisen.se/api/events`) is a recent, selected set of events with approximate locations, not geolocated theft reports.

### Nov 5 — Senses: Sight
1. Viewshed from the domkyrka towers over the cycle network.
2. Sight lines at dangerous junctions or level crossings.
3. Best views from the Uppsala–Stockholm line.

Data: Geotorget DTM grid 1 and Laserdata skog 🔑. Viewshed with `terra` or GRASS `r.viewshed`.

### Nov 6 — Vintage
1. Modern UL network in the style of an 18th-century Upsala map.
2. The Uppsala tram (1906–1953) over a modern OSM base.
3. Uppsala as a 1920s rail hub, including Upsala–Lenna.

Data: [Lantmäteriet historiska kartor](https://historiskakartor.lantmateriet.se). Tram ways in [OpenHistoricalMap](https://www.openhistoricalmap.org/way/200138414) and network PDFs from [Spårvägssällskapet](https://www.sparvagssallskapet.se/fakta/swedish_tramway_cities.php). OSM `railway=abandoned`/`disused`.

### Nov 7 — 10 minute map
1. Cykelräknare locations scaled by yearly count.
2. Uppsala train stations with osmdata and ggplot2.
3. UL zone boundaries with the city centre marked.

Data: cykelbokslut (Nov 1), OSM. No UL zone geodata found.

### Nov 8 — Utopia
1. A car-free Uppsala and the resulting walking distances.
2. The Uppsala tram as if built.
3. Cycle superhighways to Knivsta, Enköping and Östhammar.

Data: OSM. The tram has been under construction since 2024, with service planned for 2029. Route from the [section A–D PM](https://www.uppsala.se/contentassets/816ce04907e74b33a4ea1930b1649904/pm-uppsala-sparvag-spar-och-gatuutformning-18-oktober-2023.pdf). SCB commuting between kommuner.

### Nov 9 — Urban-rural
1. Distance to the nearest stop with 10+ departures per day, on the SCB 1 km grid.
2. Commuting flows from villages into Uppsala as arcs.
3. Car ownership by DeSO, city versus landsbygd.

Data: SCB 1 km grid + GTFS. SCB commuting is kommun-to-kommun only, so village flows need a paid order. DeSO car ownership.

### Nov 10 — Prompting only
1. UL GTFS network, end to end with Claude Code CLI.
2. Bike lanes per kommun from the Chart Challenge NVDB data.
3. Station catchments from OSM.

Data: as above.

### Nov 11 — Senses: Sound
1. Road traffic noise contours.
2. Railway noise along Ostkustbanan.
3. Arlanda flight noise over northern Uppsala län.

Data: Uppsala noise mapping [2022](https://uppsala.se/contentassets/4f9fb79cd3d3495b8e6de01a6198e012/bilaga-bullerkartlaggning-2022.pdf) and [2017](https://www.uppsala.se/contentassets/7117fa5c34d544aa8e4a8caff0a5ac52/slutrapport-bullerkartlaggning-uppsala_webb_6318.pdf) are PDF maps, so ask the kommun for GIS. [EEA END noise data](https://sdi.eea.europa.eu/catalogue/srv/api/records/8a876222-166c-45ac-8462-5cdde7503bd3). Arlanda: [Swedavia quarterly noise reports](https://www.swedavia.se/contentassets/5bdbd9bb67544d72ba086bf40f3795ad/2024---kvartal-3---juli-sept-flygvags--och-flygbullerkontroll-stockholm-arlanda-airport.pdf), no GIS found.

### Nov 12 — Power
1. UL electric bus depots and charging, with grid capacity.
2. New car registrations by kommun, EV share and horsepower.
3. Rail catenary and traction substations.

Data: OSM `power=*`, `electrified=contact_line`, `substation=traction`. Vattenfall Eldistribution capacity map (web only, coarse). [Trafa fordon i län och kommuner 2025](https://www.trafa.se/globalassets/statistik/vagtrafik/fordon/2026/fordon-i-lan-och-kommuner-2025.pdf), Excel on the same Trafa page. Mobility Sweden from the Chart Challenge. Horsepower likely needs a paid Trafa order.

### Nov 13 — Interactions
1. Interactive map of live departures per stop.
2. Bike-car crossing points with accident data.
3. Interchanges between UL bus, regional train and SJ.

Data: Trafiklab Realtime and ResRobot APIs 🔑. Lastkajen GCM × roads, STRADA. GTFS Sweden 3.

### Nov 14 — Borgesian map
1. One intersection at 1:1 with every marking and sign.
2. Every bike parking space in the centre at true size.
3. Platforms and tracks at Uppsala C.

Data: OSM, Geotorget ortofoto. OSM indoor tagging at Uppsala C is likely sparse.

### Nov 15 — Inside out
1. Cycle network as the roads, car network as thin background lines.
2. South-up map with bus lines as outline.
3. Inverted isochrone: area that cannot reach the centre in 30 min by public transport.

Data: OSM, NVDB, GTFS + r5r.

### Nov 16 — Collaborative map
1. Cyclists mark dangerous crossings.
2. Student commute routes with a colleague at Uppsala University.
3. OSM mapathon before/after.

Data: OSM Notes API `https://api.openstreetmap.org/api/0.6/notes`, own survey, MapRoulette.

### Nov 17 — Light & dark
1. Streetlight coverage along cycle paths.
2. Winter shadow map of the bike commute.
3. Night buses glowing on a black Uppsala.

Data: OSM `highway=street_lamp`, `lit=*` (patchy). Byggnad + Laserdata for heights, `suncalc`. GTFS trips after 23:00.

### Nov 18 — NULL
1. Parts of Uppsala län with no GTFS service.
2. Roads with null speed limit or surface.
3. Counters with null counts.

Data: GTFS, NVDB, OSM, counter data.

### Nov 19 — Senses: Smell
1. NO2 and PM10 along main streets.
2. UL fleet by fuel type over route lines.
3. Bakeries and cafés near stations and bike routes.

Data: [SMHI datavärdskap luft](https://datavardluft.smhi.se) station series. Dispersion maps only as PDF in [kommun air quality reports](https://uppsala.se/contentassets/d2f8e09d68b14dc6aa7ee871defc6da0/luftkvalitetsutredning-31-mars-2023.pdf). Fleet fuel from UL/Region Uppsala annual reports. OSM `amenity=cafe`, `shop=bakery`.

### Nov 20 — Hexagons
1. H3 weekday departures per hour.
2. Hex-binned cycle density.
3. Walking-time hexes coloured by daily train connections.

Data: GTFS + `h3jsr`. Strava Metro is for organisations only, so use counters or OSM. GTFS + r5r.

### Nov 21 — OpenStreetMap
1. OSM bus stop completeness versus UL GTFS.
2. Share of cycleways with surface and lit tags.
3. OSM-only transport snapshot.

Data: OSM, GTFS.

### Nov 22 — Projections
1. SWEREF 99 TM versus Web Mercator distortion.
2. Azimuthal equidistant map of direct train and air connections.
3. Travel-time cartogram from Uppsala C.

Data: `sf`, [Swedavia FlightInfo API](https://apideveloper.swedavia.se/) 🔑, GTFS Sweden 3.

### Nov 23 — Senses: Taste
1. Restaurants and cafés within 5 minutes' walk of stations.
2. Restaurants and distance to the nearest cycleway.
3. Food stops along the main bus routes.

Data: OSM, GTFS.

### Nov 24 — Network
1. UL network as a graph with betweenness.
2. Low-stress cycling network and its disconnected parts.
3. Schematic Uppsala–Stockholm–Gävle–Västerås rail network.

Data: GTFS + `tidygraph`. OSM with an LTS classification script. GTFS Sweden 3.

### Nov 25 — Is this a map?
1. Stringline diagram of UL line 1 over Uppsala.
2. Rail delays as a woven texture.
3. Tube map of cycle routes.

Data: GTFS. Trafa punctuality (Excel) and `TrainAnnouncement`. OSM, Lastkajen GCM.

### Nov 26 — Water
1. Bridges over Fyrisån by mode.
2. Flood risk for roads and rail.
3. Boats on Ekoln and former canal routes.

Data: OSM `bridge=yes`, `waterway=river`, `route=ferry` (little ferry traffic near Uppsala). [MCF (formerly MSB) flood portal](https://gisapp.msb.se/apps/oversvamningsportal/). [Länsstyrelsen flood maps for Uppsala, May 2026](https://www.lansstyrelsen.se/uppsala/om-oss/nyheter-och-press/nyheter---uppsala/2026-05-13-nya-hot--och-riskkartor-for-oversvamning-i-uppsala-underlattar-planering.html) and [skyfall data](https://www.lansstyrelsen.se/uppsala/om-oss/vara-tjanster/oppna-data.html).

### Nov 27 — New tool
1. MapLibre GL JS / mapgl for UL routes.
2. r5r or OpenTripPlanner isochrones.
3. QGIS 4 or Mapbox Studio on NVDB.

Data: as above.

### Nov 28 — Senses: Feeling
1. Perceived bike safety on the cycle network.
2. Crowding on UL buses by line and hour.
3. Waiting time at stops as an emotional colour scale.

Data: [Trygghetsundersökning 2023](https://www.uppsala.se/kommun-och-politik/sa-arbetar-vi-med-olika-amnen/sa-arbetar-vi-med-trygghet-och-sakerhet/trygghetsundersokning-2023/) (by area, not network). Per-line ridership is not open, so use the Chart Challenge UL data. GTFS-RT `occupancy_status`, if UL publishes it. GTFS headways.

### Nov 29 — Raster
1. Travel-time raster from Uppsala C.
2. Noise or NO2 raster with roads on top.
3. Population-weighted accessibility on the 1 km grid.

Data: r5r, SCB 1 km grid. Noise and NO2 rasters on request from the kommun.

### Nov 30 — Pen & paper
1. Hand-drawn personal commute.
2. Hand-drawn schematic bus map.
3. Sketch of the bike route around Fyrisån.

Data: none.
