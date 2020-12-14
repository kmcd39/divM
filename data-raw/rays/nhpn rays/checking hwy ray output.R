# setup ws ----------------------------------------------------------------
source("places & rays/setup ray ws.R")

devtools::load_all()





# checking variations ---------------------------------------------

profvis::profvis({
  count.rays(plc.ids["Philadelphia"],
             plc, hwys
             #,include.intersectiong = T
             #,min.segment.length = 10
  )
})


divM::count.rays(plc.ids["Greensboro"],
           plc, hwys
           ,include.intersecting = T
           ,min.segment.length = 10
           ,ray.node.distance.threshold= NULL
)


count.rays(plc.ids["Houston"],
           plc, hwys
           ,include.intersecting = T
           ,min.segment.length = 10
           ,ray.node.distance.threshold= NULL #100
)

names(plc.ids)
plc.ids["Huntsville"]

count.rays(plc.ids["Oklahoma City"],
           plc, hwys
           ,include.intersecting = T
           ,min.segment.length = 10
           ,ray.node.distance.threshold= NULL #100
)


phl <- count.rays(plc.ids["Philadelphia"],
           plc, hwys
           ,include.intersecting = T
           ,min.segment.length = 10
           ,ray.node.distance.threshold= NULL #100
)

phl$map +
  mapview(
    st_boundary(
      st_convex_hull(plc[plc$GEOID %in% plc.ids["Philadelphia"], ])
    ))


# test viz's for the ray in a void peninsula issue?
void.test.viz <- function(plc.id , ...) {

  rays <- count.rays(plc.id,
                    plc, hwys
                    ,include.intersecting = T
                    ,min.segment.length = 10
                    ,ray.node.distance.threshold= 100
                    , cex = 12, ...
  )

  rays$map +
    mapview(
      st_boundary(
        st_convex_hull(plc[plc$GEOID %in% plc.id, ])
      ))
}

void.test.viz(plc.ids["Philadelphia"])

void.test.viz(plc.ids["Boston"])
void.test.viz(plc.ids["Houston"])
st_



tmp <- plc[plc$GEOID %in% plc.ids["Houston"], ]$geometry %>%
  st_coordinates()

tmp %>%
  data.frame() %>%
  filter(L2 == 2)
  st_sfg()

tmp %>%
  data.frame() %>%
  count(L2)
  st_multipolygon(lapply(., function(x) x[1])) %>%
  mapview()

hstn <- plc[plc$GEOID %in% plc.ids["Houston"], ]
hwys %>%
  st_intersection(buffered.hull(hstn)) %>%
  mapview() +
  mapview(st_boundary(buffered.hull(hstn))
          ,color = "red") +
  mapview(st_boundary(hstn)
          ,color = "green")


modded.hstn <- nngeo::st_remove_holes(hstn)

mapview(st_boundary(hstn)) +
  mapview(modded.hstn, lwd = 3,
            color = "#208000")

mapview(st_boundary(modded.hstn), lwd = 3,
          color = "#208000") +
  mapview(
    st_boundary(
      st_buffer(modded.hstn, 100))
    , lwd = 3
    ,color = "#800020")




# checking versions of the output -----------------------------------------
v1 <- read.csv("~/R/all sharkey geoseg work/dividedness-measures/outputs/rays-version-1.csv")
v2 <- read.csv("~/R/all sharkey geoseg work/dividedness-measures/outputs/rays-version-2.csv")

v1 %>% head()
v2 %>% head()

v1[v1$rays_interstate_only != v2$rays_interstate_only, ] %>% nrow()

v1[v1$rays_include_intersecting != v2$rays_include_intersecting, ] %>% nrow()
v1 %>%
  filter(rays_include_intersecting != v2$rays_include_intersecting) %>%
  select(2,3,8,9)

rayv <- readRDS("outputs/natl-rays_interstates-and-intersections.RDS")
rayv$Miami
v2[v2$place_name=="Miami",]
count.rays(plc.ids["Miami"],
           plc, hwys,
           include.intersecting = T)


v1[v1$place_name=="Greenville",]
v2[v2$place_name=="Greenville",]

rayv[names(rayv) == "Greenville"][4]
count.rays(4530850 ,
           plc, hwys,
           include.intersecting = T)



# checking plan rays ------------------------------------------------------
bfpr <- read.csv("~/R/all sharkey geoseg work/dividedness-measures/outputs/1947-plan-rays_buffered-places.csv")
pr <- read.csv("~/R/all sharkey geoseg work/dividedness-measures/outputs/1947-plan-rays.csv")

bfpr %>% head()
pr %>% head()

list(bfpr, pr) %>% map(~summary(.$plan_rays))


pr[pr$plan_rays != bfpr$plan_rays, ]
bfpr[pr$plan_rays != bfpr$plan_rays, ]
