# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(lat1, long1, lat2, long2) {
  R <- 6371 # Earth mean radius [km]
  lat1 <- deg2rad(lat1)
  lat2 <- deg2rad(lat2)
  long1 <- deg2rad(long1)
  long2 <- deg2rad(long2)
  d <- R * acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) 
  return(d) # Distance in km
}