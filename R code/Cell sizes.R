#Bethany Allen   25th Jan 2023
#Code to calculate the areas of each grid cell

#Code uses the equation of Santini et al. (2010)
# S = R^2(λ2-λ1)(sinφ2-sinφ1)
# where
# S = surface area of a latitude-longitude grid cell
# R = radius of the Earth
# λ1, λ2 = longitude bounds of the cell, in radians
# φ1, φ2 = latitude bounds of the cell, in radians

#Mean radius of the Earth
R = 6371008.8

#Desginate edges and convert to radians
latitude_a <- 83.5
latitude_a <- as_units(latitude_a, "degrees")
latitude_a <- set_units(latitude_a, "radians")

latitude_b <- 84
latitude_b <- as_units(latitude_b, "degrees")
latitude_b <- set_units(latitude_b, "radians")

longitude_a <- 10
longitude_a <- as_units(longitude_a, "degrees")
longitude_a <- set_units(longitude_a, "radians")

longitude_b <- 10.5
longitude_b <- as_units(longitude_b, "degrees")
longitude_b <- set_units(longitude_b, "radians")

#Calculate area in square metres
area = (R^2) * (longitude_b - longitude_a) *
  (sin(latitude_b) - sin(latitude_a))

#Remove units and convert to square kilometres
units(area) <- NULL
area <- area / 1000000

