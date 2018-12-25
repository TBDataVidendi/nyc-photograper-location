
#install.packages("devtools")
#devtools::install_github("dkahle/ggmap")


# loading libraries
require(RColorBrewer)
require(reshape)
require(MASS)
require(ggmap)
#install.packages("osmar")
require(osmar)
require(geosphere)
require(mvtnorm)
require(truncnorm)
require(coda)

register_google(key = 'AIzaSyA5glCK4woGllJ6JZkeKgLnBfYh1gGpf4Q')

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
############################################################################################## FUNCTIONS

# function to calculate the bearing angle given 2 points
CalculateBearing <- function(lon1, lat1, lon2, lat2)
{
    (360 + atan((lon2 - lon1) / (lat2 - lat1)) * 180/pi) %% 360
}

# testing the function with 2 of the reference points
CalculateBearing(-4.923315, 55.945681, -4.868940, 55.796819)
# same longitude, bearing 0
CalculateBearing(-4.923315, 55.945681, -4.923315, 55.796819)
# same latitude, bearing 90
CalculateBearing(-4.923315, 55.945681, -4.868940, 55.945681)

CalculateBearing(-73.979528, 40.759120, landmarks[5,1], landmarks[5,2])



intersectBearings <- function(p1,b1,p2,b2) {
    x1 <- p1[1]
    x2 <- p1[1] + 0.1*sin(b1*pi/180)
    x3 <- p2[1]
    x4 <- p2[1] + 0.1*sin(b2*pi/180)
    y1 <- p1[2]
    y2 <- p1[2] + 0.1*cos(b1*pi/180)
    y3 <- p2[2]
    y4 <- p2[2] + 0.1*cos(b2*pi/180)
    x <- ((x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))
    y <- ((x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4))/((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))
    return(as.numeric(c(x,y)))
}

distanceToRoad <- function(par) {
    distance <- dist2Line(c(par[1],par[2]), hw_lines)
    proper.distance <- distance[1] * 3.28084
    return(proper.distance)
}


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
############################################################################################## CALCULATE THE BEARINGS

# BETWEEN kimpton hotel eventi AND statue of liberty
CalculateBearing(-73.990143, 40.747103, -74.044565, 40.689262)

# BETWEEN Metropolitan Life Insurance Company Tower AND Verrazzano bridge, easternmost pillar
CalculateBearing(-73.987364, 40.741270, -74.052438, 40.604104)

# BETWEEN Times square AND 1 Marine View Plaza, Hoboken
CalculateBearing(-73.986590, 40.756129, -74.028468, 40.740896)

# BETWEEN 101 Park Ave AND consolidated edison power plant
CalculateBearing(-73.977692, 40.750853, -73.974059, 40.728115)


# testing the maps  
map <- get_map(c(-73.97, 40.76), zoom=12, maptype="satellite")
ggmap(map)

# plotting the lines
landmarks<-data.frame(lon=c(-73.990143, -73.987364, -73.986590, -73.977692),
                      lat=c(40.747103, 40.741270, 40.756129, 40.750853))


fartherLandmarks<-data.frame(lon=c(-74.044565, -74.052438, -74.028468, -73.974059),
                      lat=c(40.689262, 40.604104, 40.740896, 40.728115))

alpha <- 43.25558 # First bearing
beta <- 25.3805 # Second bearing
gamma <- 70.01129 # Third bearing
delta <- 350.9222 # fourth bearing

# kimpton hotel
map <- get_map(c(landmarks[1,1], landmarks[1,2]), zoom=18, maptype="satellite")
ggmap(map) + ggtitle(landmarks$names[4])

# Metropolitan Life Insurance Company Tower
map <- get_map(c(landmarks[2,1], landmarks[2,2]), zoom=18, maptype="satellite")
ggmap(map)

# Times square
map <- get_map(c(landmarks[3,1], landmarks[3,2]), zoom=18, maptype="satellite")
ggmap(map)

# 101 Park Ave
map <- get_map(c(landmarks[4,1], landmarks[4,2]), zoom=18, maptype="satellite")
ggmap(map)


d <- seq(-1, 1, 0.0001) # Length of the line
line1 <- data.frame(lon=landmarks[1,1] + d*sin(alpha*pi/180+pi),
                    lat=landmarks[1,2] + d*cos(alpha*pi/180+pi))
line2 <- data.frame(lon=landmarks[2,1] + d*sin(beta*pi/180+pi),
                    lat=landmarks[2,2] + d*cos(beta*pi/180+pi))
line3 <- data.frame(lon=landmarks[3,1] + d*sin(gamma*pi/180+pi),
                    lat=landmarks[3,2] + d*cos(gamma*pi/180+pi))
line4 <- data.frame(lon=landmarks[4,1] + d*sin(delta*pi/180+pi),
                    lat=landmarks[4,2] + d*cos(delta*pi/180+pi))


# map of the area
map <- get_map(c(-73.979, 40.759), zoom=17, maptype="road")
mapPlot <- ggmap(map)+
    geom_point(aes(x = lon, y = lat), size = 3, data = landmarks, alpha = 1, colour = "red") +
    geom_line(aes(x=lon,y=lat),data=line1, col = "black") +
    geom_line(aes(x=lon,y=lat),data=line2, col = "black") +
    geom_line(aes(x=lon,y=lat),data=line3, col = "black") +
    geom_line(aes(x=lon,y=lat),data=line4, col = "black")
mapPlot

map <- get_map(c(-73.979, 40.68), zoom=12, maptype="road")
mapPlot <- ggmap(map)+
    geom_point(aes(x = lon, y = lat), size = 3, data = landmarks, alpha = 1, colour = "red") +
    geom_point(aes(x = lon, y = lat), size = 3, data = fartherLandmarks, alpha = 1, colour = "blue") +
    geom_line(aes(x=lon,y=lat),data=line1, col = "black") +
    geom_line(aes(x=lon,y=lat),data=line2, col = "black") +
    geom_line(aes(x=lon,y=lat),data=line3, col = "black") +
    geom_line(aes(x=lon,y=lat),data=line4, col = "black")
mapPlot

# lambda = longitude
# phi = latitude


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
############################################################################################## USING THE FINE GRID EVALUATION

# grid step for longitude and latitude
step = 0.0001
# limit of the plot for x (lon)
xl <- c(-73.9800, -73.9780)
# limit of the plot for y (lat)
yl <- c(40.7584, 40.7596)

#lambdagrid <- seq(-5.5, -4.5, 0.01)
#phigrid <- seq(55, 56, 0.01)

# grid on longitude
lambdagrid <- seq(xl[1], xl[2], step)
# grid on latitude
phigrid <- seq(yl[1], yl[2], step)
# grid for the variance
sigma2grid <- seq (0.01, 0.2, 0.001)

# matrix of the posterior probability
posterior <- array(0, dim=c(length(lambdagrid), length(phigrid), length(sigma2grid)))
# calculate the posterior probability
for(i in 1:length(lambdagrid))
{
    for(j in 1:length(phigrid))
    {
        for(k in 1:length(sigma2grid))
        {
            bearing1 <- CalculateBearing(lambdagrid[i], phigrid[j], landmarks$lon[1], landmarks$lat[1])
            bearing2 <- CalculateBearing(lambdagrid[i], phigrid[j], landmarks$lon[2], landmarks$lat[2])
            bearing3 <- CalculateBearing(lambdagrid[i], phigrid[j], landmarks$lon[3], landmarks$lat[3])
            bearing4 <- CalculateBearing(lambdagrid[i], phigrid[j], landmarks$lon[4], landmarks$lat[4])
            
            posterior[i, j, k] <- 
                dexp(sigma2grid[k], rate = 20, log = T) + 
                dnorm(alpha, bearing1, sigma2grid[k], log = T) +
                dnorm(beta, bearing2, sigma2grid[k], log = T) +
                dnorm(gamma, bearing3, sigma2grid[k], log = T) +
                dnorm(delta, bearing4, sigma2grid[k], log = T) 
            # here I should include the prior probabilities for lambda and phi, but they are either 1 or a constant value.
            # the normalization process will take care of them
        }
    }
}

#plot(sigma2grid, dexp(sigma2grid, rate = 1, log = F))

# normalize the posterior
posterior2 <- (posterior/sum(posterior)) * (1 / (step^2)) * 1000

# taking the lambda/phi marginal summing up the variance values
sample <- apply(posterior2, 1:2, sum) * 0.001

# heatmap of the resulting probability with the bearing lines (no map)
image(lambdagrid, phigrid, sample, useRaster=T, col=c("#FFFFFF", colorRampPalette(brewer.pal(9, "Reds"))(500)))
lines(line1$lon, line1$lat, type = 'l')
lines(line2$lon, line2$lat, type = 'l')
lines(line3$lon, line3$lat, type = 'l')
lines(line4$lon, line4$lat, type = 'l')

# this displays the probability as contour lines
contour(lambdagrid, phigrid, sample, nlevels = 300)

# reshape the data to use with ggplot
z <- melt(sample)
z$Var1<-lambdagrid[z$X1]
z$Var2<-phigrid[z$X2]

hist(z$value, breaks = 100)

# this is not great, geom_raster doesn't seem to provide a great visualization because the difference in the z dimension are too subtle
ggplot() + geom_raster(data=z, aes(x=Var1, y=Var2, fill=value)) +
    geom_line(aes(x=lon,y=lat),data=line1) +
    geom_line(aes(x=lon,y=lat),data=line2) +
    geom_line(aes(x=lon,y=lat),data=line3) +
    geom_line(aes(x=lon,y=lat),data=line4) +
    xlim(mean(xl) - offset, mean(xl) + offset) + ylim(mean(yl) - offset, mean(yl) + offset)


# getting the map from google
map <- get_map(c(mean(xl),mean(yl)), zoom=21, maptype="road")
ggmap(map)
offset = 0.0002

# plotting map, bearing lines and contour lines on the same plot
g <- ggmap(map) + #contour(lambdagrid, phigrid, sample, nlevels = 100 ) +
    geom_contour(data = z, aes(x = Var1, y = Var2, z = value), binwidth = 1000, alpha = 0.5, colour = "red") +
    #geom_raster(data=z, aes(x=Var1, y=Var2, fill=value), alpha = 0.5) +
    guides(fill=FALSE,alpha=FALSE) +
    coord_cartesian() +
    #geom_tile(aes(x = z$Var1, y = z$Var2, fill = z$value)) +
    geom_line(aes(x=lon,y=lat),data=line1) +
    geom_line(aes(x=lon,y=lat),data=line2) +
    geom_line(aes(x=lon,y=lat),data=line3) +
    geom_line(aes(x=lon,y=lat),data=line4) +
    xlim(mean(xl) - offset, mean(xl) + offset) + ylim(mean(yl) - offset, mean(yl) + offset)
g

#############################################################################################################################
# proximity to the nearest road

intersection <- intersectBearings(landmarks[1,],alpha,landmarks[2,],beta)
roads.box <- center_bbox(center_lon=intersection[1],
                         center_lat=intersection[2],
                         width=100,
                         height=100)
api <- osmsource_api(url="https://api.openstreetmap.org/api/0.6/")
roads <- get_osm(roads.box, source=api)
ways <- find(roads, way(tags(k=="highway")))
ways <- find_down(roads, way(ways))
ways <- subset(roads, ids=ways)
hw_lines <- as_sp(ways,"lines")


# matrix of the posterior probability
posterior <- array(0, dim=c(length(lambdagrid), length(phigrid), length(sigma2grid)))
# calculate the posterior probability
for(i in 1:length(lambdagrid))
{
    for(j in 1:length(phigrid))
    {
        for(k in 1:length(sigma2grid))
        {
            bearing1 <- CalculateBearing(lambdagrid[i], phigrid[j], landmarks$lon[1], landmarks$lat[1])
            bearing2 <- CalculateBearing(lambdagrid[i], phigrid[j], landmarks$lon[2], landmarks$lat[2])
            bearing3 <- CalculateBearing(lambdagrid[i], phigrid[j], landmarks$lon[3], landmarks$lat[3])
            bearing4 <- CalculateBearing(lambdagrid[i], phigrid[j], landmarks$lon[4], landmarks$lat[4])
            
            posterior[i, j, k] <- 
                dnorm(0, distanceToRoad(c(lambdagrid[i], phigrid[j])), 6, log = F) *
                dexp(sigma2grid[k], rate = 20, log = F) *
                dnorm(alpha, bearing1, sigma2grid[k], log = F) *
                dnorm(beta, bearing2, sigma2grid[k], log = F) *
                dnorm(gamma, bearing3, sigma2grid[k], log = F) *
                dnorm(delta, bearing4, sigma2grid[k], log = F)
        }
    }
}

# normalize the posterior
posterior2 <- (posterior/sum(posterior)) * (1 / (step^2)) * 1000

# taking the lambda/phi marginal summing up the variance values
sample <- apply(posterior2, 1:2, sum) * 0.001

# heatmap of the resulting probability with the bearing lines (no map)
image(lambdagrid, phigrid, sample, useRaster=T, col=c("#FFFFFF", colorRampPalette(brewer.pal(9, "Reds"))(500)))
lines(line1$lon, line1$lat, type = 'l')
lines(line2$lon, line2$lat, type = 'l')
lines(line3$lon, line3$lat, type = 'l')

# this displays the probability as contour lines
contour(lambdagrid, phigrid, sample, nlevels = 100)

# reshape the data to use with ggplot
z <- melt(sample)
z$Var1<-lambdagrid[z$X1]
z$Var2<-phigrid[z$X2]

# getting the map from google
map <- get_map(c(mean(xl),mean(yl)), zoom=19, maptype="road")
ggmap(map)

# plotting map, bearing lines and contour lines on the same plot
g <- ggmap(map) + #contour(lambdagrid, phigrid, sample, nlevels = 100 ) +
    geom_contour(data = z, aes(x = Var1, y = Var2, z = value), binwidth = 2000000, alpha = 0.2) +
    #geom_raster(data=z, aes(x=Var1, y=Var2, fill=value), alpha = 0.5) +
    guides(fill=FALSE,alpha=FALSE) +
    coord_cartesian() +
    #geom_tile(aes(x = z$Var1, y = z$Var2, fill = z$value)) +
    geom_line(aes(x=lon,y=lat),data=line1) +
    geom_line(aes(x=lon,y=lat),data=line2) +
    geom_line(aes(x=lon,y=lat),data=line3) +
    xxlim(mean(xl) - offset, mean(xl) + offset) + ylim(mean(yl) - offset, mean(yl) + offset)
g



#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
######################################################################################## MCMC ALGORITHM - METROPOLIS HASTINGS


likelihood <- function (params) 
{
    #params = c(-7.397882e+01,  4.075919e+01,  1.193184e-04)
    bearing1 <- CalculateBearing(params[1], params[2], landmarks$lon[1], landmarks$lat[1])
    bearing2 <- CalculateBearing(params[1], params[2], landmarks$lon[2], landmarks$lat[2])
    bearing3 <- CalculateBearing(params[1], params[2], landmarks$lon[3], landmarks$lat[3])
    bearing4 <- CalculateBearing(params[1], params[2], landmarks$lon[4], landmarks$lat[4])
    
    return (dnorm(alpha, bearing1, params[3], log = F) *
                dnorm(beta, bearing2, params[3], log = F) *
                dnorm(gamma, bearing3, params[3], log = F) * 
                dnorm(delta, bearing4, params[3], log = F))
}

prior <- function(params) 
{
    return (
        dunif(params[1], -74.5, -73.5, log = F) *
            dunif(params[2], 40, 41, log = F) *
            dexp(params[3], rate = 20, log = F))
}

#plot(1:10, dexp(1:10, rate = 100, log = T))

N = 4000
intersection1 <- intersectBearings(landmarks[1,], alpha, landmarks[2,], beta)
intersection2 <- intersectBearings(landmarks[2,], beta, landmarks[3,], gamma)
intersection3 <- intersectBearings(landmarks[3,], alpha, landmarks[4,], delta)
intersection4 <- intersectBearings(landmarks[1,], alpha, landmarks[4,], delta)
intersection <- c(mean(intersection1[1], intersection2[1], intersection3[1], intersection4[1]),
                  mean(intersection1[2], intersection2[2], intersection3[2], intersection4[2]))
draws <- array(0, dim=c(N, 3, 3))
draws[N, 1, ] <- runif(3, intersection[1] - 0.00001, intersection[1] + 0.00001)
draws[N, 2, ] <- runif(3, intersection[2] - 0.00001, intersection[2] + 0.00001)
#draws[N, 1, ] <- intersection[1]
#draws[N, 2, ] <- intersection[2]
draws[N, 3, ] <- rexp(3, 20)
prop.cov <- c(1e-9, 1e-9, 1e-3) * diag(3)

#draws[1,1,]

converged <- FALSE
while(!converged)
{
    accepted <- rep(0,3)
    draws[1,,] <- draws[N,,]
    
    for (step in 2:N) 
    {
        if(step%%100==0) {print(step)}
        #step <- 2
        for (chain in 1:3) 
        {
            repeat
            {
                repeat
                {
                    proposed <- rmvnorm(1, draws[step-1,,chain], prop.cov)
                    proposed <- as.vector(proposed)
                    #print(proposed)
                    
                    if(proposed[3] > 0)
                        break
                } 
                
                r <-( likelihood(proposed) *
                          prior(proposed) *
                          dtruncnorm(draws[step-1,3,chain],0,100,proposed[3],0.5)) / 
                    (likelihood(draws[step-1,,chain]) * 
                         prior(draws[step-1,,chain]) *
                         dtruncnorm(proposed[3],0,100,draws[step-1,3,chain],0.5))
                
                if(!is.nan(r))
                    break
            }
            
            alpha2 <- min(1, r)
            u <- runif(1)
            
            if (u < alpha2) 
            {
                draws[step,,chain] <- proposed
                accepted[chain] <- accepted[chain]+1
            } 
            else 
            {
                draws[step,,chain] <- draws[step-1,,chain]
            }
        }
    }
    
    print(sprintf("Acceptance rate: %f", accepted/N*100))
    chainlist <- mcmc.list(Chain1=mcmc(draws[,,1]),
                           Chain2=mcmc(draws[,,2]),
                           Chain3=mcmc(draws[,,3]))
    converged <- all((gelman.diag(chainlist)$psrf[,2]) < 1.1)
    plot(chainlist) # This plots current state of the chains
    Sys.sleep(0.1)
}

# Once convergence is achieved,
# we begin collecting the final sample from the posterior
sample <- array(0, dim=c(10000,3,3))
sample[1,,] <- draws[N,,]
for (step in 2:10000) 
{
    for (chain in 1:3) 
    {
        repeat
        {
            repeat
            {
                proposed <- rmvnorm(1, sample[step-1,,chain], prop.cov)
                proposed <- as.vector(proposed)
                #print(proposed)
                
                if(proposed[3] > 0)
                    break
            } 
            
            r <- (likelihood(proposed) *
                      prior(proposed) *
                      dtruncnorm(sample[step-1,3,chain],0,100,proposed[3],0.5)) / 
                (likelihood(sample[step-1,,chain]) *
                     prior(sample[step-1,,chain]) *
                     dtruncnorm(proposed[3],0,100,sample[step-1,3,chain],0.5))
            
            if(!is.nan(r))
                break
        }
        
        alpha2 <- min(1, r)
        u <- runif(1)
        
        if (u < alpha2) 
        {
            sample[step,,chain] <- proposed
        } 
        else 
        {
            sample[step,,chain] <- sample[step-1,,chain]
        }
        
    }
}
finalsample <- rbind(sample[,,1],sample[,,2],sample[,,3])

#setwd("D:/OneDrive/DataScience/Training/DataAnalyticsMSc/Uncertainty Assessment and Bayesian Comp")
#saveRDS(finalsample, file = "finalSampleNY.rds")
# Restore the object
#finalsample <- readRDS(file = "finalSample3.rds")

#acf(finalsample[,1])
#acf(finalsample[,2])
finalsample2 <- finalsample[seq(1, 10000, 4),]
#finalsample2 <- finalsample
#acf(finalsample2[,1])
#acf(finalsample2[,2])

D <- kde2d(finalsample2[,1],finalsample2[,2],
           h=c(sd(finalsample2[,1]),sd(finalsample2[,2])),
           n=1024,
           lims=c(-73.9800, -73.9780, 40.758, 40.760)) # Enough to cover map
z <- melt(D$z)
z$Var1<-D$x[z$X1]
z$Var2<-D$y[z$X2]
map <- get_map(c(mean(finalsample2[,1]),mean(finalsample2[,2])),zoom=18,maptype="road")
mapPoints <- ggmap(map)+
    geom_point(aes(x = lon, y = lat), size = 1, data = landmarks, alpha = .5) +
    geom_raster(data=z,aes(x=Var1,y=Var2,fill=value))+
    guides(fill=FALSE,alpha=FALSE)+
    scale_fill_gradientn(colours=c("#0000FF00","#0000FFFF"))+
    coord_cartesian() +
    geom_line(aes(x=lon,y=lat),data=line1) +
    geom_line(aes(x=lon,y=lat),data=line2) +
    geom_line(aes(x=lon,y=lat),data=line3) +
    geom_line(aes(x=lon,y=lat),data=line4) +
    geom_point(aes(x=lon,y=lat),
               data=data.frame(lon=mean(finalsample2[,1]),lat=mean(finalsample2[,2])),
               size=0.5,colour="#FF0000")
mapPoints


####################################################################
# road proximity




likelihood <- function (params) 
{
    bearing1 <- CalculateBearing(params[1], params[2], landmarks$lon[1], landmarks$lat[1])
    bearing2 <- CalculateBearing(params[1], params[2], landmarks$lon[2], landmarks$lat[2])
    bearing3 <- CalculateBearing(params[1], params[2], landmarks$lon[3], landmarks$lat[3])
    bearing4 <- CalculateBearing(params[1], params[2], landmarks$lon[4], landmarks$lat[4])
    
    return (dnorm(alpha, bearing1, params[3], log = F) *
                dnorm(beta, bearing2, params[3], log = F) *
                dnorm(gamma, bearing3, params[3], log = F) * 
                dnorm(delta, bearing4, params[3], log = F))
}

prior <- function(params) 
{
    return (
        dnorm(0, distanceToRoad(c(params[1], params[2])), 6, log = F) *
            dexp(params[3], rate = 20, log = F))
}


N = 4000
intersection1 <- intersectBearings(landmarks[1,], alpha, landmarks[2,], beta)
intersection2 <- intersectBearings(landmarks[2,], beta, landmarks[3,], gamma)
intersection3 <- intersectBearings(landmarks[3,], alpha, landmarks[4,], delta)
intersection4 <- intersectBearings(landmarks[1,], alpha, landmarks[4,], delta)
intersection <- c(mean(intersection1[1], intersection2[1], intersection3[1], intersection4[1]),
                  mean(intersection1[2], intersection2[2], intersection3[2], intersection4[2]))
intersection <- c(-73.97899, 40.75891) # from part 1
draws <- array(0, dim=c(N, 3, 3))
#draws[N, 1, ] <- runif(3, intersection[1] - 0.00001, intersection[1] + 0.00001)
#draws[N, 2, ] <- runif(3, intersection[2] - 0.00001, intersection[2] + 0.00001)
draws[N, 1, ] <- intersection[1]
draws[N, 2, ] <- intersection[2]
draws[N, 3, ] <- rexp(3, 20)
prop.cov <- c(5e-9, 5e-9, 1e-3) * diag(3)


###################################################

#intersection <- intersectBearings(landmarks[1,],alpha,landmarks[2,],beta)
roads.box <- center_bbox(center_lon=intersection[1],
                         center_lat=intersection[2],
                         width=100,
                         height=100)
api <- osmsource_api(url="https://api.openstreetmap.org/api/0.6/")
roads <- get_osm(roads.box, source=api)
ways <- find(roads, way(tags(k=="highway")))
ways <- find_down(roads, way(ways))
ways <- subset(roads, ids=ways)
hw_lines <- as_sp(ways,"lines")

