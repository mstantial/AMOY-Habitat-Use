library(raster)
library(tmap)
library(sf)
library(colorblindcheck)
library(landscapemetrics)
library(dplyr)
library(ggplot2)

nj.tif <- raster("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/NJcoastalDEMclipped.tif")
nj.slope <- raster("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/slope_image.tif")

nests <- read_sf("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/NestRandAllData.shp")
nests$Nest <- as.factor(nests$Nest)
jetties <- read_sf("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/jettygroinmerge.shp")
marshes <- read_sf("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/2015_Saltmarsh.shp")
dunes <- read_sf("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/2015_Dune_layer.shp")
develop <- read_sf("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/2015_Human_development_layert.shp")
beaches <- read_sf("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/2015_Beach_layer.shp")
bays <- read_sf("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/Bay_layer.shp")
mudflat <- read_sf("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/2015_Mudflat_layer.shp")
inlets<- read_sf("C:/Users/mstantial/OneDrive - DOI/SUNY Stuff/SUNY/AMOY Project/Maps/Inlets.shp")

pal <- c("#143935", "#2e847c","#40B6AA", 
         "#E5B221", 
         "#C53826","#962b1d","#40120c")
palette_check(pal, plot=T) #check palette for colorblindness viewability

tmap_mode("view") #view for interactive, plot for static
(amoymap <- tm_shape(nj.tif, raster.downsample = T) +
  tm_raster(drop.levels=T, title="Elevation", 
            style="quantile", n=10, palette=colorRampPalette(c("#143935","#E5B221","#40120c"))(10), legend.hist = T) +
    tm_shape(jetties) +
    tm_lines(col="gray22", lwd=3) +
    tm_shape(marshes) +
    tm_polygons(col="#B6DEDE", alpha=.5) +
    tm_shape(dunes) +
    tm_polygons(col="#527C6E", alpha=.5) +    
    tm_shape(develop) +
    tm_polygons(col="#B6BFC6", alpha=.5) +
    tm_shape(beaches) +
    tm_polygons(col="#D1B18A", alpha=.5) +
    tm_shape(nests) +
    tm_dots(col="Nest", size=0.1, palette=c("black","red")) +
  tm_layout(legend.outside=T))

#tmap_save(amoymap, "amoy_elev_map.html") #save as .html to save as interactive, .png for static map



###################PREP DATA FOR ANALYSIS#########################

#calculate distance of each nest/random point to features
#distance to nearest marsh polygon
nearmarsh <- st_nearest_feature(nests,marshes) #identifies nearest marsh features
marshes2 <- marshes %>% slice(nearmarsh)# slice the marsh data based on the index of which marsh is closest
marsh_dist <- st_distance(nests, marshes2, by_element = TRUE)# calculate distance between nests and marshes
marsh_dist_m <- conv_unit(marsh_dist,"ft","m") #convert distance to meters
#distance to jetty
nearjetty <- st_nearest_feature(nests,jetties)
jetty2 <- jetties %>% slice(nearjetty)
jetty_dist <- st_distance(nests,jetty2, by_element = T)
jetty_dist_m <- conv_unit(jetty_dist, "ft", "m")
#distance to development
neardevelop <- st_nearest_feature(nests,develop)
develop2 <- develop %>% slice(neardevelop)
develop_dist <- st_distance(nests,develop2, by_element=T)
develop_dist_m <- conv_unit(develop_dist, "ft","m")
#distance to dunes
neardune <- st_nearest_feature(nests, dunes)
dune2 <- dunes %>% slice(neardune)
dune_dist <- st_distance(nests,dune2,by_element = T)
dune_dist_m <- conv_unit(dune_dist,"ft","m")
#distance to bay
nearbay <- st_nearest_feature(nests, bays)
bay2 <- bays %>% slice(nearbay)
bay_dist <- st_distance(nests,bay2,by_element = T)
bay_dist_m <- conv_unit(bay_dist,"ft","m")
#distance to mudflat
nearmudflat <- st_nearest_feature(nests, mudflat)
mud2 <- mudflat %>% slice(nearmudflat)
mud_dist <- st_distance(nests,mud2,by_element = T)
mud_dist_m <- conv_unit(mud_dist,"ft","m")
#distance to inlet
nearinlet <- st_nearest_feature(nests,inlets)
inlet2 <- inlets %>% slice(nearinlet)
inlet_dist <- st_distance(nests,inlet2,by_element = T)
inlet_dist_m <- conv_unit(inlet_dist,"ft","m")

elev <- extract(nj.tif, nests)#identify elevation at each nest/random point
slope <- extract(nj.slope, nests)#identify slope at each nest/random point

covs <- cbind.data.frame(marsh_dist_m, jetty_dist_m,develop_dist_m,
                         dune_dist_m, bay_dist_m, mud_dist_m, inlet_dist_m, 
                         elev, slope) #combine all covariates into a single dataframe

#test for correlation among covariates using pearson pairwise comparison
ggcorr(covs, method = c("pairwise","pearson"), label=T) #jetty and development correlated, bay and marsh correlated, and marsh and mudflat correlated <- use jetty, and use marsh because those are the two covs of interest; slope and elevation also correlated...not sure which is better to use though...

#scale and center covariates around 0
alldat <- as.data.frame(scale(covs, center=T)) 
nestpoints <- fastDummies::dummy_cols(nests$Nest) #create dummy variable to add whether point is a nest
alldat$nests <- nestpoints[,3]
alldat$develop <- as.factor(nests$Develop) #add whether nest is in front of development
alldat$year <- nests$Year #add year
alldat$year[alldat$year==0] <- NA #change all 0s to NAs for later
  
alldat <- alldat[which(alldat$develop==1),] #select only oceanside nests in front of development

#randomly assign the random points to a year
y <- sum(is.na(alldat$year)) #sum the number of NAs
alldat$year[is.na(alldat$year)] <- sample(c(2012,2013,2014,2015,2016,2017,2018,2019), y, replace=T) #sample from the possible years
year2 <- as.factor(alldat$year) #change year to a factor
alldat$year <- as.numeric(year2) #change year to a number for JAGS
str(alldat)

################AMOY HABITAT USE MODEL IN JAGS###########################
##FIRST MODEL USES INDICATOR VARIABLE APPROACH TO MODEL SELECTION########



#logistic regression model - random year intercept - indicator variable selection
sink("log_reg_AMOY2.txt")
cat("
model {
  # N observations
  for (i in 1:N) {
    nest[i] ~ dbern(p[i])
    logit(p[i]) <- alpha[year[i]] + 
    beta.jetty*jetty[i]*inc.jetty + 
    beta.marsh*marsh[i]*inc.marsh + 
    beta.dune*dune[i]*inc.dune +
    beta.inlet*inlet[i]*inc.inlet +
    beta.elevation*elevation[i]*inc.elevation +
    beta.elevation2*elevation[i]*elevation[i]*inc.elevation2 +
    beta.jetty.marsh*jetty[i]*marsh[i]*inc.jetty.marsh +
    beta.jetty.dune*jetty[i]*dune[i]*inc.jetty.dune +
    beta.jetty.inlet*jetty[i]*inlet[i]*inc.jetty.inlet +
    beta.inlet.marsh*marsh[i]*inlet[i]*inc.inlet.marsh +
    beta.inlet.dune*dune[i]*inlet[i]*inc.inlet.dune
  }

    # Priors
    for (i in 1:nyear){
    alpha[i] ~ dnorm(mu.alpha, tau.alpha) #hyperparameter 
    }
    
    tau.alpha <- 1/(pow(sigma.alpha,2))
    sigma.alpha ~ dunif(0,50)  #SD for year effect
    
    mu.alpha ~ dnorm(0, 0.001) #normal distribution, mean 0, sd 0.001
    beta.jetty ~dnorm(0, tau.model)
    beta.marsh ~dnorm(0, tau.model)
    beta.dune ~dnorm(0, tau.model)
    beta.inlet ~dnorm(0, tau.model)
    beta.elevation ~dnorm(0, tau.model)
    beta.elevation2 ~dnorm(0, tau.model)
    beta.jetty.marsh ~dnorm(0, tau.model)
    beta.jetty.dune ~dnorm(0, tau.model)
    beta.jetty.inlet ~dnorm(0, tau.model)
    beta.inlet.marsh~dnorm(0, tau.model)
    beta.inlet.dune~dnorm(0, tau.model)

    # Inclusion priors
    inc.jetty~ dbern(0.5)
    inc.marsh~ dbern(0.5)
    inc.dune~ dbern(0.5)
    inc.inlet~ dbern(0.5)
    inc.elevation~ dbern(0.5)
    inc.elevation2~ dbern(0.5)
    inc.jetty.marsh~ dbern(0.5)
    inc.jetty.dune~ dbern(0.5)
    inc.jetty.inlet~ dbern(0.5)
    inc.inlet.marsh~ dbern(0.5)
    inc.inlet.dune~ dbern(0.5)


    #number of parameters entering model
    K <- (inc.jetty + inc.marsh + inc.dune + inc.inlet + inc.elevation +
    inc.elevation2 + inc.jetty.marsh + inc.jetty.dune + inc.jetty.inlet +
    inc.inlet.marsh + inc.inlet.dune)
  
    #priors for constant total model variance regardless of the number of parameters entering the model
    tau.v ~ dgamma(3.29, 7.8) #these priors were suggested in Link and Barker 2006
  
    #total model variance
    tau.model <- tau.v*K

}
    ",fill=TRUE)
sink()

str(win.data <- list(nest=as.integer(alldat$nests), N=length(alldat$nests), 
                     jetty=alldat$jetty_dist_m,
                 marsh=alldat$marsh_dist_m, dune=alldat$dune_dist_m,
                 inlet=alldat$inlet_dist_m, elevation=alldat$elev,
                 year=alldat$year, nyear=max(alldat$year)))

inits <- function(){list(inc.jetty=1, inc.marsh=1, inc.dune=1, inc.inlet=1,
                          inc.elevation=1, inc.elevation2=1, 
                          inc.jetty.marsh=1, inc.jetty.dune=1,
                          inc.jetty.inlet=1, inc.inlet.marsh=1, 
                         inc.inlet.dune=1)}

params <- c("mu.alpha", "beta.jetty", "beta.marsh", "beta.dune", "beta.inlet",
            "beta.elevation", "beta.elevation2", "beta.jetty.marsh",
            "beta.jetty.dune", "beta.jetty.inlet", "beta.inlet.marsh",
            "beta.inlet.dune",
            "inc.jetty", "inc.marsh", "inc.dune", "inc.inlet", "inc.elevation",
            "inc.elevation2", "inc.jetty.marsh", "inc.jetty.dune",
            "inc.jetty.inlet", "inc.inlet.marsh", "inc.inlet.dune",
            "alpha", "sigma.alpha")

out3 <- autojags(win.data, inits, params, "log_reg_AMOY2.txt", n.chains=3, 
                 iter.increment=2000, n.burnin=70000, n.thin=1, parallel=T, 
                 max.iter=150000)


print(out3); plot(out3)


iv <- data.frame(parameter=c("jetty","marsh", "dune", "inlet", "elevation",
                             "elevation2", "jetty.marsh", "jetty.dune",
                             "jetty.inlet", "inlet.marsh", "inlet.dune"))
iv$iv <- c(out3$mean$inc.jetty, out3$mean$inc.marsh, out3$mean$inc.dune,
           out3$mean$inc.inlet,
           out3$mean$inc.elevation, out3$mean$inc.elevation2, 
           out3$mean$inc.jetty.marsh, out3$mean$inc.jetty.dune,
           out3$mean$inc.jetty.inlet, out3$mean$inc.inlet.marsh,
           out3$mean$inc.inlet.dune)
iv$bf <- ((iv$iv/(1-iv$iv))/(0.5/(1-0.5)))

#levels of BF from lee and wagenmakers et al 2014
iv$evidence <- 0
iv$evidence[iv$bf<1] <- "no evidence"  
iv$evidence[iv$bf>100] <- "extreme evidence"  
iv$evidence[iv$bf>1 & iv$bf<3] <- "weak evidence"  
iv$evidence[iv$bf>30 & iv$bf<100] <- "very strong evidence"  
iv$evidence[iv$bf>3 & iv$bf<10] <- "moderate evidence"
iv$evidence[iv$bf>10 & iv$bf<30] <- "strong evidence"

#add estimates to iv
iv$estimate <- c(out3$mean$beta.jetty, out3$mean$beta.develop, 
                 out3$mean$beta.marsh, out3$mean$beta.jetty.develop,
                 out3$mean$beta.jetty.marsh, out3$mean$beta.develop.marsh,
                 out3$mean$beta.elevation, out3$mean$beta.elevation2,
                 out3$mean$inc.dune, out3$mean$inc.bay, out3$mean$inc.mudflat,
                 out3$mean$inc.jetty.bay, out3$mean$inc.jetty.dune)
iv




#############REMOVE POOR INDICATORS############
##COVARIATES TO KEEP: jetty, marsh, dune, inlet, elevation, inlet.dune

#logistic regression model - random year intercept 
sink("log_reg_AMOY.txt")
cat("
model {
  # N observations
  for (i in 1:N) {
    nest[i] ~ dbern(p[i])
    logit(p[i]) <- alpha[year[i]] + beta.jetty*jetty[i] + 
    beta.marsh*marsh[i] + beta.dune*dune[i] + beta.inlet*inlet[i] +
    beta.elevation*elevation[i] +
    beta.elevation2*elevation[i]*elevation[i] +
    beta.inlet.dune*dune[i]*inlet[i]
  }

    # Priors
    for (i in 1:nyear){
    alpha[i] ~ dnorm(mu.alpha, tau.alpha) #hyperparameter 
    }
    
    tau.alpha <- 1/(pow(sigma.alpha,2))
    sigma.alpha ~ dunif(0,50)  #SD for year effect
    
    mu.alpha ~ dnorm(0, 0.001) #normal distribution, mean 0, sd 0.001
    beta.jetty~ dnorm(0, 0.001)
    beta.marsh~ dnorm(0, 0.001)
    beta.dune~ dnorm(0, 0.001)
    beta.inlet~ dnorm(0, 0.001)
    beta.elevation~ dnorm(0, 0.001)
    beta.elevation2~ dnorm(0, 0.001)
    beta.inlet.dune~ dnorm(0, 0.001)

}
    ",fill=TRUE)
sink()

str(win.data2 <- list(nest=as.integer(alldat$nests), N=length(alldat$nests), 
                     jetty=alldat$jetty_dist_m,
                     marsh=alldat$marsh_dist_m, dune=alldat$dune_dist_m,
                     inlet=alldat$inlet_dist_m, elevation=alldat$elev,
                     year=alldat$year, nyear=max(alldat$year)))


inits2 <- function(){ list()}

params2 <- c("mu.alpha","beta.jetty", "beta.marsh", "beta.dune", "beta.inlet",
            "beta.elevation", "beta.elevation2", "beta.inlet.dune",
            "alpha", "sigma.alpha")

out.final <- autojags(win.data2, inits2, params2, "log_reg_AMOY.txt", 
                      n.chains=3,iter.increment=2000, n.burnin=10000, 
                      n.thin=1, parallel=T, max.iter=70000)


print(out.final); plot(out.final) #print results and look at trace plots




#point estimates
plogis(out.final$summary[,c(1,3,7)]) #mean, 2.5% and 97.5% quantiles




###PLOT PREDICTIONS FROM JAGS OUTPUT###

#predict distance to jetty
jet2 <- seq(0,10,.01)
jettypred <- plogis(out.final$mean$mu.alpha + out.final$mean$beta.jetty*jet2) 
jettypred.ucl <- plogis(out.final$q97.5$mu.alpha + 
                          out.final$q97.5$beta.jetty*jet2)
jettypred.lcl <- plogis(out.final$q2.5$mu.alpha + 
                          out.final$q2.5$beta.jetty*jet2)
jetdat <- cbind.data.frame(jettypred, jettypred.lcl, jettypred.ucl)

#plot distance to jetty
(jetplot <- ggplot(jetdat, aes(x=jet2, y=jettypred)) +
  geom_ribbon(aes(ymin=jettypred.lcl, ymax=jettypred.ucl), 
              alpha=0.5, fill="#5F9299") + geom_line(size=2, color="#5F9299") +
    ylim(0,0.3) + xlim(0,3) + xlab("Distance from hardened structure (km)") + 
  ylab("Probability of nest") + theme_classic() +  
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 18)) + 
  theme(legend.position=c(0.1, 0.2)) + theme(legend.position = "top"))



#plot of average elevation for nests
elevation2 <- seq(-1,8,.01)
elevationpred <- plogis(out.final$mean$mu.alpha + 
                          out.final$mean$beta.elevation*elevation2 + 
                          out.final$mean$beta.elevation2*elevation2*elevation2) elevationpred.ucl <- plogis(out.final$q97.5$mu.alpha + 
                          out.final$q97.5$beta.elevation*elevation2 + 
                          out.final$q97.5$beta.elevation2*elevation*elevation2)
elevationpred.lcl <- plogis(out.final$q2.5$mu.alpha + 
                          out.final$q2.5$beta.elevation*elevation2 + 
                          out.final$q2.5$beta.elevation2*elevation2*elevation2)
elevationdat <- cbind.data.frame(elevationpred, elevationpred.lcl,
                                 elevationpred.ucl)
#plot of nest elevation
(elevplot <- ggplot(elevationdat, aes(x=elevation2, y=elevationpred)) + 
    geom_ribbon(aes(ymin=elevationpred.lcl, ymax=elevationpred.ucl), 
                alpha=0.3, fill="#ED966A") +
    geom_line(size=2, color="#ED966A") + 
  xlab("Elevation (m)") + xlim(-1,5) + ylim(0,1) +
  ylab("Probability of a nest") + 
    theme_classic() + theme(legend.title = element_blank()) +
    theme(text = element_text(size = 18)) + 
    theme(legend.position=c(0.1, 0.2)) + theme(legend.position = "top"))



#predict distance to dune
dune2 <- seq(0,5,.01) #c(max(dune_dist_m),min(dune_dist_m))
dunepred <- plogis(out.final$mean$mu.alpha + out.final$mean$beta.dune*dune2) 
dunepred.ucl <- plogis(out.final$q97.5$mu.alpha + 
                          out.final$q97.5$beta.dune*dune2)
dunepred.lcl <- plogis(out.final$q2.5$mu.alpha + 
                          out.final$q2.5$beta.dune*dune2)
dunedat <- cbind.data.frame(dunepred, dunepred.lcl, dunepred.ucl)

#plot distance to dune
(duneplot <- ggplot(dunedat, aes(x=dune2, y=dunepred)) +
    geom_ribbon(aes(ymin=dunepred.lcl, ymax=dunepred.ucl), 
                alpha=0.5, fill="#736295") + 
    geom_line(size=2, color="#736295") +
    ylim(0,0.3) + xlim(0,5) + xlab("Distance from primary dune (km)") + 
    ylab("Probability of nest") + theme_classic() +  
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 18)) + 
    theme(legend.position=c(0.1, 0.2)) + theme(legend.position = "top"))




#predict distance to inlet
inlet2 <- seq(0,18,.01) #c(max(inlet_dist_m),min(inlet_dist_m))
inletpred <- plogis(out.final$mean$mu.alpha + out.final$mean$beta.inlet*inlet2)
inletpred.ucl <- plogis(out.final$q97.5$mu.alpha + 
                         out.final$q97.5$beta.inlet*inlet2)
inletpred.lcl <- plogis(out.final$q2.5$mu.alpha + 
                         out.final$q2.5$beta.inlet*inlet2)
inletdat <- cbind.data.frame(inletpred, inletpred.lcl, inletpred.ucl)

#plot distance to inlet
(inletplot <- ggplot(inletdat, aes(x=inlet2, y=inletpred)) +
    geom_ribbon(aes(ymin=inletpred.lcl, ymax=inletpred.ucl), 
                alpha=0.5, fill="#91C2B1") + 
    geom_line(size=2, color="#91C2B1") +
    ylim(0,.3) + xlim(0,5) + xlab("Distance from inlet (km)") + 
    ylab("Probability of nest") + theme_classic() +  
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 18)) + 
    theme(legend.position=c(0.1, 0.2)) + theme(legend.position = "top"))



#predict distance to marsh
marsh2 <- seq(0,4,.01) #c(max(marsh_dist_m),min(marsh_dist_m))
marshpred <- plogis(out.final$mean$mu.alpha + out.final$mean$beta.marsh*marsh2)
marshpred.ucl <- plogis(out.final$q97.5$mu.alpha + 
                          out.final$q97.5$beta.marsh*marsh2)
marshpred.lcl <- plogis(out.final$q2.5$mu.alpha + 
                          out.final$q2.5$beta.marsh*marsh2)
marshdat <- cbind.data.frame(marshpred, marshpred.lcl, marshpred.ucl)

#plot distance to marsh
(marshplot <- ggplot(marshdat, aes(x=marsh2, y=marshpred)) +
    geom_ribbon(aes(ymin=marshpred.lcl, ymax=marshpred.ucl), 
                alpha=0.5, fill="#14706C") + 
    geom_line(size=2, color="#14706C") +
    ylim(0,.3) + xlim(0,4) + xlab("Distance from salt marsh (km)") + 
    ylab("Probability of nest") + theme_classic() +  
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 18)) + 
    theme(legend.position=c(0.1, 0.2)) + theme(legend.position = "top"))




#predict inlet/dune interaction
#prob of nest at 0 m from dune, and all values of marsh
marsh.inlet.pred <- plogis(out.final$mean$mu.alpha + 
                             out.final$mean$beta.marsh*marsh2 + 
                             out.final$mean$beta.dune*0 + 
                             out.final$mean$beta.inlet.dune*marsh2*0)
marsh.inlet.pred.ucl <- plogis(out.final$q97.5$mu.alpha + 
                          out.final$q97.5$beta.marsh*marsh2 + 
                            out.final$q97.5$beta.dune*0+ 
                            out.final$q97.5$beta.inlet.dune*marsh2*0)
marsh.inlet.pred.lcl <- plogis(out.final$q2.5$mu.alpha + 
                          out.final$q2.5$beta.marsh*marsh2 + 
                            out.final$q2.5$beta.dune*0+ 
                            out.final$q2.5$beta.inlet.dune*marsh2*0)
#prob of nest at 2.15 km from dune and all values of marsh
mean.marsh.inlet.pred <- plogis(out.final$mean$mu.alpha + 
                             out.final$mean$beta.marsh*marsh2 + 
                             out.final$mean$beta.dune*2.15 + 
                             out.final$mean$beta.inlet.dune*marsh2*2.15)
mean.marsh.inlet.pred.ucl <- plogis(out.final$q97.5$mu.alpha + 
                                 out.final$q97.5$beta.marsh*marsh2 + 
                                 out.final$q97.5$beta.dune*2.15+ 
                                 out.final$q97.5$beta.inlet.dune*marsh2*2.15)
mean.marsh.inlet.pred.lcl <- plogis(out.final$q2.5$mu.alpha + 
                                 out.final$q2.5$beta.marsh*marsh2 + 
                                 out.final$q2.5$beta.dune*2.15+ 
                                 out.final$q2.5$beta.inlet.dune*marsh2*2.15)

marsh.inlet.dat <- cbind.data.frame(marsh.inlet.pred, marsh.inlet.pred.lcl, 
                                    marsh.inlet.pred.ucl, 
                                    mean.marsh.inlet.pred, 
                                    mean.marsh.inlet.pred.lcl, 
                                    mean.marsh.inlet.pred.ucl)

#plot distance to marsh
(marsh.inlet.plot <- ggplot(marsh.inlet.dat, 
                            aes(x=marsh2, y=marsh.inlet.pred)) +
    geom_ribbon(aes(ymin=marsh.inlet.pred.lcl, ymax=marsh.inlet.pred.ucl), 
                alpha=0.5, fill="#14706C") + 
    geom_line(size=2, color="#14706C") +
    geom_ribbon(aes(ymin=mean.marsh.inlet.pred.lcl, 
                    ymax=mean.marsh.inlet.pred.ucl), 
                alpha=0.5, fill="#736295") +
    geom_line(aes(x=marsh2, y=mean.marsh.inlet.pred), size=2, 
              color="#736295") +
    ylim(0,1) + xlim(0,4) + xlab("Distance from salt marsh (km)") + 
    ylab("Probability of nest") + theme_classic() +  
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 18)) + 
    theme(legend.position=c(0.1, 0.2)) + theme(legend.position = "top"))


