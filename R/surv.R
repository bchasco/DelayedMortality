# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#' @param dam The name of the dam
#' @param dam_n sample size of the fish released at dam
#' @param phi project survival
#' @param p detection probability
#' @param d_phi delayed project survival
#' @param delayTime Length of time for delayed mortality to occur
#' @param travelTime Length of time to get to the dam
#'
surv <- function(dam = c("RI","MCN","BON_J","BON_A"),
                 dam_n = c(2,0,0,0),
                 abv_phi = c(1.0,0.7,0.6,0.01),
                 blw_phi = c(0.98,1.0,1.0,0.0),
                 p = c(0.,0.1,0.1,1.0),
                 d_phi = c(0.1,0.0,0.0,0.0),
                 startDelayTime = c(30,0,0,0),
                 endDelayTime = c(90,0,0,0),
                 travelTime = c(0,30,30,720)) {

  c_ij <- as.data.frame(matrix(0,nrow=sum(dam_n)*2,ncol=2+length(dam)))
  z_ij <- as.data.frame(matrix(0,nrow=sum(dam_n)*2,ncol=2+length(dam)))
  names(c_ij) <- c("i","loc",dam)
  names(z_ij) <- c("i","loc",dam)

  locs <- c("above","below")

  damWithDelayMortality <- (paste(dam[cumsum(travelTime)>=startDelayTime[1] & cumsum(travelTime)<=endDelayTime[1]],collapse = ','))
  icnt <- 1
  for(dn in dam_n){
    if(dn>0){
      for(i in 1:dn){
        for(l in 1:2){
          z_ij[icnt,"i"] <- i
          z_ij[icnt,"loc"] <- locs[l]

          seq <- seq_along(dam)
          seq <- 2:length(seq)

          if(l==1){
            tmp_phi <- abv_phi[1]
          }else{
            tmp_phi <- 1
          }
          z_ij[icnt,"RI"] <- rbinom(1,1, tmp_phi)
          c_ij[icnt,"RI"] <- rbinom(1,z_ij[icnt,"RI"], 1.)


          for(d in seq){
            tmp_phi <- abv_phi[d]
            if((l==1) & (sum(travelTime[1:d])>=startDelayTime[1]) & (sum(travelTime[1:d])<=endDelayTime[1])){
              tmp_phi <- tmp_phi * (1-d_phi[1])
            }

            z_ij[icnt,dam[d]] <- rbinom(1,z_ij[icnt,dam[d-1]], tmp_phi)
            c_ij[icnt,dam[d]] <- rbinom(1,z_ij[icnt,dam[d]], p[d])
            if(!z_ij[icnt,dam[d]]) break;
          }
          icnt <- icnt + 1
        }
      }
    }
  }

  return(list(z_ij=z_ij,
              c_ij = c_ij,
              dam = dam,
              dam_n = dam_n,
              abv_phi = abv_phi,
              blw_phi = blw_phi,
              p = p,
              d_phi = d_phi,
              startDelayTime = startDelayTime,
              endDelayTime = endDelayTime,
              travelTime = travelTime,
              damWithDelayMortality = damWithDelayMortality))
  # return(p[3,])
}
