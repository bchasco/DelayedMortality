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
survMultinom <- function(dam = c("RI","MCN","BON_J","BON_A"),
                 dam_n = c(1e4,0,0,0),
                 abv_phi = c(0.98,0.7,0.6,0.01),
                 blw_phi = c(1.0,0.7,0.6,0.01),
                 p = c(1.,0.1,0.1,1.0),
                 d_phi = c(0.1,0.0,0.0,0.0),
                 Delay = c(30,0,0,0),
                 travelTime = c(0,30,30,720),
                 phi_sig = 0.1,
                 phi_BONA_sig = 0.01,
                 p_cv = 0.1) {

  c_ij <- as.data.frame(matrix(0,nrow=2,ncol=2+length(dam)))
  z_ij <- as.data.frame(matrix(0,nrow=2,ncol=2+length(dam)))
  names(c_ij) <- c("i","loc",dam)
  names(z_ij) <- c("i","loc",dam)

  locs <- c("above","below")

  damWithDelayMortality <- dam[cumsum(travelTime)==Delay[1]]

  icnt <- 1
  tmp_phi <- data.frame(matrix(0,2,length(dam)))
  tmp_p <- data.frame(matrix(0,1,length(dam)))
  names(tmp_phi) <- dam
  names(tmp_p) <- dam
  for(l in 1:2){

    z_ij[l,"loc"] <- locs[l]
    c_ij[l,"loc"] <- locs[l]
    if(l==1){
      tmp_phi[1,] <- abv_phi
      tmp_dphi <- 1 - d_phi[1] * (cumsum(travelTime)==Delay[1])
      tmp_phi[1,] <- tmp_phi[1,] * tmp_dphi

    }else{
      tmp_phi[2,] <- blw_phi
    }
    # Set the desired mean and CV
    tmp_cv <- c(1e-100,phi_sig)
    for(phi_i in seq_along(tmp_phi[l,])){
      # Calculate the shape parameters of the beta distribution

      alpha <- (tmp_phi[l,phi_i]^2) * ((1 - tmp_phi[l,phi_i]) / tmp_cv[phi_i]^2) - tmp_phi[l,phi_i]
      beta <- alpha * ((1 - tmp_phi[l,phi_i]) / tmp_phi[l,phi_i])
      # Generate random proportions from the beta distribution
      if(tmp_phi[l,phi_i]!=1){
        tmp_phi[l,phi_i] <- rbeta(n = 1, shape1 = alpha, shape2 = beta)
      }else{
        tmp_phi[l,phi_i] <- 1
      }
    }

    # print(dam)
    # print()
    damsWithDelayedMortality <- dam[cumsum(travelTime)==Delay[1]]
    # print(tmp_dphi)
    # print(damsWithDelayedMortality)
    #Initial condition
    z_ij[l,3] <- rbinom(1, dam_n[1], tmp_phi[l,1])
    c_ij[l,3] <- rbinom(1,z_ij[l,3], 1)

    for(dd in seq_along(dam)){
      if(p[dd]!=1){
        alpha <- (p[dd]^2) * ((1 - p[dd]) / p_cv^2) - p[dd]
        beta <- alpha * ((1 - p[dd]) / p[dd])
        tmp_p[1,dd] <- rbeta(1,alpha,beta)
      }else{
        tmp_p[1,dd] <- 1
      }
      if(dd>1){
        z_ij[l,dd+2] <- rbinom(1,z_ij[l,dd+2-1], tmp_phi[l,dd])
        c_ij[l,dd+2] <- rbinom(1,z_ij[l,dd+2], tmp_p[1,dd])
      }
    }
  }

  # print(tmp_phi)

  return(list(z_ij=z_ij,
              c_ij = c_ij,
              dam = dam,
              dam_n = dam_n,
              abv_phi = abv_phi,
              blw_phi = blw_phi,
              p = p,
              randomPhi = tmp_phi,
              d_phi = d_phi,
              randomP = tmp_p,
              Delay = Delay,
              travelTime = travelTime,
              damCol = cumsum(travelTime)>=Delay,
              damsWithDelayedMortality = damWithDelayMortality
              ))
  # return(p[3,])
}
