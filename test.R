library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

# Create a tibble data frame
df <- tibble(
  MCN = double(0),     # Initialize an empty double column
  BON_J = double(0),     # Initialize an empty double column
  BON_A = double(0)      # Initialize an empty double column
)

# set.seed(100)
p <- list()
r_p <- list()
r_phi <- list()
icnt <- 1
starttime <- Sys.time()

nsim <- 100
nStrata <- c(10, 20, 25, 30)
DelayOptions <- c(2,3) #Bon_j, Bon_A
parVar <- data.frame(low = c(5e-3,5e-3,1e-3),
                     med = c(5e-2,5e-2,5e-3),
                     high = c(1e-1,1e-1,1e-2))

pParVar <- data.frame(low = c(5e-3),
                     med = c(1e-2),
                     high = c(5e-2))


for(nSt in nStrata){ #number of strata
  for(var in 1:ncol(parVar)){ #variability in survival and detection probability
    for(sim in 1:nsim){ #each simulation
      for(d_phi in c(0.05, 0.1,0.2,0.3)){ #delayed mortality
        for(n in c(1e3, 5e3, 1e4, 2e4)){ #sample size for each strata
          for(Delay in DelayOptions){ #Where does delayed mortality occur
            for(strata in 1:nSt){ #each strata
              out <- DelayedMortality::survMultinom(dam = c("RI","MCN","BON_J","BON_A"),
                                                    dam_n = c(n,0,0,0),
                                                    abv_phi = c(1.0,0.4,0.75,0.01),
                                                    blw_phi = c(1.0,0.4,0.75,0.01),
                                                    p = c(1.0,0.1,0.1,1.0),
                                                    d_phi = c(d_phi,0.0,0.0,0.0),
                                                    Delay = c(Delay,0,0,0),
                                                    travelTime = c(0,1,1,1),
                                                    phi_sig = parVar[,var],
                                                    phi_BONA_sig = parVar[3,var],
                                                    p_cv = pParVar[1,var])

              # print(parVar[,var])
              p[[icnt]] <- out[['c_ij']] %>%
                group_by(loc) %>%
                summarize(across(c("RI", "MCN","BON_J","BON_A"),sum)) %>%
                pivot_longer(cols = -loc, names_to =  "dam", values_to = "returns")

              p[[icnt]]$d_phi <- d_phi
              p[[icnt]]$strata <- strata
              p[[icnt]]$n <- n
              p[[icnt]]$nStrata <- nSt
              p[[icnt]]$sim <- sim
              p[[icnt]]$var <- names(parVar)[var]
              p[[icnt]]$delayedMortalityLocation <- out[['damWithDelayedMortality']]
              p[[icnt]]$delay <- out[['dam']][cumsum(out[['travelTime']])==out[['Delay']][1]]
              p[[icnt]]$travelTime <- rep(cumsum(travelTime),2)
              p[[icnt]]$damCol <- rep(cumsum(travelTime),2)>=Delay

              r_phi[[icnt]] <- out[["randomPhi"]] %>%
                pivot_longer(cols = MCN:BON_A, names_to = "dam", values_to = "par")
              r_phi[[icnt]]$parName <- "phi"
              r_phi[[icnt]]$d_phi <- d_phi
              r_phi[[icnt]]$var <- names(parVar)[var]

              r_p[[icnt]] <- out[["randomP"]] %>%
                pivot_longer(cols = MCN:BON_A, names_to = "dam", values_to = "par")
              r_p[[icnt]]$parName <- "p"
              r_p[[icnt]]$d_phi <- d_phi
              r_p[[icnt]]$var <- names(parVar)[var]
              icnt <- icnt + 1

            }
          }
        }
      }
    }
  }
}



# simout2 <- list(r_p = r_p, r_phi = r_phi, p = p)
# saveRDS(file="simoutput2.rds", simout2)


