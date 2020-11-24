  # Setting up the regression coefficients matrix
  tertau ~ dgamma(0.0001,0.01)

  for (k in 1:ne) # Number of environmental drivers
  {
    for (j in 1:nt) # Number of traits
    {
      for (m in 1:ns) # Number of species
      {
        ter[j,k,m] ~ dnorm(0,tertau)
      }
    }
  }
  # monitor # ter, tertau
