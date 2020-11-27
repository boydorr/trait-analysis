  # Setting up the precision matrix

  # Setting up the variances vector (the diagonal of the var-cov matrix)
  for (j in 1:nt)
  {
    avsigma[j,j]~dgamma(1, 0.01) # mean 100 variance high
  }

  # Setting up the lower diagonal part of var-cov matrix
  avttrtau~dgamma(1.00, 1.00)

  for (j in 2:nt)
  {
    for (k in 1:(j-1))
    {
      avsigma[j,k]~dnorm(0,avttrtau)
    }
  }

  # Mirroring the lower diagonal to the upper diagonal of the var-cov matrix
  for (j in 1:(nt-1))
  {
    for (k in (j+1):nt)
    {
      avsigma[j,k]<-avsigma[k,j]
    }
  }

  # Conversion of var-cov matrix into precision matrix
  avomega<-inverse(avsigma)

  # monitor # avsigma, avttrtau
