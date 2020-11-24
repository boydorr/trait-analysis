  # Setting up the precision matrices

  # Setting up the variances vector (the diagonal of the var-cov matrix)
  for (j in 1:nt)
  {
    for (m in 1:ns)
    {
      sigma[m,j,j]~dgamma(1, 0.01) # mean 100 variance high
    }
  }

  # Setting up the lower diagonal part of var-cov matrix
  ttrtau~dgamma(1.00, 1.00)

  for (j in 2:nt)
  {
    for (k in 1:(j-1))
    {
      for (m in 1:ns)
      {
        sigma[m,j,k]~dnorm(0,ttrtau)
      }
    }
  }

  # Mirroring the lower diagonal to the upper diagonal of the var-cov matrix
  for (j in 1:(nt-1))
  {
    for (k in (j+1):nt)
    {
      for (m in 1:ns)
      {
        sigma[m,j,k]<-sigma[m,k,j]
      }
    }
  }

  # Conversion of var-cov matrix into precision matrix
  for (m in 1:ns)
  {
    omega[m,1:nt,1:nt]<-inverse(sigma[m,1:nt,1:nt])
  }

  # monitor # sigma, ttrtau
