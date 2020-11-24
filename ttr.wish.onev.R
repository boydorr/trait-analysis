  # Setting up the precision matrix
  dof ~ dgamma(1.0, 1.0)
  avomega ~ dwish(ttrI, nt - 1 + dof)
  # Conversion of precision matrix into var-cov matrix
  avsigma <- inverse(avomega)

  # monitor # avsigma, dof
