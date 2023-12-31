#' Compute Cosine Similarity Between Two Vectors
#'
#' This function computes the cosine similarity between two vectors, each represented by two scalar values.
#'
#' @param a1 The first scalar value of the first vector.
#' @param a2 The second scalar value of the first vector.
#' @param b1 The first scalar value of the second vector.
#' @param b2 The second scalar value of the second vector.
#'
#' @return A scalar value representing the cosine similarity between the two vectors.
#' @export
#' @examples
#' cosine_similarity(1, 0, 0, 1) # Should return 0
#' cosine_similarity(1, 0, 1, 0) # Should return 1
cosine_similarity <- function(a1, a2, b1, b2) {
  A <- c(a1, a2)
  B <- c(b1, b2)
  # Compute dot product of A and B
  dot_product <- sum(A * B)

  # Compute magnitude (norm) of A and B
  mag_A <- sqrt(sum(A * A))
  mag_B <- sqrt(sum(B * B))

  # Compute cosine similarity
  cosine_similarity <- dot_product / (mag_A * mag_B)

  return(cosine_similarity)
}

#' Rotate Vector by Angle Specified with Slope and Origin
#'
#' This function rotates a vector by an angle determined by a given slope and origin.
#'
#' @param x The x coordinate of the vector.
#' @param y The y coordinate of the vector.
#' @param slope The slope for the rotation (default is 1).
#' @param origin A vector of x and y coordinates of the origin for the rotation (default is c(0, 0)).
#'
#' @return A matrix of x and y coordinates of the rotated vector.
#' @export
#' @examples
#' # Rotates the vector (1,0) by the angle corresponding to slope = 1
#' # around the origin
#' rotate_vector(1, 0, slope = 1, origin = c(0, 0))
rotate_vector <- function(x, y, slope = 1, origin = c(0, 0)) {
  A <- c(x, y)

  # Convert slope to angle in radians
  angle <- atan(slope)

  # Define the rotation matrix
  rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), nrow = 2)

  # Adjust the vector A by the origin
  A_adjusted <- A - origin

  # Rotate the vector
  A_rotated <- rotation_matrix %*% A_adjusted

  # Adjust back by adding the origin
  A_rotated <- A_rotated + matrix(origin, nrow = 2)

  return(A_rotated)
}

#' Rotate Matrix by Angle Specified with Slope and Origin
#'
#' This function rotates a matrix of vectors by an angle determined by a given slope and origin. Each row of the matrix represents a vector.
#'
#' @param M A matrix of x and y coordinates of the vectors to be rotated (each row is a vector of length 2).
#' @param slope The slope for the rotation (default is 1).
#' @param origin A vector of x and y coordinates of the origin for the rotation (default is c(0, 0)).
#'
#' @return A matrix of x and y coordinates of the rotated vectors.
#' @export
#' @examples
#' M <- matrix(c(1, 0, 0, 1), ncol = 2) # Two vectors: (1,0) and (0,1)
#' rotate_matrix(M, slope = 1) # Rotates both vectors
rotate_matrix <- function(M, slope = 1, origin = c(0, 0)) {
  # Convert slope to angle in radians
  angle <- atan(slope)

  # Define the rotation matrix
  rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), nrow = 2)

  # Adjust the M by the origin
  adjusted_M <- t(apply(M, 1, function(v) v - origin))

  # Rotate the M
  rotated_M <- adjusted_M %*% t(rotation_matrix)

  # Adjust back by adding the origin
  rotated_M <- t(apply(rotated_M, 1, function(v) v + origin))

  return(rotated_M)
}
#' Transform Cartesian Coordinates to Polar Coordinates
#'
#' This function converts Cartesian coordinates (x, y) to polar coordinates (radius, angle).
#'
#' @param x The x coordinate (vector).
#' @param y The y coordinate (vector).
#'
#' @return A tibble of radius (r) and angle (theta) in polar coordinates.
#' @importFrom dplyr tibble
#' @export
cartesian_to_polar <- function(x, y) {
  # Compute the radius
  r <- sqrt(x^2 + y^2)

  # Compute the angle
  theta <- atan2(y, x)

  # Return the radius and angle
  return(tibble(r = r, theta = theta))
}
#' Transform Polar Coordinates to Cartesian Coordinates
#'
#' This function converts polar coordinates (radius, angle) to Cartesian coordinates (x, y).
#'
#' @param r The radius (vector).
#' @param theta The angle (vector).
#'
#' @return A tibble of x and y coordinates in Cartesian coordinates.
#' @importFrom dplyr tibble
#' @export
polar_to_cartesian <- function(r, theta) {
  # Compute the x coordinate
  x <- r * cos(theta)

  # Compute the y coordinate
  y <- r * sin(theta)

  # Return the x and y coordinates
  return(tibble(x = x, y = y))
}


