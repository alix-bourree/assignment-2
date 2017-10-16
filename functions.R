# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
  # Set a default value to return
  result <- 0
  x <- d[[var]] # Remember, as we showed in class: there are two ways
  # to access a column, and this is one; you should try
  # and figure out why the other way won't work here,
  # but that's not part of the homework
  if (!is.null(x)) { # This tests to see whether the column exists in
    # d; again, you should try and find a way to test
    # this out for yourself to verify that it's true,
    # but that's not part of the homework
    # YOUR CODE HERE: if x contains numbers, set the variable
    for (i in x){
      if (is.numeric(i)){
        result <- result + i
      }
      else{
        return(NULL)
      }
        
    }
    # result to be the sum of the values in x
    #
    # You will need to do a bit of research in order to figure out how
    # to test whether a vector contains numbers.
  }
  # YOUR CODE HERE: return the result
  return(result)
}


# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL
#
my_sum <- function(x){
  #affectation NULL pour le résultat
  result <- 0
  #si x n'est pas un vecteur null
  if (!is.null(x)){
    #pour toutes les valeurs de x
    for(i in x){
      #si la valeur est un nombre
      if(is.numeric(i)){
        #on somme tous ces nombre
        result <- result + i
      }
      else{
        #sinon on renvoie NULL
        return(NULL)
      }
    }
    
  }
  #on retourne la somme
  return(result)
    
}

# Retourne la somme d'un vecteur divisé par k.
#
# ARGUMENTS:
# x: a vector
# k: un nombre
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values divised by k; otherwise, returns NULL
#
sum_divided_by <- function(x,k){
  #si la somme du vecteur n'est pas null et que k est un nombre
  if (!is.null(my_sum(x)) && is.numeric(k)){
    #on retourne la somme du vecteur divisé par k
    return(my_sum(x)/k)

  } 
  else{
    #sinon on retourne NULL
    return(NULL)
  }

}

my_mean <- function(x){
  #la fonction length renvoie la taille du vecteur, donc on retourne la somme des valeurs de x divisé par la taille du vecteur x
  return(sum_divided_by(x, length(x)))
}


# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
# string
# grouping_var: the name of a column of d containing a grouping variable,
# provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))
  # YOUR CODE HERE: Create a violin plot
  p <- p + geom_violin()
  return(p)
}

# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  g1 <- d_1[[var]]
  g2 <- d_2[[var]]
  result <- median(g1)-median(g2)
  return(result)

}

randomize_rts <- function(d) {
  result <- d
  n <- nrow(d)
  d$logRT <- sample(d$logRT, n)
  return(d)
}

# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
# provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  result <- d
  n <- nrow(d)
  d[[var]] <- sample(d[[var]], n)
  return(d)
}

# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
# provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
# a data frame, the name of a variable on which to calculate the
# test statistic, the name of a grouping variable, the value of
# the grouping variable corresponding to the first group, and
# the value of the grouping variable corresponding to the second
# group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
# - observed: the value of statistic() in d
# - permuted: a vector containing the values of statistic() under n_samples
# permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    random <- randomize(d, var)
    permutation_statistics[i] <- statistic(random,var, grouping_var, group1, group2 )
  
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

new_test_statistic <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  g1 <- d_1[[var]]
  g2 <- d_2[[var]]
  #calcul la moyenne des médianes
  result <- (median(g1)+median(g2))/2
  return(result)
  
}

permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}

permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}