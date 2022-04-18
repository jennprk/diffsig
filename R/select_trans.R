#' Selecting L mutation transitions out of 96
#'
#' @export
#' @param Y mutation count matrix Y (96xN)
#' @param C mutation signature matrix C (96xK)
#' @param L number of mutation transitions to be reduced to
#' @return list of subsetted signature matrix C and subsetted count data Y (=list(C,Y))
#'
select_trans <- function(Y,C,L) {
  ranks <- apply(C, 2, function(col) rank(-col)) # rank from high to low contribution of transitions
  selected_transitions <- c()

  if (L == 96) {
    selected_transitions = 1:L # if l=96, simply choose all transitions (1 to 96)
  } else { # if l<96, selection process
    while (length(selected_transitions)<L) { # run until we get 'L' transitions
      for (col in 1:K) {
        current_pick = which.min(ranks[,col]) # choose transition with highest contribution in column
        selected_transitions = c(selected_transitions, current_pick) # append
        selected_transitions = unique(selected_transitions) # remove if duplicated

        ranks[current_pick, ] = Inf # replace chosen transition row to infinity so it would not be detected in next iteration
        if (length(selected_transitions)==L) break
      }
    }
  }

  C_selected = C[selected_transitions, ]
  C_selected = apply(C_selected, 2, function(col) col/sum(col))

  Y_selected = Y[selected_transitions, ]

  return(list(C = C_selected, Y = Y_selected))
}
