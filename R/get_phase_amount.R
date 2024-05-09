

#' Get the amount of mobile phases needed for a sequence.
#'
#' Given a dataframe containing the gradient steps, it calculates the amount of each phase needed for the sequence.
#'
#' @param grad a dataframe with 4 columns. The first column contains the minute of each gradient step. The second column is the flow in mL/min of each gradient step. The third column is the percentage of the A-phase at each step. The four column is the percentage of the B-phase at each step.
#' @param ambient numeric of length 1. The number of minutes the system stays at the t0 conditions between each analysis.
#' @param an numeric of length 1. The number of analyses in the sequence.
#'
#'
#' @return It prints the information on the console.
#'
#' @export
get_phase_amount <- function(grad, ambient = 2, an = 1) {
  
  if (!is.data.frame(grad)) {stop("grad must be a dataframe")}
  if (length(grad)!=4) {stop("grad must be a data frame with 4 columns containing: minutes, flow, %A, %B")}
  if (!all(map_lgl(grad, is.numeric))) {stop("grad must be a data frame with 4 columns containing: minutes, flow, %A, %B. All of them must be numeric")}
  
  if (length(ambient)!=1) {stop("ambient must be a sigle number")}
  if (is.na(ambient)) {stop("ambient must be a sigle number, not a missing value")}
  if (!is.numeric(ambient)) {stop("ambient must be a sigle number")}
  
  if (length(an)!=1) {stop("an must be a sigle number")}
  if (is.na(an)) {stop("an must be a sigle number, not a missing value")}
  if (!is.numeric(an)) {stop("an must be a sigle number")}
  
  
  gradm <- grad
  colnames(gradm) <- c("min", "flow", "A", "B")
  gradm <- mutate(gradm, A=A/100, B=B/100, tot=A+B)
  
  if (any(is.na(gradm))) {stop("please, no missing values!")}
  if (any(gradm$tot!=1)) {stop("Total percentage is not 100!!!")}
  if (length(gradm$min)<2) {stop("At least two steps required!!!")}
  if (gradm$min[1]!=0) {stop("The first step must be minute zero!!")}
  
  needed_A <- 0
  needed_B <- 0
  for (i in 1:(length(gradm$min)-1)) {
    min_ts <- gradm$min[i+1]-gradm$min[i]
    if (min_ts<=0) {stop("minute column must be crescent!!!")}
    flow_ts <- (gradm$flow[i+1]+gradm$flow[i])/2
    A_ts <- (gradm$A[i+1]+gradm$A[i])/2
    B_ts <- (gradm$B[i+1]+gradm$B[i])/2
    
    needed_A <- needed_A+(flow_ts*min_ts*A_ts)
    needed_B <- needed_B+(flow_ts*min_ts*B_ts)
  }
  
  needed_A <- needed_A+(gradm$A[1]*gradm$flow[1]*ambient)
  needed_B <- needed_B+(gradm$B[1]*gradm$flow[1]*ambient)
  
  needed_A <- needed_A*an
  needed_B <- needed_B*an
  
  final_vector <- c(needed_A, needed_B)
  names(final_vector) <- c(paste0("  mL of ",colnames(grad)[3], " phase:"),
                           paste0("  mL of ",colnames(grad)[4], " phase:"))
  
  print(final_vector)
}

