#' Calculate neutral mass considering the observed m/z and the assigned adducts.
#'
#' Given mass-to-charge ratio measurement(s) and given adduct estimation(s), it calculates the related neutral mass.
#'
#' @param observed_mz numeric. Observed mass-to-charge ratios.
#' @param adduct character. Possible adduct estimated. Examples: .
#'
#'
#' @return A numeric vector, with the calculated neutral masses. NA will be computed in case of adducts with formula elements not recognised.
#' 
#' @examples
#' \dontrun{
#'
#'
#' ## Examples of adducts include:
#' 
#' # "[M+H]+"
#' # "[M-H]-"
#' # "[M+NH4]+"
#' # "[M+Na]+"
#' # "[M+H-H2O]+"
#' # "[M+2H]2+"
#' # "[M-H2O-H]-"
#' # "[M+Cl]-"
#' # "[M+FA-H]-"
#' # "[M+HCOO]-"
#' 
#' 
#' calculate_neutral_mass(observed_mz = c(174.14748, 240.84195, 460.3591, 245.22432),
#'                        adduct = c("[M+H]+", "[M+H]+", "[M+Na]+", "[M+H-H2O]+"))
#'
#'
#' }
#'
#' @export
calculate_neutral_mass <- function(observed_mz, adduct) {
  
  observed_mz <- as.numeric(observed_mz)
  adduct <- as.character(adduct)
  
  output_vector <- rep(NA_real_, length(observed_mz))
  
  if (length(observed_mz)>0 & length(adduct)>0) {
    
    if (length(observed_mz)>1 & length(adduct)==1) {
      adduct <- rep(adduct, length(observed_mz))
    }
    if (length(observed_mz)==1 & length(adduct)>1) {
      adduct <- adduct[1]
    }
    if (length(observed_mz)!=length(adduct)) {
      stop("observed_mz and adduct do not have the same length")
    }
    improveformula <- function(frml) {
      frml <- gsub("([A-Z][a-z]?)(?=[A-Z])", "\\11", frml, perl = TRUE)
      frml <- gsub("([A-Z][a-z]?)$", "\\11", frml, perl = TRUE)
      return(frml)
    }
    electron_mass <- 0.000548579909065
    most_common_masses <- c(H = 1.0078250322,
                            H2O = 18.010564683,
                            NH4 = 18.034374132,
                            Na = 22.98976928,
                            K = 38.96370649,
                            Cl = 34.968852682,
                            Br = 78.9183376,
                            HCOO = 44.997654270,
                            FA = 46.005479302,
                            HCOOH = 46.005479302,
                            CH2O2 = 46.005479302,
                            CH3COO = 59.013304334,
                            Hac = 60.021129366,
                            CH3COOH = 60.021129366,
                            C2H4O2 = 60.021129366,
                            IsoProp = 60.057514874,
                            CH3CHOHCH3 = 60.057514874,
                            C3H7OH = 60.057514874,
                            C3H8O = 60.057514874,
                            DMSO = 78.01393598,
                            C2H6OS = 78.01393598,
                            TFA = 113.99286376,
                            C2HF3O2 = 113.99286376,
                            CF3COOH = 113.99286376,
                            ACN = 41.026549100,
                            CH3CN = 41.026549100,
                            C2H3N = 41.026549100,
                            MeOH = 32.026214747,
                            CH4O = 32.026214747,
                            CH3OH = 32.026214747,
                            EtOH = 46.041864811,
                            C2H6O = 46.041864811,
                            CH3CH2OH = 46.041864811)
    
    for (i in seq(length(output_vector))) {
      
      this_observed_mz <- observed_mz[i]
      this_adduct <- adduct[i]
      this_calculated_neutral_mass <- NA_real_
      
      if (!is.na(this_observed_mz) & !is.na(this_adduct)) {
        if (startsWith(this_adduct, "[M") & sub("^\\[M(.)(.*)$", "\\1", this_adduct)%in%c("-", "+") & grepl("]", this_adduct) & (endsWith(this_adduct, "-") | endsWith(this_adduct, "+"))) {
          
          this_adduct_operations <- tibble(add_or_subctr = sub("^\\[M(.)(.*)$", "\\1", this_adduct),
                                           formulaa = sub("^\\[M(.)(.*)$", "\\2", this_adduct))
          
          repeat {
            if (!grepl("[-+]", sub("\\].*$", "", this_adduct_operations$formulaa[nrow(this_adduct_operations)]))) {
              whats_after_the_sqrt_brkt <- sub(".*\\]", "", this_adduct_operations$formulaa[nrow(this_adduct_operations)])
              this_adduct_operations$formulaa[nrow(this_adduct_operations)] <- sub("\\].*$", "", this_adduct_operations$formulaa[nrow(this_adduct_operations)])
              break
            } else {
              this_adduct_operations_new_row <- tibble(add_or_subctr = sub(".*?([-+]).*", "\\1", this_adduct_operations$formulaa[nrow(this_adduct_operations)]),
                                                       formulaa = this_adduct_operations$formulaa[nrow(this_adduct_operations)])
              this_adduct_operations$formulaa[nrow(this_adduct_operations)] <- sub("[+-].*", "", this_adduct_operations$formulaa[nrow(this_adduct_operations)])
              
              this_adduct_operations_new_row$formulaa[1] <- substr(this_adduct_operations_new_row$formulaa[1], nchar(paste0(this_adduct_operations$formulaa[nrow(this_adduct_operations)], this_adduct_operations_new_row$add_or_subctr[1])) + 1, nchar(this_adduct_operations_new_row$formulaa[1]))
              
              this_adduct_operations <- bind_rows(this_adduct_operations, this_adduct_operations_new_row)
            }
          }
          
          the_operations <- 0
          
          for (taop in seq(nrow(this_adduct_operations))) {
            
            if (grepl("^[0-9]", this_adduct_operations$formulaa[taop])) {
              number_of_times_for_this <- as.numeric(sub("^([0-9]+).*", "\\1", this_adduct_operations$formulaa[taop]))
              this_formulaa   <- sub("^[0-9]+", "", this_adduct_operations$formulaa[taop])
            } else {
              number_of_times_for_this <- 1
              this_formulaa   <- this_adduct_operations$formulaa[taop]
            }
            
            if (this_formulaa %in% names(most_common_masses)) {
              number_to_apply <- unname(most_common_masses[which(names(most_common_masses)==this_formulaa)])
              
              if (this_formulaa=="FA") {
                this_formulaa <- "HCOOH"
              } else if (this_formulaa=="Hac") {
                this_formulaa <- "CH3COOH"
              } else if (this_formulaa=="IsoProp") {
                this_formulaa <- "C3H7OH"
              } else if (this_formulaa=="DMSO") {
                this_formulaa <- "C2H6OS"
              } else if (this_formulaa=="TFA") {
                this_formulaa <- "C2HF3O2"
              } else if (this_formulaa=="ACN") {
                this_formulaa <- "CH3CN"
              }
            } else {
              number_to_apply <- as.numeric(tryCatch({MSbox::mass(improveformula(this_formulaa))}, error = function(e) {conditionMessage(e)}))
            }
            
            if (!is.na(number_to_apply)) {
              
              if (this_formulaa %in% c("H", "Na", "K", "NH4")) {
                number_to_apply <- number_to_apply - electron_mass
              } else if (this_formulaa %in% c("Cl", "Br", "HCOO", "CH3COO")) {
                number_to_apply <- number_to_apply + electron_mass
              }
              
              number_to_apply_multiplied <- number_of_times_for_this*number_to_apply
              
              if (this_adduct_operations$add_or_subctr[taop] == "-") {
                the_operations <- the_operations - number_to_apply_multiplied
              } else if (this_adduct_operations$add_or_subctr[taop] == "+") {
                the_operations <- the_operations + number_to_apply_multiplied
              }
            }
          }
          
          the_z <- sub(".*?(\\d*)([+-])$", "\\1", whats_after_the_sqrt_brkt)
          if (the_z == "") {the_z <- 1}
          the_z <- as.numeric(the_z)
          
          this_calculated_neutral_mass <- this_observed_mz*the_z-the_operations
          
        }
      }
      
      output_vector[i] <- this_calculated_neutral_mass
    }
  }
  
  return(output_vector)
}

