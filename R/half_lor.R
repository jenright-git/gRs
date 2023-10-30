
half_lor <- function(data){

  data <- data %>%
    mutate(prefix = replace_na(prefix, "="),
           concentration = ifelse(prefix == "<",
                                  yes = concentration*0.5,
                                  no = concentration),
           prefix = "=")

  return(data)


}

