iterative_latex <- function(title, equation, highlight = NULL, explanation = "", color = "#86a293") {
  start <- glue::glue("## {title}\n
             
             $${equation}$$\n")
  
  d <- tibble::tibble(
    eqn = purrr::map_chr(highlight, highlight_latex, 
                         equation = equation, color = color),
    explanation = explanation,
    title = title
  )
  
  cat(glue::glue("{start}\n\n{
                 glue::glue_collapse(
                 glue::glue_data(d, '## {title}\n\n{eqn}\n\n{explanation}<br>\n\n', .trim = FALSE))}"))
  
}


highlight_latex <- function(highlight, equation, color) {
  highlight_ <- remove_bracket(highlight)
  up <- gsub(highlight, 
             glue::glue("{\\\\require{color}\\\\colorbox{.<color.>}{$.<highlight_.>$}}", .open = ".<", .close = ".>"),
             equation)
  glue::glue("$${up}$$")
}

check_bracket <- function(x) {
  grepl("^\\{(.*)\\}$", x)
}

remove_bracket <- function(x) {
  if (check_bracket(x)) {
    return(gsub("\\{|\\}", "", x))
  } else {
    return(x)
  }
}

