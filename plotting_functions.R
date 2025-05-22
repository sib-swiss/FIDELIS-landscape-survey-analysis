fidelis_colours <-
  c(
    "#FBB03B",
    # Pastel orange
    "#001B6D",
    # Midnight blue
    "#3C85FA",
    # Bright blue
    "#6e5200",
    #Very dark orange
    "#040405",
    # Very dark (mostly black) blue
    "#e4fa3c",
    # Bright yellow
    "#356d00" # Dark green
  )

fid_color_func <- colorRampPalette(c(
  "#FBB03B",
  # Pastel orange
  "#001B6D",
  # Midnight blue
  "#3C85FA",
  # Bright blue
  "#6e5200",
  #Very dark orange
  "#e4fa3c",
  # Bright yellow
  "#356d00" # Dark green
))

dt_buttons <- function(x) {
  datatable(
    x,
    extensions = 'Buttons',
    options = list(
      pageLength = 20,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    )
  )
}

theme_fidelis <- function (base_size = 18,
                           base_family = "") {
  theme_classic() %+replace%
    theme(
      plot.background = element_rect(colour = "#ffffff"),
      # White
      legend.position = "bottom",
      # Legend at bottom of visualisation
      panel.grid.major  = element_line(colour = "#ffffff"),
      # White
      panel.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
      # White
      panel.border = element_rect(colour = "#ffffff", fill = NA),
      # White
      axis.line = element_line(colour = "#97999B"),
      #Cool Gray 7
      axis.ticks = element_line(colour = "#97999B"),
      # Cool Gray 7
      axis.text = element_text(colour = "#75787B"),
      # Cool Gray 9
      text = element_text(colour = "#75787B") # Cool Gray 9
    )
}



plot_mcq <- function(survey_results,
                     question_metadata,
                     question_id = "851d8426") {
  qmeta <- question_metadata |>
    filter(qid == question_id)
  
  if (nrow(qmeta) == 0)
    stop("Question ID does not exist")
  if (qmeta$question_type != "mcq")
    stop("Not multiple choice question")
  
  
  
  answers <- survey_results |>
    select(!!sym(question_id)) |>
    rename(answers = 1)
  
  n_answers <- sum(!is.na(pull(answers, 1)))
  n_no_answers <- sum(is.na(pull(answers, 1)))
  
  answer_counts <- answers |>
    mutate(answers = ifelse(is.na(answers), "not answered", answers)) |>
    count(answers) |>
    arrange(desc(n))
  
  p <- answer_counts |>
    ggplot(aes(fct_reorder(answers, n), n)) +
    geom_col(fill = fidelis_colours[1]) +
    coord_flip() +
    geom_text(aes(label = n), hjust = -.5) +
    labs(
      x = NULL,
      y = NULL,
      fill = "",
      caption = paste0("answered = ", n_answers, "; not answered = ", n_no_answers)
    ) +
    theme_fidelis() +
    theme(
      legend.position = "none",
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      text = element_text(size = 12)
    )
  
  return(p)
}

plot_mcq_table <- function(survey_results,
                           question_metadata,
                           question_ids = c(
                             `As deposited` = "ecdbc02c",
                             `Deposit compliance` = "af45399f",
                             `Initial curation` = "580ca91e",
                             `Active preservation` = "a255b47e"
                           )) {
  answers <- survey_results |>
    select(all_of(question_ids)) |>
    pivot_longer(cols = everything(),
                 names_to = "question",
                 values_to = "answers") |>
    mutate(answers = ifelse(is.na(answers), "not answered", answers)) |>
    group_by(question, answers) |>
    summarise(n = n())
  
  answers |>
    ggplot(aes(x = n, y = question, fill = answers)) +
    geom_bar(stat = "identity") +
    scale_color_manual(values = fidelis_colours) +
    scale_fill_manual(values = fidelis_colours) +
    geom_text(
      aes(label = n),
      position = position_stack(vjust = .5),
      size = 3,
      colour = "white"
    ) +
    labs(x = NULL,
         y = NULL,
         fill = "",
         theme_fidelis()) +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.line.x = element_blank(),
      axis.text.x = element_blank(),
      panel.background = element_blank()
    ) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}
