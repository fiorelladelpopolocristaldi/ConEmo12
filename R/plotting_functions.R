#--- Plotting functions

# This file contains useful functions to produce plot and prepare/clean data
# for plotting purposes

# theme_paper -------------------------------------------------------------

# the general theme used across figures

theme_paper <- function(){
  cowplot::theme_minimal_hgrid(font_size = 18) +
    theme(axis.title.x = element_blank(),
          legend.position = "right")
}

# clean_names_plot --------------------------------------------------------

# cleaning the names for the plot

clean_names_plot <- function(data, mod = c("prereg", "cong")){

  dat_clean <- data %>%
    mutate(valence = case_when(valence == "neg" ~ "Neg",
                               valence == "neu" ~ "Neu",
                               TRUE ~ ""),
           resp = case_when(resp == "arrating" ~ "Arousal",
                            resp == "exprating" ~ "Expectancy",
                            resp == "valrating" ~ "Valence"))
  if(mod == "prereg"){
    dat_clean %>%
      mutate(s1_color = as.character(s1_color),
             s1_color = case_when(s1_color == "neu" ~ "Neu",
                                  s1_color == "neg" ~ "Neg",
                                  TRUE ~ ""))
  }else if(mod == "cong"){
    dat_clean %>%
      mutate(Cong = ifelse(Cong == 1, "NCon", "Con"))
  }
}

# box_plot ----------------------------------------------------------------

# the main function for producing the plots. Use lazy evaluation with the
# rlang package for more flexibility

box_plot <- function(data, ..., nrow = NULL, ncol = NULL){
  dots <- rlang::enexprs(...) # which factors to consider
  data %>%
    mutate(resp = case_when(resp == "Exprating" ~ "Expectancy",
                            resp == "Valrating" ~ "Valence",
                            resp == "Arrating" ~ "Arousal",
                            TRUE ~ resp),
           resp = factor(resp, levels = c("Expectancy", "Valence", "Arousal"))) %>%
    ggplot(aes(x = interaction(!!!dots, sep = ""), y = .mean, fill = group)) +
    geom_point(aes(color = group),
               position=position_jitterdodge(jitter.width = 0.4, jitter.height = 0)) +
    geom_boxplot(outlier.shape=NA, alpha = 0.7) +
    facet_wrap(~resp, scales = "free_x", nrow = nrow, ncol = ncol) +
    theme_paper() +
    scale_color_manual(name = "", values=c("#4DCA87", "#F09A0F")) +
    scale_fill_manual(name = "", values=c("#4DCA87", "#F09A0F")) +
    ylab("Rating (%)") +
    theme(axis.text.x = ggtext::element_markdown()) # parsing html tags
}

# remove_axis_text -------------------------------------------------------------

# remove the text from a specific axes for plotting purposes

remove_axis_text <- function(plot, axes){
  if(axes == "x"){
    plot +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }else{
    plot +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }

}

# remove_facets_text ------------------------------------------------------

# remove the text from a specific facet for plotting purposes

remove_facets_text <- function(x){
  x +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
}
