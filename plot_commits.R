plot_commits <- function(commits, repositories, label = FALSE,
                         cat_levels = c("Tools", "Simulation", "ClustTrees",
                                        "Analysis", "Reports", "Other",
                                        "SideProject"),
                         cat_labels = c("Tools", "Simulation",
                                        "Clustering trees",
                                        "Analysis", "Reports", "Other",
                                        "Side projects"),
                         pal = c("#00ADEF", "#8DC63F", "#EC008C", "#00B7C6",
                                 "#7A52C7", "#F47920", "grey50"),
                         # This isn't used in this function but has to go
                         # here for the animation. Something to do with
                         # namespaces I think...
                         start_date = "2016-02-08"
                         ) {

    `%>%` <- magrittr::`%>%`

    commits <- dplyr::left_join(commits, repositories, by = "Repository")

    if (any(is.na(commits$Category))) {
        stop("Some categories are NA!")
    }

    if (any(is.na(commits$Type))) {
        stop("Some types are NA!")
    }

    commits <- dplyr::mutate(
        commits,
        Category = factor(
            Category,
            levels = rev(cat_levels),
            labels = rev(cat_labels))
        )

    commits_plot <- ggplot2::ggplot(
        commits,
        ggplot2::aes(x = Category, y = When, colour = Category, shape = Type)
    ) +
        ggplot2::geom_jitter(
            # Give each point it's own group for animation
            ggplot2::aes(group = seq_along(SHA)),
            width = 0.2, height = 0, alpha = 0.8, size = 2
        ) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_shape_manual(values = c(16, 1)) +
        ggplot2::scale_colour_manual(values = rev(pal), guide = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::labs(
            title = "A PhD in git commits",
            subtitle = glue::glue(
                "{nrow(commits)} commits in ",
                "{length(unique(commits$Repository))} repositories")
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size = 20),
            plot.subtitle = ggplot2::element_text(size = 16),
            axis.title = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(size = 12),
            legend.position = "bottom"
        )

    if (label) {
        # Get the first commit for each repository
        first_commits <- commits %>%
            dplyr::group_by(Repository) %>%
            dplyr::arrange(When) %>%
            dplyr::filter(dplyr::row_number() == 1) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Repository = stringr::str_trunc(Repository, 13))

        # Put half above the commits...
        first_commits_top <- first_commits %>%
            dplyr::group_by(Category) %>%
            dplyr::filter(dplyr::row_number() %% 2 == 1) %>%
            dplyr::ungroup() %>%
            # Stagger the adjustment to avoid overlaps
            dplyr::mutate(vjust = rep(c(3, 5), length.out = dplyr::n()))

        # ... and half below
        first_commits_bottom <- first_commits %>%
            dplyr::group_by(Category) %>%
            dplyr::filter(dplyr::row_number() %% 2 == 0) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(vjust = rep(c(3, 5), length.out = dplyr::n()))

        commits_plot <- commits_plot +
            # ggplot2::geom_text(data = first_commits, aes(label = Repository),
            #                   vjust = -2) +
            ggrepel::geom_text_repel(
                data = first_commits_top,
                ggplot2::aes(label = Repository, vjust = -vjust,
                             group = seq_len(nrow(first_commits_top))),
                size = 3,
                direction = "x",
                show.legend = FALSE
            ) +
            ggrepel::geom_text_repel(
                data = first_commits_bottom,
                ggplot2::aes(label = Repository, vjust = vjust,
                             group = seq_len(nrow(first_commits_bottom))),
                size = 3,
                direction = "x",
                show.legend = FALSE
            )
    }

    return(commits_plot)
}

# Functions used in the animated subtitle
get_days <- function(the_date, start_date) {
    lubridate::as.period(
        lubridate::as_datetime(the_date) -
            lubridate::as_datetime(start_date)
    )@day
}

get_nrepos <- function(commits, frame_along) {
    length(unique(commits$Repository[commits$When <= frame_along]))
}

animate_commits <- function(commits_plot) {

    # Glue together the subtitle. `frame_along` is a variable created by
    # `transition_reveal()`.
    subtitle <- paste(
       "{get_nrepos(commits, frame_along)} repositories with",
       "{sum(commits$When <= frame_along)} commits in",
       "{get_days(frame_along, start_date)} days by",
       "{as.Date(frame_along)}"
    )

    commits_ani <- commits_plot +
        gganimate::transition_reveal(When) +
        ggplot2::labs(subtitle = subtitle)

    gganimate::animate(commits_ani, height = 600, width = 800,
                       start_pause = 10, end_pause = 20)
}
