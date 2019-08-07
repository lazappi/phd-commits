plot_commits <- function(commits, repositories) {
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
            levels = rev(c("Tools", "Simulation", "ClustTrees", "Analysis",
                           "Reports", "Other", "SideProject")),
            labels = rev(c("Tools", "Simulation", "Clustering trees",
                           "Analysis", "Reports", "Other", "Side projects")))
        )

    first_commits <- commits %>%
        dplyr::group_by(Repository) %>%
        dplyr::arrange(When) %>%
        dplyr::filter(dplyr::row_number() == 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Repository = stringr::str_trunc(Repository, 13))

    first_commits_top <- first_commits %>%
        dplyr::group_by(Category) %>%
        dplyr::filter(dplyr::row_number() %% 2 == 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(vjust = rep(c(3, 5), length.out = n()))

    first_commits_bottom <- first_commits %>%
        dplyr::group_by(Category) %>%
        dplyr::filter(dplyr::row_number() %% 2 == 0) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(vjust = rep(c(3, 5), length.out = n()))

    pal <- c(
        "#00ADEF", # Blue
        "#8DC63F", # Green
        "#EC008C", # Pink
        "#00B7C6", # Teal
        "#7A52C7", # Purple
        "#F47920", # Orange
        "grey50"   # Grey
    )

    ggplot2::ggplot(
        commits,
        ggplot2::aes(x = Category, y = When, colour = Category, shape = Type)
    ) +
        ggplot2::geom_jitter(
            aes(group = seq_along(SHA)),
            width = 0.2, height = 0, alpha = 0.8, size = 2
        ) +
        #ggplot2::geom_text(data = first_commits, aes(label = Repository),
        #                   vjust = -2) +
        # ggrepel::geom_text_repel(
        #     data = first_commits_top,
        #     aes(label = Repository, vjust = -vjust,
        #         group = seq_len(nrow(first_commits_top))),
        #     size = 3,
        #     direction = "x",
        #     show.legend = FALSE
        # ) +
        # ggrepel::geom_text_repel(
        #     data = first_commits_bottom,
        #     aes(label = Repository, vjust = vjust,
        #         group = seq_len(nrow(first_commits_bottom))),
        #     size = 3,
        #     direction = "x",
        #     show.legend = FALSE
        # ) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_shape_manual(values = c(16, 1)) +
        ggplot2::scale_colour_manual(values = rev(pal)) +
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
            legend.position = "bottom"
        )
}

get_date <- function(frame_along, commits) {
    the_date <- dplyr::if_else(frame_along > min(commits$When),
                               frame_along, min(commits$When))
    the_date <- dplyr::if_else(the_date < max(commits$When), the_date,
                               max(commits$When))
    return(the_date)
}

get_days <- function(the_date) {
    lubridate::as.period(
        lubridate::as_datetime(the_date) - lubridate::as_datetime("2016-02-08")
    )@day
}

animate_commits <- function(commits_plot) {

    subtitle <- paste(
        "{sum(first_commits$When <= frame_along)} repositories with",
        "{sum(commits$When <= frame_along)} commits in",
        "{get_days(get_date(frame_along, commits))} days by",
        "{as.Date(get_date(frame_along, commits))}"
    )

    commits_plot +
        gganimate::transition_reveal(
            When,
            range = c(lubridate::as_datetime("2016-01-01"),
                      lubridate::as_datetime("2020-01-31"))
        ) +
        labs(subtitle = subtitle)
}
