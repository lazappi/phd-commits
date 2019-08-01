plot_commits <- function(commits, repositories) {
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

    ggplot2::ggplot(
        commits,
        ggplot2::aes(x = Category, y = When, colour = Category, shape = Type)
    ) +
        ggplot2::geom_jitter(width = 0.2, height = 0, alpha = 0.8, size = 2) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_shape_manual(values = c(16, 1)) +
        ggplot2::coord_flip() +
        ggplot2::labs(
            title = "Commits during my PhD!"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            legend.position = "bottom"
        )
}
