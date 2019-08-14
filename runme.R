#############################################################################
# Complete script to plot clustered commits for a given set of repositories #
#############################################################################

# Dependencies required
#devtools::install_github("dgrtwo/gganimate")
#devtools::install_github("r-rust/gifski")
#install.packages("png")

# Define the methods needed

# TODO: Change your username here (e.g., GitHub user)
# TODO: Change date
get_commits <- function(path, users, distinct = TRUE, filter = TRUE, from = "2016-02-08") {
    dirs <- fs::dir_ls(path, type = "directory")

    message("Searching ", length(dirs), " directories...")

    commits <- purrr::map_dfr(dirs, function(dir) {
        message("Processing ", fs::path_file(dir), "...")
        if (git2r::in_repository(dir)) {
            commits_list <- git2r::commits(dir)
            if (length(commits_list) > 0) {
                dir_commits <- purrr::map_dfr(commits_list, function(commit) {
                    tibble::tibble(
                        SHA  = commit$sha,
                        Name = commit$author$name,
                        When = lubridate::as_datetime(commit$author$when$time)
                    )
                })
                dir_commits$Repository <- fs::path_file(dir)

                return(dir_commits)
            }
        }
    })

    message("Found ", nrow(commits), " commits")

    if (distinct) {
        message("Selecting distinct SHAs...")
        commits <- dplyr::distinct(commits, SHA, .keep_all = TRUE)
        message("Found ", nrow(commits), " distinct commits")
    }

    message("Filtering dates...")
    commits <- dplyr::filter(commits, When >= from)
    message("Found ", nrow(commits), " from ", from)

    if (filter) {
        message("Filtering names...")
        commits <- dplyr::filter(
            commits, Name %in% users
        )
        message("Found ", nrow(commits), " commits by me")
    }

    return(commits)
}

# TODO: Change the groups defined in your repositories.tsv file. Keep the order you want to see in the plot
# TODO: Same than above
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


do_it <- function(repository_directory, info_path, output_path, users) {
    #Get the commits of all the repositories located in this folder

    message('Searching users: ', users)
    # TODO: Change the path to the directory where your repositories are located
    commits <- get_commits(path=repository_directory, users=users)

    # Load my repositories.tsv that define the groups

    # TODO: Change the path to the file with grouping info as shown in the example
    repositories <- read.csv(file=info_path, header=TRUE, sep="\t")

    # Plot commits
    commits_plot <- plot_commits(commits, repositories)

    # Visualize the plot
    plot(commits_plot)

    # Get the animated one
    animate_commits(commits_plot)

    anim_save(output_path)
}

main <- function() {
    args = commandArgs(trailingOnly=TRUE)
    do_it(args[1], args[2], args[3], args[4:length(args)])
}

main()
