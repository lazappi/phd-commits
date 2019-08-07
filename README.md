# PhD commits

Functions for scraping git commits from repositories associated with a PhD (or
anything else) and plotting them. The result looks something like this.

![Commits GIF](phd-commits.gif)

## How to use

### 1. Get your commits

The `get_commits()` function takes a path and searches each of the
subdirectories. For each subdirectory that is a git repository it extracts
the git commits. The other arguments can be used to filter the commits to
a specific date or users. The output is a `tibble` with columns "SHA", "Name",
"When" and "Repository".

```r
commits <- get_commits("/path/to/repos")
```

If you have repositories in multiple locations you can combine them using
`dplyr::bind_rows()` and filter on the SHA to remove any duplicates.

```r
commits1 <- get_commits("/path/to/repos1")
commits2 <- get_commits("/path/to/repos2")

commits <- commits1 %>%
    dplyr::bind_rows(commits2) %>%
    dplyr::distinct(SHA, .keep_all = TRUE)
```

### 2. Categorise your repositories

Each repository needs a category and a type. Categories can be broad
projects/thesis chapters and I used Type to specify if the repository was
mostly code or writing. I did this by printing all the repositories using this
code:

```r
cat(paste(sort(unique(commits$Repository)), collapse = "\n"))
```

Then copying that list into Excel and categorising each repository. There is
probably a better/more R way to do it but this works. What you need at the end
is a second repository tibble with the columns "Repository", "Category" and
"Type".

### 3. Plot your commits

You can now pass these two tibbles to the `plot_commits()` function.

```r
commits_plot <- plot_commits(commits, repositories)
```

Change the other arguments to make sure you have the right category
order/labels, colours etc. The `label` parameter labels the first commit of
each repository with the (truncated) repository name.

### 4. Animate your commits

The last step is to animate everything using `animate_commits()`. This is
mostly pretty easy, the tricky part is getting the subtitle right.

```r
animate_commits(commits_plot)
```

Save your animation as a git using `gganimate::anim_save()`.

```r
anim_save("commits.gif")
```

## Notes

* Hopefully this is useful/interesting for some people.
* This code is not well tested, use at your own risk.
* I tried to make the functions as generic as possible but there may be some
  places where things specific to me are still there.
* I couldn't get any kind of enter tranistion to work for the points. I think
  it has something to do with having to give each point it's own group so they
  don't disappear. Solutions welcome.
* Thanks to everyone who wrote the packages I used and everyone who has posted
  tutorials/answered questions on the internet.
