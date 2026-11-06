# OUCRU Introduction to Medical Statistics

Source for the website of the OUCRU *Introduction to Medical Statistics* course,
built with [Quarto](https://quarto.org).

## For learners

Course website: <https://oucru-biostats.github.io/Introduction-to-Medical-Statistics/>

## For instructors

### Repository layout

| Path | What it is |
|---|---|
| `index.qmd` | Home page: welcome, logistics, contributors, pre-course setup |
| `_data/schedule.yml` | **The course schedule**: days, sessions, topics, and links to slides, exercises, answers and QR codes |
| `_filters/course-schedule.lua` | Turns `schedule.yml` into the "Course Topics" section of the home page |
| `unlock.html` | Popup for the "Click to unlock" buttons (QR codes, optionally answers) |
| `others/` | Other pages: course videos, extra R resources, pre-course setup |
| `slides/day_N/` | Lecture slides (PDF) |
| `handouts/day_N/` | Exercises, answers and R scripts, already rendered (HTML/PDF/R) |
| `handouts/` | Course-wide handouts, agenda, dictionary, QR codes |
| `_extensions/` | Quarto extensions used when authoring the exercises (webr, etc.) |
| `custom_styles.css` | Site-wide CSS |
| `_quarto.yml` | Site configuration (navbar, footer, comments, analytics) |

### Updating the course materials

1. Put the file in the right folder, e.g. `slides/day_3/` or `handouts/day_3/`.
2. Link it from **`_data/schedule.yml`**, not `index.qmd`. The comments at
   the top of that file list every field. Paths are relative to the
   repository root.
3. Preview with `quarto preview`. Any path in `schedule.yml` that does
   not exist is reported as a `[course-schedule] file not found` warning.

To hide a session's answers until the end of a session, add
`answers-code: "<code>"` to that session. QR-code buttons use the
top-level `unlock-code`. These codes can be read from the page source, so
they only keep links out of sight. They do not restrict access.

### Exercises

Exercises are authored as Quarto documents elsewhere (with the `webr` and
`assign` extensions in `_extensions/`) and committed here **already rendered**.
The website build does not run any R code.

For local R work, use `renv`:

- start a session with `renv::restore()`
- run `renv::snapshot()` before committing changes to package usage

### Preview and publish

- `quarto preview`: live preview that re-renders on save
- `quarto render`: one-off build into `_render/`
- **Publish** by pushing to `main`. GitHub Actions renders the site and
  deploys it to the `gh-pages` branch. Avoid `quarto publish gh-pages`
  from your own machine.

Page-level YAML headers override the project defaults in `_quarto.yml`.
