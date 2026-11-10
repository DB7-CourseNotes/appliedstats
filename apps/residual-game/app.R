library(shiny)
library(bslib)

# Each LINE violation has one or more methods. A method has a plain-language
# description and an `apply` function that changes the simulated data `d`
# (columns x, mu, e). `s` is the strength (1 = easy, 0.5 = hard).
# To add a method, add an entry to the list for its letter.
violations <- list(
  L = list(
    list(
      desc = "Linearity is violated: the true mean is curved (a quadratic term), so a straight line does not fit the trend.",
      apply = function(d, s) {
        d$mu <- d$mu + s * 0.5 * (d$x - 5)^2
        d
      }
    )
  ),
  I = list(
    list(
      desc = "Independence is violated: each error carries over part of the previous error (AR(1) along observation order). The four plots do not show observation order, so this cannot be seen in them.",
      apply = function(d, s) {
        d$e <- as.numeric(stats::filter(d$e, 0.7 * s, method = "recursive"))
        d
      }
    )
  ),
  N = list(
    list(
      desc = "Normality is violated: the errors have heavy tails (t distribution with 2 df), so extreme residuals are more common than a normal distribution would give.",
      apply = function(d, s) {
        d$e <- (1 - s) * d$e + s * qt(pnorm(d$e), df = 2)
        d
      }
    ),
    list(
      desc = "Normality is violated: the errors are right-skewed (a centred log-normal shape).",
      apply = function(d, s) {
        d$e <- (1 - s) * d$e + s * (exp(d$e) - exp(0.5))
        d
      }
    )
  ),
  E = list(
    list(
      desc = "Equal variance is violated: the spread of the errors grows with x (a funnel shape).",
      apply = function(d, s) {
        d$e <- d$e * (1 + s * 0.3 * d$x)
        d
      }
    )
  )
)

line_letters <- c("L", "I", "N", "E")
full_names <- c(
  L = "Linearity",
  I = "Independent errors",
  N = "Normal residuals",
  E = "Equal variance"
)

difficulty <- list(
  Easy = list(n = 100, s = 2),
  Hard = list(n = 50, s = 1)
)

# Each letter is included with probability 1/2, so every subset of the four
# letters (including none) is equally likely.
make_round <- function(level = "Easy") {
  seed <- sample.int(10000, 1)
  set.seed(seed)
  cfg <- difficulty[[level]]
  truth <- line_letters[sample(c(TRUE, FALSE), 4, replace = TRUE)]

  d <- data.frame(x = runif(cfg$n, 0, 10))
  d$mu <- 1 + 2 * d$x
  d$e <- rnorm(cfg$n)

  descs <- character(0)
  for (l in truth) {
    methods <- violations[[l]]
    m <- methods[[sample(length(methods), 1)]]
    d <- m$apply(d, cfg$s)
    descs <- c(descs, setNames(m$desc, l))
  }
  d$y <- d$mu + d$e

  dat <- d[c("x", "y")]
  list(
    data = dat,
    fit = lm(y ~ x, data = dat),
    truth = truth,
    descs = descs,
    seed = seed
  )
}

# Independence cannot be judged from these plots, so it is not scored.
feedback_ui <- function(rd, picked) {
  scored <- setdiff(line_letters, "I")
  status <- sapply(scored, function(l) {
    is_true <- l %in% rd$truth
    is_picked <- l %in% picked
    if (is_true && is_picked) {
      "Correct: violated and selected"
    } else if (is_true) {
      "Missed: violated but not selected"
    } else if (is_picked) {
      "False alarm: selected but not violated"
    } else {
      "Correct: not violated and not selected"
    }
  })
  note <- if ("I" %in% picked) {
    "Independent errors cannot be judged from these plots, so it is not scored."
  }

  sim_text <- if (length(rd$descs) == 0) {
    "No violations were simulated. The data come from the base model: a straight-line mean with independent, normal errors of equal variance."
  } else {
    unname(rd$descs)
  }

  tagList(
    h5("Your answer"),
    tags$ul(lapply(scored, function(l) {
      tags$li(paste0(full_names[[l]], ": ", status[[l]]))
    })),
    if (!is.null(note)) p(note),
    h5("How the data were simulated"),
    tags$ul(lapply(sim_text, tags$li))
  )
}

ui <- page_sidebar(
  title = "Residual assumption game",
  sidebar = sidebar(
    checkboxGroupInput(
      "picked",
      "Which assumptions are violated?",
      choices = setNames(line_letters, paste0(line_letters, ": ", full_names[line_letters]))
    ),
    radioButtons("level", "Difficulty (applies to new dataset)", names(difficulty), inline = TRUE),
    actionButton("submit", "Submit", class = "btn-primary"),
    actionButton("new", "New dataset")
  ),
  card(
    card_header("Residual diagnostics"),
    plotOutput("diag", height = "600px")
  ),
  card(
    card_header("Feedback"),
    uiOutput("feedback")
  )
)

server <- function(input, output, session) {
  rd <- reactiveVal(make_round("Easy"))
  result <- reactiveVal(NULL)

  observeEvent(input$new, {
    rd(make_round(input$level))
    result(NULL)
  })

  observeEvent(input$submit, {
    result(as.character(input$picked))
  })

  output$diag <- renderPlot({
    op <- par(mfrow = c(2, 2))
    on.exit(par(op))
    plot(rd()$fit, which = c(1, 2, 3, 5))
  })

  output$feedback <- renderUI({
    picked <- result()
    req(picked)
    feedback_ui(rd(), picked)
  })
}

shinyApp(ui, server)
