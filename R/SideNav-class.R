
SideNav <- R6::R6Class(
  classname = "SideNav",
  private = list(
    .init_steps = function() {
      one <- list(
        name = "Data import",
        index = 1,
        section_id = ids()$data_import$section_id,
        node_id = ids()$side_bar$inputs$side_nav_1,
        status = "active",
        processed = FALSE,
        skipped = FALSE
      )
      two <- list(
        name = "Recalibration",
        index = 2,
        section_id = ids()$recalibration$section_id,
        node_id = ids()$side_bar$inputs$side_nav_2,
        status = "inactive",
        processed = FALSE,
        skipped = FALSE
      )
      three <- list(
        name = "Stats and plots",
        index = 3,
        section_id = ids()$plot_section$section_id,
        node_id = ids()$side_bar$inputs$side_nav_3,
        status = "inactive",
        processed = FALSE,
        skipped = FALSE
      )
      four <- list(
        name = "Save results",
        index = 4,
        section_id = ids()$saving_section$section_id,
        node_id = ids()$side_bar$inputs$side_nav_4,
        status = "inactive",
        processed = FALSE,
        skipped = FALSE
      )
      return(list(one, two, three, four))
    }
  ),
  public = list(
    steps = NULL,
    nav_id = NULL,
    status_flag = NULL,
    current_active = 1,
    initialize = function(nav_id, status_flag) {
      self$steps <- private$.init_steps()
      self$nav_id = nav_id
      self$status_flag = status_flag
      for (step in self$steps) {
        golem::invoke_js("addSideNavNode",
                         list(
                           nodeIndex = step$index,
                           svgId = self$nav_id,
                           uniqueNodeId = step$node_id
                         ))
      }
    },
    go_to_next = function(skip = FALSE) {
      current_page_id <- self$steps[[self$current_active]]$section_id
      next_page_id <- self$steps[[self$current_active + 1]]$section_id
      # Current active node becomes inactive
      self$steps[[self$current_active]]$status <- if (skip) {
        "skipped"
      } else {
        "inactive"
      }
      # Processed becomes true for current node
      self$steps[[self$current_active]]$processed <- TRUE
      # Next node becomes active
      self$steps[[self$current_active + 1]]$status <- "active"
      self$current_active <- self$current_active + 1

      # Trigger page slide
      .page_scroll_next(current_page_id = current_page_id,
                        next_page_id = next_page_id)

      gargoyle::trigger(self$status_flag)
    },
    go_to_previous = function() {
      current_page_id <- self$steps[[self$current_active]]$section_id
      prev_page_id <- self$steps[[self$current_active - 1]]$section_id
      # Current active node becomes inactive
      self$steps[[self$current_active]]$status <- "inactive"
      # Next node becomes active
      self$steps[[self$current_active - 1]]$status <- "active"
      self$current_active <- self$current_active - 1

      # Trigger page slide
      .page_scroll_next(current_page_id, prev_page_id)

      gargoyle::trigger(self$status_flag)
    }
  )
)
