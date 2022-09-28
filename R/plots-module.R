# Plots module -----------------------------------------------------------------

PlotsUI <- function(id) {
  ns <- NS(id)
  div(
    id = id,
    style = "display: none;",
    width = "100%",
    div("Per pool stats",
        class = "display-2"
    ),
    div(
      class = "input-row-container",
      div(
        style = "margin-right: 10px; width: 40%;",
        selectizeInput(
          inputId = ns(id_list()$plot_section$inputs$data_type),
          choices = c("Original data"),
          label = "Data source",
          selected = "Original data",
          multiple = FALSE,
          width = "100%"
        )
      ),
      div(
        style = "width:55%;",
        selectizeInput(
          inputId = ns(id_list()$plot_section$inputs$pool_select),
          choices = c(),
          label = "Pool selection",
          multiple = FALSE,
          width = "100%"
        )
      )
    ),
    div(
      class = "input-row-container",
      div(
        style = "margin-right: 15px; width: 75%;",
        sliderInput(
          inputId = ns(id_list()$plot_section$inputs$seq_filter),
          label = "Sequence count threshold",
          min = 0,
          max = 1000,
          value = 0,
          step = 1,
          animate = TRUE,
          width = "100%"
        )
      ),
      div(
        style = "width: 20%;",
        numericInput(
          ns(id_list()$plot_section$inputs$seq_filter_in),
          label = "",
          min = 0,
          max = 1000,
          value = 0,
          step = 1,
          width = "100%"
        )
      )
    ),
    div(
      id = ns(id_list()$plot_section$inputs$plot_panel_1),
      style = "width:80%; margin-bottom: 15px;",
      class = "card",
      div(
        class = "card-body",
        h2(
          class = "card-title",
          "Total counts per pool"
        ),
        div(
          class = "plot-subcard",
          shinycustomloader::withLoader(plotly::plotlyOutput(
            ns(id_list()$plot_section$outputs$counts_plot_sc),
            width = "100%"
          ), type = "html", loader = "dnaspin")
        ),
        div(
          class = "plot-subcard",
            shinycustomloader::withLoader(plotly::plotlyOutput(
              ns(id_list()$plot_section$outputs$counts_plot_bm),
              width = "100%"
            ), type = "html",
            loader = "dnaspin")
        )
      )
    ),
    .generate_plots_card(ns, 2),
    .generate_plots_card(ns, 3)
  )
}

PlotsServer <- function(id, workflow, data, data_rec,
                        metadata, indep_sample_id,
                        project_id) {
  moduleServer(
    id,
    function(input, output, session) {
      pool_col <- ISAnalytics::association_file_columns(TRUE) %>%
        dplyr::filter(.data$tag == "vispa_concatenate") %>%
        dplyr::pull(.data$names)
      ## Joint datasets
      joint_data_meta <- reactive({
        if (is.null(data) || is.null(data_rec) || is.null(metadata())) {
          return(NULL)
        }
        list(
          original = data() %>%
            dplyr::left_join(metadata(), by = ISAnalytics::pcr_id_column()),
          rec = data_rec() %>%
            dplyr::left_join(metadata(), by = ISAnalytics::pcr_id_column())
        )
      })
      ## Dataset chosen between original and recalibrated if available
      chosen_dataset <- reactiveVal(NULL)
      ## Dataset filtered with threshold seqCount
      filtered_dataset <- reactiveVal(NULL)
      ## Updating ui elements
      data_type_options <- reactive({
        if (!is.null(data_rec())) {
          c("Recalibrated data", "Original data")
        } else {
          c("Original data")
        }
      })
      max_seq_count <- reactive({
        if (is.null(filtered_dataset())) {
          1000
        } else {
          max(filtered_dataset() %>%
                dplyr::filter(
                    .data[[pool_col]] == input[[
                      id_list()$plot_section$inputs$pool_select]]
                ) %>%
                dplyr::pull(.data$seqCount))
        }
      })
      observeEvent(data_type_options(), {
        updateSelectizeInput(session = session,
                             inputId = id_list()$plot_section$inputs$data_type,
                             choices = data_type_options(),
                             selected = data_type_options()[1])
      })
      observeEvent(max_seq_count(), {
        updateSliderInput(
          session = session,
          inputId = id_list()$plot_section$inputs$seq_filter,
          max = max_seq_count()
        )
        updateNumericInput(
          session = session,
          inputId = id_list()$plot_section$inputs$seq_filter_in,
          max = max_seq_count()
        )
      })
      observeEvent(
        input[[
          id_list()$plot_section$inputs$seq_filter_in
        ]], {
          updateSliderInput(
            session = session,
            inputId = id_list()$plot_section$inputs$seq_filter,
            value = input[[
              id_list()$plot_section$inputs$seq_filter_in
            ]]
          )
        }
      )
      observeEvent(metadata(), {
        if (!is.null(metadata())) {
          pools_av <- unique(metadata()[[pool_col]])
          updateSelectizeInput(
            session = session,
            inputId = id_list()$plot_section$inputs$pool_select,
            choices = pools_av,
            selected = pools_av[1])
        }
      })
      ## Dataset updates
      observeEvent(input[[
        id_list()$plot_section$inputs$data_type
      ]], {
        if (input[[id_list()$plot_section$inputs$data_type]] ==
            "Recalibrated data") {
          chosen_dataset(joint_data_meta()$rec)
        } else {
          chosen_dataset(joint_data_meta()$original)
        }
      })
      ## Changing chosen dataset updates filtered dataset
      observeEvent({
        chosen_dataset()
        input[[
          id_list()$plot_section$inputs$seq_filter
        ]]
      }, {
        if (is.null(chosen_dataset())) {
          filtered_dataset(NULL)
        } else {
          filtered_dataset(
            chosen_dataset() %>%
              dplyr::filter(.data$seqCount >= input[[
                id_list()$plot_section$inputs$seq_filter
              ]])
          )
        }
      })
      ## Calculating group counts
      group_counts <- reactiveVal(NULL)
      sc_ratios <- reactiveVal(NULL)
      repl_ratios <- reactiveVal(NULL)
      is_shared <- reactiveVal(NULL)
      observeEvent({
        filtered_dataset()
      }, {
        req(filtered_dataset())
        withProgress({
          counts <- .get_counts(filtered_dataset(),
                                indep_sample_id(),
                                pool_col)
          group_counts(counts)
          incProgress(1/5, detail = "2 - Calculating sc ratios")

          m_cols <- colnames(data())
          r_sc <- shared_IS_ratio(af = metadata(),
                                  matrix = filtered_dataset() %>%
                                    dplyr::filter(.data[[pool_col]] == input[[
                                      id_list()$plot_section$inputs$pool_select
                                    ]]) %>%
                                    dplyr::select(dplyr::all_of(
                                      m_cols
                                    )),
                                  subject_col = indep_sample_id(),
                                  amp_col = ISAnalytics::pcr_id_column(),
                                  value_col = "seqCount",
                                  ctrl = "CEM37") # must be dynamic
          r_sc_is <- shared_IS_ratio_byIS(af = metadata(),
                                          matrix = filtered_dataset() %>%
                                            dplyr::filter(.data[[pool_col]] == input[[
                                              id_list()$plot_section$inputs$pool_select
                                            ]]) %>%
                                            dplyr::select(dplyr::all_of(
                                              m_cols
                                            )),
                                          subject_col = indep_sample_id(),
                                          amp_col = ISAnalytics::pcr_id_column(),
                                          value_col = "seqCount",
                                          ctrl = "CEM37")  # must be dynamic
          sc_ratios(list(by_sample = r_sc, by_is = r_sc_is))

          incProgress(1/5, detail = "3 - Calculating replicate ratios")
          r_rep <- replicates_IS_ratio(
            af = metadata(),
            matrix = filtered_dataset() %>%
              dplyr::filter(.data[[pool_col]] == input[[
                id_list()$plot_section$inputs$pool_select
              ]]) %>%
              dplyr::select(dplyr::all_of(
                m_cols
              )),
            subject_col = indep_sample_id(),
            amp_col = ISAnalytics::pcr_id_column(),
            value_col = "seqCount",
            ctrl = "CEM37"
          )
          r_rep_is <- replicates_IS_ratio_byIS(
            af = metadata(),
            matrix = filtered_dataset() %>%
              dplyr::filter(.data[[pool_col]] == input[[
                id_list()$plot_section$inputs$pool_select
              ]]) %>%
              dplyr::select(dplyr::all_of(
                m_cols
              )),
            subject_col = indep_sample_id(),
            amp_col = ISAnalytics::pcr_id_column(),
            value_col = "seqCount",
            ctrl = "CEM37"
          )
          repl_ratios(list(by_sample = r_rep, by_is = r_rep_is))

          incProgress(1/5, detail = "4 - Calculating sharing")
          shared_is <- ISAnalytics::is_sharing(
            filtered_dataset() %>%
              dplyr::filter(.data[[pool_col]] == input[[
                id_list()$plot_section$inputs$pool_select
              ]]),
            group_key = indep_sample_id(), minimal = FALSE,
            include_self_comp = TRUE, n_comp = 2
          )
          is_shared(shared_is)

          incProgress(1/5, detail = "", message = "Done!")
        },
        message = "Performing calculations...",
        detail = "1 - Calculating group counts")
      })
      observeEvent({
        group_counts()
        input[[
          id_list()$plot_section$inputs$pool_select
        ]]
      }, {
        plots <- .get_counts_plots(counts_single = group_counts()$single,
                                   counts_sample = group_counts()$sample,
                                   ind_sample_id = indep_sample_id(),
                                   pool_col = pool_col,
                                   pool_id = input[[
                                     id_list()$plot_section$inputs$pool_select
                                     ]],
                                   proj_name = project_id(),
                                   threshold = input[[
                                     id_list()$plot_section$inputs$seq_filter
                                   ]])
        output[[
          id_list()$plot_section$outputs$counts_plot_sc
        ]] <- plotly::renderPlotly(plots$sc_plot)
        output[[
          id_list()$plot_section$outputs$counts_plot_bm
        ]] <- plotly::renderPlotly(plots$bm_plot)
      })
      observeEvent({
        sc_ratios()
        repl_ratios()
      }, {
        output[[
          id_list()$plot_section$outputs$sc_ratio_control_all
        ]] <- renderText(sc_ratios()$by_sample %>%
          dplyr::filter(.data$Sample == "All samples",
                        .data$IS_Source == "Control") %>%
          dplyr::pull(.data$CEM37) %>%
          round(digits = 2) %>%
          format(big.mark = ",") %>%
          as.character()
        )
        output[[
          id_list()$plot_section$outputs$rc_ratio_control_all
        ]] <- renderText(repl_ratios()$by_sample %>%
                           dplyr::filter(.data$Sample == "All samples",
                                         .data$IS_Source == "Control") %>%
                           dplyr::pull(.data$CEM37) %>%
                           round(digits = 2) %>%
                           format(big.mark = ",") %>%
                           as.character()
        )
        ## HEATMAPS
        is_shared_heatmap <- plotly::plot_ly(
          x = is_shared()$g1,
          y = is_shared()$g2,
          z = is_shared()$shared,
          type = "heatmap"
        )
        output[[
          id_list()$plot_section$outputs$shared_is_heatmap_control
        ]] <- plotly::renderPlotly(is_shared_heatmap)
      })
    })
}
