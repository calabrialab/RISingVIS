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
      style = "align-items: center;",
      div(
        style = "margin-right: 15px; width: 55%;",
        sliderInput(
          inputId = ns(id_list()$plot_section$inputs$seq_filter),
          label = "Sequence count threshold",
          min = 0,
          max = 1000,
          value = 0,
          step = 1,
          animate = FALSE,
          width = "100%"
        )
      ),
      div(
        style = "width: 20%; margin-right: 15px;",
        numericInput(
          ns(id_list()$plot_section$inputs$seq_filter_in),
          label = "",
          min = 0,
          max = 1000,
          value = 0,
          step = 1,
          width = "100%"
        )
      ),
      div(
        style = "width: 15%;",
        tagAppendAttributes(
          actionButton(
            inputId = ns(id_list()$plot_section$inputs$recompute_btn),
            label = "Calculate"
          ),
          style = "font-size: 90%;"
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns(id_list()$plot_section$inputs$plots_panels),
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
        div(
          style = "width:80%; margin-bottom: 15px;",
          class = "card",
          div(
            class = "card-body",
            h2(
              class = "card-title",
              "Pool overview stats"
            ),
            div(
              class = "plot-subcard",
              .generate_hm_card_content(ns, type = "overall")
            )
          )
        ),
        .generate_plots_card(ns, 2),
        .generate_plots_card(ns, 3),
        div(
          align = "center",
          style = "width: 80%;",
          tagAppendAttributes(
            actionButton(
              inputId = ns(id_list()$plot_section$inputs$next_btn),
              label = "NEXT"
            ),
            class = "btn btn-primary btn-lg",
            style = paste("margin-top: 10px;")
          )
        )
      )
    )
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
      ## Associating uuids
      uuids <- reactiveValues(
        metadata = NULL,
        data = NULL,
        data_rec = NULL
      )
      observeEvent(metadata(), {
        uuids$metadata <- if (is.null(metadata())) {
          NULL
        } else {
          uuid::UUIDgenerate()
        }
      })
      observeEvent(data(), {
        uuids$data <- if (is.null(data())) {
          NULL
        } else {
          uuid::UUIDgenerate()
        }
      })
      observeEvent(data_rec(), {
        uuids$data <- if (is.null(data_rec())) {
          NULL
        } else {
          uuid::UUIDgenerate()
        }
      })

      ## Updating ui elements ----
      data_type_options <- reactive({
        if (!is.null(data_rec())) {
          c("Recalibrated data", "Original data")
        } else {
          c("Original data")
        }
      })
      data_type_convert <- function(type) {
        if (type == "Original data") {
          return("original")
        } else {
          return("rec")
        }
      }
      observeEvent(data_type_options(), {
        updateSelectizeInput(session = session,
                             inputId = id_list()$plot_section$inputs$data_type,
                             choices = data_type_options(),
                             selected = data_type_options()[1])
      })
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
      max_seq_count <- reactive({
        req(input[[id_list()$plot_section$inputs$pool_select]])
        sel_df <- NULL
        if (input[[
          id_list()$plot_section$inputs$data_type]] == "Original data") {
          req(data())
          sel_df <- data() %>%
            dplyr::left_join(metadata(), by = ISAnalytics::pcr_id_column()) %>%
            dplyr::filter(.data[[pool_col]] == input[[
              id_list()$plot_section$inputs$pool_select]])
        } else {
          req(data_rec())
          sel_df <- data_rec() %>%
            dplyr::left_join(metadata(), by = ISAnalytics::pcr_id_column()) %>%
            dplyr::filter(.data[[pool_col]] == input[[
              id_list()$plot_section$inputs$pool_select]])
        }
        return(max(sel_df$seqCount, na.rm = TRUE))
      }) %>%
        bindCache(
          input[[id_list()$plot_section$inputs$data_type]],
          input[[id_list()$plot_section$inputs$pool_select]],
          uuids$metadata,
          uuids$data,
          uuids$data_rec
        )
      observeEvent(c(
        metadata(),
        data(),
        data_rec(),
        input[[id_list()$plot_section$inputs$pool_select]]
      ), {
        req(metadata())
        req(data())
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
          if (!is.na(input[[
            id_list()$plot_section$inputs$seq_filter_in
          ]]) && input[[
            id_list()$plot_section$inputs$seq_filter_in
          ]] != input[[id_list()$plot_section$inputs$seq_filter]]) {
            updateSliderInput(
              session = session,
              inputId = id_list()$plot_section$inputs$seq_filter,
              value = input[[
                id_list()$plot_section$inputs$seq_filter_in
              ]]
            )
          }
        }
      )
      observeEvent(
        input[[
          id_list()$plot_section$inputs$seq_filter
        ]], {
          updateNumericInput(
            session = session,
            inputId = id_list()$plot_section$inputs$seq_filter_in,
            value = input[[
              id_list()$plot_section$inputs$seq_filter
            ]]
          )
        }
      )

      ## Performing calculations ----
      datatype <- reactive({
        input[[id_list()$plot_section$inputs$data_type]]
      }) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )
      chosen_pool <- reactive({
        input[[id_list()$plot_section$inputs$pool_select]]
      }) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )
      applied_filter <- reactive({
        input[[id_list()$plot_section$inputs$seq_filter]]
      }) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )
      joint_data_meta <- reactive({
        req(metadata())
        req(data())
        req(datatype())
        req(chosen_pool())
        if (datatype() == "Original data") {
          jnt <- data() %>%
            dplyr::left_join(metadata(), by = ISAnalytics::pcr_id_column()) %>%
            dplyr::filter(.data[[pool_col]] == chosen_pool())
        } else {
          req(data_rec())
          jnt <- data_rec() %>%
            dplyr::left_join(metadata(), by = ISAnalytics::pcr_id_column()) %>%
            dplyr::filter(.data[[pool_col]] == chosen_pool())
        }
        uuids$joint_dataset <- uuid::UUIDgenerate()
        return(jnt)
      }) %>%
        bindCache(
          datatype(),
          chosen_pool(),
          uuids$metadata,
          uuids$data,
          uuids$data_rec
        ) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )

      filtered_dataset <- reactive({
        req(joint_data_meta())
        req(applied_filter())
        uuids$filtered_dataset <- uuid::UUIDgenerate()
        joint_data_meta() %>%
          dplyr::filter(.data$seqCount >= applied_filter())
      }) %>%
        bindCache(
          datatype(),
          chosen_pool(),
          uuids$joint_dataset,
          applied_filter()
        ) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )

      ### Values sum for samples sc and raw reads ----
      group_counts <- reactive({
        .get_counts(filtered_dataset(), indep_sample_id(), pool_col)
      }) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )

      ### Shared IS ----
      is_shared <- reactive({
        .get_sharing(filtered_dataset(),
                     indep_sample_id(),
                     joint_data_meta() %>%
                       dplyr::select(dplyr::all_of(indep_sample_id())) %>%
                       dplyr::distinct() %>%
                       tidyr::unite(col = "id") %>%
                       dplyr::pull(.data$id))
      }) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )

      ### Shared IS stats and heatmaps ----
      hm_to_show <- reactiveValues(
        overall = NULL,
        control = NULL,
        other = NULL
      )
      shared_is_desc <- paste(
        "The heatmap shows the number of shared distinct integration sites",
        "among samples. Hover on cells for detail."
      )
      shared_is_seq_desc <- paste(
        "The heatmap shows the total sequence count for the shared",
        "integration sites between the samples.",
        "Percentages shown are relative to the value of total seq count",
        "in samples on x axis.",
        "Hover for detail."
      )
      shared_is_rep_desc <- paste(
        "The heatmap shows the percentage of PCR replicates in which",
        "shared IS where tracked. Percentages shown are referred to the total",
        "number of replicates of the samples on x axis."
      )
      #### Overall ----
      is_shared_counts_overall <- reactive({
        req(is_shared())
        .get_is_counts_with_plots(
        type = "overall", filtered_dataset = filtered_dataset(),
        is_shared = is_shared(), indep_sample_id = indep_sample_id(),
        chosen_pool = chosen_pool(), pool_col = pool_col, input = input,
        uuids = uuids
      )}) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$rbtn_1]]
        )
      abs_shared_is_hm_overall <- reactive({
        req(is_shared())
        .get_heatmap(
          is_shared()$overall,
          on_x = "g1",
          on_y = "g2",
          value = "shared",
          colorscale = viridisLite::inferno(256)
        )
      }) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )
      hm_to_show$overall <- reactive({
        if (input[[id_list()$plot_section$inputs$rbtn_1]] == "Shared IS") {
          abs_shared_is_hm_overall()
        } else if (input[[
          id_list()$plot_section$inputs$rbtn_1]] == "Shared IS seq count") {
          req(is_shared_counts_overall())
          is_shared_counts_overall()$sc_r_hmps$sc
        } else {
          req(is_shared_counts_overall())
          is_shared_counts_overall()$sc_r_hmps$rep
        }
      })
      overall_desc <- reactive({
        if (input[[
          id_list()$plot_section$inputs$rbtn_1]] == "Shared IS") {
          shared_is_desc
        } else if (input[[
          id_list()$plot_section$inputs$rbtn_1]] == "Shared IS seq count") {
          shared_is_seq_desc
        } else {
          shared_is_rep_desc
        }
      })
      #### Control to sample contamination ----
      is_shared_counts_control <- reactive({
        req(is_shared())
        .get_is_counts_with_plots(
          type = "control", filtered_dataset = filtered_dataset(),
          is_shared = is_shared(), indep_sample_id = indep_sample_id(),
          chosen_pool = chosen_pool(), pool_col = pool_col, input = input,
          uuids = uuids
        )}) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$rbtn_2]]
        )
      abs_shared_is_hm_control <- reactive({
        req(is_shared())
        .get_heatmap(
          is_shared()$control,
          on_x = "g1",
          on_y = "g2",
          value = "shared",
          colorscale = viridisLite::inferno(256)
        )
      }) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )
      hm_to_show$control <- reactive({
        if (input[[id_list()$plot_section$inputs$rbtn_2]] == "Shared IS") {
          abs_shared_is_hm_control()
        } else if (input[[
          id_list()$plot_section$inputs$rbtn_2]] == "Shared IS seq count") {
          req(is_shared_counts_control())
          is_shared_counts_control()$sc_r_hmps$sc
        } else {
          req(is_shared_counts_control())
          is_shared_counts_control()$sc_r_hmps$rep
        }
      })
      control_desc <- reactive({
        if (input[[
          id_list()$plot_section$inputs$rbtn_2]] == "Shared IS") {
          shared_is_desc
        } else if (input[[
          id_list()$plot_section$inputs$rbtn_2]] == "Shared IS seq count") {
          shared_is_seq_desc
        } else {
          shared_is_rep_desc
        }
      })
      #### Samples to control contamination ----
      is_shared_counts_other <- reactive({
        req(is_shared())
        .get_is_counts_with_plots(
          type = "other", filtered_dataset = filtered_dataset(),
          is_shared = is_shared(), indep_sample_id = indep_sample_id(),
          chosen_pool = chosen_pool(), pool_col = pool_col, input = input,
          uuids = uuids
        )}) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$rbtn_3]]
        )
      abs_shared_is_hm_other <- reactive({
        req(is_shared())
        .get_heatmap(
          is_shared()$other,
          on_x = "g1",
          on_y = "g2",
          value = "shared",
          colorscale = viridisLite::inferno(256)
        )
      }) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )
      hm_to_show$other <- reactive({
        if (input[[id_list()$plot_section$inputs$rbtn_3]] == "Shared IS") {
          abs_shared_is_hm_other()
        } else if (input[[
          id_list()$plot_section$inputs$rbtn_3]] == "Shared IS seq count") {
          req(is_shared_counts_other())
          is_shared_counts_other()$sc_r_hmps$sc
        } else {
          req(is_shared_counts_other())
          is_shared_counts_other()$sc_r_hmps$rep
        }
      })
      other_desc <- reactive({
        if (input[[
          id_list()$plot_section$inputs$rbtn_3]] == "Shared IS") {
          shared_is_desc
        } else if (input[[
          id_list()$plot_section$inputs$rbtn_3]] == "Shared IS seq count") {
          shared_is_seq_desc
        } else {
          shared_is_rep_desc
        }
      })

      ### Ratios ----
      #### Per sample ----
      control_sc_r_barplot_persample <- reactiveVal(NULL)
      samples_sc_r_barplot_persample <- reactiveVal(NULL)
      control_sc_r_table_persample <- reactiveVal(NULL)
      samples_sc_r_table_persample <- reactiveVal(NULL)

      control_rep_r_barplot_persample <- reactiveVal(NULL)
      samples_rep_r_barplot_persample <- reactiveVal(NULL)
      control_rep_r_table_persample <- reactiveVal(NULL)
      samples_rep_r_table_persample <- reactiveVal(NULL)

      quiet_shared_IS_ratio <- purrr::quietly(shared_IS_ratio)
      quiet_replicates_IS_ratio <- purrr::quietly(replicates_IS_ratio)
      sc_ratio_sample <- reactive({
        req(filtered_dataset())
        quiet_shared_IS_ratio(
          af = metadata(),
          matrix = filtered_dataset() %>%
            dplyr::select(dplyr::all_of(colnames(data()))),
          subject_col = indep_sample_id(),
          amp_col = ISAnalytics::pcr_id_column(),
          value_col = "seqCount", ctrl = "CEM37"
        )$result
      }) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )
      rep_ratio_sample <- reactive({
        quiet_replicates_IS_ratio(
          af = filtered_dataset() %>%
            dplyr::select(dplyr::all_of(colnames(metadata()))) %>%
            dplyr::distinct(),
          matrix = filtered_dataset() %>%
            dplyr::select(dplyr::all_of(colnames(data()))),
          subject_col = indep_sample_id(),
          amp_col = ISAnalytics::pcr_id_column(),
          value_col = "seqCount", ctrl = "CEM37"
        )$result
      }) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )
      samples_ratios <- reactive({
        prog <- progressr::progressor(steps = 2)
        progressr::withProgressShiny({
          sc <- sc_ratio_sample()
          prog()
          rep  <- rep_ratio_sample()
          prog()
          return(list(sc = sc, rep = rep))
        }, message = "Calculating ratios per sample...")
      })

      observeEvent(samples_ratios(), {
        req(samples_ratios())
        # Manage sequence count ratios
        # if IS_Source contains only NA it means there are no shared IS
        # overall (in any of the samples)
        control_data <- samples_ratios()$sc %>%
          dplyr::filter(.data$IS_Source %in% c("Controls", NA))
        samples_data <- samples_ratios()$sc %>%
          dplyr::filter(.data$IS_Source %in% c("Samples", NA))
        if (all(is.na(control_data %>%
                      dplyr::pull(.data$IS_Source)))) {
          output[[
            id_list()$plot_section$outputs$sc_ratio_control_all
          ]] <- renderText("NA")
          control_sc_r_barplot_persample(NULL)
        } else {
          ## Text to display
          output[[
            id_list()$plot_section$outputs$sc_ratio_control_all
          ]] <- renderText({
            ratio_all <- control_data %>%
              dplyr::filter(.data$Sample == "All_Samples",
                            .data$IS_Source == "Controls") %>%
              dplyr::pull(.data$Ratio_CEM37)
            if (!all(is.na(ratio_all))) {
              ratio_all %>%
                round(digits = 2) %>%
                format(big.mark = ",") %>%
                as.character()
            } else {
              "NA"
            }
          }
          )
          ## Barplots
          ratio_sc_control_bp <- .get_ratio_barplot(
            samples_ratios()$sc,
            type = "sc",
            source = "Controls"
          )
          control_sc_r_barplot_persample(ratio_sc_control_bp)
        }
        if (all(is.na(samples_data %>%
                      dplyr::pull(.data$IS_Source)))) {
          output[[
            id_list()$plot_section$outputs$sc_ratio_sample_all
          ]] <- renderText("NA")
          samples_sc_r_barplot_persample(NULL)
        } else {
          ## Text to display
          output[[
            id_list()$plot_section$outputs$sc_ratio_sample_all
          ]] <- renderText({
            ratio_all <- samples_data %>%
              dplyr::filter(.data$Sample == "All_Samples",
                            .data$IS_Source == "Samples") %>%
              dplyr::pull(.data$Ratio_CEM37)
            if (!all(is.na(ratio_all))) {
              ratio_all %>%
                round(digits = 2) %>%
                format(big.mark = ",") %>%
                as.character()
            } else {
              "NA"
            }
          }
          )
          ## Barplots
          ratio_sc_samples_bp <- .get_ratio_barplot(
            samples_ratios()$sc,
            type = "sc",
            source = "Samples"
          )
          samples_sc_r_barplot_persample(ratio_sc_samples_bp)
        }
        ## Tables
        tbl_sc_c <- .render_ratio_tbl(
          control_data,
          session$ns(id_list()$plot_section$outputs$ratio_tbl_sc_control))
        tbl_sc_s <- .render_ratio_tbl(
          samples_data,
          session$ns(id_list()$plot_section$outputs$ratio_tbl_sc_samples))
        control_sc_r_table_persample(tbl_sc_c)
        samples_sc_r_table_persample(tbl_sc_s)
      })

      observeEvent(samples_ratios(), {
        req(samples_ratios())
        # Manage sequence count ratios
        # if IS_Source contains only NA it means there are no shared IS
        # overall (in any of the samples)
        control_data <- samples_ratios()$rep %>%
          dplyr::filter(.data$IS_Source %in% c("Controls", NA))
        samples_data <- samples_ratios()$rep %>%
          dplyr::filter(.data$IS_Source %in% c("Samples", NA))
        if (all(is.na(control_data %>%
                      dplyr::pull(.data$IS_Source)))) {
          output[[
            id_list()$plot_section$outputs$rc_ratio_control_all
          ]] <- renderText("NA")
          control_rep_r_barplot_persample(NULL)
        } else {
          ## Text to display
          output[[
            id_list()$plot_section$outputs$rc_ratio_control_all
          ]] <- renderText({
            ratio_all <- control_data %>%
              dplyr::filter(.data$Sample == "All_Samples",
                            .data$IS_Source == "Controls") %>%
              dplyr::pull(.data$Ratio_CEM37)
            if (!all(is.na(ratio_all))) {
              ratio_all %>%
                round(digits = 2) %>%
                format(big.mark = ",") %>%
                as.character()
            } else {
              "NA"
            }
          }
          )
          ## Barplots
          ratio_rc_control_bp <- .get_ratio_barplot(
            samples_ratios()$rep,
            type = "rep",
            source = "Controls"
          )
          control_rep_r_barplot_persample(ratio_rc_control_bp)
        }
        if (all(is.na(samples_data %>%
                      dplyr::pull(.data$IS_Source)))) {
          output[[
            id_list()$plot_section$outputs$rc_ratio_sample_all
          ]] <- renderText("NA")
          samples_rep_r_barplot_persample(NULL)
        } else {
          ## Text to display
          output[[
            id_list()$plot_section$outputs$rc_ratio_sample_all
          ]] <- renderText({
            ratio_all <- samples_data %>%
              dplyr::filter(.data$Sample == "All_Samples",
                            .data$IS_Source == "Samples") %>%
              dplyr::pull(.data$Ratio_CEM37)
            if (!all(is.na(ratio_all))) {
              ratio_all %>%
                round(digits = 2) %>%
                format(big.mark = ",") %>%
                as.character()
            } else {
              "NA"
            }
          }
          )
          ## Barplots
          ratio_rc_samples_bp <- .get_ratio_barplot(
            samples_ratios()$rep,
            type = "rep",
            source = "Samples"
          )
          samples_rep_r_barplot_persample(ratio_rc_samples_bp)
        }
        ## Tables
        tbl_rep_c <- .render_ratio_tbl(
          control_data,
          session$ns(id_list()$plot_section$outputs$ratio_tbl_rc_control))
        tbl_rep_s <- .render_ratio_tbl(
          samples_data,
          session$ns(id_list()$plot_section$outputs$ratio_tbl_rc_samples))
        control_rep_r_table_persample(tbl_rep_c)
        samples_rep_r_table_persample(tbl_rep_s)
      })

      #### Per IS ----
      quiet_shared_IS_ratio_byIS <- purrr::quietly(shared_IS_ratio_byIS)
      quiet_replicates_IS_ratio_byIS <- purrr::quietly(replicates_IS_ratio_byIS)
      control_ratio_contour_plot <- reactiveVal(NULL)
      samples_ratio_contour_plot <- reactiveVal(NULL)
      sc_ratio_perIs <- reactive({
        req(filtered_dataset())
        quiet_shared_IS_ratio_byIS(
          af = metadata(),
          matrix = filtered_dataset() %>%
            dplyr::select(dplyr::all_of(colnames(data()))),
          subject_col = indep_sample_id(),
          amp_col = ISAnalytics::pcr_id_column(),
          value_col = "seqCount", ctrl = "CEM37"
        )$result
      }) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )
      rep_ratio_perIs <- reactive({
        req(filtered_dataset())
        quiet_replicates_IS_ratio_byIS(
          af = filtered_dataset() %>%
            dplyr::select(dplyr::all_of(colnames(metadata()))) %>%
            dplyr::distinct(),
          matrix = filtered_dataset() %>%
            dplyr::select(dplyr::all_of(colnames(data()))),
          subject_col = indep_sample_id(),
          amp_col = ISAnalytics::pcr_id_column(),
          value_col = "seqCount", ctrl = "CEM37"
        )$result
      }) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )
      is_ratios <- reactive({
        prog <- progressr::progressor(steps = 2)
        progressr::withProgressShiny({
          sc <- sc_ratio_perIs()
          prog()
          rep  <- rep_ratio_perIs()
          prog()
          all <- if (all(is.na(sc$IS_Source))) {
            sc %>%
              dplyr::rename(Ratio_CEM37_sc = "Ratio_CEM37") %>%
              dplyr::select(-dplyr::all_of("IS_Source")) %>%
              dplyr::inner_join(rep, by = c("Sample")) %>%
              dplyr::rename(Ratio_CEM37_rep = "Ratio_CEM37") %>%
              dplyr::mutate(Overall_Ratio = NA_real_)
          } else {
            sc %>%
              dplyr::rename(Ratio_CEM37_sc = "Ratio_CEM37") %>%
              dplyr::inner_join(rep, by = c(ISAnalytics::mandatory_IS_vars(),
                                            "Sample", "IS_Source")) %>%
              dplyr::rename(Ratio_CEM37_rep = "Ratio_CEM37") %>%
              dplyr::mutate(sc_rep_ratio = Ratio_CEM37_sc / Ratio_CEM37_rep)
          }
          return(all)
        }, message = "Calculating ratios per IS...")
      })

      observeEvent(is_ratios(), {
        req(is_ratios())
        if (all(is.na(is_ratios()$IS_Source))) {
          # if IS_Source contains only NA it means there are no shared IS
          # overall (in any of the samples)
          # Plots don't show
          control_ratio_contour_plot(NULL)
          samples_ratio_contour_plot(NULL)
          return()
        }
        control_contour <- .get_ratio_contour(is_ratios(), "Controls")
        samples_contour <- .get_ratio_contour(is_ratios(), "Samples")
        control_ratio_contour_plot(control_contour)
        samples_ratio_contour_plot(samples_contour)
      })

      ## Trace plots ----
      count_plots <- reactive({
        .get_counts_plots(
          counts_single = group_counts()$single,
          counts_sample = group_counts()$sample,
          ind_sample_id = indep_sample_id(),
          pool_col = pool_col,
          chosen_pool = chosen_pool(),
          proj_name = project_id(),
          threshold = applied_filter()
        )
      }) %>%
        bindCache(uuids$filtered_dataset) %>%
        bindEvent(
          input[[id_list()$plot_section$inputs$recompute_btn]]
        )

      output[[
        id_list()$plot_section$outputs$counts_plot_sc
      ]] <- plotly::renderPlotly(count_plots()$sc_plot)
      output[[
        id_list()$plot_section$outputs$counts_plot_bm
      ]] <- plotly::renderPlotly(count_plots()$bm_plot)

      output[[
        id_list()$plot_section$outputs$overall_is_heatmap
      ]] <- plotly::renderPlotly(hm_to_show$overall())
      output[[
        id_list()$plot_section$outputs$hm_desc_1
      ]] <- renderText(overall_desc())
      output[[
        id_list()$plot_section$outputs$control_is_heatmap
      ]] <- plotly::renderPlotly(hm_to_show$control())
      output[[
        id_list()$plot_section$outputs$hm_desc_2
      ]] <- renderText(control_desc())

      output[[
        id_list()$plot_section$outputs$samples_is_heatmap
      ]] <- plotly::renderPlotly(hm_to_show$other())
      output[[
        id_list()$plot_section$outputs$hm_desc_3
      ]] <- renderText(other_desc())

      observeEvent({
        control_sc_r_table_persample()
        samples_sc_r_table_persample()
      }, {
        req(control_sc_r_table_persample())
        req(samples_sc_r_table_persample())
        output[[
          id_list()$plot_section$outputs$ratio_tbl_sc_control
        ]] <- reactable::renderReactable(control_sc_r_table_persample())
        output[[
          id_list()$plot_section$outputs$ratio_tbl_sc_samples
        ]] <- reactable::renderReactable(samples_sc_r_table_persample())
      })
      observeEvent({
        control_sc_r_barplot_persample()
        samples_sc_r_barplot_persample()
      }, {
        req(control_sc_r_barplot_persample())
        req(samples_sc_r_barplot_persample())
        output[[
          id_list()$plot_section$outputs$ratio_barplot_sc_control
        ]] <- plotly::renderPlotly(control_sc_r_barplot_persample())
        output[[
          id_list()$plot_section$outputs$ratio_barplot_sc_samples
        ]] <- plotly::renderPlotly(samples_sc_r_barplot_persample())
      })

      observeEvent({
        control_rep_r_table_persample()
        samples_rep_r_table_persample()
      }, {
        req(control_rep_r_table_persample())
        req(samples_rep_r_table_persample())
        output[[
          id_list()$plot_section$outputs$ratio_tbl_rep_control
        ]] <- reactable::renderReactable(control_rep_r_table_persample())
        output[[
          id_list()$plot_section$outputs$ratio_tbl_rep_samples
        ]] <- reactable::renderReactable(samples_rep_r_table_persample())
      })
      observeEvent({
        control_rep_r_barplot_persample()
        samples_rep_r_barplot_persample()
      }, {
        req(control_rep_r_barplot_persample())
        req(samples_rep_r_barplot_persample())
        output[[
          id_list()$plot_section$outputs$ratio_barplot_rep_control
        ]] <- plotly::renderPlotly(control_rep_r_barplot_persample())
        output[[
          id_list()$plot_section$outputs$ratio_barplot_rep_samples
        ]] <- plotly::renderPlotly(samples_rep_r_barplot_persample())
      })

      observeEvent({
        control_ratio_contour_plot()
        samples_ratio_contour_plot()
      }, {
        output[[
          id_list()$plot_section$outputs$ratio_contour_controls
        ]] <- plotly::renderPlotly(control_ratio_contour_plot())
        output[[
          id_list()$plot_section$outputs$ratio_contour_samples
        ]] <- plotly::renderPlotly(samples_ratio_contour_plot())
      })

      observeEvent(input[[id_list()$plot_section$inputs$recompute_btn]], {
        shinyjs::show(id_list()$plot_section$inputs$plots_panels)
      })

      data_to_pass <- reactive({
        req(data())
        req(applied_filter())
        data() %>%
          dplyr::filter(.data$seqCount >= applied_filter())
      })

      observeEvent(input[[
        id_list()$plot_section$inputs$next_btn
      ]], {
        shinyjs::runjs(sprintf("
                   $('#%s').slideUp('fast')
                   ", id_list()$plot_section$section_id))
        shinyjs::show(id = id_list()$saving_section$section_id, asis = TRUE)
      })

      return(list(data = reactive({data_to_pass()}),
                  col_names = reactive({colnames(data())})))
    }
  )
}
