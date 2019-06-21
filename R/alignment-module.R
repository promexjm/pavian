

refseq_assemblies <-
  c("archaea"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/archaea/assembly_summary.txt",
    "bacteria"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/bacteria/assembly_summary.txt",
    "fungi"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/fungi/assembly_summary.txt",
    "invertebrate"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/invertebrate/assembly_summary.txt",
    "plant"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/plant/assembly_summary.txt",
    "protozoa"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/protozoa/assembly_summary.txt",
    "vertebrate_mammalian"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/vertebrate_mammalian/assembly_summary.txt",
    "vertebrate_other"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/vertebrate_other/assembly_summary.txt",
    "viral"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/viral/assembly_summary.txt")

genbank_assemblies <-
  c("archaea"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/archaea/assembly_summary.txt",
    "bacteria"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/bacteria/assembly_summary.txt",
    "fungi"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/fungi/assembly_summary.txt",
    "invertebrate"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/invertebrate/assembly_summary.txt",
    "plant"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/plant/assembly_summary.txt",
    "protozoa"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/protozoa/assembly_summary.txt",
    "vertebrate_mammalian"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/vertebrate_mammalian/assembly_summary.txt",
    "vertebrate_other"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/vertebrate_other/assembly_summary.txt")

ncbi_viral <- c("NCBI Viral Genomes"="http://www.ncbi.nlm.nih.gov/genomes/GenomesGroup.cgi?taxid=10239&cmd=download2")

#assembly_resources = c(refseq_assemblies, genbank_assemblies, ncbi_viral)
assembly_resources = names(refseq_assemblies)

#' UI part of alignment module
#'
#' @param id Shiny namespace id
#'
#' @return UI part of alignment module
#' @export
alignmentModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width=12,
        collapsible=TRUE, collapsed=FALSE,
        title="Instructions",
        HTML("The alignment viewer requires a <a target='blank' href='https://genome.ucsc.edu/goldenpath/help/bam.html'>BAM alignment file and BAI index</a>.

To generate a BAM file, download a genome of interest, and align to it with an aligner like <a target='blank' href='http://bowtie-bio.sourceforge.net/bowtie2/index.shtml'>Bowtie2</a> or <a target='blank' href='https://github.com/lh3/bwa'>bwa mem</a>. The resulting SAM file can be compressed into a binary BAM file and indexed with <a target='blank' href='http://www.htslib.org'>samtools</a>.
")
    ),
    tabBox(width = 12,
           tabPanel(
             title = "Use data on server",
             id = "server_dir",
             " Select a directory in the file browser and press 'Read selected directories' to load it into Pavian.",
             " You may also use wildcards to directly upload specified paths.",
             br(),
             uiOutput(ns("warn_Rsamtools_server"), width = 12),
             div(id="server_data_dir_div",
                 shinyWidgets::searchInput(ns("search_data_dir"),
                                           label = "Specify directory on machine running Pavian",
                                           value = getOption("pavian.alignment_server_dir", ""),
                                           btnReset = icon("level-up", lib="glyphicon"),
                                           resetValue = NULL,
                                           width = "100%",
                                           btnSearch = icon("server"))),
             div(style="max-height:400px; overflow-y: scroll",
                 shinyFileTree::shinyFileTreeOutput(ns('file_tree'))
             ),
             shinyjs::hidden(actionButton(ns("btn_read_tree_dirs"), "Read selected bam file")),
             shinyjs::hidden(div(id=ns("align_view_rsamtools_server"),
                                 
                                 uiOutput(ns("info_server")),
                                 shiny::uiOutput(ns("bam_name_server")),
                                 br(),
                                 DT::dataTableOutput(ns("table_server")),
                                 br(),
                                 shiny::plotOutput(ns("sample_align_server"), brush = brushOpts(id=ns("align_brush"), direction = "x", resetOnNew = TRUE), height = "200px"),
                                 shiny::plotOutput(ns("plot_brush_server"), height = "200px"),
                                 
                                 downloadLink(ns("pdf_server"), "PDF"),
                                 downloadLink(ns("pdf_brush_server"), "PDF"))),
             uiOutput(ns('rud'))
             
           ),     
           
      tabPanel(
        title = "View alignment",
        uiOutput(ns("warn_Rsamtools"), width = 12),
        shinyjs::hidden(div(id=ns("align_view_rsamtools"),
        fluidRow(
          column(5,shiny::fileInput(ns("bam_file_upload"),"Upload BAM and BAI file", accept=c(".bam",".bai"), multiple=TRUE)),
          column(3,shiny::actionButton(ns("btn_load_example_data"), "Load example data")),
          column(4,shiny::sliderInput(ns("mapq"),"Minimum MAPQ",0,50,0,step=1))),
        uiOutput(ns("info")),
        shinyjs::hidden(shiny::checkboxInput(ns("align_loess"), "Show smoothed LOESS curve")),
        shinyjs::hidden(shiny::checkboxInput(ns("align_moving_avg"), "Show moving average", value = TRUE)),
        shiny::uiOutput(ns("bam_name")),
        br(),
        DT::dataTableOutput(ns("table")),
        br(),
        shiny::plotOutput(ns("sample_align"), brush = brushOpts(id=ns("align_brush"), direction = "x", resetOnNew = TRUE), height = "200px"),
        shiny::plotOutput(ns("plot_brush"), height = "200px"),
        downloadLink(ns("pdf"), "PDF"),
        downloadLink(ns("pdf_brush"), "PDF")))
      ),
      
     
      
      tabPanel(
        title = "Download genomes for alignment",
        
        HTML(
          "Gather and display the content of the assembly_summary.txt from the selected domain from <a target='blank' href='ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq'>ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq</a>. An active internet connection is required, and currently no files are cached."),
        br(),
        br(),
        div(class="row-fluid",
            div(class="col-sm-6 col-xs-12",
                shiny::selectizeInput(ns("cbo_assemblies"), choices = assembly_resources, selected = "RefSeq bacteria", label = NULL, width="100%")),
            div(class="col-sm-6 col-xs-12",
                shiny::actionButton(ns("btn_load_assembly_info"), "Update assembly information",width="100%"))),
        br(),
        br(),
        DT::dataTableOutput(ns("dt_assembly_info")),
        htmlOutput(ns("dl_genome"))
      )

     
    )
  )
}

#' Server part of alignment module
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session
#' @param sample_data \code{data.frame} for samples
#' @param datatable_opts Additional options for creating the datatable.
#'
#' @return Alignment module server functionality
#' @export
#' @import shinydashboard
alignmentModule <- function(input, output, session, sample_data, datatable_opts, pavian_options = NULL, config_dir = NULL) {
  
  # Use data on server need use function 
  sample_sets <- reactiveValues(val=NULL, selected=NULL) # val is the list of all sample sets
  sample_sets_selected <- NULL # selected is just used to initialize the radioButtons in the module
  sample_sets <- reactiveValues(val=NULL, selected=NULL) # val is the list of all sample sets
  sample_sets_selected <- NULL # selected is just used to initialize the radioButtons in the module
  
  ns <- session$ns
  
  #shinyFiles::shinyDirChoose(input, ns('search_data_dir'), roots = server_dirs, filetypes = c(""))
  
  read_error_msg <- reactiveValues(val_pos = NULL, val_neg = NULL)
  
  output$uploaded_sample_sets <- renderUI({
    req(sample_sets$val)
    req(names(sample_sets$val))
    sample_set_names <- sort(names(sample_sets$val))
    box(
      width = 12,
      collapsible = TRUE,
      title = "Available sample sets",
      status = "primary",
      
      column(
        6,
        div(class = "styled-radios",
            radioButtons(
              ns("sample_set_select"),
              label = NULL,
              choices = sample_set_names,
              selected = ifelse(is.null(sample_sets$selected), sample_set_names[1], sample_sets$selected)
            )
        )
      ),
      column(
        6,
        shinyjs::hidden(textInput(ns(
          "txt_rename_sample_set"
        ), label = "New name")),
        actionButton(ns("btn_view_results"), "View results"),
        actionButton(ns("btn_rename_sample_set"), "Rename sample set", icon=icon("pencil")),
        actionButton(ns("btn_remove_sample_set"), label = "Remove sample set", icon=icon("trash"))
      ),
      br(),
      rhandsontable::rHandsontableOutput(ns("table")),
      p("You can specify which samples to include as well as their names. Be sure to save the table to make the changes persistent."),
      actionButton(
        ns("btn_save_table"),
        "Save table"
      )
    )
  })
  
  output$upload_info <- renderUI({
    req(!is.null(read_error_msg$val_pos) ||
          !is.null(read_error_msg$val_neg))
    box(width = 12,
        div(HTML(read_error_msg$val_pos), style = "color:green"),
        div(HTML(
          paste(read_error_msg$val_neg, collapse = " ")
        ), style = "color:red"))
  })
  
  output$uploaded_sample_sets <- renderUI({
    req(sample_sets$val)
    req(names(sample_sets$val))
    sample_set_names <- sort(names(sample_sets$val))
    box(
      width = 12,
      collapsible = TRUE,
      title = "Available sample sets",
      status = "primary",
      
      column(
        6,
        div(class = "styled-radios",
            radioButtons(
              ns("sample_set_select"),
              label = NULL,
              choices = sample_set_names,
              selected = ifelse(is.null(sample_sets$selected), sample_set_names[1], sample_sets$selected)
            )
        )
      ),
      column(
        6,
        shinyjs::hidden(textInput(ns(
          "txt_rename_sample_set"
        ), label = "New name")),
        actionButton(ns("btn_view_results"), "View results"),
        actionButton(ns("btn_rename_sample_set"), "Rename sample set", icon=icon("pencil")),
        actionButton(ns("btn_remove_sample_set"), label = "Remove sample set", icon=icon("trash"))
      ),
      br(),
      rhandsontable::rHandsontableOutput(ns("table")),
      p("You can specify which samples to include as well as their names. Be sure to save the table to make the changes persistent."),
      actionButton(
        ns("btn_save_table"),
        "Save table"
      )
    )
  })
  
  read_server_directory <- function(...) {
    withProgress({read_server_directory2(...)}, message = "Reading directory on server ...")
  }
  
  
  read_server_directory2 <-
    function(data_dir, sample_set_name = NULL, ...) {
      sample_sets_val <- isolate(sample_sets$val)
      res <-
        read_server_directory1(data_dir,
                               sample_set_name = sample_set_name,
                               existing_sample_set_names = names(sample_sets_val),
                               ...,
                               display_messages = FALSE)
      read_error_msg$val_pos <- res$error_msg$val_pos
      read_error_msg$val_neg <- res$error_msg$val_neg
      if (is.null(read_error_msg$val_pos)) {
        if (is.null(res$error_msg$val_neg)) {
          read_error_msg$val_neg <- sprintf("Unable to read report files from directory %s.", data_dir)
        }
        return(FALSE)
      }
      
      validate(
        need(res$sample_sets, message = "No sample sets available. Set a different directory")
      )
      
      sample_sets$val <-
        c(sample_sets_val, res$sample_sets[!names(res$sample_sets) %in% names(sample_sets_val)])
      sample_sets$selected <- names(res$sample_sets)[1]
      return(TRUE)
    }
  
  observeEvent(pavian_options$server_dir, {
    req(pavian_options$server_dir)
    # require(shinyWidgets)
    #req(exists('updateSearchInput', where='package:shinyWidgets', mode='function'))
    tryCatch({
      shinyWidgets::updateSearchInput(session, "search_data_dir", value=pavian_options$server_dir)
    }, error = message)
  })
  
  observeEvent(input$btn_read_tree_dirs, {
    fnames <- input$file_tree_selected
    print("btn_read_tree_dirs is running")
    fnames <- sub(" \\([0-9]+ f[io].*\\)$", "", fnames)
    if (all(startsWith(fnames, fnames[1]))) {
      fnames <- fnames[1]
      
    }
    bam_file_rv_server$val <- fnames
    #res <- read_server_directory(fnames)
  })
 
  
  ## Save and retrieve recently used directories
  recently_used_dirs <- reactiveValues(val = NULL)
  recently_used_dir_user_config <- NULL
  if (!is.null(config_dir) && dir.exists(config_dir)) {
    recently_used_dir_user_config <- file.path(config_dir, "recently_used_dirs.txt")
    if (file.exists(recently_used_dir_user_config)) {
      recently_used_dirs$val <- readLines(recently_used_dir_user_config)
    }
  }
  
  file_tree_dir <- reactiveValues(val = NULL)
  display_tree <- reactiveValues(val = FALSE)
  
  output$file_tree <- shinyFileTree::renderShinyFileTree({
    data_dir <- file_tree_dir$val
    req(data_dir)
    req(display_tree$val)
    req(length(data_dir) > 0 && nchar(data_dir) > 0)
    shinyjs::disable("btn_read_tree_dirs")
    shinyjs::show("btn_read_tree_dirs")
    shinyFileTree::shinyFileTree(list(text=data_dir,
                                      state=list(opened=TRUE),
                                      children=shinyFileTree::get_list_from_directory(data_dir,
                                                                                      max_depth=1,
                                                                                      show_dir_info=TRUE)),
                                 opts = shinyFileTree::shinyFileTreeOpts(
                                   animation = FALSE, 
                                   themes.stripes = FALSE,
                                   multiple = TRUE),
                                 plugins = c("wholerow", "types"))
  })
  
  observeEvent(input$file_tree_dblclick, {
    #message("doubleclick observed")
    fnames <- sub(" \\([0-9]+ f[io].*\\)$", "", input$file_tree_selected)
    if (dir.exists(fnames)) {
      tryCatch({
        shinyWidgets::updateSearchInput(session, "search_data_dir", value = fnames)
        file_tree_dir$val <- fnames
      }, error = message)
    }
  })
  
  observeEvent(input$file_tree_selected, {
    if (length(input$file_tree_selected) == 0) {
      shinyjs::disable("btn_read_tree_dirs")
    } else {
      shinyjs::enable("btn_read_tree_dirs")
    }
  })
  
  
  observeEvent(input$search_data_dir_reset, {
    tryCatch({
      fnames <- dirname(input$search_data_dir)
      shinyWidgets::updateSearchInput(session, "search_data_dir", value = fnames)
      file_tree_dir$val <- fnames
    }, error = message)
  })
  
  observeEvent(input$search_data_dir, {
    req(input$search_data_dir)
    do_glob <- !dir.exists(input$search_data_dir)
    display_tree$val <- !do_glob
    if (do_glob && length(Sys.glob(input$search_data_dir) == 0)) {
      shinyjs::addClass(class="red_background", selector="")
    }
    
    if (!isTRUE(display_tree$val)) {
      shinyjs::hide("btn_read_tree_dirs")
      display_tree$val <- FALSE
      res <- read_server_directory(input$search_data_dir, sample_set_name="Server files", glob_files=do_glob)
      if (res && !input$search_data_dir %in% recently_used_dirs$val) {
        recently_used_dirs$val <-
          c(input$search_data_dir, recently_used_dirs$val)
        if (!is.null(recently_used_dir_user_config))
          writeLines(recently_used_dirs$val, recently_used_dir_user_config)
      }
    } else {
      file_tree_dir$val <- input$search_data_dir
    }
  })
  
  
  output$rud <- renderUI({
    req(recently_used_dirs$val)
    
    shiny::tagList(br(),
                   "Recently used directories: ",
                   lapply(seq(from = 1, to = min(length(
                     recently_used_dirs$val
                   ), 5)),
                   function(i)
                     actionLink(ns(paste0("rud_", i)), recently_used_dirs$val[i])))
    
  })
  
  observeEvent(input$rud_1, {
    shinyWidgets::updateSearchInput(session, "search_data_dir", value = recently_used_dirs$val[1])
  })
  observeEvent(input$rud_2, {
    shinyWidgets::updateSearchInput(session, "search_data_dir", value = recently_used_dirs$val[2])
  })
  observeEvent(input$rud_3, {
    shinyWidgets::updateSearchInput(session, "search_data_dir", value = recently_used_dirs$val[3])
  })
  observeEvent(input$rud_4, {
    shinyWidgets::updateSearchInput(session, "search_data_dir", value = recently_used_dirs$val[4])
  })
  observeEvent(input$rud_5, {
    shinyWidgets::updateSearchInput(session, "search_data_dir", value = recently_used_dirs$val[5])
  })
  
  
  
  # from server parse table data as with view alignment   
  
  bam_file_rv_server <- reactiveValues(val = NULL, txt = NULL)
  
  
  
  output$bam_name_server <- renderUI({
   
    req(bam_file_rv_server$txt)
    bam_file_rv_server$txt
  })
  
  pileup_server <- reactive({
    req(bam_file_rv_server$val)
   
    bam_file_rv_server$txt <- shiny::tagList("Loaded ", shiny::strong(basename(bam_file_rv_server$val)),". Click on a row to see the genome coverage.")
    unique(get_pileup(bam_file_rv_server$val, min_mapq = input$mapq))
  })
  
  pileup_brush_server <- reactive({
   
    req(pileup_server())
    req(ranges_server$x)
    
    
    selected_row_server <- seqinfo_df_server()[1, , drop=FALSE]
    
    mypileup <- pileup_server() %>%
      dplyr::filter(seqnames %in% selected_row_server$seqnames &
                      findInterval(pos,ranges_server$x) == 1)
    
    sel <- mypileup$count > 0
    validate(need(sum(sel) > 0, message = "No reads in selected region"))
    
    attr(mypileup, "covered_bp") = tapply(mypileup[sel,"pos"], mypileup[sel,"seqnames"], function(x) length(unique(x)))
    attr(mypileup, "sum_count") = tapply(mypileup[sel,"count"], mypileup[sel,"seqnames"], sum)
    
    mypileup
  })
  output$warn_Rsamtools_server <- renderUI({
    if (!requireNamespace("Rsamtools")) {
      shinyjs::hide("align_view_rsamtools_server")
      
      div(id=session$ns("warn_div"),
          infoBox(
            "Functionality requires package Rsamtools",
            "See https://bioconductor.org/packages/release/bioc/html/Rsamtools.html for installation instructions.",
            icon = icon("exclamation-triangle"),
            color = "red", width = 12
          ),
          actionButton(session$ns("btn_install_rsamtools"),"Install Rsamtools")
      )
    } else {
      shinyjs::show("align_view_rsamtools_server")
    }
  })
  
  
  observeEvent(input$btn_install_rsamtools, {
    shiny::withProgress({
      tryCatch({
        source("https://bioconductor.org/biocLite.R")
        biocLite("Rsamtools")
      }, error=function(e) {})
    }, message = "Installing Rsamtools ... (will take a while)")
    if (requireNameSpace("Rsamtools")) {
      shinyjs::hide("warn_Rsamtools")
      shinyjs::show("align_view_rsamtools")
      shinyjs::alert("Successfully installed Rsamtools!")
    } else {
      shinyjs::alert("Rsamtools installation unsuccessful.")
    }
  })
  
  req_bioc <- function(pkg) {
    #req(require(pkg, character.only=TRUE))
    
    validate(need(requireNamespace(pkg), message=sprintf(
      "%s is needed for this functionality. See https://bioconductor.org/packages/release/bioc/html/%s.html for information on how to install it",
      pkg, pkg)))
  }
  
  
  info_text_server <- reactiveValues(val=NULL)
  
  output$info_server <- renderUI({
    
    info_text_server$val
  })
  
 
  
  seqinfo_df_server <- reactive({
    req(bam_file_rv_server$val)
   
    covered_bp <- attr(pileup_server(),"covered_bp")
    covered_bp[setdiff(names(seq_lengths_server()),names(covered_bp))] <- 0
    sum_count <- attr(pileup_server(),"sum_count")
    sum_count[setdiff(names(seq_lengths_server()),names(sum_count))] <- 0
    
    seq_info_df <-
      data.frame(seqnames=names(seq_lengths_server()),
                 genome_size=seq_lengths_server(),
                 avg_coverage=signif(sum_count/seq_lengths_server(),3),
                 covered_bp=covered_bp,
                 n_reads=nreads_server(),
                 avg_mapq=signif(avg_mapq_server(),3))
    
    
    seq_info_df$perc_covered = 100*signif(seq_info_df$covered_bp / seq_info_df$genome_size,3)
    
    seq_info_df[order(-seq_info_df$n_reads,-seq_info_df$perc_covered), , drop=F]
    
  })
  
  seqinfo_df_brush_server <- reactive({
    
    req(bam_file_rv_server$val)
    covered_bp <- attr(pileup_brush_server(),"covered_bp")
    covered_bp[setdiff(names(seq_lengths_range_x_server()),names(covered_bp))] <- 0
    sum_count <- attr(pileup_brush_server(),"sum_count")
    sum_count[setdiff(names(seq_lengths_range_x_server()),names(sum_count))] <- 0
    
    seq_info_df <- do.call(rbind,lapply(names(seq_lengths_server()), function(name) {
      data.frame(seqnames=name,
                 genome_size=seq_lengths_range_x_server()[name],
                 avg_coverage=signif(sum_count[name]/seq_lengths_range_x_server()[name],3),
                 covered_bp=covered_bp[name],
                 n_reads=nreads_range_x_server()[name]
      )
    }))
    
    seq_info_df$perc_covered = 100*signif(seq_info_df$covered_bp / seq_info_df$genome_size,3)
    seq_info_df[order(-seq_info_df$n_reads,-seq_info_df$perc_covered), , drop=F]
  })
  
  
  
  
  output$table_server <- DT::renderDataTable({
    
    req(seqinfo_df_server())
    
    DT::datatable(seqinfo_df_server(),
                 
                  selection = list(mode='single', selected = 1, target = 'row'),
                 
                  rownames = FALSE,
                  
                  colnames = c("Sequence"="seqnames","Length"="genome_size","# of reads"="n_reads",
                               "Covered bp"="covered_bp","Average\ncoverage"="avg_coverage",
                               "Average\nMAPQ"="avg_mapq",
                               "Percent\ncovered"="perc_covered"),
                  
                 
                  extensions = datatable_opts$extensions,
                 
                  class=datatable_opts$class,
                 
                  options=list(
                    buttons = common_buttons(sub(".bam$","",basename(isolate(bam_file_rv_server$val)), ignore.case = T),"alignment-summary"),
                   
                    columnDefs=list(
                      list(targets = c(2:ncol(seqinfo_df_server()), orderSequence = c('desc', 'asc'))
                      )))) %>%
     
      DT::formatCurrency(2, currency = '', digits = 0 ) %>%
      DT::formatString(3, suffix = "x") %>%
      DT::formatString(7, suffix = "%")
    
  }, server = FALSE)
  
  #plot_pileup_act_server <- eventReactive(input$btn_load_example_data, {
  plot_pileup_act_server <- reactive ({
   
    req_bioc("Rsamtools")
   
    req(pileup_server())
    selected_row_server <- seqinfo_df_server()[1, , drop=FALSE]
    pileup_res <- pileup_server() %>% dplyr::filter(seqnames %in% selected_row_server$seqnames)
    plot_pileup(pileup_res,
                selected_row_server,
                text_size = 4
    )
  })
  
  bam2_server <- reactive( {
   
    get_bam2(bam_file_rv_server$val)
  })
  
  bam2_mapq_server <- reactive( {
  
    req(input$mapq)
    bam2_server() %>% dplyr::filter(mapq >= input$mapq)
  })
  
  selected_bam_server <- reactive( {
   
    req(selected_seq_server())
    bam2_mapq_server() %>% dplyr::filter(rname %in% selected_seq_server())
  })
  
  seq_lengths_server <- reactive({ get_seqlengths(bam_file_rv_server$val) })
  
  selected_seq_server <- reactive({
   
   
    seqinfo_df_server()[1,"seqnames"]
  })
  
  nreads_server <- reactive({
   
    bam <- bam2_mapq_server()
    res <- tapply(bam$qname, bam$rname, function(x) length(unique(x)))
    res[is.na(res)] <- 0
    res
  })
  
  avg_mapq_server <- reactive({
   
    bam <- bam2_mapq_server()
    res <- tapply(bam$mapq, bam$rname, mean)
    res[is.na(res)] <- 0
    res
  })
  
  nreads_range_x_server <- reactive({
  
    bam <- selected_bam_server()
    sel <- findInterval(bam$pos, ranges_server$x)==1
    tapply(bam$qname[sel], bam$rname[sel], function(x) length(unique(x)))
  })
  
  seq_lengths_range_x_server <- reactive({
  
    rr <- ranges_server$x[2] - ranges_server$x[1]
    a <- get_seqlengths(bam_file_rv_server$val)
    sapply(a, function(x) rr)
  })
  
  
  output$sample_align_server <- renderPlot({
    req(bam_file_rv_server$val)
   
    info_text_server$val <- ""
    req_bioc("Rsamtools")
    #ranges_server$x <- NULL
    plot_pileup_act_server()
  }, res = 72)
  
  output$pdf_server <- downloadHandler(filename=function() { "bla.pdf" },
                                content=function(file) { ggsave(file, plot_pileup_act(), "pdf",
                                                                height = 2.5, units = "in") } )
  
  ranges_server <- reactiveValues(x = NULL)
  
  observe({
    brush <- input$align_brush
   
    if (!is.null(brush)) {
      ranges_server$x <- round(c(brush$xmin, brush$xmax))
    } else {
      ranges_server$x <- NULL
    }
  })
  
  output$plot_brush_server <- shiny::renderPlot({
   
    req(ranges_server$x)
    #xlim <- ranges_server$x
    #xlim[1] <- max(0,ceiling(xlim[1]))
    #xlim[2] <- min(seqinfo_df_brush()$genome_size,floor(xlim[2]))
    selected_row_server <- seqinfo_df_brush_server()[1, , drop=FALSE]
    
    plot_pileup(pileup_brush_server(), selected_row_server, text_size = 4)
  }, res=72)
  
  output$pdf_brush_server <- downloadHandler(filename=function() { "bla.pdf" },
                                      content=function(file) { ggsave(file,
                                                                      plot_pileup(pileup_brush_server(), seqinfo_df_brush_server()[1, , drop=FALSE],
                                                                                  text_size = 4), "pdf",
                                                                      height = 2.5, units = "in") } )
  
  
  
  
  
  
  
  
  
  #  View alignment   need use function

  bam_file_rv <- reactiveValues(val = NULL, txt = NULL)

  output$bam_name <- renderUI({
    req(bam_file_rv$txt)
    bam_file_rv$txt
  })

  pileup <- reactive({
    req(bam_file_rv$val)
    bam_file_rv$txt <- shiny::tagList("Loaded ", shiny::strong(basename(bam_file_rv$val)),". Click on a row to see the genome coverage.")
    unique(get_pileup(bam_file_rv$val, min_mapq = input$mapq))
  })

  pileup_brush <- reactive({
    req(pileup())
    req(ranges$x)
    req(input$table_rows_selected)

    selected_row <- seqinfo_df()[input$table_rows_selected, , drop=FALSE]

    mypileup <- pileup() %>%
      dplyr::filter(seqnames %in% selected_row$seqnames &
                      findInterval(pos,ranges$x) == 1)

    sel <- mypileup$count > 0
    validate(need(sum(sel) > 0, message = "No reads in selected region"))

    attr(mypileup, "covered_bp") = tapply(mypileup[sel,"pos"], mypileup[sel,"seqnames"], function(x) length(unique(x)))
    attr(mypileup, "sum_count") = tapply(mypileup[sel,"count"], mypileup[sel,"seqnames"], sum)

    mypileup
  })

  output$warn_Rsamtools <- renderUI({
    if (!requireNamespace("Rsamtools")) {
      shinyjs::hide("align_view_rsamtools")

      div(id=session$ns("warn_div"),
      infoBox(
        "Functionality requires package Rsamtools",
        "See https://bioconductor.org/packages/release/bioc/html/Rsamtools.html for installation instructions.",
        icon = icon("exclamation-triangle"),
        color = "red", width = 12
      ),
      actionButton(session$ns("btn_install_rsamtools"),"Install Rsamtools")
      )
    } else {
      shinyjs::show("align_view_rsamtools")
    }
  })
  
  observeEvent(input$btn_install_rsamtools, {
    shiny::withProgress({
      tryCatch({
        source("https://bioconductor.org/biocLite.R")
        biocLite("Rsamtools")
      }, error=function(e) {})
    }, message = "Installing Rsamtools ... (will take a while)")
    if (requireNameSpace("Rsamtools")) {
      shinyjs::hide("warn_Rsamtools")
      shinyjs::show("align_view_rsamtools")
      shinyjs::alert("Successfully installed Rsamtools!")
    } else {
      shinyjs::alert("Rsamtools installation unsuccessful.")
    }
  })

  req_bioc <- function(pkg) {
    #req(require(pkg, character.only=TRUE))
    validate(need(requireNamespace(pkg), message=sprintf(
      "%s is needed for this functionality. See https://bioconductor.org/packages/release/bioc/html/%s.html for information on how to install it",
      pkg, pkg)))
  }

  observeEvent(input$bam_file_upload, {
    req(input$bam_file_upload)
    message("got BAM file")
    bam_file_upload <- NULL
    if (!is.null(input$bam_file_upload)) {
      if (!is.data.frame(input$bam_file_upload)) {
        bam_file_rv$txt <- "Upload is no data.frame."
      } else if (nrow(input$bam_file_upload) != 2) {
        bam_file_rv$txt <- shiny::strong(style="color:red;","Please upload exactly two files at once; one BAM file its corresponding BAI file.")
      } else if (sum(grepl(".bam$",input$bam_file_upload$name, ignore.case = T)) !=1 ) {
        bam_file_rv$txt <- shiny::strong(style="color:red;","Did not get a file with the extension .bam or .BAM .")
      } else if (sum(grepl(".bai$",input$bam_file_upload$name)) !=1 ) {
        bam_file_rv$txt <- shiny::strong(style="color:red;","Did not get a file with the extension .bai or .BAI .")
      } else {
        file.rename(input$bam_file_upload$datapath[1], file.path(dirname(input$bam_file_upload$datapath[1]),input$bam_file_upload$name[1]))
        file.rename(input$bam_file_upload$datapath[2], file.path(dirname(input$bam_file_upload$datapath[2]),input$bam_file_upload$name[2]))
        bam_pos <- grep(".bam$",input$bam_file_upload$name)
        bam_file_upload = file.path(dirname(input$bam_file_upload$datapath[bam_pos]),input$bam_file_upload$name[bam_pos])
      }
    }
    message("BAM file = ",bam_file_upload)
    bam_file_rv$val <- bam_file_upload
  })
  
  info_text <- reactiveValues(val=NULL)
  
  output$info <- renderUI({
    info_text$val
  })

  observeEvent(input$btn_load_example_data, {
    message("Loading example alignment")
    example_file <- system.file("shinyapp","example-data","PT5-JC_polyomavirus.bam", package="pavian")
    if (file.exists(example_file)) {
      bam_file_rv$val <- example_file
    } else {
      info_text$val <- "Could not load example file."
    }
  })

  seqinfo_df <- reactive({
    req(bam_file_rv$val)

    covered_bp <- attr(pileup(),"covered_bp")
    covered_bp[setdiff(names(seq_lengths()),names(covered_bp))] <- 0
    sum_count <- attr(pileup(),"sum_count")
    sum_count[setdiff(names(seq_lengths()),names(sum_count))] <- 0

    seq_info_df <-
      data.frame(seqnames=names(seq_lengths()),
                 genome_size=seq_lengths(),
                 avg_coverage=signif(sum_count/seq_lengths(),3),
                 covered_bp=covered_bp,
                 n_reads=nreads(),
                 avg_mapq=signif(avg_mapq(),3))

    seq_info_df$perc_covered = 100*signif(seq_info_df$covered_bp / seq_info_df$genome_size,3)
    seq_info_df[order(-seq_info_df$n_reads,-seq_info_df$perc_covered), , drop=F]
  })

  seqinfo_df_brush <- reactive({
    req(bam_file_rv$val)
    covered_bp <- attr(pileup_brush(),"covered_bp")
    covered_bp[setdiff(names(seq_lengths_range_x()),names(covered_bp))] <- 0
    sum_count <- attr(pileup_brush(),"sum_count")
    sum_count[setdiff(names(seq_lengths_range_x()),names(sum_count))] <- 0

    seq_info_df <- do.call(rbind,lapply(names(seq_lengths()), function(name) {
      data.frame(seqnames=name,
                 genome_size=seq_lengths_range_x()[name],
                 avg_coverage=signif(sum_count[name]/seq_lengths_range_x()[name],3),
                 covered_bp=covered_bp[name],
                 n_reads=nreads_range_x()[name]
      )
    }))

    seq_info_df$perc_covered = 100*signif(seq_info_df$covered_bp / seq_info_df$genome_size,3)
    seq_info_df[order(-seq_info_df$n_reads,-seq_info_df$perc_covered), , drop=F]
  })




  output$table <- DT::renderDataTable({
    req(seqinfo_df())

    DT::datatable(seqinfo_df(), selection = list(mode='single', selected = 1, target = 'row'),
              rownames = FALSE,
              colnames = c("Sequence"="seqnames","Length"="genome_size","# of reads"="n_reads",
                           "Covered bp"="covered_bp","Average\ncoverage"="avg_coverage",
                           "Average\nMAPQ"="avg_mapq",
                           "Percent\ncovered"="perc_covered"),
              extensions = datatable_opts$extensions,
              class=datatable_opts$class,
              options=list(
                buttons = common_buttons(sub(".bam$","",basename(isolate(bam_file_rv$val)), ignore.case = T),"alignment-summary"),
                columnDefs=list(
                  list(targets = c(2:ncol(seqinfo_df()), orderSequence = c('desc', 'asc'))
                  )))) %>%
      DT::formatCurrency(2, currency = '', digits = 0 ) %>%
      DT::formatString(3, suffix = "x") %>%
      DT::formatString(7, suffix = "%")
  }, server = FALSE)

  #plot_pileup_act <- eventReactive(input$btn_load_example_data, {
  plot_pileup_act <- reactive ({
    req_bioc("Rsamtools")
    req(input$table_rows_selected)
    req(pileup())
    selected_row <- seqinfo_df()[input$table_rows_selected, , drop=FALSE]
    pileup_res <- pileup() %>% dplyr::filter(seqnames %in% selected_row$seqnames)
    plot_pileup(pileup_res,
                selected_row,
                text_size = 4
    )
  })

  bam2 <- reactive( {
    get_bam2(bam_file_rv$val)
  })

  bam2_mapq <- reactive( {
    req(input$mapq)
    bam2() %>% dplyr::filter(mapq >= input$mapq)
  })

  selected_bam <- reactive( {
    req(selected_seq())
    bam2_mapq() %>% dplyr::filter(rname %in% selected_seq())
  })

  seq_lengths <- reactive({ get_seqlengths(bam_file_rv$val) })

  selected_seq <- reactive({
    req(input$table_rows_selected)
    seqinfo_df()[input$table_rows_selected,"seqnames"]
  })

  nreads <- reactive({
    bam <- bam2_mapq()
    res <- tapply(bam$qname, bam$rname, function(x) length(unique(x)))
    res[is.na(res)] <- 0
    res
  })

  avg_mapq <- reactive({
    bam <- bam2_mapq()
    res <- tapply(bam$mapq, bam$rname, mean)
    res[is.na(res)] <- 0
    res
  })

  nreads_range_x <- reactive({
    bam <- selected_bam()
    sel <- findInterval(bam$pos, ranges$x)==1
    tapply(bam$qname[sel], bam$rname[sel], function(x) length(unique(x)))
  })

  seq_lengths_range_x <- reactive({
    rr <- ranges$x[2] - ranges$x[1]
    a <- get_seqlengths(bam_file_rv$val)
    sapply(a, function(x) rr)
  })


  output$sample_align <- renderPlot({
    req(bam_file_rv$val)
    info_text$val <- ""
    req_bioc("Rsamtools")
    #ranges$x <- NULL
    plot_pileup_act()
  }, res = 72)

  output$pdf <- downloadHandler(filename=function() { "bla.pdf" },
                                content=function(file) { ggsave(file, plot_pileup_act(), "pdf",
                                                                height = 2.5, units = "in") } )

  ranges <- reactiveValues(x = NULL)

  observe({
    brush <- input$align_brush
    if (!is.null(brush)) {
      ranges$x <- round(c(brush$xmin, brush$xmax))
    } else {
      ranges$x <- NULL
    }
  })

  output$plot_brush <- shiny::renderPlot({
    req(input$table_rows_selected)
    req(ranges$x)
    #xlim <- ranges$x
    #xlim[1] <- max(0,ceiling(xlim[1]))
    #xlim[2] <- min(seqinfo_df_brush()$genome_size,floor(xlim[2]))
    selected_row <- seqinfo_df_brush()[input$table_rows_selected, , drop=FALSE]

    plot_pileup(pileup_brush(), selected_row, text_size = 4)
  }, res=72)

  output$pdf_brush <- downloadHandler(filename=function() { "bla.pdf" },
                                content=function(file) { ggsave(file,
                                                                plot_pileup(pileup_brush(), seqinfo_df_brush()[input$table_rows_selected, , drop=FALSE],
                                                                            text_size = 4), "pdf",
                                                                height = 2.5, units = "in") } )

  
  
   #  Download genomes for alignment  need use functions
  
  stored_assembly_info <- reactiveValues()
  
  assembly_info <- reactive({
    if (!input$cbo_assemblies %in% names(stored_assembly_info)) {
      stored_assembly_info[[input$cbo_assemblies]] <- 
        withProgress({
          download_assembly_info(refseq_assemblies[[input$cbo_assemblies]])
        }, message = "Downloading and parsing assembly info ... ")
    }
    return(stored_assembly_info[[input$cbo_assemblies]])
  })
  
  download_assembly_info <- function(url) {
    
    # Check that the first two lines are headers, and make them nice
    colClasses = c(
      AC = "character",
      bioproject = "NULL",
      biosample = "NULL",
      wgs_master = "NULL",
      refseq_category = "NULL",
      TaxId = "character",
      Species_TaxID = "character",
      Name = "character",
      Strain = "character",
      isolate = "NULL",
      Version = "character",
      "Assembly level" = "factor",
      release_type = "NULL",
      genome_rep = "NULL",
      Date = "Date",
      asm_name = "NULL",
      Submitter = "NULL",
      gbrs_paired_asm = "NULL",
      paired_asm_comp = "NULL",
      URL = "character",
      excluded_from_refseq = "NULL",
      relation_to_type_material = "NULL"
    )

    ai <- 
      # assembly_accession    bioproject  biosample   wgs_master  refseq_category
      # taxid   species_taxid   organism_name   infraspecific_name  isolate
      # version_status  assembly_taxRank  release_type    genome_rep  seq_rel_date    asm_name    submitter   gbrs_paired_asm paired_asm_comp ftp_path    excluded_from_refseq
      utils::read.delim(url, comment.char = "#",
                        colClasses = as.character(colClasses),
                        col.names = names(colClasses),
                        fill = TRUE,
                        header = FALSE
      )

    ai <- ai[ai$Version == "latest", setdiff(colnames(ai), "Version")]

    #ai$Strain <- sub("^strain=", "", ai$Strain)
    #ends_with_infraspecific_name <-
    #  substr(ai$Name,
    #         nchar(ai$Name) - nchar(ai$Strain) + 1,
    #         nchar(ai$Name)
    #  ) == ai$Strain
    #ai$Name[!ends_with_infraspecific_name] <-
    #  paste(ai$Name, ai$Strain)[!ends_with_infraspecific_name]

    beautify_colnames(ai)
  }


  output$dt_assembly_info <- DT::renderDataTable({
    ai <- assembly_info()
    req(ai)
    my_title <- sprintf("%s-assembly-info-%s", names(refseq_assemblies)[refseq_assemblies==input$cbo_assemblies], format(Sys.time(), "%y%m%d"))
    DT::datatable(
      ai,
      filter = 'bottom',
      selection = 'single',
      extensions = datatable_opts$extensions,
      class = paste(datatable_opts$class, "nowrap"),
      options(buttons = common_buttons(names(refseq_assemblies)[refseq_assemblies==input$cbo_assemblies], "assembly-info"))
    )
  })

  output$dl_genome <- renderUI ({
    req(input$dt_assembly_info_rows_selected)
    res <- assembly_info()[input$dt_assembly_info_rows_selected,]

    fname <- paste0(basename(res$URL),"_genomic.fna")
    ncbi_link <- sprintf("https://www.ncbi.nlm.nih.gov/assembly/%s", res$AC)
    dl_link <- sprintf("%s/%s.gz", res$URL, fname)
    #https://www.ncbi.nlm.nih.gov/assembly/GCF_000015825.1
    myname <- sprintf("%s-%s-%s.fna", gsub("[ \\]","_", res$Name), res$TaxId, res$AC)

    shiny::tagList(
      h3(res$Name, res$Strain),
      p("NCBI Assembly: ", a(ncbi_link, href=ncbi_link, target="_blank")),
      p("Genome FASTA: ", a(dl_link, href=dl_link, target="_blank")),
      p("To download and build an index with bowtie2, execute the following commands:"),
      code(
        sprintf("wget %s && gunzip %s\n", dl_link, myname),
        sprintf("# Optional: sed -i '/^>/ s/ /_/g' %s\n", myname),
        sprintf("bowtie2-build %s %s\n", myname, myname))
    )
  })
}
