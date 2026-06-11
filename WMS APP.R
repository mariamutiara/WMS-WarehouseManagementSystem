# ==============================================================================
# LIBRARY
# ==============================================================================

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)

# ==============================================================================
# DATA AWAL
# ==============================================================================

warehouse_awal <- data.frame(
  ID = c(1, 2, 3),
  Nama = c("Gudang Utama", "Gudang Barat", "Gudang Timur"),
  Deskripsi = c("Gudang elektronik", "Gudang distribusi", "Gudang penyimpanan"),
  Alamat = c("Jakarta", "Bandung", "Surabaya"),
  PIC = c("Budi", "Siti", "Andi"),
  stringsAsFactors = FALSE
)

kategori_vector <- c("Persediaan", "Non Persediaan", "Jasa")

# Matrix stok awal
matrix_stok <- matrix(
  c(100, 50, 25, 80, 60, 20, 120, 70, 30),
  nrow = 3, byrow = TRUE
)
rownames(matrix_stok) <- c("Gudang Utama", "Gudang Barat", "Gudang Timur")
colnames(matrix_stok) <- c("Laptop", "Printer", "SSD")

# Data item awal
item_awal <- data.frame(
  ID = 1:21,
  Warehouse = c(
    "Gudang Utama","Gudang Utama","Gudang Utama","Gudang Barat","Gudang Barat",
    "Gudang Barat","Gudang Timur","Gudang Timur","Gudang Timur","Gudang Utama",
    "Gudang Barat","Gudang Timur","Gudang Utama","Gudang Barat","Gudang Timur",
    "Gudang Utama","Gudang Barat","Gudang Timur","Gudang Utama","Gudang Barat","Gudang Timur"
  ),
  NamaItem = c(
    "Laptop Asus","Printer Epson","Mouse Logitech","Router Cisco","SSD Samsung",
    "Keyboard Gaming","Monitor LG","Harddisk External","Kabel LAN","RAM Kingston",
    "CPU Intel","Jasa Maintenance","Biaya Internet","Projector Epson","Webcam Logitech",
    "Tablet Android","Barcode Scanner","UPS Power Supply",
    "Kursi Kerja Ergonomis","Meja Kubikel","AC Split 1 PK"
  ),
  Tipe = c(
    "Persediaan","Persediaan","Persediaan","Persediaan","Persediaan",
    "Persediaan","Persediaan","Persediaan","Persediaan","Persediaan",
    "Persediaan","Jasa","Non Persediaan","Persediaan","Persediaan",
    "Persediaan","Persediaan","Persediaan",
    "Non Persediaan","Non Persediaan","Non Persediaan"
  ),
  Qty = c(10,5,25,8,15,20,7,10,100,12,5,0,0,3,8,15,12,6,8,4,3),
  Harga = c(
    12000000,3500000,250000,5000000,1800000,1500000,4000000,2000000,15000,
    1200000,4500000,750000,1200000,6000000,900000,3500000,1200000,2500000,
    1500000,2500000,4000000
  ),
  Vendor = c(
    "PT Teknologi","PT Epson","PT Logitech","PT Cisco","PT Samsung",
    "PT Gaming","PT LG","PT Storage","PT Kabel","PT Kingston",
    "PT Intel","CV Service","Indihome","PT Epson","PT Logitech",
    "PT Teknologi","PT Scanner Indo","PT Powerindo",
    "PT Mebeul Karya","PT Mebeul Karya","CV Pendingin"
  ),
  stringsAsFactors = FALSE
)
item_awal$TotalValue <- item_awal$Qty * item_awal$Harga

# Data Transaksi
transaction_awal <- data.frame(
  ID = 1:5,
  TransactionNo = c("PO-001","PO-002","SI-001","PO-003","SI-002"),
  TransactionType = c("Purchase Invoice","Purchase Invoice","Sales Invoice","Purchase Invoice","Sales Invoice"),
  TransactionDate = c("2024-01-15","2024-01-20","2024-01-25","2024-02-01","2024-02-05"),
  Vendor_Customer = c("PT Elektronik Jaya","PT Komputer Solusindo","PT Maju Bersama","PT Sinar Terang","CV Teknologi Cepat"),
  Status = c("Completed","Completed","Completed","Pending","Completed"),
  stringsAsFactors = FALSE
)

# Data Serial Number Entries awal
serial_entries_awal <- data.frame(
  ID = 1:8,
  EntryNo = c("SNE-001","SNE-001","SNE-002","SNE-002","SNE-002","SNE-003","SNE-004","SNE-004"),
  EntryDate = c("2024-01-16","2024-01-16","2024-01-21","2024-01-21","2024-01-21","2024-01-26","2024-02-02","2024-02-02"),
  PreparedBy = c("Warehouse Staff","Warehouse Staff","Gudang","Gudang","Gudang","Admin Gudang","Staff Logistik","Staff Logistik"),
  TransactionType = c("Purchase Invoice","Purchase Invoice","Purchase Invoice","Purchase Invoice","Purchase Invoice","Sales Invoice","Purchase Invoice","Purchase Invoice"),
  TransactionNo = c("PO-001","PO-001","PO-002","PO-002","PO-002","SI-001","PO-003","PO-003"),
  ItemName = c("Laptop Asus","Laptop Asus","SSD Samsung","SSD Samsung","SSD Samsung","Laptop Asus","Printer Epson","Printer Epson"),
  SerialNumber = c("SN-LAP-001","SN-LAP-002","SN-SSD-001","SN-SSD-002","SN-SSD-003","SN-LAP-001","SN-PRN-001","SN-PRN-002"),
  ExpiredDate = c("2026-01-15","2026-01-15",NA,NA,NA,"2026-01-15","2025-12-31","2025-12-31"),
  Qty = c(1,1,1,1,1,1,1,1),
  BatchNumber = c(NA,NA,"BATCH-001","BATCH-001","BATCH-001",NA,"BATCH-002","BATCH-002"),
  stringsAsFactors = FALSE
)

item_with_serial_awal <- data.frame(
  ItemName = c("Laptop Asus","SSD Samsung","Printer Epson"),
  ManageSerialNumber = c(TRUE,TRUE,TRUE),
  ManageExpiredDate = c(TRUE,FALSE,TRUE),
  SerialNumberType = c("Unique Number","Batch Number","Unique Number"),
  stringsAsFactors = FALSE
)

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color:#F7F8FD; font-family:'Segoe UI',sans-serif; color:#2C2C2C; }
      .navbar-custom {
        background-color:#1A1A2E; padding:14px 28px; display:flex;
        align-items:center; gap:12px; margin-bottom:24px; border-bottom:4px solid #F4C430;
      }
      .navbar-custom h2 { color:#F4C430; margin:0; font-size:22px; font-weight:700; }
      .navbar-custom span { color:#AAAACC; font-size:14px; }
      .card-panel {
        background:#FFFFFF; border-radius:12px; padding:24px; margin-bottom:20px;
        box-shadow:0 2px 12px rgba(0,0,0,0.07); border-left:5px solid #F4C430;
      }
      .card-panel h4 {
        margin-top:0; color:#1A1A2E; font-weight:700; font-size:16px;
        border-bottom:1px solid #EEEEEE; padding-bottom:10px; margin-bottom:16px;
      }
      .dashboard-card { text-align:center; }
      .dashboard-card h2 { color:#1A1A2E; font-size:32px; font-weight:bold; margin:10px 0 0 0; }
      .dashboard-card h4 { color:#666; font-size:14px; border-bottom:none; padding-bottom:0; }
      .preview-box { background:#FFFBE6; border:1px solid #F4C430; border-radius:8px; padding:16px; margin-bottom:16px; }
      .btn-new { background-color:#F4C430 !important; color:#1A1A2E !important; font-weight:700 !important; border:none !important; border-radius:6px !important; }
      .btn-ok  { background-color:#1A1A2E !important; color:#F4C430 !important; font-weight:700 !important; border:none !important; }
      .btn-cancel { background-color:#EEEEEE !important; color:#555555 !important; }
      /* Tombol Edit & Delete seragam untuk Tab Warehouse DAN Tab List Item */
      .btn-edit-style {
        background-color:#4A90D9 !important; color:white !important; border:none !important;
        border-radius:4px !important; padding:3px 10px !important; margin-right:4px !important;
        cursor:pointer !important; font-size:12px !important;
      }
      .btn-delete-style {
        background-color:#E74C3C !important; color:white !important; border:none !important;
        border-radius:4px !important; padding:3px 10px !important;
        cursor:pointer !important; font-size:12px !important;
      }
      .badge-serial {
        background-color:#1A1A2E; color:#F4C430; padding:4px 10px;
        border-radius:20px; font-size:11px; font-weight:bold;
      }
      .info-box { background:#E8F4FD; padding:12px; border-radius:8px; margin-bottom:16px; font-size:13px; }
      .notif-error { background:#F2DEDE; color:#A94442; padding:10px; border-radius:6px; margin-bottom:10px; }
      .notif-sukses { background:#DFF0D8; color:#3C763D; padding:10px; border-radius:6px; margin-bottom:10px; }
      .step-bar { display:flex; margin-bottom:20px; align-items:center; }
      .step { padding:6px 16px; font-size:12px; font-weight:600; background:#DDDDDD; color:#888;
              clip-path:polygon(0 0,90% 0,100% 50%,90% 100%,0 100%,10% 50%); padding-left:20px; }
      .step:first-child { clip-path:polygon(0 0,90% 0,100% 50%,90% 100%,0 100%); padding-left:12px; }
      .step.active { background:#F4C430; color:#1A1A2E; }
      .step.done   { background:#1A1A2E; color:#F4C430; }
    "))
  ),

  div(class="navbar-custom",
      tags$img(src="https://cdn-icons-png.flaticon.com/512/2038/2038854.png", height="36px"),
      h2("WMS - Warehouse Management System"),
      tags$span("Manajemen Data Gudang & Serial Number")
  ),

  tabsetPanel(

    # TAB 1: WAREHOUSE
    tabPanel("Warehouse", br(),
      fluidRow(
        column(4, div(class="card-panel dashboard-card", h4("Total Warehouse"), h2(textOutput("total_wh")))),
        column(4, div(class="card-panel dashboard-card", h4("Total Item"),      h2(textOutput("total_item")))),
        column(4, div(class="card-panel dashboard-card", h4("Total Inventory Value"), h2(textOutput("total_value"))))
      ), br(),
      fluidRow(
        column(4,
          conditionalPanel(condition="output.tahap_form != 'tutup'",
            div(class="card-panel",
              h4(textOutput("judul_form")),
              uiOutput("step_indicator"),
              uiOutput("notifikasi_ui"),
              conditionalPanel(condition="output.tahap_form == 'isi'",
                textInput("inp_nama",   "Nama Warehouse *",            placeholder="cth: Gudang Utama"),
                textAreaInput("inp_desk","Deskripsi *",                placeholder="cth: Gudang penyimpanan", rows=3),
                textInput("inp_alamat", "Alamat *",                    placeholder="cth: Jl. Industri No.1"),
                textInput("inp_pic",    "Person In Charge (PIC) *",    placeholder="cth: Budi Santoso"),
                br(),
                actionButton("btn_next",  "Next",  class="btn-new"),
                actionButton("btn_batal", "Batal", class="btn-cancel")
              ),
              conditionalPanel(condition="output.tahap_form == 'preview'",
                div(class="preview-box", uiOutput("preview_data")),
                p("Apakah data di atas sudah benar?", style="font-size:13px;color:#555;"),
                actionButton("btn_ok",      "Ok / Simpan", class="btn-ok"),
                actionButton("btn_kembali", "Kembali",     class="btn-cancel")
              )
            )
          ),
          div(class="card-panel", style="border-left-color:#1A1A2E;",
            h4("Tambah Warehouse Baru"),
            actionButton("btn_new", "+ New Warehouse", class="btn-new", width="100%")
          )
        ),
        column(8,
          div(class="card-panel",
            h4("Daftar Warehouse"),
            DTOutput("tabel_warehouse")
          )
        )
      ), br(),
      fluidRow(column(12, div(class="card-panel", h4("Matrix Stok Warehouse"), tableOutput("matrix_view"))))
    ),

    # TAB 2: LIST ITEM
    tabPanel("List Item", br(),
      fluidRow(
        column(4,
          div(class="card-panel",
            h4("Input Item"),
            selectInput("warehouse",  "Warehouse",  choices=NULL),
            textInput("nama_item",    "Nama Item"),
            selectInput("tipe_item",  "Tipe Item",  choices=kategori_vector),
            numericInput("qty",       "Qty",         value=0),
            numericInput("harga",     "Harga",       value=0),
            textInput("vendor",       "Vendor"),
            checkboxInput("use_serial","Manage Serial Number", value=FALSE),
            conditionalPanel(condition="input.use_serial == true",
              radioButtons("serial_type","Serial Number Type",
                choices=c("Unique Number"="unique","Batch Number"="batch"), inline=TRUE),
              checkboxInput("use_expired","Manage Expired Date", value=FALSE)
            ),
            br(),
            actionButton("btn_item","Save Item", class="btn-new", style="width:100%;")
          )
        ),
        column(8,
          div(class="card-panel",
            h4("Daftar Item"),
            DTOutput("tbl_item")
          )
        )
      )
    ),

    # TAB 3: ABC ANALYSIS
    tabPanel("ABC Analysis", br(),
      fluidRow(
        column(4,
          div(class="card-panel",
            h4("Analisis ABC"),
            p("Klik tombol di bawah untuk generate analisis ABC berdasarkan data item terkini (termasuk item baru)."),
            actionButton("btn_abc","Generate ABC", class="btn-new", style="width:100%;")
          )
        ),
        column(8,
          div(class="card-panel",
            h4("Hasil Analisis ABC"),
            DTOutput("tbl_abc"),
            br(),
            plotOutput("plot_abc", height="500px")
          )
        )
      )
    ),

    # TAB 4: SERIAL NUMBER ENTRY
    tabPanel("Serial Number Entry", br(),
      fluidRow(column(12,
        div(class="card-panel",
          h4("Pencatatan Nomor Seri"),
          p("Formulir untuk mencatat Serial Number dan Expired Date barang yang dibeli atau dijual."),
          hr()
        )
      )),
      fluidRow(
        column(6,
          div(class="card-panel",
            h4("Pilih Transaksi"),
            selectInput("trans_type","Transaction Type *",
              choices=c("Purchase Invoice","Sales Invoice","Purchase Order","Sales Order")),
            selectInput("trans_no","Transaction No *", choices=NULL),
            dateInput("entry_date","Entry Date", value=Sys.Date()),
            textInput("prepared_by","Prepared By *", value="Warehouse Staff"),
            br(),
            actionButton("btn_load_transaction","Load Transaction Details", class="btn-new", style="width:100%;")
          )
        ),
        column(6,
          div(class="card-panel",
            h4("Informasi Transaksi"),
            uiOutput("transaction_info"),
            uiOutput("serial_requirement_note")
          )
        )
      ), br(),
      fluidRow(column(12,
        div(class="card-panel",
          h4("Detail Barang & Input Serial Number"),
          DTOutput("tbl_transaction_items"),
          br(),
          actionButton("btn_save_serial_entries","Simpan Semua Serial Number", class="btn-ok", style="width:100%;")
        )
      ))
    ),

    # TAB 5: LIST SERIAL NUMBER
    tabPanel("List Serial Number", br(),
      fluidRow(
        column(3,
          div(class="card-panel",
            h4("Filter Data"),
            selectInput("filter_trans_type","Transaction Type",
              choices=c("All","Purchase Invoice","Sales Invoice","Purchase Order","Sales Order")),
            dateRangeInput("filter_date","Entry Date Range", start=Sys.Date()-30, end=Sys.Date()),
            br(),
            actionButton("btn_refresh_list","Refresh", class="btn-new", style="width:100%;")
          )
        ),
        column(9,
          div(class="card-panel",
            h4("Daftar Pencatatan Nomor Seri"),
            DTOutput("tbl_serial_entries_list")
          )
        )
      ), br(),
      fluidRow(column(12,
        div(class="card-panel",
          h4("Detail Serial Number per Entry"),
          p("Klik 'View Details' untuk melihat detail Serial Number."),
          DTOutput("tbl_serial_details")
        )
      ))
    )
  ),

  # JavaScript — semua event listener terpusat
  tags$script(HTML("
    // Warehouse
    $(document).on('click', '.btn-edit-row', function() {
      Shiny.setInputValue('klik_edit', $(this).data('id'), {priority:'event'});
    });
    $(document).on('click', '.btn-delete-row', function() {
      var id = $(this).data('id');
      if (confirm('Yakin ingin menghapus warehouse ID ' + id + '?')) {
        Shiny.setInputValue('klik_delete', id, {priority:'event'});
      }
    });
    // List Item
    $(document).on('click', '.btn-edit-item', function() {
      Shiny.setInputValue('klik_edit_item', parseInt($(this).data('id')), {priority:'event'});
    });
    $(document).on('click', '.btn-delete-item', function() {
      var id = parseInt($(this).data('id'));
      if (confirm('Yakin ingin menghapus item ID ' + id + '?')) {
        Shiny.setInputValue('klik_delete_item', id, {priority:'event'});
      }
    });
    // Serial Number
    $(document).on('click', '.btn-view-serial', function() {
      Shiny.setInputValue('view_serial_detail', $(this).data('entryno'), {priority:'event'});
    });
    // Serial input (unified handler)
    $(document).on('click', '.btn-input-serial', function() {
      Shiny.setInputValue('input_serial_btn', parseInt($(this).data('rowidx')), {priority:'event'});
    });
  "))
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # ===== STATE =====
  rv_warehouse        <- reactiveVal(warehouse_awal)
  rv_item             <- reactiveVal(item_awal)
  rv_abc              <- reactiveVal(data.frame())
  rv_next_id          <- reactiveVal(4)
  next_item_id        <- reactiveVal(22)
  rv_item_with_serial <- reactiveVal(item_with_serial_awal)
  rv_edit_item_id     <- reactiveVal(NULL)

  rv_transactions              <- reactiveVal(transaction_awal)
  rv_serial_entries            <- reactiveVal(serial_entries_awal)
  rv_next_entry_id             <- reactiveVal(9)
  rv_current_transaction_items <- reactiveVal(data.frame())
  rv_serial_inputs             <- reactiveVal(list())
  rv_current_serial_row        <- reactiveVal(1)

  rv_mode_form <- reactiveVal("tutup")
  rv_mode_op   <- reactiveVal("tambah")
  rv_edit_id   <- reactiveVal(NULL)
  rv_notif     <- reactiveVal(NULL)

  output$tahap_form <- reactive({ rv_mode_form() })
  outputOptions(output, "tahap_form", suspendWhenHidden = FALSE)

  # ===== HELPERS =====
  is_item_use_serial <- function(nm) nm %in% rv_item_with_serial()$ItemName
  get_item_serial_type <- function(nm) {
    si <- rv_item_with_serial()
    if (nm %in% si$ItemName) si$SerialNumberType[si$ItemName == nm] else "Unique Number"
  }

  # ===== SYNC DROPDOWN WAREHOUSE =====
  observe({ updateSelectInput(session, "warehouse", choices = rv_warehouse()$Nama) })

  # ===== WAREHOUSE FORM =====
  output$judul_form <- renderText({
    if (rv_mode_op() == "tambah")
      switch(rv_mode_form(), "isi"="Isi Data Warehouse Baru", "preview"="Preview Data Baru", "Form Warehouse")
    else
      switch(rv_mode_form(), "isi"=paste0("Edit Warehouse (ID:",rv_edit_id(),")"),
             "preview"=paste0("Preview Edit (ID:",rv_edit_id(),")"), "Form Edit")
  })

  output$step_indicator <- renderUI({
    if (rv_mode_form()=="tutup") return(NULL)
    div(class="step-bar",
      div(class=if(rv_mode_form()=="isi") "step active" else "step done", "1. Isi Form"),
      div(class=if(rv_mode_form()=="preview") "step active" else "step",  "2. Preview"),
      div(class="step", "3. Selesai")
    )
  })

  output$notifikasi_ui <- renderUI({
    n <- rv_notif(); if(is.null(n)) return(NULL)
    div(class=if(n$tipe=="sukses") "notif-sukses" else "notif-error", n$pesan)
  })

  observeEvent(input$btn_new, {
    updateTextInput(session,"inp_nama",value=""); updateTextAreaInput(session,"inp_desk",value="")
    updateTextInput(session,"inp_alamat",value=""); updateTextInput(session,"inp_pic",value="")
    rv_mode_op("tambah"); rv_edit_id(NULL); rv_mode_form("isi"); rv_notif(NULL)
  })

  observeEvent(input$btn_next, {
    if (any(trimws(c(input$inp_nama,input$inp_desk,input$inp_alamat,input$inp_pic))=="")) {
      rv_notif(list(tipe="error",pesan="Semua field wajib diisi!")); return()
    }
    rv_notif(NULL); rv_mode_form("preview")
  })

  observeEvent(input$btn_kembali, { rv_mode_form("isi") })
  observeEvent(input$btn_batal,   { rv_mode_form("tutup"); rv_notif(NULL) })

  output$preview_data <- renderUI({
    tags$table(style="width:100%;font-size:13px;",
      tags$tr(tags$td(tags$strong("Nama")),    tags$td(input$inp_nama)),
      tags$tr(tags$td(tags$strong("Deskripsi")),tags$td(input$inp_desk)),
      tags$tr(tags$td(tags$strong("Alamat")),  tags$td(input$inp_alamat)),
      tags$tr(tags$td(tags$strong("PIC")),     tags$td(input$inp_pic))
    )
  })

  observeEvent(input$btn_ok, {
    df <- rv_warehouse()
    if (rv_mode_op()=="tambah") {
      rv_warehouse(rbind(df, data.frame(
        ID=rv_next_id(), Nama=trimws(input$inp_nama), Deskripsi=trimws(input$inp_desk),
        Alamat=trimws(input$inp_alamat), PIC=trimws(input$inp_pic), stringsAsFactors=FALSE
      )))
      rv_next_id(rv_next_id()+1)
    } else {
      idx <- which(df$ID==rv_edit_id())
      if (length(idx)>0) {
        nama_lama <- df[idx,"Nama"]
        df[idx,c("Nama","Deskripsi","Alamat","PIC")] <- list(
          trimws(input$inp_nama),trimws(input$inp_desk),trimws(input$inp_alamat),trimws(input$inp_pic))
        rv_warehouse(df)
        di <- rv_item(); di$Warehouse[di$Warehouse==nama_lama] <- trimws(input$inp_nama); rv_item(di)
      }
    }
    rv_mode_form("tutup"); rv_edit_id(NULL)
  })

  observeEvent(input$klik_edit, {
    id_p <- as.integer(input$klik_edit)
    rd   <- rv_warehouse()[rv_warehouse()$ID==id_p,]
    updateTextInput(session,"inp_nama",value=rd$Nama)
    updateTextAreaInput(session,"inp_desk",value=rd$Deskripsi)
    updateTextInput(session,"inp_alamat",value=rd$Alamat)
    updateTextInput(session,"inp_pic",value=rd$PIC)
    rv_mode_op("edit"); rv_edit_id(id_p); rv_mode_form("isi"); rv_notif(NULL)
  })

  observeEvent(input$klik_delete, {
    id_p <- as.integer(input$klik_delete)
    rv_warehouse(rv_warehouse()[rv_warehouse()$ID!=id_p,])
  })

  # ===== LIST ITEM: TAMBAH =====
  observeEvent(input$btn_item, {
    if (trimws(input$nama_item)=="") { showNotification("Nama Item wajib diisi!", type="error"); return() }
    rv_item(rbind(rv_item(), data.frame(
      ID=next_item_id(), Warehouse=input$warehouse, NamaItem=trimws(input$nama_item),
      Tipe=input$tipe_item, Qty=input$qty, Harga=input$harga,
      Vendor=trimws(input$vendor), TotalValue=input$qty*input$harga, stringsAsFactors=FALSE
    )))
    next_item_id(next_item_id()+1)
    if (input$use_serial) {
      rv_item_with_serial(rbind(rv_item_with_serial(), data.frame(
        ItemName=trimws(input$nama_item), ManageSerialNumber=TRUE,
        ManageExpiredDate=input$use_expired,
        SerialNumberType=ifelse(input$serial_type=="unique","Unique Number","Batch Number"),
        stringsAsFactors=FALSE
      )))
    }
    updateTextInput(session,"nama_item",value=""); updateNumericInput(session,"qty",value=0)
    updateNumericInput(session,"harga",value=0); updateTextInput(session,"vendor",value="")
    updateCheckboxInput(session,"use_serial",value=FALSE)
    showNotification("Item berhasil disimpan!", type="message")
  })

  # ===== LIST ITEM: EDIT (Modal) =====
  observeEvent(input$klik_edit_item, {
    id_p      <- as.integer(input$klik_edit_item)
    df        <- rv_item()
    item_data <- df[df$ID==id_p,]
    if (nrow(item_data)==0) { showNotification("Item tidak ditemukan!", type="error"); return() }
    rv_edit_item_id(id_p)

    showModal(modalDialog(
      title=paste("Edit Item - ID:", id_p), size="m", easyClose=FALSE,
      footer=tagList(
        actionButton("btn_update_item","Update Item", class="btn-ok"),
        modalButton("Batal")
      ),
      selectInput("edit_item_wh",   "Warehouse", choices=rv_warehouse()$Nama, selected=item_data$Warehouse),
      textInput("edit_item_nama",   "Nama Item", value=item_data$NamaItem),
      selectInput("edit_item_tipe", "Tipe Item", choices=kategori_vector,    selected=item_data$Tipe),
      numericInput("edit_item_qty", "Qty",       value=item_data$Qty),
      numericInput("edit_item_harga","Harga",    value=item_data$Harga),
      textInput("edit_item_vendor", "Vendor",    value=item_data$Vendor)
    ))
  })

  observeEvent(input$btn_update_item, {
    id_edit <- rv_edit_item_id()
    if (is.null(id_edit)) { showNotification("Tidak ada item dipilih!", type="error"); return() }
    df  <- rv_item()
    idx <- which(df$ID==id_edit)
    if (length(idx)>0) {
      df[idx,"Warehouse"]  <- input$edit_item_wh
      df[idx,"NamaItem"]   <- trimws(input$edit_item_nama)
      df[idx,"Tipe"]       <- input$edit_item_tipe
      df[idx,"Qty"]        <- input$edit_item_qty
      df[idx,"Harga"]      <- input$edit_item_harga
      df[idx,"Vendor"]     <- trimws(input$edit_item_vendor)
      df[idx,"TotalValue"] <- input$edit_item_qty * input$edit_item_harga
      rv_item(df); removeModal()
      showNotification("Data item berhasil diperbarui!", type="message")
    } else {
      showNotification("Item tidak ditemukan!", type="error")
    }
  })

  # ===== LIST ITEM: HAPUS =====
  observeEvent(input$klik_delete_item, {
    id_p <- as.integer(input$klik_delete_item)
    rv_item(rv_item()[rv_item()$ID!=id_p,])
    showNotification("Item berhasil dihapus!", type="warning")
  })

  # ===== SERIAL NUMBER ENTRY =====
  observeEvent(input$trans_type, {
    df_f <- rv_transactions()[rv_transactions()$TransactionType==input$trans_type,]
    updateSelectInput(session,"trans_no", choices=df_f$TransactionNo)
  })

  observeEvent(input$btn_load_transaction, {
    req(input$trans_no)
    td <- rv_transactions()[rv_transactions()$TransactionNo==input$trans_no,]
    output$transaction_info <- renderUI({
      if (nrow(td)>0)
        div(class="info-box",
          tags$strong("Transaction No: "), td$TransactionNo[1], tags$br(),
          tags$strong("Date: "), td$TransactionDate[1], tags$br(),
          tags$strong("Vendor/Customer: "), td$Vendor_Customer[1], tags$br(),
          tags$strong("Status: "), td$Status[1])
      else div(class="info-box","Transaksi tidak ditemukan.")
    })
    if (input$trans_type=="Purchase Invoice") {
      pers <- rv_item()[rv_item()$Tipe=="Persediaan",]
      n <- min(3,nrow(pers))
      if (n>0) {
        id <- pers[1:n,c("NamaItem","Qty","Harga")]
        names(id) <- c("ItemName","Quantity","UnitPrice")
      } else {
        id <- data.frame(ItemName=character(),Quantity=numeric(),UnitPrice=numeric(),stringsAsFactors=FALSE)
      }
    } else {
      id <- data.frame(ItemName="Laptop Asus",Quantity=2,UnitPrice=12000000,stringsAsFactors=FALSE)
    }
    if (nrow(id)>0) {
      id$NeedSerial <- sapply(id$ItemName, is_item_use_serial)
      id$SerialType <- sapply(id$ItemName, get_item_serial_type)
      id$SerialNumbers <- ""; id$ExpiredDates <- ""; id$BatchNumbers <- ""
    }
    rv_current_transaction_items(id); rv_serial_inputs(list())
    output$tbl_transaction_items <- renderDT({
      df <- rv_current_transaction_items()
      if (nrow(df)>0) {
        df$Action <- sapply(1:nrow(df), function(i) {
          if (df$NeedSerial[i])
            paste0('<button class="btn-input-serial btn-new" data-rowidx="',i,
                   '" style="font-size:11px;padding:2px 8px;">Input Serial</button>')
          else '<span class="badge-serial">No Serial Required</span>'
        })
        datatable(df[,c("ItemName","Quantity","UnitPrice","SerialType","Action")],
          escape=FALSE, rownames=FALSE,
          options=list(pageLength=10,scrollX=TRUE,
            columnDefs=list(list(className='dt-center',targets='_all'))))
      } else {
        datatable(data.frame(Message="Tidak ada item ditemukan"),escape=FALSE,rownames=FALSE,options=list(dom='t'))
      }
    })
  })

  show_serial_modal <- function(row_index) {
    items_df <- rv_current_transaction_items(); req(nrow(items_df)>=row_index)
    item <- items_df[row_index,]; qty <- item$Quantity; stype <- item$SerialType
    showModal(modalDialog(
      title=paste("Input Serial Number -",item$ItemName), size="l", easyClose=FALSE, footer=NULL,
      if (stype=="Batch Number") {
        tagList(
          p(paste("Batch Number. Jumlah:",qty,"unit.")),
          textInput("batch_number_input","Batch Number *",placeholder="cth: BATCH-2024-001"),
          dateInput("expired_date_batch","Expired Date (opsional)", value=NA),
          br(),
          actionButton("save_batch_modal","Simpan Batch Number",class="btn-ok"),
          actionButton("cancel_modal","Batal",class="btn-cancel")
        )
      } else {
        mf <- min(qty,20)
        tagList(
          p(paste("Unique Serial Number. Jumlah:",qty,"unit.")),
          lapply(1:mf, function(i)
            div(style="margin-bottom:8px;",
              textInput(paste0("serial_",i), paste("Serial Number",i,":"),
                        placeholder=paste0(gsub(" ","_",item$ItemName),"-",sprintf("%03d",i))))),
          hr(),
          p("Optional: Expired Date"),
          dateInput("expired_date_unique","Expired Date", value=NA),
          br(),
          actionButton("save_serial_modal","Simpan Serial Numbers",class="btn-ok"),
          actionButton("cancel_modal","Batal",class="btn-cancel")
        )
      }
    ))
  }

  observeEvent(input$input_serial_btn, {
    rv_current_serial_row(as.integer(input$input_serial_btn))
    show_serial_modal(as.integer(input$input_serial_btn))
  })

  observeEvent(input$save_batch_modal, {
    req(input$batch_number_input)
    ri <- rv_current_serial_row(); cur <- rv_serial_inputs()
    cur[[as.character(ri)]] <- list(
      type="batch", batch_number=input$batch_number_input,
      expired_date=ifelse(is.null(input$expired_date_batch)||input$expired_date_batch=="",NA,as.character(input$expired_date_batch)),
      quantity=1)
    rv_serial_inputs(cur); removeModal()
    showNotification("Batch Number berhasil disimpan!", type="message")
  })

  observeEvent(input$save_serial_modal, {
    serials <- unlist(Filter(function(x) !is.null(x)&&x!="", lapply(1:20, function(i) input[[paste0("serial_",i)]])))
    if (length(serials)==0) { showNotification("Minimal 1 Serial Number harus diisi!",type="error"); return() }
    ri <- rv_current_serial_row(); cur <- rv_serial_inputs()
    cur[[as.character(ri)]] <- list(
      type="unique", serial_numbers=serials,
      expired_date=ifelse(is.null(input$expired_date_unique)||input$expired_date_unique=="",NA,as.character(input$expired_date_unique)),
      quantity=length(serials))
    rv_serial_inputs(cur); removeModal()
    showNotification(paste(length(serials),"Serial Number berhasil disimpan!"),type="message")
  })

  observeEvent(input$cancel_modal, { removeModal() })

  observeEvent(input$btn_save_serial_entries, {
    si <- rv_serial_inputs()
    if (length(si)==0) { showNotification("Tidak ada serial number yang diinput!",type="warning"); return() }
    en <- paste0("SNE-",format(Sys.Date(),"%Y%m%d"),"-",format(Sys.time(),"%H%M%S"))
    items_df <- rv_current_transaction_items(); new_entries <- data.frame()
    for (i in seq_along(si)) {
      sd <- si[[as.character(i)]]
      if (i<=nrow(items_df)) {
        id_row <- items_df[i,]
        if (sd$type=="batch") {
          new_entries <- rbind(new_entries, data.frame(
            ID=rv_next_entry_id(),EntryNo=en,EntryDate=as.character(input$entry_date),
            PreparedBy=input$prepared_by,TransactionType=input$trans_type,TransactionNo=input$trans_no,
            ItemName=id_row$ItemName,SerialNumber=sd$batch_number,ExpiredDate=sd$expired_date,
            Qty=id_row$Quantity,BatchNumber=sd$batch_number,stringsAsFactors=FALSE))
          rv_next_entry_id(rv_next_entry_id()+1)
        } else {
          for (sn in sd$serial_numbers) {
            new_entries <- rbind(new_entries, data.frame(
              ID=rv_next_entry_id(),EntryNo=en,EntryDate=as.character(input$entry_date),
              PreparedBy=input$prepared_by,TransactionType=input$trans_type,TransactionNo=input$trans_no,
              ItemName=id_row$ItemName,SerialNumber=sn,ExpiredDate=sd$expired_date,
              Qty=1,BatchNumber=NA,stringsAsFactors=FALSE))
            rv_next_entry_id(rv_next_entry_id()+1)
          }
        }
      }
    }
    if (nrow(new_entries)>0) rv_serial_entries(rbind(rv_serial_entries(),new_entries))
    rv_serial_inputs(list()); rv_current_transaction_items(data.frame())
    showNotification(paste("Entry",en,"berhasil disimpan!"),type="message")
  })

  # ===== TAB 5 =====
  output$tbl_serial_entries_list <- renderDT({
    input$btn_refresh_list
    df <- rv_serial_entries()
    if (nrow(df)==0) return(datatable(data.frame(Message="Belum ada data"),escape=FALSE,rownames=FALSE,options=list(dom='t')))
    if (input$filter_trans_type!="All") df <- df[df$TransactionType==input$filter_trans_type,]
    df <- df[as.Date(df$EntryDate)>=input$filter_date[1] & as.Date(df$EntryDate)<=input$filter_date[2],]
    if (nrow(df)==0) return(datatable(data.frame(Message="Tidak ada data sesuai filter"),escape=FALSE,rownames=FALSE,options=list(dom='t')))
    ds <- df %>% group_by(EntryNo,EntryDate,PreparedBy,TransactionType,TransactionNo) %>%
      summarise(TotalItems=n_distinct(ItemName),TotalSerialQty=n(),.groups='drop')
    ds$Action <- sapply(ds$EntryNo, function(en)
      paste0('<button class="btn-view-serial" data-entryno="',en,
             '" style="background:#4A90D9;color:white;border:none;border-radius:4px;padding:3px 10px;">View Details</button>'))
    datatable(ds,escape=FALSE,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE))
  })

  observeEvent(input$view_serial_detail, {
    en <- input$view_serial_detail
    dd <- rv_serial_entries()[rv_serial_entries()$EntryNo==en,]
    output$tbl_serial_details <- renderDT({
      datatable(dd[,c("ItemName","SerialNumber","BatchNumber","ExpiredDate","Qty")],
        escape=FALSE,rownames=FALSE,caption=paste("Detail -",en),
        options=list(pageLength=15,scrollX=TRUE))
    })
  })

  # ===== DASHBOARD =====
  output$total_wh    <- renderText({ nrow(rv_warehouse()) })
  output$total_item  <- renderText({ nrow(rv_item()) })
  output$total_value <- renderText({ paste("Rp", format(sum(rv_item()$TotalValue,na.rm=TRUE),big.mark=",")) })
  output$matrix_view <- renderTable({ matrix_stok }, rownames=TRUE)

  output$tabel_warehouse <- renderDT({
    df <- rv_warehouse()
    # Tombol seragam: biru Edit, merah Delete
    df$Aksi <- paste0(
      '<button class="btn-edit-row btn-edit-style" data-id="',df$ID,'">Edit</button>',
      '<button class="btn-delete-row btn-delete-style" data-id="',df$ID,'">Delete</button>'
    )
    datatable(df,escape=FALSE,rownames=FALSE,options=list(pageLength=5,scrollX=TRUE))
  })

  output$tbl_item <- renderDT({
    df <- rv_item()
    # Tombol seragam: biru Edit, merah Delete
    df$Aksi <- paste0(
      '<button class="btn-edit-item btn-edit-style" data-id="',df$ID,'">Edit</button>',
      '<button class="btn-delete-item btn-delete-style" data-id="',df$ID,'">Delete</button>'
    )
    datatable(df,escape=FALSE,rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE))
  })

  # ===== ABC ANALYSIS =====
  # Tabel ABC dengan format persen 2 desimal
  output$tbl_abc <- renderDT({
    df <- rv_abc()
    if (nrow(df)==0) return(datatable(data.frame(Message="Klik 'Generate ABC' untuk memulai"),escape=FALSE,rownames=FALSE,options=list(dom='t')))
    df$Persentase <- round(df$Persentase,2)
    df$Kumulatif  <- round(df$Kumulatif,2)
    datatable(df,rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE))
  })

  # Diagram batang ABC dalam % dengan max 2 desimal
  output$plot_abc <- renderPlot({
    df <- rv_abc()
    if (nrow(df)==0) return(NULL)
    total_nilai <- sum(df$TotalValue, na.rm=TRUE)
    df$Pct      <- round(df$TotalValue / total_nilai * 100, 2)

    ggplot(df, aes(x=reorder(NamaItem, Pct), y=Pct, fill=KategoriABC)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=paste0(Pct,"%")), hjust=-0.1, size=3.2, color="#333333") +
      coord_flip() +
      scale_y_continuous(
        labels=function(x) paste0(x,"%"),
        expand=expansion(mult=c(0,0.18))
      ) +
      scale_fill_manual(values=c("A"="#F4C430","B"="#4A90D9","C"="#E74C3C")) +
      theme_minimal(base_size=13) +
      labs(
        title="ABC Analysis - Kontribusi Nilai Inventori (%)",
        x="Nama Item", y="Persentase (%)", fill="Kategori"
      ) +
      theme(
        plot.title=element_text(face="bold",color="#1A1A2E",size=14),
        axis.text.y=element_text(size=10),
        legend.position="bottom"
      )
  })

  # Generate ABC — selalu ambil data terkini dari rv_item()
  observeEvent(input$btn_abc, {
    df <- rv_item() %>% filter(Tipe=="Persediaan")
    if (nrow(df)==0) { showNotification("Tidak ada item Persediaan!",type="warning"); return() }
    df <- df %>% arrange(desc(TotalValue))
    total <- sum(df$TotalValue, na.rm=TRUE)
    df$Persentase  <- round(df$TotalValue / total * 100, 2)
    df$Kumulatif   <- round(cumsum(df$Persentase), 2)
    df$KategoriABC <- ifelse(df$Kumulatif<=80,"A", ifelse(df$Kumulatif<=95,"B","C"))
    rv_abc(df)
    showNotification(paste("ABC Analysis berhasil untuk",nrow(df),"item!"),type="message")
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================
shinyApp(ui=ui, server=server)
