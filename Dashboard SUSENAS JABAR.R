# ==============================================================================
# 1. LIBRARY & SETUP
# ==============================================================================
library(shiny)
library(bslib)
library(htmltools)
library(dplyr)
library(tibble)
library(tidyr) 
library(highcharter)
library(leaflet)
library(base64enc)
library(readr) 

# --- Setup Gambar Header & Logo ---
# Ganti path ini sesuai lokasi file gambar di laptop Anda
img_path <- "D:/MUH. SUNAN/KULIAH IPB/SEMESTER 3/Dashboard/foto_header.png"
if(file.exists(img_path)) {
  img_base64 <- dataURI(file = img_path, mime = "image/jpeg")
} else {
  img_base64 <- "https://images.unsplash.com/photo-1542281286-9e0a16bb7366"
}

path_logo_jabar <- "D:/MUH. SUNAN/KULIAH IPB/SEMESTER 3/Dashboard/logo_jabar.png" 
if(file.exists(path_logo_jabar)) {
  logo_base64 <- dataURI(file = path_logo_jabar, mime = "image/png")
} else {
  logo_base64 <- "https://upload.wikimedia.org/wikipedia/commons/thumb/9/99/Coat_of_arms_of_West_Java.svg/600px-Coat_of_arms_of_West_Java.svg.png"
}

# ==============================================================================
# 2. DATA LOADING & PREPROCESSING
# ==============================================================================

# A. DATA UTAMA
file_utama <- "Susenas_Jabar_2024_SIAP_PAKAI.csv"
if(!file.exists(file_utama)) stop("ERROR: File 'Susenas_Jabar_2024_SIAP_PAKAI.csv' tidak ditemukan!")
raw_data <- read_csv(file_utama, show_col_types = FALSE)

# B. DATA EKONOMI (BLOK 43)
file_eko <- "Data_Ekonomi_Jabar_2024.csv"
if(!file.exists(file_eko)) stop("ERROR: File 'Data_Ekonomi_Jabar_2024.csv' tidak ditemukan!")
raw_eko <- read_csv(file_eko, show_col_types = FALSE)


# Pastikan KabKota ada isinya untuk mapping
if("Kode_KabKota" %in% names(raw_data)) raw_data <- raw_data %>% rename(KabKota = Kode_KabKota)
raw_data$KabKota_Angka <- as.numeric(as.character(raw_data$KabKota))


# --- 2.1. Mapping & Cleaning Data (FINAL & ROBUST JKN) ---
dummy <- raw_data %>%
  mutate(
    # --- 1. MAPPING WILAYAH ---
    kabkota = case_when(
      KabKota_Angka == 1 ~ "Kab. Bogor",
      KabKota_Angka == 2 ~ "Kab. Sukabumi",
      KabKota_Angka == 3 ~ "Kab. Cianjur",
      KabKota_Angka == 4 ~ "Kab. Bandung",
      KabKota_Angka == 5 ~ "Kab. Garut",
      KabKota_Angka == 6 ~ "Kab. Tasikmalaya",
      KabKota_Angka == 7 ~ "Kab. Ciamis",
      KabKota_Angka == 8 ~ "Kab. Kuningan",
      KabKota_Angka == 9 ~ "Kab. Cirebon",
      KabKota_Angka == 10 ~ "Kab. Majalengka",
      KabKota_Angka == 11 ~ "Kab. Sumedang",
      KabKota_Angka == 12 ~ "Kab. Indramayu",
      KabKota_Angka == 13 ~ "Kab. Subang",
      KabKota_Angka == 14 ~ "Kab. Purwakarta",
      KabKota_Angka == 15 ~ "Kab. Karawang",
      KabKota_Angka == 16 ~ "Kab. Bekasi",
      KabKota_Angka == 17 ~ "Kab. Bandung Barat",
      KabKota_Angka == 18 ~ "Kab. Pangandaran",
      KabKota_Angka == 71 ~ "Kota Bogor",
      KabKota_Angka == 72 ~ "Kota Sukabumi",
      KabKota_Angka == 73 ~ "Kota Bandung",
      KabKota_Angka == 74 ~ "Kota Cirebon",
      KabKota_Angka == 75 ~ "Kota Bekasi",
      KabKota_Angka == 76 ~ "Kota Depok",
      KabKota_Angka == 77 ~ "Kota Cimahi",
      KabKota_Angka == 78 ~ "Kota Tasikmalaya",
      KabKota_Angka == 79 ~ "Kota Banjar",
      TRUE ~ paste("Kode", KabKota)
    ),
    
    Hubungan_KRT = as.numeric(Hubungan_KRT),
    
    # --- 2. DEMOGRAFI ---
    kecamatan = "Semua", 
    kelurahan = "Semua", 
    gender = ifelse(Jenis_Kelamin %in% c("1", "Laki-laki"), "Laki-laki", "Perempuan"),
    urban_rural = ifelse(Tipe_Daerah %in% c("1", "Perkotaan"), "Perkotaan", "Perdesaan"),
    age = as.numeric(Umur),
    
    
    status_kawin = case_when(
      as.numeric(Status_Kawin) == 1 ~ "Belum Kawin",
      as.numeric(Status_Kawin) == 2 ~ "Kawin",
      as.numeric(Status_Kawin) == 3 ~ "Cerai Hidup",
      as.numeric(Status_Kawin) == 4 ~ "Cerai Mati",
      TRUE ~ "Lainnya"
    ),
    
    # --- 3. PENDIDIKAN ---
    partisipasi_sekolah = as.numeric(Partisipasi_Sekolah),
    
    rls = case_when(
      as.numeric(Ijazah_Tertinggi) == 25 ~ 0,
      as.numeric(Ijazah_Tertinggi) <= 5 ~ 6,
      as.numeric(Ijazah_Tertinggi) <= 10 ~ 9,
      as.numeric(Ijazah_Tertinggi) <= 17 ~ 12,
      as.numeric(Ijazah_Tertinggi) >= 18 ~ 15,
      TRUE ~ 0
    ),
    
    pendidikan = case_when(
      as.numeric(Ijazah_Tertinggi) == 25 ~ "Tidak Sekolah/Belum Tamat SD",
      as.numeric(Ijazah_Tertinggi) <= 5 ~ "SD/Sederajat",
      as.numeric(Ijazah_Tertinggi) <= 10 ~ "SMP/Sederajat",
      as.numeric(Ijazah_Tertinggi) <= 17 ~ "SMA/Sederajat",
      as.numeric(Ijazah_Tertinggi) >= 18 ~ "Perguruan Tinggi",
      TRUE ~ "Lainnya"
    ),
    
    # --- 4. KESEHATAN (PERBAIKAN JKN) ---
    # Logika Baru: Menerima kode "1", "A" (untuk PBI), dan "B" (untuk Mandiri)
    jkn = case_when(
      BPJS_PBI %in% c("1", 1, "A") | BPJS_Mandiri %in% c("1", 1, "B") ~ "Punya JKN",
      TRUE ~ "Tidak Punya"
    ),
    
    keluhan = ifelse(!is.na(Keluhan_Kesehatan) & Keluhan_Kesehatan == "1", "Ada Keluhan", "Tidak Ada"),
    
    # --- 5. PERUMAHAN ---
    lantai = case_when(
      as.numeric(Jenis_Lantai) %in% c(1,2,3,4) ~ "Layak (Keramik/Marmer)",
      as.numeric(Jenis_Lantai) %in% c(5,6) ~ "Semen/Kayu", 
      TRUE ~ "Tanah/Bambu"
    ),
    
    air_minum = ifelse(as.numeric(Sumber_Air) %in% c(1,2,3,4,5,7), "Layak/Terlindungi", "Tidak Layak"),
    
    jamban = case_when(
      as.numeric(Fasilitas_BAB) == 1 ~ "Sendiri",
      as.numeric(Fasilitas_BAB) %in% c(2,3,4) ~ "Bersama/Umum",
      TRUE ~ "Tidak Ada/Sembarangan"
    ),
    
    # --- PERBAIKAN SUMBER PENERANGAN (LEBIH DETAIL) ---
    # Kode 1: Listrik PLN dengan meteran
    # Kode 2: Listrik PLN tanpa meteran
    # Kode 3: Listrik Non-PLN (Genset/Surya/Accu)
    # Kode 4: Bukan Listrik (Sentir/Lilin/Obor)
    
    penerangan = case_when(
      as.numeric(Sumber_Listrik) == 1 ~ "PLN (Meteran)",
      as.numeric(Sumber_Listrik) == 2 ~ "PLN (Tanpa Meteran)",
      as.numeric(Sumber_Listrik) == 3 ~ "Listrik Non-PLN",
      as.numeric(Sumber_Listrik) == 4 ~ "Bukan Listrik (Api)",
      TRUE ~ "Lainnya/NA"
    ),
    
    bakar_masak = case_when(
      as.numeric(Bahan_Bakar) %in% c(2, 3, 4, 5, 6) ~ "Gas/LPG", 
      as.numeric(Bahan_Bakar) == 1 ~ "Listrik",
      as.numeric(Bahan_Bakar) == 10 ~ "Kayu Bakar",
      TRUE ~ "Lainnya"
    ),
    
    # --- 6. EKONOMI ---
    
    
    # 1. Sumber Penghasilan
    sumber_penghasilan = case_when(
      as.numeric(Sumber_Penghasilan) == 1 ~ "Bekerja (Gaji/Upah)",
      as.numeric(Sumber_Penghasilan) == 2 ~ "Kiriman Uang/Barang",
      as.numeric(Sumber_Penghasilan) == 3 ~ "Investasi",
      as.numeric(Sumber_Penghasilan) == 4 ~ "Pensiunan",
      TRUE ~ "Tidak Ada Data"
    ),
    
    # 2. Sektor Pekerjaan (Dari Kode Lapangan Usaha)
    # 1=Pertanian, 2=Industri, 3=Pengadaan Air/Listrik, 4=Konstruksi, 5-9=Jasa
    sektor_pekerjaan = case_when(
      is.na(Lapangan_Usaha) ~ "Tidak Bekerja/Lainnya",
      as.numeric(Lapangan_Usaha) == 1 ~ "Pertanian",
      as.numeric(Lapangan_Usaha) %in% c(2,3) ~ "Industri & Manufaktur",
      as.numeric(Lapangan_Usaha) == 4 ~ "Konstruksi",
      as.numeric(Lapangan_Usaha) %in% c(5:9) ~ "Perdagangan & Jasa",
      TRUE ~ "Lainnya"
    ),
  
    
    
    # Proxy Ekonomi (Lama)
    miskin = ifelse(as.numeric(Jenis_Lantai) >= 7 & as.numeric(Fasilitas_BAB) != 1, "Rentan", "Tidak Rentan"),
    disabilitas = "Tidak Ada Data" 
  )

# --- 2.2. Mapping Data Ekonomi (Rupiah) ---
dummy_eko <- raw_eko %>%
  mutate(
    kabkota = case_when(
      KabKota_Angka == 1 ~ "Kab. Bogor", KabKota_Angka == 2 ~ "Kab. Sukabumi", KabKota_Angka == 3 ~ "Kab. Cianjur",
      KabKota_Angka == 4 ~ "Kab. Bandung", KabKota_Angka == 5 ~ "Kab. Garut", KabKota_Angka == 6 ~ "Kab. Tasikmalaya",
      KabKota_Angka == 7 ~ "Kab. Ciamis", KabKota_Angka == 8 ~ "Kab. Kuningan", KabKota_Angka == 9 ~ "Kab. Cirebon",
      KabKota_Angka == 10 ~ "Kab. Majalengka", KabKota_Angka == 11 ~ "Kab. Sumedang", KabKota_Angka == 12 ~ "Kab. Indramayu",
      KabKota_Angka == 13 ~ "Kab. Subang", KabKota_Angka == 14 ~ "Kab. Purwakarta", KabKota_Angka == 15 ~ "Kab. Karawang",
      KabKota_Angka == 16 ~ "Kab. Bekasi", KabKota_Angka == 17 ~ "Kab. Bandung Barat", KabKota_Angka == 18 ~ "Kab. Pangandaran",
      KabKota_Angka == 71 ~ "Kota Bogor", KabKota_Angka == 72 ~ "Kota Sukabumi", KabKota_Angka == 73 ~ "Kota Bandung",
      KabKota_Angka == 74 ~ "Kota Cirebon", KabKota_Angka == 75 ~ "Kota Bekasi", KabKota_Angka == 76 ~ "Kota Depok",
      KabKota_Angka == 77 ~ "Kota Cimahi", KabKota_Angka == 78 ~ "Kota Tasikmalaya", KabKota_Angka == 79 ~ "Kota Banjar",
      TRUE ~ paste("Kode", KabKota_Angka)
    )
  )

# --- 2.2. Centroid Peta ---
centroids <- tibble(
  kabkota = c("Kab. Bogor", "Kab. Sukabumi", "Kab. Cianjur", "Kab. Bandung", "Kab. Garut", "Kab. Tasikmalaya", "Kab. Ciamis", "Kab. Kuningan", "Kab. Cirebon", "Kab. Majalengka", "Kab. Sumedang", "Kab. Indramayu", "Kab. Subang", "Kab. Purwakarta", "Kab. Karawang", "Kab. Bekasi", "Kab. Bandung Barat", "Kab. Pangandaran", "Kota Bogor", "Kota Sukabumi", "Kota Bandung", "Kota Cirebon", "Kota Bekasi", "Kota Depok", "Kota Cimahi", "Kota Tasikmalaya", "Kota Banjar"),
  lat = c(-6.55, -7.05, -6.95, -7.10, -7.30, -7.45, -7.30, -6.95, -6.75, -6.80, -6.85, -6.45, -6.55, -6.55, -6.30, -6.25, -6.80, -7.65, -6.60, -6.92, -6.91, -6.73, -6.24, -6.40, -6.87, -7.33, -7.37),
  lon = c(106.80, 106.70, 107.15, 107.50, 107.80, 108.10, 108.40, 108.50, 108.55, 108.20, 107.95, 108.15, 107.75, 107.45, 107.30, 107.15, 107.45, 108.60, 106.80, 106.93, 107.61, 108.55, 107.00, 106.82, 107.54, 108.22, 108.53)
)

# ==============================================================================
# 3. STYLING (CSS)
# ==============================================================================
col_primary   <- "#2E7D32" 
col_secondary <- "#81D4FA" 
col_bg        <- "#F5F5F5" 
col_card_bg   <- "#FFFFFF" 
col_text      <- "#333333" 

theme_modern <- bs_theme(
  version = 5,
  base_font = font_google("Poppins"),
  heading_font = font_google("Poppins"),
  primary = col_primary,
  secondary = col_secondary,
  bg = col_bg,
  fg = col_text
)

app_css <- paste0("
  body, h1, h2, h3, h4, h5, h6, p, div { font-family: 'Poppins', sans-serif !important; color: ", col_text, " !important; }
  body { background-color: ", col_bg, " !important; padding-top: 80px !important; overflow-x: hidden !important; width: 100% !important; margin: 0 !important; }
  .navbar { background-color: ", col_primary, " !important; min-height: 70px !important; padding: 0 20px !important; border: none !important; box-shadow: 0 4px 12px rgba(0,0,0,0.1); }
  .navbar > .container-fluid { display: flex !important; justify-content: flex-start !important; align-items: center !important; position: relative !important; }
  .navbar-brand { float: none !important; margin-right: 15px !important; font-size: 20px; font-weight: 700; color: #ffffff !important; display: flex; align-items: center; z-index: 10; }
  .navbar-brand span { color: #ffffff !important; }
  .navbar-nav { float: none !important; margin: 0 !important; display: flex; flex-direction: row; align-items: center; }
  @media (min-width: 1400px) { .navbar-nav { position: absolute !important; left: 50% !important; transform: translateX(-50%) !important; } }
  @media (max-width: 1399px) { .navbar-nav { position: relative !important; margin-left: auto !important; left: auto !important; transform: none !important; } }
  .nav-link { color: rgba(255,255,255,0.9) !important; font-weight: 600; font-size: 14px; padding: 8px 15px !important; border-radius: 50px !important; margin: 0 2px !important; transition: all 0.3s ease; white-space: nowrap; }
  .nav-link:hover { background-color: rgba(255,255,255,0.2) !important; color: #ffffff !important; transform: translateY(-2px); }
  .nav-link.active { background-color: #ffffff !important; color: ", col_primary, " !important; font-weight: 700 !important; box-shadow: 0px 4px 10px rgba(0,0,0,0.15); }
  .card { background-color: ", col_card_bg, " !important; border: none !important; border-radius: 20px !important; box-shadow: 0 10px 25px -5px rgba(0, 0, 0, 0.05), 0 8px 10px -6px rgba(0, 0, 0, 0.01) !important; transition: all 0.3s ease; margin-bottom: 20px; overflow: hidden; }
  .card::before { content: ''; position: absolute; top: 0; left: 0; width: 100%; height: 6px; background: linear-gradient(90deg, ", col_primary, ", ", col_secondary, "); }
  .card:hover { transform: translateY(-5px); box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04) !important; }
  .menu-card { cursor: pointer; }
  .stat-card { background: #FFFFFF; border-radius: 20px; padding: 25px; border: none; box-shadow: 0 4px 15px rgba(0,0,0,0.03); transition: all 0.3s ease; height: 100%; display: flex; align-items: center; justify-content: space-between; }
  .stat-card:hover { transform: translateY(-5px); box-shadow: 0 10px 25px rgba(0,0,0,0.08); }
  .stat-icon-box { width: 60px; height: 60px; border-radius: 18px; display: flex; align-items: center; justify-content: center; font-size: 28px; }
  .bg-soft-green { background-color: #E8F5E9; color: #2E7D32; }
  .bg-soft-blue { background-color: #E3F2FD; color: #1565C0; }
  .bg-soft-orange { background-color: #FFF3E0; color: #EF6C00; }
  .bg-soft-purple { background-color: #F3E5F5; color: #7B1FA2; }
  .bg-soft-red { background-color: #FFEBEE; color: #C62828; }
  .stat-label { font-size: 14px; color: #9CA3AF; font-weight: 500; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 5px; }
  .stat-value { font-size: 28px; font-weight: 800; color: #1F2937; line-height: 1.1; }
  .chart-card { background: #FFFFFF; border-radius: 24px; padding: 30px; border: none; box-shadow: 0 4px 15px rgba(0,0,0,0.03); margin-bottom: 30px; }
  .chart-title { font-size: 18px; font-weight: 700; color: #111827; margin-bottom: 5px; }
  .chart-subtitle { font-size: 13px; color: #6B7280; margin-bottom: 20px; font-weight: 400; }
  .filter-box { background: #ffffff; padding: 24px; border-radius: 20px; border: 1px solid #edf2f7; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05); }
  .team-card { text-align: center; padding: 40px 20px; background: white; border-radius: 24px; border: 1px solid #F3F4F6; box-shadow: 0 4px 20px rgba(0,0,0,0.03); transition: all 0.4s ease; position: relative; overflow: hidden; }
  .team-card:hover { transform: translateY(-10px); box-shadow: 0 20px 40px rgba(0,0,0,0.08); border-color: ", col_primary, "; }
  .team-img { width: 130px; height: 130px; border-radius: 50%; object-fit: cover; margin-bottom: 20px; border: 5px solid #F3F4F6; transition: all 0.3s ease; }
  .team-card:hover .team-img { border-color: ", col_primary, "; transform: scale(1.05); }
  .team-name { font-size: 18px; font-weight: 700; color: #1F2937; margin-bottom: 5px; }
  .team-role { font-size: 14px; font-weight: 500; color: #6B7280; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 10px; }
  .team-org { font-size: 13px; color: #9CA3AF; font-style: italic; }
  .fa-users { color: #4CAF50 !important; } .fa-graduation-cap { color: #2196F3 !important; } .fa-heartbeat { color: #E91E63 !important; } .fa-house { color: #FF9800 !important; } .fa-chart-line { color: #9C27B0 !important; } .fa-map { color: #00BCD4 !important; }
  .fa-house-user, .fa-people-roof, .fa-person, .fa-baby, .fa-child, .fa-person-cane, .fa-user-slash, .fa-user-graduate, .fa-wheelchair { color: ", col_primary, " !important; opacity: 0.8; }
")

# ==============================================================================
# 4. USER INTERFACE (UI)
# ==============================================================================

ui <- page_navbar(
  theme = theme_modern,
  position = "fixed-top",
  title = tags$div(style="display:flex; align-items:center; justify-content:flex-start; gap: 12px; padding: 5px 0;", 
                   tags$img(src = logo_base64, style="height:45px; width:auto; filter: drop-shadow(0px 2px 3px rgba(0,0,0,0.3));"), 
                   tags$div(style="display:flex; flex-direction:column; justify-content:center; line-height:1.2;", 
                            tags$span("PROVINSI JAWA BARAT", style="font-weight:800; font-size:20px; color:white; letter-spacing: 0.5px;"), 
                            tags$span("Dashboard Data SUSENAS 2024", style="font-weight:500; font-size:12px; color:#e9ecef; letter-spacing: 1px; text-transform:uppercase;"))),
  tags$head(tags$style(app_css)),
  
  # --- BERANDA ---
  nav_panel("Beranda",
            div(style = "background-image: url('https://images.unsplash.com/photo-1542281286-9e0a16bb7366'); background-size: cover; background-position: center; background-repeat: no-repeat; width: 100vw; height: 100vh; position: relative; left: 50%; right: 50%; margin-left: -50vw; margin-right: -50vw; margin-top: -100px; padding-top: 80px; display: flex; align-items: center; justify-content: center; text-align: center; color: white;", 
                div(style = "background: rgba(0, 0, 0, 0.45); padding: 35px; border-radius: 18px; width: 65%; max-width: 900px; backdrop-filter: blur(3px); box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);", 
                    tags$h1("Dashboard SUSENAS Jawa Barat 2024", style="font-size:48px; font-weight:700; margin-bottom:10px; color:white !important;"), 
                    tags$p("Eksplorasi indikator sosial-ekonomi Jawa Barat (Modul KOR) dengan tampilan modern.", style="font-size:18px; line-height:1.6; margin-bottom:20px; color:white !important;"), 
                    tags$button("Mulai Jelajah", class="btn btn-light mt-2", style="padding:12px 28px; font-size:16px; border-radius:30px; color:#0D6E4F !important; font-weight:600;", onclick="document.getElementById('menu-section').scrollIntoView({ behavior: 'smooth' });"))),
            div(id="menu-section", class="container mt-5 mb-5", style="background: transparent; padding: 35px; position: relative; z-index: 2;", 
                tags$h2("Pilih Kategori Dashboard", style="font-weight:700; text-align:center; margin-bottom:25px; color:#0D6E4F !important;"), 
                layout_columns(col_widths = c(4,4,4), class="mb-4", 
                               div(class="card shadow-sm p-4 menu-card", onclick="document.querySelector('[data-value=\"Demografi\"]').click();", tags$div(style="font-size:36px;", icon("users")), tags$h4("Demografi", style="font-weight:600; margin-top:10px;"), p("Distribusi umur, gender, urban-rural.")), 
                               div(class="card shadow-sm p-4 menu-card", onclick="document.querySelector('[data-value=\"Pendidikan\"]').click();", tags$div(style="font-size:36px;", icon("graduation-cap")), tags$h4("Pendidikan", style="font-weight:600; margin-top:10px;"), p("Jenjang pendidikan, APS, RLS.")), 
                               div(class="card shadow-sm p-4 menu-card", onclick="document.querySelector('[data-value=\"Kesehatan\"]').click();", tags$div(style="font-size:36px;", icon("heartbeat")), tags$h4("Kesehatan", style="font-weight:600; margin-top:10px;"), p("Akses layanan, BPJS, keluhan."))), 
                layout_columns(col_widths = c(4,4,4), 
                               div(class="card shadow-sm p-4 menu-card", onclick="document.querySelector('[data-value=\"Perumahan\"]').click();", tags$div(style="font-size:36px;", icon("house")), tags$h4("Perumahan", style="font-weight:600; margin-top:10px;"), p("Air minum, sanitasi, bangunan.")), 
                               div(class="card shadow-sm p-4 menu-card", onclick="document.querySelector('[data-value=\"Ekonomi\"]').click();", tags$div(style="font-size:36px;", icon("chart-line")), tags$h4("Ekonomi (Proxy)", style="font-weight:600; margin-top:10px;"), p("Indikator Aset & Kesejahteraan.")), 
                               div(class="card shadow-sm p-4 menu-card", onclick="document.querySelector('[data-value=\"Tim\"]').click();", tags$div(style="font-size:36px;", icon("map")), tags$h4("Tim Pengembang", style="font-weight:600; margin-top:10px;"), p("Visualisasi spasial indikator."))))
  ),
  
  # --- DEMOGRAFI ---
  nav_panel("Demografi",
            div(class="container mt-4 mb-4",
                tags$h2("Analisis Demografi", style="font-weight:700; color:#0D6E4F !important; margin-bottom:20px;"),
                layout_columns(col_widths = c(4,4,4), class="mb-4", 
                               div(class="stat-card", div(div(class="stat-label", "Kepala Rumah Tangga"), div(class="stat-value", textOutput("val_krt"))), div(class="stat-icon-box bg-soft-green", icon("house-user"))), 
                               div(class="stat-card", div(div(class="stat-label", "Estimasi Total KK"), div(class="stat-value", textOutput("val_kk"))), div(class="stat-icon-box bg-soft-blue", icon("people-roof"))), 
                               div(class="stat-card", div(div(class="stat-label", "Total Penduduk"), div(class="stat-value", textOutput("val_individu"))), div(class="stat-icon-box bg-soft-orange", icon("person")))),
                layout_columns(col_widths = c(4,4,4), class="mb-5", 
                               div(class="stat-card", div(div(class="stat-label", "Balita (0-4 Thn)"), div(class="stat-value", textOutput("val_balita"))), div(class="stat-icon-box bg-soft-purple", icon("baby"))), 
                               div(class="stat-card", div(div(class="stat-label", "Anak (5-17 Thn)"), div(class="stat-value", textOutput("val_anak"))), div(class="stat-icon-box bg-soft-green", icon("child"))), 
                               div(class="stat-card", div(div(class="stat-label", "Lansia (>60 Thn)"), div(class="stat-value", textOutput("val_lansia"))), div(class="stat-icon-box bg-soft-blue", icon("person-cane")))),
                layout_columns(col_widths = c(4,4,4), class="mb-4", 
                               div(class="chart-card", div(class="chart-title", "Proporsi Wilayah"), highchartOutput("pie_kd_demo", height="250px")), 
                               div(class="chart-card", div(class="chart-title", "Status Pernikahan"), highchartOutput("pie_status_kawin", height="250px")), 
                               div(class="chart-card", div(class="chart-title", "Proporsi Gender"), highchartOutput("pie_gender", height="250px"))),
                layout_columns(col_widths = c(6,6), 
                               div(class="chart-card", div(class="chart-title", "Piramida Penduduk"), highchartOutput("pyramid", height="400px")), 
                               div(class="chart-card", div(class="chart-title", "Distribusi Kelompok Umur"), highchartOutput("hist_umur", height="400px")))
            )
  ),
  
  # --- PENDIDIKAN ---
  nav_panel("Pendidikan",
            div(class="container mt-4 mb-5",
                tags$h2("Analisis Pendidikan", style="font-weight:700; color:#0D6E4F !important; margin-bottom:20px;"),
                layout_columns(
                  col_widths = c(4,4,4), class="mb-5",
                  div(class="stat-card", div(div(class="stat-label", "Anak Tidak Sekolah"), div(class="stat-label", "(Usia 7-18 Thn)"), div(class="stat-value", textOutput("val_ats"), style="color: #C62828; font-size: 32px;")), div(class="stat-icon-box bg-soft-red", icon("user-slash"))),
                  div(class="stat-card", div(div(class="stat-label", "Pendidikan Tinggi"), div(class="stat-label", "(Usia 25+ Thn)"), div(class="stat-value", textOutput("val_pt"), style="color: #2E7D32; font-size: 32px;")), div(class="stat-icon-box bg-soft-green", icon("user-graduate"))),
                  div(class="stat-card", div(div(class="stat-label", "Disabilitas"), div(class="stat-label", "(Data KOR)"), div(class="stat-value", "N/A", style="color: #9CA3AF; font-size: 32px;")), div(class="stat-icon-box bg-soft-blue", icon("wheelchair")))
                ),
                layout_columns(col_widths = c(8,4), class="mb-4", 
                               div(class="chart-card", h4("Estimasi Rata-rata Lama Sekolah (RLS)"), highchartOutput("bar_rls", height="350px")), 
                               div(class="chart-card", h4("Jenjang Pendidikan Terakhir"), highchartOutput("pie_pendidikan", height="350px"))),
                div(class="chart-card", h4("Angka Partisipasi Sekolah (APS)"), highchartOutput("group_participation", height="350px"))
            )
  ),
  
  # --- KESEHATAN ---
  nav_panel("Kesehatan", div(class="container mt-4 mb-5", tags$h2("Analisis Kesehatan", style="font-weight:700; color:#0D6E4F !important; margin-bottom:20px;"), 
                             div(class="row", div(class="col-md-4 mb-4", div(class="card shadow-sm p-4", tags$h4("Kepemilikan JKN / BPJS", style="font-weight:600;"), highchartOutput("pie_jkn", height="350px"))), 
                                 div(class="col-md-8 mb-4", div(class="card shadow-sm p-4", tags$h4("Keluhan Kesehatan", style="font-weight:600;"), highchartOutput("bar_keluhan", height="350px")))), 
                             div(class="row", div(class="col-md-12 mb-4", div(class="card shadow-sm p-4", tags$h4("Peta Akses Kesehatan (Sebaran)", style="font-weight:600;"), leafletOutput("map_kes", height="500px")))))),
  
  # --- PERUMAHAN ---
  nav_panel("Perumahan", div(class="container mt-4 mb-5", tags$h2("Analisis Perumahan", style="font-weight:700; color:#0D6E4F !important; margin-bottom:20px;"), 
                             div(class="row", div(class="col-md-8 mb-4", div(class="card shadow-sm p-4", tags$h4("Kualitas Lantai", style="font-weight:600;"), highchartOutput("stack_lantai", height="400px"))), 
                                 div(class="col-md-4 mb-4", div(class="card shadow-sm p-4", tags$h4("Sumber Air Minum", style="font-weight:600;"), highchartOutput("pie_air", height="400px")))), 
                             div(class="row", div(class="col-md-4 mb-4", div(class="card shadow-sm p-4", tags$h4("Fasilitas Jamban", style="font-weight:600;"), highchartOutput("pie_jamban", height="350px"))), 
                                 div(class="col-md-4 mb-4", div(class="card shadow-sm p-4", tags$h4("Sumber Penerangan", style="font-weight:600;"), highchartOutput("bar_penerangan", height="350px"))), 
                                 div(class="col-md-4 mb-4", div(class="card shadow-sm p-4", tags$h4("Bahan Bakar Masak", style="font-weight:600;"), highchartOutput("bar_bakar", height="350px")))))),
  
  # --- EKONOMI (UPDATE: TAMBAH GRAFIK PEKERJAAN & PENGHASILAN) ---
  nav_panel("Ekonomi", div(class="container mt-4", h2("Analisis Ekonomi & Pekerjaan"), 
                           div(class="alert alert-success", icon("check-circle"), " Menampilkan Data Pengeluaran (Blok 43) dan Profil Pekerjaan Kepala Rumah Tangga."),
                           
                           # Baris 1: Grafik Sumber Penghasilan & Sektor Pekerjaan (BARU!)
                           layout_columns(col_widths=c(6,6), class="mb-4",
                                          div(class="chart-card", h4("Sumber Penghasilan Utama Rumah Tangga"), highchartOutput("pie_sumber_income")),
                                          div(class="chart-card", h4("Sektor Pekerjaan Kepala Rumah Tangga"), highchartOutput("pie_sektor_kerja"))
                           ),
                           
                           # Baris 2: Pengeluaran Rupiah
                           layout_columns(col_widths=c(8,4), highchartOutput("bar_pengeluaran"), highchartOutput("pie_pangan")),
                           leafletOutput("map_kalori", height="500px")
  )),
  
  # --- TIM ---
  nav_panel("Tim",
            div(class="container mt-5 mb-5",
                div(style="text-align: center; margin-bottom: 50px;", 
                    tags$h2("Tim Pengembang Dashboard", style="font-weight:800; color:#0D6E4F;"), 
                    tags$p("Tim yang berdedikasi dalam membangun visualisasi data SUSENAS Jawa Barat.", style="color:#6c757d; font-size:18px;")
                ),
                
                layout_columns(col_widths = c(4, 4, 4),
                               
                               # --- ANGGOTA 1 ---
                               tags$a(href="https://www.instagram.com/m_sunan25/", target="_blank", style="text-decoration: none; color: inherit;", 
                                      div(class="team-card", 
                                          # GANTI 'ini.jpg' DENGAN NAMA FILE FOTO ANDA DI FOLDER WWW
                                          tags$img(src="IMG_1353.jpeg", class="team-img"), 
                                          div(class="team-name", "Muh. Sunan"), 
                                          div(class="team-role", "Backend Developer"), 
                                          div(class="team-org", "Tim Dashboard Jabar")
                                      )
                               ),
                               
                               # --- ANGGOTA 2 ---
                               tags$a(href="https://www.instagram.com/claudian_dian/", target="_blank", style="text-decoration: none; color: inherit;", 
                                      div(class="team-card", 
                                          # GANTI 'mega.jpg' DENGAN NAMA FILE FOTO ANDA
                                          tags$img(src="ini.png", class="team-img"), 
                                          div(class="team-name", "Claudian Tangngilomban"), 
                                          div(class="team-role", "UI/UX Designer"), 
                                          div(class="team-org", "Tim Dashboard Jabar")
                                      )
                               ),
                               
                               # --- ANGGOTA 3 ---
                               tags$a(href="https://www.instagram.com/naufaliaalfryl/", target="_blank", style="text-decoration: none; color: inherit;", 
                                      div(class="team-card", 
                                          # GANTI 'fani.jpg' DENGAN NAMA FILE FOTO ANDA
                                          tags$img(src="IMG_20240327_164342.jpg", class="team-img"), 
                                          div(class="team-name", "Naufalia Alfriyal"), 
                                          div(class="team-role", "Data Analysis"), 
                                          div(class="team-org", "Tim Dashboard Jabar")
                                      )
                               )
                )
            )
  )
)

# ==============================================================================
# 5. SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # --- 0. SETUP TEMA GLOBAL HIGHCHARTS ---
  colors_pastel <- c("#4CAF50", "#81D4FA", "#FFD54F", "#9575CD", "#F06292", "#4DB6AC", "#A1887F")
  
  options(highcharter.theme = hc_theme_smpl(
    colors = colors_pastel, 
    chart = list(style = list(fontFamily = "Poppins"), backgroundColor = "transparent"), 
    title = list(style = list(color = "#333333", fontWeight = "bold")), 
    
    data_main <- reactive({ dummy }),
    data_eko  <- reactive({ dummy_eko }),
    
    # --- PENGATURAN LEGENDA (Di Bawah) ---
    legend = list(
      enabled = TRUE,
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal",
      itemStyle = list(color = "#333333", fontSize = "12px")
    ),
    
    # --- PENGATURAN GRAFIK (Pie Tanpa Panah) ---
    plotOptions = list(
      pie = list(
        allowPointSelect = TRUE,
        cursor = "pointer",
        dataLabels = list(enabled = FALSE), # Matikan Panah
        showInLegend = TRUE,                # Nyalakan Legenda
        innerSize = "60%" 
      ),
      column = list(
        dataLabels = list(enabled = FALSE)
      )
    )
  ))
  
  # --- DATA REAKTIF ---
  data_demo <- reactive({ dummy })
  data_pdd  <- reactive({ dummy })
  data_kes  <- reactive({ dummy })
  data_rmh  <- reactive({ dummy })
  data_eko  <- reactive({ dummy })
  
  # --- LOGIC DEMOGRAFI ---
  output$val_individu <- renderText({ total <- sum(data_demo()$Penimbang, na.rm=TRUE); format(round(total), big.mark=".", decimal.mark=",") })
  output$val_kk <- renderText({ total <- sum(data_demo()$Penimbang[data_demo()$Hubungan_KRT == 1], na.rm=TRUE); format(round(total), big.mark=".", decimal.mark=",") })
  output$val_krt <- renderText({ total <- sum(data_demo()$Penimbang[data_demo()$Hubungan_KRT == 1], na.rm=TRUE); format(round(total), big.mark=".", decimal.mark=",") })
  output$val_balita <- renderText({ total <- sum(data_demo()$Penimbang[data_demo()$age < 5], na.rm=TRUE); format(round(total), big.mark=".", decimal.mark=",") })
  output$val_anak <- renderText({ total <- sum(data_demo()$Penimbang[data_demo()$age >= 5 & data_demo()$age <= 17], na.rm=TRUE); format(round(total), big.mark=".", decimal.mark=",") })
  output$val_lansia <- renderText({ total <- sum(data_demo()$Penimbang[data_demo()$age >= 60], na.rm=TRUE); format(round(total), big.mark=".", decimal.mark=",") })
  
  output$pyramid <- renderHighchart({ 
    d <- data_demo(); d$age_group <- cut(d$age, breaks = seq(0, 100, 5), right = FALSE, include.lowest = TRUE)
    df_pyramid <- d %>% group_by(gender, age_group) %>% summarise(n = sum(Penimbang, na.rm=TRUE)) %>% ungroup()
    df_male <- df_pyramid %>% filter(gender == "Laki-laki") %>% mutate(n = -n)
    df_female <- df_pyramid %>% filter(gender == "Perempuan")
    cats <- levels(d$age_group)
    highchart() %>% hc_chart(type = "bar") %>% hc_title(text = "Piramida Penduduk") %>% hc_xAxis(categories = cats, reversed = FALSE) %>% hc_yAxis(title = list(text = "Jumlah Penduduk"), labels = list(formatter = JS("function(){ return Math.abs(this.value); }"))) %>% hc_plotOptions(series = list(stacking = "normal")) %>% hc_add_series(name = "Laki-laki", data = df_male$n, color = "#4CAF50") %>% hc_add_series(name = "Perempuan", data = df_female$n, color = "#FFD54F")
  })
  
  output$hist_umur <- renderHighchart({ hchart(data_demo()$age, color="#4CAF50", name="Usia") %>% hc_title(text="") })
  
  output$pie_kd_demo <- renderHighchart({ d <- data_demo() %>% group_by(urban_rural) %>% summarise(n = sum(Penimbang)); hchart(d, "pie", hcaes(name=urban_rural, y=n)) %>% hc_title(text="") })
  output$pie_status_kawin <- renderHighchart({ d <- data_demo() %>% group_by(status_kawin) %>% summarise(n = sum(Penimbang)); hchart(d, "pie", hcaes(name=status_kawin, y=n)) %>% hc_title(text="") })
  output$pie_gender <- renderHighchart({ d <- data_demo() %>% group_by(gender) %>% summarise(n = sum(Penimbang)); hchart(d, "pie", hcaes(name=gender, y=n)) %>% hc_title(text="") })
  
  # --- LOGIC PENDIDIKAN ---
  # ATS: Anak 7-18 tahun yang STATUSNYA BUKAN 2 (Masih Sekolah)
  output$val_ats <- renderText({ 
    d <- data_pdd() %>% filter(age >= 7, age <= 18)
    if(nrow(d) == 0) return("0%")
    pct <- (sum(d$Penimbang[d$partisipasi_sekolah != 2], na.rm=TRUE) / sum(d$Penimbang, na.rm=TRUE)) * 100
    paste0(round(pct, 2), "%") 
  })
  
  # PT: Usia 25+ yang Ijazahnya >= 18 (D1-S3)
  output$val_pt <- renderText({ 
    d <- data_pdd() %>% filter(age >= 25)
    if(nrow(d) == 0) return("0%")
    pct <- (sum(d$Penimbang[as.numeric(d$Ijazah_Tertinggi) >= 18], na.rm=TRUE) / sum(d$Penimbang, na.rm=TRUE)) * 100
    paste0(round(pct, 2), "%") 
  })
  
  output$bar_rls <- renderHighchart({ d <- data_pdd() %>% group_by(pendidikan) %>% summarise(avg_rls = weighted.mean(rls, Penimbang, na.rm=TRUE)); hchart(d, "column", hcaes(x=pendidikan, y=avg_rls), color="#4CAF50") %>% hc_yAxis(title=list(text="Rata-rata Lama Sekolah")) })
  output$pie_pendidikan <- renderHighchart({ d <- data_pdd() %>% group_by(pendidikan) %>% summarise(n = sum(Penimbang)); hchart(d, "pie", hcaes(name=pendidikan, y=n)) %>% hc_title(text="") })
  
  # APS: Angka Partisipasi Sekolah (Hitung yg status=2 dibagi total umur)
  output$group_participation <- renderHighchart({ 
    d <- data_pdd()
    # Helper function
    calc_aps <- function(min_a, max_a) {
      num <- sum(d$Penimbang[d$age >= min_a & d$age <= max_a & d$partisipasi_sekolah == 2], na.rm=T)
      den <- sum(d$Penimbang[d$age >= min_a & d$age <= max_a], na.rm=T)
      if(den==0) return(0) else return((num/den)*100)
    }
    
    aps_sd <- calc_aps(7, 12)
    aps_smp <- calc_aps(13, 15)
    aps_sma <- calc_aps(16, 18)
    aps_pt <- calc_aps(19, 23)
    
    aps_df <- tibble(Kelompok = c("SD (7-12)", "SMP (13-15)", "SMA (16-18)", "PT (19-23)"), Partisipasi = c(aps_sd, aps_smp, aps_sma, aps_pt))
    hchart(aps_df, "column", hcaes(x=Kelompok, y=Partisipasi), color="#4CAF50") %>% hc_yAxis(max=100) %>% hc_plotOptions(column=list(dataLabels=list(enabled=TRUE, format="{point.y:.1f}%")))
  })
  
  # --- LOGIC KESEHATAN ---
  output$pie_jkn <- renderHighchart({ d <- data_kes() %>% group_by(jkn) %>% summarise(n = sum(Penimbang)); hchart(d, "pie", hcaes(name=jkn, y=n)) %>% hc_title(text="") })
  output$bar_keluhan <- renderHighchart({ d <- data_kes() %>% group_by(keluhan) %>% summarise(n = sum(Penimbang)); hchart(d, "column", hcaes(x=keluhan, y=n), color="#4CAF50") })
  output$map_kes <- renderLeaflet({ d <- dummy %>% group_by(kabkota) %>% summarise(pct_sakit = sum(Penimbang[keluhan=="Ada Keluhan"])/sum(Penimbang)*100) %>% left_join(centroids, by="kabkota"); leaflet(d) %>% addTiles() %>% addCircleMarkers(lng=~lon, lat=~lat, radius=~sqrt(pct_sakit)*3, color="#4CAF50", fillColor="#4CAF50", fillOpacity=0.7, popup=~paste0("<b>",kabkota,"</b><br>Angka Kesakitan: ",round(pct_sakit,2),"%")) })
  
  # --- LOGIC PERUMAHAN ---
  output$stack_lantai <- renderHighchart({ d <- data_rmh() %>% group_by(lantai) %>% summarise(n = sum(Penimbang)) %>% mutate(p=round(n/sum(n)*100,2)); hchart(d, "column", hcaes(x=lantai, y=p, group=lantai)) })
  output$pie_air <- renderHighchart({ d <- data_rmh() %>% group_by(air_minum) %>% summarise(n = sum(Penimbang)); hchart(d, "pie", hcaes(name=air_minum, y=n)) %>% hc_title(text="") })
  output$pie_jamban <- renderHighchart({ d <- data_rmh() %>% group_by(jamban) %>% summarise(n = sum(Penimbang)); hchart(d, "pie", hcaes(name=jamban, y=n)) %>% hc_title(text="") })
  output$bar_penerangan <- renderHighchart({ d <- data_rmh() %>% group_by(penerangan) %>% summarise(n = sum(Penimbang)); hchart(d, "column", hcaes(x=penerangan, y=n), color="#4CAF50") })
  output$bar_bakar <- renderHighchart({ d <- data_rmh() %>% group_by(bakar_masak) %>% summarise(n = sum(Penimbang)); hchart(d, "column", hcaes(x=bakar_masak, y=n), color="#FFD54F") })
  
  # --- LOGIC EKONOMI (UPDATE) ---
  
  output$pie_sumber_income <- renderHighchart({
    d <- data_main() %>% 
      filter(Hubungan_KRT == 1) %>% 
      group_by(sumber_penghasilan) %>% 
      # PERBAIKAN 1: Tambah na.rm=TRUE (Anti Gagal Hitung)
      summarise(n = sum(Penimbang, na.rm = TRUE)) %>%
      # PERBAIKAN 2: Buang kategori yang kosong/tidak jelas
      filter(!is.na(sumber_penghasilan))
    
    hchart(d, "pie", hcaes(name=sumber_penghasilan, y=n)) %>% 
      hc_title(text="") 
  })
  
  output$pie_sektor_kerja <- renderHighchart({
    # Filter hanya Kepala Rumah Tangga (KRT)
    d <- data_main() %>% filter(Hubungan_KRT == 1) %>% group_by(sektor_pekerjaan) %>% summarise(n = sum(Penimbang))
    hchart(d, "pie", hcaes(name=sektor_pekerjaan, y=n))
  })
  
  output$bar_pengeluaran <- renderHighchart({
    d <- data_eko() %>% group_by(kabkota) %>% summarise(v = weighted.mean(Pengeluaran_PerKapita, Penimbang_Penduduk, na.rm=T)) %>% arrange(desc(v))
    hchart(d, "column", hcaes(x=kabkota, y=v)) %>% hc_title(text="Rata-rata Pengeluaran Perkapita (Rupiah/Bulan)") %>% hc_yAxis(labels=list(format="{value:,.0f}"))
  })
  output$pie_pangan <- renderHighchart({
    d <- data_eko() %>% summarise(Pangan = sum(Makanan_Rupiah*Penimbang_RT, na.rm=T), NonPangan = sum(NonMakanan_Rupiah*Penimbang_RT, na.rm=T)) %>% pivot_longer(cols=everything(), names_to="Tipe", values_to="Nilai")
    hchart(d, "pie", hcaes(name=Tipe, y=Nilai)) %>% hc_title(text="Proporsi Pengeluaran Jabar")
  })
  output$map_kalori <- renderLeaflet({
    d <- data_eko() %>% group_by(kabkota) %>% summarise(v = weighted.mean(Kalori_PerKapita, Penimbang_Penduduk, na.rm=T)) %>% left_join(centroids)
    pal <- colorNumeric("YlGn", d$v); leaflet(d) %>% addProviderTiles(providers$CartoDB.Positron) %>% addCircleMarkers(~lon,~lat, radius=10, color=~pal(v), popup=~paste(kabkota, round(v),"kkal")) %>% addLegend("bottomright", pal=pal, values=~v, title="Kalori/Kapita")
  })
}

shinyApp(ui, server)
