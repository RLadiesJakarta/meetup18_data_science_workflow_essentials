# Load the necessary packages
library(rvest)
library(tidyverse)
library(ggplot2)

# 1. Collect the data
# Define the URL
url <- "https://ptinklusif.kemdikbud.go.id/s/5/pt-siap-menerima-mhs-disabilitas"

# Read the HTML of the page
webpage <- read_html(url)

# Extract the table
table_data <- html_table(webpage)

# Assuming the table you want is the first (and perhaps only) one on the page
desired_table <- table_data[[1]]


# 2. Data cleaning and preparation
# Perguruan Tinggi
# Separate the original columns into two columns at the position where a lowercase 
# letter is followed by an uppercase letter
desired_table <- desired_table %>%
  separate(`Perguruan Tinggi`, into = c("perguruan_tinggi", "alamat"), 
           sep = "(?<=[a-z])(?=[A-Z])", 
           remove = FALSE, 
           fill = "right")

# Check
desired_table %>% filter(perguruan_tinggi %in% "Universitas Indraprasta PGRIJl Nangka No 59 Tanjung Barat Jagakarsa") %>% as.data.frame()
# There is one row that needs manual fix

# Manual fix
desired_table <- desired_table %>%
  mutate(perguruan_tinggi = case_when(perguruan_tinggi %in% "Universitas Indraprasta PGRIJl Nangka No 59 Tanjung Barat Jagakarsa" ~ "Universitas Indraprasta PGRI",
                                      TRUE ~ perguruan_tinggi),
         alamat = case_when(perguruan_tinggi %in% "Universitas Indraprasta PGRI" ~ "Jl Nangka No 59 Tanjung Barat Jagakarsa",
                            TRUE ~ alamat))

# Program Studi
# Separate the 'Name' into multiple columns at the position where a lowercase letter 
# is followed by an uppercase letter
desired_table <- desired_table %>% 
  mutate(Name_split = str_split(`Program Studi`, "(?<=[a-z])(?=[A-Z])")) %>% 
  unnest_wider(Name_split, names_sep = "_")

# Pre-processing further program studi 
# Create a data frame that has a nested structure of university, major and disability
desired_table <- desired_table %>% 
  select(perguruan_tinggi, alamat, Name_split_1:Name_split_44) %>% 
  # Pivots the Name_split columns to long format using pivot_longer, preserving perguruan_tinggi.
  pivot_longer(-c(perguruan_tinggi, alamat), names_to = "key", values_to = "value") %>% 
  # Creates a new column, group, to distinguish between 'prodi' (program) and 'disabilitas' (disability type) based on the original column names.
  mutate(group = if_else(str_detect(key, "[13579]$"), "prodi", "disabilitas")) %>%
  # Pivots the group column back to wide format using pivot_wider, preserving perguruan_tinggi.
  pivot_wider(names_from = group, values_from = value) %>%
  group_by(perguruan_tinggi, alamat) %>% 
  # Fill out prodi
  fill(prodi, .direction = "down") %>% 
  ungroup() %>% 
  # Filters out the rows where disabilitas is NA.
  filter(!is.na(disabilitas)) %>%
  # Separates the disabilitas values into different rows using separate_rows.
  separate_rows(disabilitas, sep = ", ") %>% 
  select(-key) 


# 3. Data exploration
uni_n <- desired_table %>% select(perguruan_tinggi) %>% unique() %>% nrow() # the number of universities
prodi_n <- desired_table %>% select(prodi) %>% unique() %>% nrow() # the number of majors
disabilitas_n <- desired_table %>% select(disabilitas) %>% unique() %>% nrow() # the number of disabilities


# 4. Data visualization
pt <- data.frame(info = c("universitas", "prodi", "disabilitas"),
                 total = c(72, 127, 23))

# Creating bar plot
p1 <- ggplot(pt, aes(y = info, x = total, fill = info)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Total", 
       y = "", 
       title = "Informasi penerimaan mahasiswa dengan disabilitas, 2023",
       caption = "Sumber data: Dikti") +
  theme(legend.position = "none") +
  geom_text(aes(label = total, x = total / 2), hjust = 0.5, vjust = 0.5, color = "white", fontface = "bold") 
  
p1

# We want to see the spatial distribution of universities accepting students with disabilities
# Get the name of kabupaten/kota
# Separate the two last words to each column
desired_table <- desired_table %>% 
  mutate(
    LastAddress = sapply(strsplit(alamat, " "), function(x) rev(x)[1]),
    SecondLastAddress = sapply(strsplit(alamat, " "), function(x) rev(x)[2])
  )

# Get the unique values of each address to understand if they contain province name or kabkota name or none
desired_table %>% select(SecondLastAddress, LastAddress) %>% unique() %>% as.data.frame()

# Identify rows with province name using the last two words
# Source: https://sig.bps.go.id/bridging-kode/index
desired_table <- desired_table %>% 
  mutate(provinceID = case_when(SecondLastAddress %in% "Kalimantan" ~ "61",
                              SecondLastAddress %in% "JAWA" ~ "32",
                              SecondLastAddress %in% "Badung" ~ "51",
                              SecondLastAddress %in% "Selong-Lombok" ~ "52",
                              SecondLastAddress %in% "TENGGARA" ~ "52",
                              SecondLastAddress %in% "Karangmalang" ~ "34",
                              LastAddress %in% "Semarang" ~ "33",
                              LastAddress %in% "Kupang" ~ "53",
                              LastAddress %in% "Cimahi" ~ "32",
                              LastAddress %in% "Surakarta" ~ "33",
                              LastAddress %in% "Cilaca" ~ "33",
                              LastAddress %in% "Banjarmasin" ~ "63",
                              LastAddress %in% "\nLampung" ~ "18",
                              LastAddress %in% "Depok-Sleman" ~ "34",
                              LastAddress %in% "Bogor" ~ "32",
                              LastAddress %in% "Palembang" ~ "16",
                              LastAddress %in% "Jatinangor" ~ "32",
                              LastAddress %in% "Banten" ~ "36"),
         kabID = case_when(SecondLastAddress %in% "Badung" ~ "5103",
                           SecondLastAddress %in% "Selong-Lombok" ~ "5203",
                           SecondLastAddress %in% "Karangmalang" ~ "3404",
                           LastAddress %in% "Semarang" ~ "3374",
                           LastAddress %in% "Kupang" ~ "5371",
                           LastAddress %in% "Cimahi" ~ "3277",
                           LastAddress %in% "Surakarta" ~ "3372",
                           LastAddress %in% "Cilaca" ~ "3301",
                           LastAddress %in% "Banjarmasin" ~ "6371",
                           LastAddress %in% "\nLampung" ~ "1801",
                           LastAddress %in% "Depok-Sleman" ~ "3404",
                           LastAddress %in% "Bogor" ~ "3201",
                           LastAddress %in% "Palembang" ~ "1671",
                           LastAddress %in% "Jatinangor" ~ "3273")) %>% 
  select(-c(LastAddress, SecondLastAddress))

# Fill out ID province and ID kabkota from the name and complete address
desired_table %>% filter(is.na(provinceID) | is.na(kabID)) %>% 
  select(perguruan_tinggi) %>% unique()

desired_table <- desired_table %>% 
  mutate(provinceID = case_when(perguruan_tinggi == "Akademi Tata Boga Bandung" ~ "32",
                                perguruan_tinggi == "IKIP PGRI Pontianak" ~ "61",
                                perguruan_tinggi == "Institut Seni Indonesia Surakarta" ~ "33",
                                perguruan_tinggi == "Institut Teknologi Bisnis AAS Indonesia" ~ "33",
                                perguruan_tinggi == "Institut Teknologi Nasional Bandung" ~ "32",
                                perguruan_tinggi == "Institut Teknologi Telkom Purwokerto" ~ "33",
                                perguruan_tinggi == "Politeknik Harapan Bangsa Surakarta" ~ "33",
                                perguruan_tinggi == "Politeknik Muhammadiyah Tegal" ~ "33",
                                perguruan_tinggi == "Sekolah Tinggi Ilmu Administrasi Amuntai" ~ "63",
                                perguruan_tinggi == "Sekolah Tinggi Pariwisata  Soromandi Bima" ~ "52",
                                perguruan_tinggi == "Sekolah Tinggi Teknologi Industri Padang" ~ "13",
                                perguruan_tinggi == "Sekolah Tinggi Informatika & Komputer Indonesia" ~ "35",
                                perguruan_tinggi == "STISIP Amal Ilmiah Yapis Wamena" ~ "94",
                                perguruan_tinggi == "STKIP Babunnajah Pandeglang" ~ "36",
                                perguruan_tinggi == "STKIP Syekh Manshur" ~ "36",
                                perguruan_tinggi == "STMIK Bina Mulia" ~ "31",
                                perguruan_tinggi == "STMIK Dharma Wacana Metro" ~ "18",
                                perguruan_tinggi == "STMIK Himsya" ~ "33",
                                perguruan_tinggi == "Universitas 45 Mataram" ~ "52",
                                perguruan_tinggi == "Universitas Airlangga" ~ "35",
                                perguruan_tinggi == "Universitas Al-Azhar Indonesia" ~ "31",
                                perguruan_tinggi == "Universitas Amal Ilmiah Yapis Wamena" ~ "94",
                                perguruan_tinggi == "Universitas Atma Jaya Yogyakarta" ~ "34",
                                perguruan_tinggi == "Universitas Ciputra Surabaya" ~ "35",
                                perguruan_tinggi == "Universitas Dhyana Pura" ~ "51",
                                perguruan_tinggi == "Universitas Dian Nuswantoro" ~ "33",
                                perguruan_tinggi == "Universitas Dwijendra" ~ "51",
                                perguruan_tinggi == "Universitas Esa Unggul" ~ "31",
                                perguruan_tinggi == "Universitas Gunung Rinjani" ~ "52",
                                perguruan_tinggi == "Universitas Indonesia" ~ "32",
                                perguruan_tinggi == "Universitas Indraprasta PGRI" ~ "31",
                                perguruan_tinggi == "Universitas Islam Al-azhar" ~ "52",
                                perguruan_tinggi == "Universitas Islam Bandung" ~ "32",
                                perguruan_tinggi == "Universitas Islam Makassar" ~ "73",
                                perguruan_tinggi == "Universitas Islam Malang" ~ "35",
                                perguruan_tinggi == "Universitas Islam Nusantara" ~ "32",
                                perguruan_tinggi == "Universitas Islam Sultan Agung" ~ "33",
                                perguruan_tinggi == "Universitas Jayabaya" ~ "31",
                                perguruan_tinggi == "Universitas Katolik Indonesia Atma Jaya" ~ "31",
                                perguruan_tinggi == "Universitas Katolik Indonesia Santu Paulus Ruteng" ~ "53",
                                perguruan_tinggi == "Universitas Kristen Duta Wacana" ~ "34",
                                perguruan_tinggi == "Universitas Kristen Maranatha" ~ "32",
                                perguruan_tinggi == "Universitas Lambung Mangkurat" ~ "63",
                                perguruan_tinggi == "Universitas Negeri Jakarta" ~ "31",
                                perguruan_tinggi == "Universitas Negeri Makassar" ~ "73",
                                perguruan_tinggi == "Universitas Negeri Malang" ~ "35",
                                perguruan_tinggi == "Universitas Negeri Semarang" ~ "33",
                                perguruan_tinggi == "Universitas Negeri Surabaya" ~ "35",
                                perguruan_tinggi == "Universitas Nusantara PGRI Kediri" ~ "35",
                                perguruan_tinggi == "Universitas Pamulang" ~ "36",
                                perguruan_tinggi == "Universitas Pendidikan Indonesia" ~ "32",
                                perguruan_tinggi == "Universitas Sultan Ageng Tirtayasa" ~ "36",
                                perguruan_tinggi == "Universitas Tanri Abeng" ~ "31",
                                perguruan_tinggi == "Universitas Tompotika Luwuk Banggai" ~ "72",
                                perguruan_tinggi == "Universitas Triatma Mulya" ~ "51",
                                perguruan_tinggi == "Universitas Trisakti" ~ "31",
                                perguruan_tinggi == "Universitas Veteran Bangun Nusantara" ~ "33",
                                perguruan_tinggi == "Universitas Widya Dharma" ~ "33",
                                TRUE ~ provinceID),
         kabID = case_when(perguruan_tinggi == "Akademi Tata Boga Bandung" ~ "3204",
                           perguruan_tinggi == "IKIP PGRI Pontianak" ~ "6171",
                           perguruan_tinggi == "Institut Seni Indonesia Surakarta" ~ "3372",
                           perguruan_tinggi == "Institut Teknologi Bisnis AAS Indonesia" ~ "3311",
                           perguruan_tinggi == "Institut Teknologi Nasional Bandung" ~ "3273",
                           perguruan_tinggi == "Institut Teknologi Telkom Purwokerto" ~ "3302",
                           perguruan_tinggi == "Politeknik Harapan Bangsa Surakarta" ~ "3372",
                           perguruan_tinggi == "Politeknik Muhammadiyah Tegal" ~ "3376",
                           perguruan_tinggi == "Sekolah Tinggi Ilmu Administrasi Amuntai" ~ "6308",
                           perguruan_tinggi == "Sekolah Tinggi Pariwisata  Soromandi Bima" ~ "5206",
                           perguruan_tinggi == "Sekolah Tinggi Teknologi Industri Padang" ~ "1371",
                           perguruan_tinggi == "Sekolah Tinggi Informatika & Komputer Indonesia" ~ "3573",
                           perguruan_tinggi == "STISIP Amal Ilmiah Yapis Wamena" ~ "9402",
                           perguruan_tinggi == "STKIP Babunnajah Pandeglang" ~ "3601",
                           perguruan_tinggi == "STKIP Syekh Manshur" ~ "3601",
                           perguruan_tinggi == "STMIK Bina Mulia" ~ "3172",
                           perguruan_tinggi == "STMIK Dharma Wacana Metro" ~ "1872",
                           perguruan_tinggi == "STMIK Himsya" ~ "3374",
                           perguruan_tinggi == "Universitas 45 Mataram" ~ "5271",
                           perguruan_tinggi == "Universitas Airlangga" ~ "3578",
                           perguruan_tinggi == "Universitas Airlangga" ~ "35",
                           perguruan_tinggi == "Universitas Al-Azhar Indonesia" ~ "3171",
                           perguruan_tinggi == "Universitas Amal Ilmiah Yapis Wamena" ~ "9402",
                           perguruan_tinggi == "Universitas Atma Jaya Yogyakarta" ~ "3471",
                           perguruan_tinggi == "Universitas Ciputra Surabaya" ~ "3578",
                           perguruan_tinggi == "Universitas Dhyana Pura" ~ "5103",
                           perguruan_tinggi == "Universitas Dian Nuswantoro" ~ "3374",
                           perguruan_tinggi == "Universitas Dwijendra" ~ "5171",
                           perguruan_tinggi == "Universitas Esa Unggul" ~ "3174",
                           perguruan_tinggi == "Universitas Gunung Rinjani" ~ "5203",
                           perguruan_tinggi == "Universitas Indonesia" ~ "3276",
                           perguruan_tinggi == "Universitas Indraprasta PGRI" ~ "3171",
                           perguruan_tinggi == "Universitas Islam Al-azhar" ~ "5271",
                           perguruan_tinggi == "Universitas Islam Bandung" ~ "3273",
                           perguruan_tinggi == "Universitas Islam Makassar" ~ "7371",
                           perguruan_tinggi == "Universitas Islam Malang" ~ "3573",
                           perguruan_tinggi == "Universitas Islam Nusantara" ~ "3273",
                           perguruan_tinggi == "Universitas Islam Sultan Agung" ~ "3374",
                           perguruan_tinggi == "Universitas Jayabaya" ~ "3172",
                           perguruan_tinggi == "Universitas Katolik Indonesia Atma Jaya" ~ "3171",
                           perguruan_tinggi == "Universitas Katolik Indonesia Santu Paulus Ruteng" ~ "5313",
                           perguruan_tinggi == "Universitas Kristen Duta Wacana" ~ "3471",
                           perguruan_tinggi == "Universitas Kristen Maranatha" ~ "3273",
                           perguruan_tinggi == "Universitas Lambung Mangkurat" ~ "6371",
                           perguruan_tinggi == "Universitas Negeri Jakarta" ~ "3172",
                           perguruan_tinggi == "Universitas Negeri Makassar" ~ "7371",
                           perguruan_tinggi == "Universitas Negeri Malang" ~ "3573",
                           perguruan_tinggi == "Universitas Negeri Semarang" ~ "3374",
                           perguruan_tinggi == "Universitas Negeri Surabaya" ~ "3578",
                           perguruan_tinggi == "Universitas Nusantara PGRI Kediri" ~ "3571",
                           perguruan_tinggi == "Universitas Pamulang" ~ "3674",
                           perguruan_tinggi == "Universitas Pendidikan Indonesia" ~ "3273",
                           perguruan_tinggi == "Universitas Sultan Ageng Tirtayasa" ~ "3604",
                           perguruan_tinggi == "Universitas Tanri Abeng" ~ "3171",
                           perguruan_tinggi == "Universitas Tompotika Luwuk Banggai" ~ "7202",
                           perguruan_tinggi == "Universitas Triatma Mulya" ~ "5103",
                           perguruan_tinggi == "Universitas Trisakti" ~ "3174",
                           perguruan_tinggi == "Universitas Veteran Bangun Nusantara" ~ "3311",
                           perguruan_tinggi == "Universitas Widya Dharma" ~ "3310",
                           TRUE ~ kabID))
                                
province_n <- desired_table %>% select(provinceID) %>% unique() %>% nrow() # the number of provinces
kab_n <- desired_table %>% select(kabID) %>% unique() %>% nrow() # the number of kabs

pt <- data.frame(info = c("universitas", "prodi", "jenis disabilitas", "provinsi", "kab"),
                 total = c(72, 127, 23, 17, 40))

p2 <- ggplot(pt, aes(y = info, x = total, fill = info)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Total", 
       y = "", 
       title = "Informasi penerimaan mahasiswa dengan disabilitas, 2023",
       caption = "Sumber data: Dikti") +
  theme(legend.position = "none") +
  geom_text(aes(label = total, x = total / 2), hjust = 0.5, vjust = 0.5, color = "white", fontface = "bold") 

p2

# Reordering the levels of "info" to improve the interpretation
pt$info <- factor(pt$info, levels = c("prodi", "universitas", "kab", "provinsi", "jenis disabilitas"))

p2 <- ggplot(pt, aes(y = info, x = total, fill = info)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Total", 
       y = "", 
       title = "Informasi penerimaan mahasiswa dengan disabilitas, 2023",
       caption = "Sumber data: Dikti") +
  theme(legend.position = "none") +
  geom_text(aes(label = total, x = total / 2), hjust = 0.5, vjust = 0.5, color = "white", fontface = "bold") 

p2

# Visualize the spatial distribution of campuses accepting students with disabilities
# Data preparation
desired_table2 <- desired_table[!duplicated(desired_table$perguruan_tinggi), ]

# Visualization
p3 <- desired_table2 %>% 
  group_by(provinceID) %>% 
  summarise(total = n()) %>% 
  arrange(total) %>% 
  mutate(provinceID = factor(provinceID, levels = provinceID)) %>% 
  ggplot(aes(y = total, x = provinceID, label = total)) +
  theme_minimal() +
  labs(x = "ID provinsi", 
       y = "Jumlah universitas", 
       title = "Penerimaan mahasiswa dengan disabilitas di setiap provinsi",
       subtitle = "2023",
       caption = "Sumber data: Dikti") +
  #geom_point() +
  geom_text(check_overlap = TRUE, hjust = -0.1) +
  coord_flip()

p3
  
  
# 5. Save the output
write.csv(desired_table, "`PerguruanTinggi.csv")
ggsave(filename = "p2.png", plot = p2, width = 6, height = 4, dpi = 300)
ggsave(filename = "p3.png", plot = p3, width = 6, height = 4, dpi = 300)
