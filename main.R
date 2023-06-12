# Memanggil library yang diperlukan tanpa
# menampilkan pesan pada konsol
suppressMessages({
  library(httr)
  library(dplyr) #penting untuk menggunakan pipeline
  library(stringr)
  library(ggplot2)
  library(ggpolypath)
  library(ggthemes)
  library(scales)
  library(tidyr)
  library(venn) #untuk membuat diagram venn
})

# Extract data daftar kata dari situs Wordle

# note: daftar kata ini sebenarnya ada dua: 

# -Berurut dari a - z (aahed - zymic); 
#  daftar kata yang bisa ditebak tapi tidak akan 
#  jadi jawaban. Pembuktiannya, daftar kata ini 
#  tidak beririsan dengan daftar jawaban 
#  dari WordFinder (ada 2 pengecualian)

# -Daftar acak setelah zymic (cigar - augur); 
#  daftar kata yang bisa jadi jawaban dan dipilih 
#  secara acak tiap hari buat jadi jawaban harian

url1 = "https://www.nytimes.com/games-assets/v2/wordle.1bc05d595206395cbc0c.js"
wordle_script_text = GET(url1) %>%
  content(as = "text", encoding = "UTF-8")

# Fungsi membersihkan data hasil substr pada data
# Harus ditulis sebelum eksekusi substr
data_bersih = function(daftar_kata){
  data_baru = daftar_kata %>% 
    str_remove_all("\"") %>%
    str_split(",") %>%
    data.frame() %>%
    select(kata = 1) %>%
    mutate(kata = toupper(kata))
  
  return(data_baru)
}

# Mengambil bagian daftar kata (seluruhnya) dari sumber Wordle
all_wordle_list = substr(
  wordle_script_text,
  # ambil dari nama variabel dalam script yang memuat daftar kata 
  # yaitu "ia=[". Mulai dari bracket di ujungnya (end) ...
  (str_locate(wordle_script_text, "(ia\\=\\[)")[,"end"]+1),
  
  # ...sampai nama variabel setelah bracket penutup daftar kata 
  # yaitu "],la". Ambil bracket di awalnya (start) antisipasi 
  # jika kata-nya bertambah. Dikurangi 1 agar pas.
  (str_locate(wordle_script_text, "(],la)")[,"start"]-1)) %>%
  data_bersih()

# Mengambil bagian daftar kata jawaban dari sumber Wordle 
# dimulai dari "cigar" sampai akhir.
wordle_list = substr(
  wordle_script_text,
  # Tidak ada pembatas yang jelas antara "zymic" dan "cigar" jadi
  # lokasi awal substr dimulai dari awal (start) kata "cigar"...
  (str_locate(wordle_script_text, "cigar")[,"start"]),
  
  # ...sampai nama variabel setelah bracket penutup daftar kata 
  # yaitu "],la". Ambil bracket di awalnya (start) antisipasi 
  # jika kata-nya bertambah.Dikurangi 1 agar pas.
  (str_locate(wordle_script_text, "(],la)")[,"start"]-1)) %>%
  data_bersih()

# Extract data daftar kata jawaban lama dari Wordfinder
# Hasil yang benar muncul jika url tidak mengacu pada
# laman/file spesifik (mis: index.html)
url2 = "https://wordfinder.yourdictionary.com/wordle/answers/"
answer_script_text = GET(url2) %>%
  content(as = "text", encoding = "UTF-8")

# Mengambil bagian daftar kata jawaban lama dari sumber WordFinder
# Ambil kata yang diawali "answer\", deretan symbol setelahnya 
# (regex \\W+), dan 1 kata string normal setelahnya (regex \\w+)
answer_list = str_extract_all(answer_script_text, 
                              '(answer\\\\\\W+\\w+)') %>% 
  data.frame() %>%
  select(kata = 1) %>%
  mutate(kata = str_replace(kata,'answer\\\\":\\\\"', ""))


# Pengecekan kata jawaban lama pada daftar kata Wordle
# yang tidak bisa jadi jawaban
checking_list = intersect(setdiff(all_wordle_list, wordle_list), answer_list)
if(count(checking_list) == 0) {
  cat("OK")
  rm(checking_list)
}

# Membuat daftar kata jawaban yang belum pernah muncul;
# Alias daftar kata dari Wordle yang belum ada di WordFinder
exclude_list = setdiff(wordle_list, answer_list)

# Fungsi menghitung jumlah penggunaan huruf di tiap kata
persen_huruf = function(daftar_kata){
  
  # Membuat data frame kosong untuk menampung daftar persentase
  persentase_huruf = data.frame(huruf = character(), persen = double())
  
  # Menguji semua huruf dalam alfabet 
  # dan menghitung jumlah kata yang mengandung huruf tersebut
  for(i in LETTERS){ #loop untuk semua karakter alfabet
    
    # Langsung menghitung jumlah kata dengan filter huruf alfabet
    hitung = count(filter(daftar_kata, grepl(i, unlist(daftar_kata[1]))))
    persentase = 100*hitung/count(daftar_kata) #hitung persentase
    
    # Memasukkan data huruf ke kolom pertama
    # dan data persentase ke kolom kedua
    # Saat memasukkan data huruf, sekalian membuat baris baru
    persentase_huruf[nrow(persentase_huruf)+1,1] = i
    persentase_huruf[nrow(persentase_huruf),2] = persentase
  }
  
  # Data disusun berdasarkan persentase terbesar
  return(arrange(persentase_huruf,desc(persen)))
}

# Fungsi membuat plot grafik dengan skala persen
# Parameter input utama adalah letter_list yang merupakan
# data persentase huruf output dari persen_huruf()
plot_huruf = function(letter_list, judul, subjudul, warna){
  
  # Variabel untuk membantu penghitungan dan tampilan grafik:
  
  frek = unlist(letter_list[2]) #Data nilai persen
  huruf0 = unlist(letter_list[1]) #Data huruf 
  
  # Menyesuaikan skala sumbu Y dengan data dan
  # memastikan semua label skala tampil dalam sumbu Y
  balance_max = ifelse(max(frek) < 20, 2, 5)
  maxscale = max(frek)+(balance_max - max(frek) %% balance_max) 
  
  # Patokan untuk posisi label berdasarkan nilai data
  patokan = maxscale*0.25 
  
  # Bagian utama plotting grafik
  # Definisi area plot dan label sumbu x,y
  print(ggplot(letter_list, 
               aes(x = reorder(huruf0, (-frek)), y = frek)) + 
          # Definisi isi data dan warna grafik
          geom_bar(fill = warna,
                   stat = 'identity') +
          # Definisi label data, posisinya berubah dari dalam batang
          # ke luar batang jika nilainya terlalu kecil
          # nilai persen dibulatkan ke 1 angka desimal
          geom_text(aes(label = sprintf("%1.1f%%", frek)),
                    vjust = 0.5,
                    hjust = if_else(frek < patokan, 1, 0),
                    size = 4,
                    nudge_x = 0.1,
                    nudge_y = if_else(frek < patokan, 
                                      0.06*patokan, 
                                      -0.06*patokan),
                    angle = 270)+
          # Definisi skala pada sumbu Y
          scale_y_continuous(labels = sprintf("%1.0f%%", 
                                              pretty(frek)),
                             breaks = pretty(frek),
                             expand = c(0,0), 
                             limits = c(0,maxscale)) +
          
          # Tema dan teks lainnya pada area plot
          theme_clean() +
          xlab("Huruf") +
          ylab("Persentase") +
          labs(caption = "Terinspirasi dari Arthur Holtz\nlinkedin.com/in/arthur-holtz/") +
          ggtitle(judul, subtitle = subjudul))
}

# Fungsi menghitung frekuensi huruf 
# dan langsung plotting grafik
persen_plot = function(daftar_kata, judul, subjudul, warna){
  
  # Memanggil fungsi membuat persentase huruf
  persentase_huruf = persen_huruf(daftar_kata)
  
  # Membuat grafik dari persentase
  # output langsung dalam file PNG
  ggsave(
    paste0(deparse(substitute(daftar_kata))
           ,"_bebas.png"),
    plot_huruf(persentase_huruf, judul, subjudul, warna),
    width = 6.5,
    height = 3.25,
    dpi = 1200
  )
  
  # Agar tampilan data pada tabel lebih mudah dibaca
  persentase_huruf[,2] = round(persentase_huruf[,2],
                               digits = 2) %>%
    format(nsmall = 2) %>%
    paste0("%")
  
  # Data disusun berdasarkan persentase terbesar
  return(arrange(persentase_huruf,desc(persen)))
}

# Fungsi memecah daftar kata jadi daftar huruf per posisi;
# Memisahkan tiap huruf ke kolom masing-masing
pecah_kata = function(daftar_kata){
  
  # Memastikan kata yang dievaluasi merupakan data frame
  # agar bisa diolah lebih lanjut
  if(!is.data.frame(daftar_kata)){
    daftar_kata = data.frame(kata = c(daftar_kata))  
  }
  
  # Menghitung panjang kata
  # Dengan ini, fungsi bisa digunakan untuk kata selain 5 huruf
  # Kata yang diambil hanya kata teratas saja yang dapat
  # mewakili kata-kata lain pada daftar
  huruf_per_kata = str_length(daftar_kata[1,1])
  
  # Memisahkan huruf-huruf ke kolom
  # Perhatikan bahwa fungsi separate menghasilkan 
  # 1 kolom kosong di awal yang akan dihapus lewat "select(-1)"
  # karenanya, parameter "into = "
  # diisi seakan kita memisahkan 1 kolom jadi 6
  letter_list = daftar_kata %>%
    separate(kata, sep = "", 
             into = as.character(1:(huruf_per_kata+1)), ) %>%
    select(-1)
  
  # Menyeragamkan nama kolom
  for(i in 1:huruf_per_kata) {
    new_name = paste0("No",i)
    names(letter_list)[i] = new_name
  }
  
  # Mengirimkan data kata yang sudah dipecah
  return(letter_list)
}

# Fungsi menghitung huruf dalam 1 kolom;
# kolom dikelompokkan dalam huruf, 
# dihitung persentasenya dan diurutkan
# berdasarkan persentase terbesar
persen_huruf_posisi = function(daftar_kata, colnum = 1){
  letter_list = daftar_kata %>%
    select(huruf = all_of(colnum)) %>% 
    group_by(huruf) %>%                
    # persen = 100 * frek/jumlah baris
    summarize(persen = 100*n()/nrow(daftar_kata)) %>% 
    arrange(desc(persen)) %>%
    data.frame()
  
  # Mengirimkan data persentase
  return(letter_list)
}

# Fungsi menghitung persentase huruf per posisi
# dan langsung membuat grafik masing-masing posisi
persen_plot_posisi = function(daftar_kata, judul, subjudul, warna){
  
  # Memecah daftar kata jadi daftar huruf  
  data_huruf = pecah_kata(daftar_kata)
  
  # Menyimpan data panjang kata/jumlah kolom
  colnum = ncol(data_huruf)
  
  # List untuk menyimpan semua hasil
  # agar nilainya bisa dikembalikan (return)
  list_semua = list()
  
  for (i in 1:colnum) {
    # Menghitung persentase huruf pada posisi ke-i
    # menggunakan persen_huruf_posisi()
    letter_list = persen_huruf_posisi(data_huruf, i)
    
    # Membuat plot grafik untuk posisi ke-i
    # output langsung dalam file PNG
    ggsave(
      paste0(deparse(substitute(daftar_kata))
             ,"_posisi_",i,".png"),
      plot_huruf(letter_list, paste0(
        judul," pada Posisi ke-",i), 
        paste0("Dari daftar kata ",subjudul), warna),
      width = 6.5,
      height = 3.25,
      dpi = 1200
    )
    
    # Agar tampilan data pada tabel lebih mudah dibaca
    letter_list[,2] = round(letter_list[,2],
                            digits = 2)  %>%
    format(nsmall = 2) %>%
    paste0("%")
      
    
    # Menampilkan data huruf di masing-masing posisi
    # pada console
    cat(paste0(judul," pada Posisi ke-",i,"\n"))
    # Hanya menampilkan 5 huruf terbanyak
    print(head(letter_list,5)) # List ga bisa pakai cat
    cat("\n")
    
    # Memasukkan data hasil pada list
    list_semua[[i]] = letter_list
  }
  
  # Mengirimkan data pada list
  return(list_semua)
}

# Fungsi menemukan kata terbaik Cara 1 (huruf bebas posisi)
tebakan_cara1 = function(daftar_kata, daftar_kata_sumber){
  
  # Membuat tabel persentase huruf
  data_huruf = persen_huruf(daftar_kata)
  
  # Mengambil daftar huruf dari persentase huruf untuk filter
  letter_filter = data_huruf[,1]
  
  # Membuat variabel untuk perubahan daftar kata yang difilter
  list_baru = daftar_kata_sumber
  
  # Variabel pembantu untuk menskip huruf
  j = 0
  
  # Menghitung panjang kata
  # dengan ini, fungsi bisa digunakan untuk kata selain 5 huruf
  # Kata yang diambil hanya kata teratas saja yang dapat
  # mewakili kata-kata lain pada daftar
  huruf_per_kata = str_length(daftar_kata[1,1])
  
  # Membuat data huruf dengan persentase tertinggi 
  # untuk ditampilkan
  data_persen_huruf = data.frame(huruf = character(), 
                                 persen = double())
  
  # Filter daftar kata dengan masing-masing huruf
  # Setelah difilter dengan satu huruf, list_baru akan diupdate
  # lalu difilter lagi dengan huruf lain
  for(i in unlist(letter_filter)){
    
    # Hasil filter langsung disimpan ke variabel baru
    list_baru0 = filter(list_baru, 
                        grepl(i, unlist(list_baru[1])))
    
    # Cek hasil filter di list_baru0:
    # jika tidak kosong (ada kata-kata yang cocok)
    # update list_baru, dan tambah j
    # Jika tidak,  loop dilanjutkan tanpa update list_baru
    # Jika j = jumlah huruf per kata, hentikan loop
    if((nrow(list_baru0)!= 0)&&(j!= huruf_per_kata)){
      j = j+1
      list_baru = list_baru0
      
      # Menyimpan data tiap huruf filter dan persentasenya
      # data persen langsung diformat dalam persentase
      data_persen_huruf[j,] = c(i,paste0(format(round(
        data_huruf[which(data_huruf[1] == i),2], 
        digits = 2),
        nsmall = 2),"%"))
      
      if (j == huruf_per_kata){
        break
      }
    }
  }
  
  # Menampilkan huruf-huruf terbanyak
  # yang bisa membentuk kata dan persentasenya
  cat("Huruf-huruf terbanyak yang tersedia\n")
  print(data_persen_huruf)
  
  # Menyeragamkan nama kolom
  names(list_baru)[1] = "kata"
  
  # Menampilkan Judul Hasil
  cat("\nDaftar Kata Hasil\n")
  
  # Mengirimkan data output
  return(list_baru)
}

# Fungsi menemukan kata terbaik Cara 2A (terikat posisi)
# Pendekatan dengan melakukan penyaringan berurut
tebakan_cara2A = function(daftar_kata, daftar_kata_sumber){
  
  # Membuat daftar kata yang dipisah per posisi
  # baik untuk sumber data huruf 
  # maupun sumber kata yang akan disaring
  data_huruf = pecah_kata(daftar_kata)
  data_kata = pecah_kata(daftar_kata_sumber)
  
  # Menyimpan data jumlah huruf per kata
  colnum = ncol(data_huruf)
  
  # Membuat data huruf dengan persentase tertinggi 
  # di tiap posisi
  max_per_pos = data.frame(huruf = character(), 
                           persen = double(), 
                           pos = integer())
  
  # Mulai loop untuk membuat daftar 
  # persentase huruf untuk semua posisi
  # dan mengisi data max_per_pos
  for (i in 1:colnum) {
    
    # Menghitung persentase huruf pada posisi ke-i
    letter_list = persen_huruf_posisi(data_huruf, i)
    
    # Memasukkan hasil dan persentase ke masing-masing 
    # tabel data per posisi
    data_huruf_pos = paste0("pos_",i)
    assign(data_huruf_pos, letter_list)
    
    # Memasukkan data persentase tertinggi 
    # ke data max_per_pos
    max_per_pos[i,] = 
      c(eval(sym(paste0("pos_",i)))[1,1],
        eval(sym(paste0("pos_",i)))[1,2],
        i)
  }

  # Mengurutkan data posisi dengan persentase tertinggi
  max_per_pos = arrange(max_per_pos,desc(persen)) %>%
    data.frame()
  
  # Memisahkan data untuk tampilan
  # dengan data untuk diproses
  max_per_pos_p = max_per_pos
  
  # Memberikan format persen 
  # pada data tampilan
  max_per_pos_p[,2] = 
    as.numeric(max_per_pos_p[,2]) %>%
    round(digits = 2)  %>%
    format(nsmall = 2) %>%
    paste0("%")
  
  # Mengecek isi max_per_pos 
  # (versi tampilan) yang sudah diurut
  print(max_per_pos_p)
  
  # Menyiapkan variabel-variabel pembantu untuk
  # loop penyaringan:
  
  # Menyalin data sumber kata ke data baru
  list_baru = data_kata
  
  j = 1 #jumlah penyaringan berhasil
  k = 1 #indeks urutan persentase huruf di posisi 
  
  # Mulai loop untuk penyaringan
  # Kata akan disaring terus hingga 
  # semua posisi sudah disaring
  # dan menghasilkan minimal 1 kata (tidak kosong)
  while(j <= nrow(max_per_pos)){
    
    # Indeks posisi huruf dari kata yang akan disaring
    # Perlu diconvert ke numeric untuk memastikan
    # angka bisa jadi indeks kolom
    idx = as.numeric(max_per_pos[j,3])
    
    # Tabel daftar persentase huruf pada posisi 
    # yang sedang disaring
    # diperlukan jika penyaringan dengan huruf teratas 
    # menghasilkan data kosong
    tabel_eval = eval(sym(paste0("pos_",idx)))
    
    # Mengambil huruf untuk penyaringan
    huruf_filter = unlist(max_per_pos[j,1])

    # Menyaring kata pada list_baru dengan 
    # huruf huruf_filter pada posisi idx
    hasil_filter_huruf = filter(list_baru, 
             list_baru[,idx] ==  huruf_filter)
    
    # Jika menghasillkan daftar kata kosong ...
    if(nrow(hasil_filter_huruf) == 0){
      
      # Mengisi k dengan indeks huruf 
      # yang sedang disaring pada tabel_eval 
      # sesuai posisi
      k = unlist(which(tabel_eval[1] ==  huruf_filter))
      
      # Jika huruf pengganti di suatu posisi habis
      # hentikan loop, berikan peringatan
      if(k >=  nrow(tabel_eval)){
        print("Kata tidak tersedia")
        break
      }
      
      # Mengubah isi max_per_pos 
      # dengan huruf terbanyak selanjutnya
      # pada posisi tersebut
      max_per_pos[j,] = c(
        tabel_eval[k+1,1], 
        tabel_eval[k+1,2], idx)
      
      # Mengurutkan data kembali sesuai persentase
      max_per_pos = arrange(max_per_pos,desc(persen))
      
      # Lanjutkan loop
      next
      
    }else{
      
      # Jika penyaringan menghasilkan 
      # daftar kata baru minimal 1 kata
      # update list_baru dengan 
      # data hasil penyaringan
      list_baru = hasil_filter_huruf
      
      # Update jumlah penyaringan berhasil
      j = j+1
      
    }
  }
  
  # Menggabungkan data dari 5 kolom huruf 
  # ke 1 kolom kata
  merge_str = unlist(list_baru[1])
  for(j in 2:colnum){
    merge_str = paste(merge_str, unlist(list_baru[j]))
  }
  
  # Membersihkan hasil merge 
  # dari separator dan memberi nama kolom
  merge_str = data.frame(
    str_replace_all(
      unlist(merge_str), " ", "")) %>%
    select(hasil = 1)
  
  # Update data hasil dengan kata yang sudah digabung
  list_baru = data.frame(merge_str)
  
  # Menyeragamkan nama kolom
  names(list_baru)[1] = "kata"
  
  # Menampilkan Judul Hasil
  cat("\nDaftar Kata Hasil\n")
  
  # Mengirimkan data output
  return(list_baru)
}

# Fungsi menemukan kata terbaik Cara 2B (terikat posisi)
# Pendekatan dengan menggunakan persentil
tebakan_cara2B = function(daftar_kata, 
                          daftar_kata_sumber, 
                          persentil = 0.954){
  
  # Memanggil fungsi memecah kata untuk 
  # daftar kata yang dicari huruf terbanyaknya
  # dan daftar kata yang akan disaring
  data_huruf = pecah_kata(daftar_kata)
  data_huruf_sumber = pecah_kata(daftar_kata_sumber)
  
  # Menghitung jumlah kolom/huruf
  colnum = ncol(data_huruf)
  
  # Variabel untuk menampung daftar kata hasil filter
  list_baru = data_huruf_sumber
  
  # Membuat data huruf dengan persentase tertinggi 
  # untuk ditampilkan
  data_persen_huruf = data.frame(huruf = character(), 
                                 persen = double())
  
  # Loop untuk mencari huruf terbanyak 
  # di masing-masing posisi
  # Huruf tersebut langsung digunakan untuk 
  # filter pada list_baru
  # Jumlah loop sesuai jumlah kolom/huruf per kata
  for (i in 1:colnum) {
    
    # Menghitung persentase huruf pada posisi ke-i
    letter_list = persen_huruf_posisi(data_huruf, i)
    
    # Persyaratan huruf terbanyak menggunakan persentil
    # nilainya tergantung parameter persentil
    # Nilai persen huruf yang sesuai persentil dihitung
    kuantil_x = quantile(unlist(letter_list[2]), 
                         probs = persentil)
    
    # Kemudian, huruf dengan nilai persen lebih besar dari
    # persentil disaring dan disimpan
    letter_filter = 
      letter_list[which(letter_list[2] > kuantil_x),1:2] 
                          
    # Membuat huruf-huruf filter dalam satu baris string
    # hanya untuk ditampilkan dalam tabel data
    merge_huruf = letter_filter[1,1]
    if(nrow(letter_filter[1])>1){
      for(x in 2:nrow(letter_filter[1])){
        merge_huruf = paste0(merge_huruf, ", " , 
                             letter_filter[x,1])
      }
    }
    
    # Memasukkan data huruf filter dan persentasenya
    data_persen_huruf[i,] = c(merge_huruf, 
                              paste0(format(round(
                                kuantil_x, 
                                digits = 2), 
                                nsmall = 2)
                                ,"%"))
    
    # Daftar kata disaring berdasarkan huruf-huruf filter
    list_baru = filter(list_baru,
                       unlist(list_baru[i]) 
                       %in% unlist(letter_filter[1]))
  }
  
  # Menggabungkan data dari 5 kolom huruf 
  # ke 1 kolom kata
  merge_str = unlist(list_baru[1])
  for(j in 2:colnum){
    merge_str = paste(merge_str, unlist(list_baru[j]))
  }
  
  # Membersihkan hasil merge 
  # dari separator dan memberi nama kolom
  merge_str = 
    data.frame(str_replace_all(
      unlist(merge_str), " ", "")) %>%
    select(hasil = 1)
  
  # Update data hasil dengan kata yang sudah digabung
  list_baru = data.frame(merge_str)
  
  # Menampilkan data huruf-huruf filter dan persentasenya
  cat(paste0("Huruf-huruf Filter di atas Persentil ke-", 
             persentil*100, "%\n"))
  print(data_persen_huruf)

  # Menyeragamkan nama kolom
  names(list_baru)[1] = "kata"
  
  # Menampilkan Judul Hasil
  cat("\nDaftar Kata Hasil\n")
  
  # Mengirimkan data output
  return(list_baru)
}

# Fungsi untuk menggambar diagram venn dari kata
# "bebas" -> huruf benar terlepas posisinya
# "posisi" -> huruf benar di posisi yang benar
gambar_venn = function(kata_eval, asal_data, mode = "bebas"){
  
  # Memastikan kata yang dievaluasi merupakan data frame
  # agar bisa diolah lebih lanjut
  if(!is.data.frame(kata_eval)){
    kata_eval = data.frame(kata = c(kata_eval))  
  }
  
  # Memecah kata evaluasi menjadi huruf per posisi
  huruf_kata = pecah_kata(kata_eval)

  # Menyiapkan asal_data sesuai mode
  # Pada mode posisi, asal_data perlu dipecah 
  # agar bisa di-filter sesuai posisi.
  # Pada mode bebas, tidak perlu dipecah
  if(mode ==  "posisi"){
    # Memecah data kata sumber menjadi huruf per posisi
    daftar_kata = pecah_kata(asal_data)
    
    # Menyimpan data jumlah huruf per kata
    colnum = ncol(daftar_kata)
    
  }else if(mode ==  "bebas"){
    
    # Agar penamaan variable tetap sama
    daftar_kata = asal_data
    
    # Menyimpan data jumlah huruf per kata
    colnum = str_length(daftar_kata[1,1])
    
  }
  
  # Pengecekan jika huruf lebih dari 7
  if(colnum > 7){
    return("Jumlah huruf terlalu banyak.")
  }
  
  # Untuk mengecek apakah jumlah huruf kata_eval
  # sama dengan huruf-huruf di daftar kata sumber
  if(ncol(huruf_kata)!= colnum){
    return("Jumlah huruf input kata 
           beda dengan daftar kata.")
  }
  
  # Menyiapkan string yang akan dieksekusi 
  # untuk membuat list sebagai input pada venn()
  fun_str = "list("
  
  # Memulai loop untuk penyaringan di tiap posisi huruf
  # Jumlah loop sesuai jumlah huruf dalam satu kata
  for (i in 1:colnum) {
    
    # Melakukan filter pada daftar kata tergantung mode
    # Data yang disaring selalu data sumber awal yang sama, 
    # bukan hasil penyaringan sebelumnya
    
    if(mode ==  "bebas"){
      
      # Pada mode bebas, penyaringan menggunakan grepl
      # untuk mencari kata yang mengandung huruf tertentu
      hasil_filter_huruf = 
        filter(daftar_kata, 
               grepl(huruf_kata[1,i], 
                     unlist(daftar_kata[1])))
      
      # Menyeragamkan variable di kedua mode
      final_list = unlist(hasil_filter_huruf)
      
      # Prefiks untuk pelabelan pada diagram
      prefiks = "_bebas"
      
    }else if (mode ==  "posisi"){
      
      # Pada mode posisi, penyaringan dilakukan 
      # dengan filter kolom
      hasil_filter_huruf = filter(daftar_kata, 
                                  unlist(daftar_kata[,i]) ==
                                    huruf_kata[1,i])
      
      # Merge 5 kolom jadi satu
      final_list = unlist(hasil_filter_huruf[,1])
      
      for(j in 2:colnum){
        final_list = paste(final_list, 
                           unlist(hasil_filter_huruf[,j]))
      }
      
      # Prefiks untuk pelabelan pada diagram
      prefiks = paste0("_pos_",i)
    }
    
    # Update daftar huruf untuk memperjelas
    # pelabelan pada diagram venn
    huruf_kata[1,i] = paste0(huruf_kata[1,i],
                             prefiks)
    
    # Memasukkan hasil merge sebagai 
    # data himpunan hasil penyaringan ke-i
    result_fil = paste0("hasil_",i)
    assign(result_fil, final_list) 
    
    # Update teks dengan variabel himpunan
    fun_str = ifelse(i == colnum, 
                     paste0(fun_str, result_fil, ")"),
                     paste0(fun_str, result_fil," , "))
  }
  
  # Mengeksekusi perintah teks untuk membuat list 
  venal_table = eval(parse(text = fun_str))
  
  # Memasukkan list dalam venn()
  venn(venal_table, snames = unlist(huruf_kata),
       ilab = TRUE, ilcs = 0.6, zcol = "style",
       ggplot = TRUE, box = FALSE) 
  
  # Fungsi tidak menghasilkan keluaran
  # selain diagram venn

}

# Fungsi menghitung semua probabilitas untuk
# semua jumlah kemunculan huruf benar 
prob_all = function(kata_eval, asal_data, mode = "bebas"){
  
  # Memastikan kata yang dievaluasi merupakan data frame
  # agar bisa diolah lebih lanjut
  if(!is.data.frame(kata_eval)){
    kata_eval = data.frame(kata = c(kata_eval))  
  }
  
  # Memecah kata evaluasi menjadi huruf per posisi
  huruf_kata = pecah_kata(kata_eval)
  
  # Menyiapkan asal_data sesuai mode
  # Pada mode posisi, asal_data perlu dipecah 
  # agar bisa di-filter sesuai posisi.
  # Pada mode bebas, tidak perlu dipecah
  if(mode ==  "posisi"){
    # Memecah data kata sumber menjadi huruf per posisi
    daftar_kata = pecah_kata(asal_data)
    
    # Menyimpan data jumlah huruf per kata
    colnum = ncol(daftar_kata)
    
    # Indeks untuk membantu penyaringan
    huruf_kata_idx = c(1:ncol(huruf_kata))
    
  }else if(mode ==  "bebas"){
    
    # Agar penamaan variable tetap sama
    daftar_kata = asal_data
    
    # Menyimpan data jumlah huruf per kata
    colnum = str_length(daftar_kata[1,1])
    
  }
  
  # Untuk mengecek apakah jumlah huruf kata_eval
  # sama dengan huruf-huruf di daftar kata sumber
  if(ncol(huruf_kata)!= colnum){
    return("Jumlah huruf input kata 
           beda dengan daftar kata.")
  }
  
  # Menyimpan nilai jumlah data pada daftar kata
  rsumber = nrow(daftar_kata)
  
  # Menyiapkan data jumlah kata per huruf minimal
  final_data1 = data.frame(set = character(), 
                           jumlah = numeric(),
                           persen = double())
  
  # Menyiapkan data jumlah kata per irisan
  final_data2 = data.frame(set = character(), 
                           jumlah = numeric(),
                           persen = double())
  
  # Variabel pembantu looping kombinasi
  l = 0
  
  # Looping untuk menghitung variasi probabilitas
  for(i in 1:length(huruf_kata)) {
    
    # Membuat kombinasi huruf dalam kata
    # yang bisa dibentuk dengan huruf
    # sebanyak i
    kombinasi = combn(huruf_kata,i)
    
    # Pada mode posisi, untuk memastikan
    # suatu huruf melakukan filter pada
    # posisi yang tepat, diperlukan indeks.
    # Ini bisa dilakukan dengan asumsi
    # urutan kombinasinya sama dengan 
    # variabel "kombinasi"
    if(mode == "posisi"){
      idx = combn(huruf_kata_idx, i)
    }
    
    # Menyiapkan string yang akan dieksekusi 
    # untuk membuat list sebagai input untuk union
    fun_str = "list("
    
    # Looping untuk filter menggunakan huruf
    # dari kombinasi lalu digabungkan dengan union
    for(j in 1:ncol(kombinasi)){
      
      # Untuk menyimpan data huruf-huruf beririsan
      judul_set = ""
      
      # Reset daftar kata ke sumber data
      list_baru = daftar_kata
      
      # Looping untuk filter huruf-huruf beririsan
      for(k in 1:nrow(kombinasi)){
        
        # Memilih huruf filter dari hasil kombinasi
        huruf = kombinasi[k,j]
        
        # Melakukan filter pada daftar kata sesuai mode
        # Untuk mensimulasikan irisan, data hasil filter
        # akan di-filter huruf selanjutnya yang
        # masih satu variasi kombinasi

        if(mode ==  "bebas"){
          # Pada mode bebas, penyaringan menggunakan grepl
          # untuk mencari kata yang mengandung huruf tertentu
          list_baru = filter(list_baru,
                             grepl(huruf, 
                                   unlist(list_baru[1])))
          

        }else if (mode ==  "posisi"){
          
          # Pada mode posisi, penyaringan dilakukan 
          # dengan filter kolom
          # idx_posisi memberikan informasi posisi
          # huruf yang di filter
          idx_posisi = idx[k,j]
          
          list_baru = filter(list_baru, 
                             unlist(list_baru[,idx_posisi]) ==
                               huruf)
        }
        
        # Pengaturan penamaan variasi kombinasi huruf
        if(k != 1){
          judul_set = paste0(judul_set, ".", huruf)
        } else {
          judul_set = paste0(judul_set,huruf)
        }
      }
      
      # Jika kombinasi huruf beririsan selesai,
      # tambah nilai l
      l = l+1
      
      # Menghitung probabilitas jumlah kata hasil irisan
      # terhadap total kata di daftar kata
      jumlah_set = nrow(list_baru)
      persen_set = round(100*jumlah_set/rsumber, 
                         digits = 2) %>%
        format(nsmall = 2) %>%
        paste0("%")
      
      # Memasukkan judul, jumlah, dan persentase irisan
      # ke dalam data
      final_data2[l,]  = c(judul_set, jumlah_set, persen_set) 
      
      # Menyimpan data kata hasil irisan
      # ke dalam data baru
      hasil_iris = paste0("iris_",j)
      
      # Pada mode bebas, data hasil irisan
      # langsung dimasukkan
      # Pada mode posisi, data digabungkan dulu
      # jadi satu kata
      if(mode ==  "bebas"){
        
        assign(hasil_iris, list_baru)
        
      } else if (mode ==  "posisi"){
      
        merge_str = unlist(list_baru[,1])
        
        for(m in 2:colnum){
          merge_str = paste(merge_str, unlist(list_baru[,m]))
        }
        
        assign(hasil_iris, merge_str)
      }
      
      # Update teks sintaks untuk list
      fun_str = ifelse(j == ncol(kombinasi), 
                       paste0(fun_str, hasil_iris, ")"),
                       paste0(fun_str, hasil_iris," , "))
    }
    
    # Mengeksekusi perintah teks untuk membuat list 
    list_union = eval(parse(text = fun_str))
    
    # Mengeksekusi perintah union untuk menggabungkan 
    # semua himpunan irisan yang dibentuk oleh 
    # jumlah huruf yang sama tanpa menghitung 
    # double data yang sama
    hasil_union = Reduce(union, list_union) %>%
      data.frame() # memastikan output berupa data frame
    
    # Menghitung probabilitas jumlah kata hasil union
    # terhadap total kata di daftar kata
    jumlah_set_union = nrow(hasil_union)
    persen_set_union = round(100*jumlah_set_union/rsumber, 
                             digits = 2) %>%
      format(nsmall = 2) %>%
      paste0("%")
    
    # Memasukkan judul, jumlah, dan persentase union
    # ke dalam data
    final_data1[i,] = c(paste("minimal ",i," huruf"),
                        jumlah_set_union,
                        persen_set_union)
  }
  
  # Menyimpan kedua data dalam list agar bisa
  # dikirim ke luar fungsi
  final_data = list(final_data1, final_data2)
  
  # Mengirimkan output fungsi
  return(final_data)
}

# Fungsi tambahan untuk mencatat cara yang digunakan
# untuk menemukan kata tebakan
# hanya berfungsi kalau daftar kata tebakan dimasukkan
# dalam variable data yang jadi parameter fungsi ini
# Output berupa data 2 kolom (kata dan cara)
keterangan_cara = function(data_eval){
  
  # Menyiapkan data output
  hasil = data.frame(kata = character(), cara = character())
  
  # Memberikan data di kolom cara untuk masing-masing kata
  # dalam data_eval
  # Fungsi deparse dan substitute akan sama menghasilkan
  # teks/string dari variabel yang dimasukkan 
  # jadi parameter data_eval
  # ex, jika fungsi ini dipanggil dengan:
  # keterangan_cara(grup_1_all) 
  # maka hasil deparse(substitute(data_eval))
  # adalah "grup_1_all"
  for(i in 1:nrow(data_eval)){
    hasil[i,] = c(data_eval[i,1],deparse(substitute(data_eval)))
  }
  
  # Mengirimkan data hasil
  return(hasil)
}

# Fungsi mencari seluruh kandidat kata terbaik
# dan mengevaluasi nilai probabilitasnya
nilai_akhir = function(daftar_kata, 
                       sumber_data, 
                       persentil = 0.954){
  
  # Menyiapkan data untuk menampung daftar 
  # kata hasil tebakan dari semua cara
  kata_eval = data.frame(kata = character())
  
  # Mencari kata kandidat dari berbagai cara
  # (bebas dan tepat posisi) dan menggunakan
  # exclude_list dan all_wordle_list 
  # (daftar_kata dan sumber_data)
  
  # Bebas posisi
  grup_1_all = tebakan_cara1(daftar_kata, sumber_data)
  grup_1_ex = tebakan_cara1(daftar_kata, daftar_kata)
  
  # Tepat posisi versi A (urutan probabilitas)
  grup_2A_all = tebakan_cara2A(daftar_kata, sumber_data)
  grup_2A_ex = tebakan_cara2A(daftar_kata, daftar_kata)
  
  # Tepat posisi versi B (persentil)
  grup_2B_all = tebakan_cara2B(daftar_kata, 
                               sumber_data, persentil)
  grup_2B_ex = tebakan_cara2B(daftar_kata, 
                              daftar_kata, persentil)
  
  # Memberikan keterangan cara untuk tiap kata
  # Output berupa data yang mengandung kata
  # serta keterangan cara tebakan
  grup_1_all = keterangan_cara(grup_1_all)
  grup_1_ex = keterangan_cara(grup_1_ex)
  
  grup_2A_all = keterangan_cara(grup_2A_all)
  grup_2A_ex = keterangan_cara(grup_2A_ex)
  
  grup_2B_all = keterangan_cara(grup_2B_all)
  grup_2B_ex = keterangan_cara(grup_2B_ex)
  
  # Menggabungkan daftar kata kandidat dari semua cara
  # Di sini menggunakan union_all agar kata 
  # yang sama dari cara yang berbeda 
  # tetap bisa ditampilkan
  kata_eval = Reduce(union_all, list(
    grup_1_all, grup_1_ex,
    grup_2A_all, grup_2A_ex,
    grup_2B_all, grup_2B_ex))
  
  # Menyiapkan variabel untuk menampung
  # data akhir
  tabel_hasil = data.frame()
  
  
  # Mengevaluasi semua kata tebakan 
  # menggunakan dua mode penghitungan 
  # memasukkan hasilnya pada data
  # Juga membuat visualisasi diagram venn
  # untuk masing-masing metode
  for(i in 1:nrow(kata_eval)){
    
    # Membuat visualisasi diagram venn
    # dari kata yang dievaluasi pada
    # posisi huruf bebas
    # OUtput langsung jadi file PNG
    ggsave(
      paste0("venn_",
             kata_eval[i,1],
             "_bebas.png"),
      gambar_venn(kata_eval[i,1], 
                daftar_kata, "bebas"),
      width = 3.25,
      height = 3.25,
      dpi = 1200
    )
    
    # Menghitung semua probabilitas kata
    # pada posisi huruf bebas
    # Hanya mengambil data primer dari 
    # fungsi prob_all() kolom 3 (data persen)
    # Karena datanya berupa string
    # dan ada tanda %, data diolah dulu
    # untuk menghapus tanda %,
    # convert ke numeric dan transpose
    # agar data menjadi satu baris
    prob_bebas = prob_all(kata_eval[i,1], 
                          daftar_kata, 
                          "bebas")[[1]][3] %>%
      unlist() %>% # agar bisa str_remove()
      str_remove("%") %>% # menghapus %
      as.numeric() %>% # convert data
      t() %>% # transpose
      data.frame() # agar dapat diolah lebih lanjut
    
    # Memberikan prefiks pada nama kolom
    names(prob_bebas) = paste0("beb_",
                               1:ncol(prob_bebas))
    
    
    # Selanjutnya melakukan hal yang sama
    # pada posisi huruf tepat
    ggsave(
      paste0("venn_",
             kata_eval[i,1],
             "_posisi.png"),
      gambar_venn(kata_eval[i,1], 
                  daftar_kata, "posisi"),
      width = 3.25,
      height = 3.25,
      dpi = 1200
    )
    
    prob_posisi = prob_all(kata_eval[i,1], 
                           daftar_kata, 
                           "posisi")[[1]][3] %>%
      unlist() %>%
      str_remove("%") %>%
      as.numeric() %>%
      t() %>%
      data.frame()
    
    names(prob_posisi) = paste0("pos_",
                               1:ncol(prob_posisi))
    
    # Menggabungkan data dari posisi bebas
    # dengan posisi tepat
    gabungan = merge(prob_bebas , prob_posisi)
    
    # Menjumlahkan semua nilai persen
    # Ingat, data gabungan hanya 1 baris
    total_gabung = rowSums(gabungan)
    
    # Menggabungkan data kata dan keterangan cara
    # dengan gabungan data persentase
    # dan dengan total jumlah persentase
    # Perlu diingat bahwa merge() hanya bisa
    # menggabungkan 2 data
    gabungan_baru = 
      merge(kata_eval[i,],
            merge(gabungan, 
                  c(total_gabung))) # harus vektor
    
    # Memasukkan data gabungan baru ke 
    # tabel_hasil, baris per baris. 
    # Karena tabel_hasil awalnya kosong,
    # baris pertama tidak pakai indeks
    if(i == 1){
      tabel_hasil = gabungan_baru
    } else {
      tabel_hasil[i,] = gabungan_baru
    }
  }
  
  # Jumlah huruf pada kata
  jumlah_huruf = str_length(kata_eval[1,1])
  
  # Nilai tengah jumlah huruf sebagai
  # acuan salah satu parameter urutan
  # pada kata 5 huruf, tengah adalah 3
  tengah = (jumlah_huruf%/%2)+(jumlah_huruf%%2)
  
  # Jumlah kolom tabel_hasil
  colnum = ncol(tabel_hasil)
  
  # Mengganti nama kolom total
  names(tabel_hasil)[colnum] = "total"
  
  # Menyusun data hasil akhir berdasarkan nilai total,
  # beb_1, pos_1, beb_tengah, dan pos_tengah
  tabel_hasil = arrange(tabel_hasil,
                         across(c(total, beb_1, pos_1,
                                  paste0("beb_",tengah), 
                                  paste0("pos_",tengah)),
                                desc))
  
  # Menyeragamkan format data-data persen
  tabel_hasil[,3:colnum] = 
    format(unlist(tabel_hasil[,3:colnum]), nsmall=2) %>%
    paste0("%")
  
  # Mengirimkan hasil
  return(tabel_hasil)
}