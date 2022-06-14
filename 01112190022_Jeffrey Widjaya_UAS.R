library(simmer)
library(dplyr)
set.seed(01112190022)

Airport = simmer("Airport")

#Kedatangan Pelanggan dengan rate 1 orang per 2 menit
Arrival = function(){rexp(1,0.5)}

#Service time asumsi
CekVaksin = function(){runif(1,2,4)}
TesAntigen = function(){runif(1,10,15)}
Branch.1 = function(){runif(1,0,1) < 0.3}
Branch.2 = function(){runif(1,0,1) < 0.2}
TidakCovid = function(){runif(1,1,6)}
Covid = function(){runif(1,1,6)}

#Alur perjalanan
traj.cv = trajectory() %>%
  log_("Penumpang Pesawat Cek Vaksinasi") %>%
  seize("Cek Vaksin") %>%
  timeout(CekVaksin) %>%
  release("Cek Vaksin")

traj.pcv = trajectory() %>%
  log_("Proses Cek Vaksinasi")
  seize("Proses Cek Vaksin")

traj.sv = trajectory() %>%
  log_("Penumpang Pesawat Sudah divaksin") %>%
  seize("Sudah Vaksin") %>%
  timeout(TidakCovid) %>%
  release("Sudah Vaksin")

traj.ta = trajectory() %>%
  log_("Penumpang Pesawat Tes Antigen") %>%
  seize("Tes Antigen") %>%
  timeout(TesAntigen) %>%
  release("Tes Antigen")
  
traj.ne = trajectory() %>%
  log_("Hasil Tes Antigen Negatif") %>%
  seize("Negatif Covid") %>%
  timeout(TidakCovid) %>%
  release("Negatif Covid") 

traj.po = trajectory() %>%
  log_("Hasil Tes Antigen Positif") %>%
  seize("Positif Covid") %>%
  timeout(Covid) %>%
  release("Positif Covid")
  
Pulang = trajectory() %>%
  log_("Penumpang Pesawat di Pulangkan")

Lanjut = trajectory() %>%
  log_("Penumpang Pesawat Melanjutkan Prosedur Penerbangan")

main.traj = trajectory () %>%
  branch(Branch.1, continue = TRUE, join(traj.ta) %>% 
           branch(Branch.2, continue = TRUE, join(traj.po)) %>% 
           join(Pulang)) %>%
  join(traj.ne) %>% join(Lanjut)

#Aturan Sistem
Airport %>%
  add_resource("Cek Vaksin",1) %>%
  add_resource("Sudah Vaksin",1) %>%
  add_resource("Tes Antigen",1) %>%
  add_resource("Negatif Covid",1) %>%
  add_resource("Positif Covid",1) %>%
  add_generator("Arrival", main.traj, Arrival) %>%
  run(300) %>%
  get_mon_arrivals()


result1 = get_mon_attributes(Airport)
result2 = get_mon_arrivals(Airport,per_resource = TRUE)

#Filter Data
R3 = filter(result2, resource != "Tes Antigen")

#Total Penumpang Pesawat
ppesawat = count(R3)

#Filter data hanya yang Tidak Covid
result4 = filter(R3, resource == "Negatif Covid")
R4 = count(result4)

#Filter data hanya yang Positif Covid
result5 = filter(R3, resource == "Positif Covid")
R5 = count(result5)

#Tingkat probabilitas penumpang pesawat yang tidak terkena covid 
#dalam penerbangan yang akan dijalankan dalam 5 jam
R4/ppesawat

#Tingkat probabilitas penumpang pesawat yang terkena covid 
#dalam penerbangan yang akan dijalankan dalam 5 jam
R5/ppesawat

#Rata rata penumpang pesawat melakukan aktifitas adalah
MAct = round(mean(R3$activity_time),3)
MAct

#Confidence Interval dari rata rata dan aktifitas yang dilakukan penumpang pesawat
quantile(R3$activity_time, prob=c(0.05,0.95))
