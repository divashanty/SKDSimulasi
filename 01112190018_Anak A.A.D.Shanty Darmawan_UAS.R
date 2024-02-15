#projek konser
library(simmer)
library(parallel)
rm(list = ls())


festival <- simmer("paralel")


arrive = function(){
  rexp(1, rate = 1.2)#ini keluarnya dia malah banyak banget orangnya, orang maks itu cuma mau 300 orang. cari cara untuk stop di orang 500.
}


ticketing = function(){
  rexp(1, rate = 0.8)
}

merch = function(){
  rexp(1, rate = 0.5)
}

checking = function(){
  rexp(1, rate = 0.7)
}

meetgreet = function(){
  rexp(1, rate = 1)
}


attendee <- trajectory("Jalur Konser") %>%
  log_("Welcome to HITC Jakarta 2021!") %>%
  set_attribute("start_time", function(){now(festival)}) %>%
  select(c("loket1", "loket2"), policy = "random") %>%  #nanti coba pakein priority kalo pake priority bisa fansign
  seize_selected() %>%
  log_(function(){paste("Sedang antri beli tiket: ", now(festival) - get_attribute(festival, "start_time"))}) %>%
  timeout(ticketing) %>%
  release_selected() %>%
  log_("Mohon barang bawaan di check terlebih dahulu sebelum memasuki venue") %>%
  select(c("counter1", "counter2", "counter3"), policy = "random") %>%
  seize_selected() %>%
  log_(function(){paste("Sedang antri checking: ", now(festival) - get_attribute(festival, "start_time"))}) %>%
  timeout(checking) %>%
  release_selected() %>%
  select(c("booth1", "booth2", "booth3"), policy = "random") %>%
  seize_selected() %>%
  log_(function(){paste("Sedang antri merch: ", now(festival) - get_attribute(festival, "start_time"))}) %>%
  timeout(merch) %>%
  release_selected() %>%
  log_("Selamat menonton!") %>%
  log_(function(){
    if(get_capacity(festival, "pintu") == 0)
      "Maaf festivalnya belum dimulai."
    else "Konsernya sudah dimulai. Silahkan masuk!"
  }) %>%
  seize("pintu") %>%
  log_("Sudah bisa masuk!") %>%
  release("pintu") %>%
  select(c("artis1", "artis2", "artis3"), policy = "random") %>%
  seize_selected() %>%
  timeout(meetgreet) %>%
  release_selected() %>%
  log_("Happy watching! <3")

openTime = 150 #berapa lama dia di sistem si doorman
door_schedule = schedule(c(0, openTime), c(0, 180))

doorman <- trajectory() %>%
  timeout(openTime) %>%
  log_("Sudah bisa masuk! Selamat menonton!")

festival %>%
  add_resource("loket1", 1) %>%
  add_resource("loket2", 1) %>%
  add_resource("counter1", 1) %>%
  add_resource("counter2", 1) %>%
  add_resource("counter3", 1) %>%
  add_resource("booth1", 1) %>%
  add_resource("booth2", 1) %>%
  add_resource("booth3", 1) %>%
  add_resource("artis1", 1) %>%
  add_resource("artis2", 1) %>%
  add_resource("artis3", 1) %>%
  add_resource("pintu", door_schedule) %>%
  add_generator("penonton", attendee, arrive) %>%
  add_generator("doorman", doorman, at(150)) %>%
  run(180) %>% invisible


data = get_mon_arrivals(festival, per_resource = TRUE) 
data


table(data$resource)


hasil <- festival %>%
  get_mon_arrivals() %>%
  transform(waiting_time = end_time - start_time - activity_time)
  paste("Rata-rata waktu penonton sebanyak", sum(hasil$finished), "melakukan aktivitas adalah", round(mean(hasil$activity_time),2), "menit.") %>% unlist()


quantile(data$activity_time, probs = c(0.05, 0.95))
