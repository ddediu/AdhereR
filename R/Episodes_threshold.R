library(AdhereR)

data <- data.frame(ID = c(1,1),
                   event.date = c("2019-01-01", "2019-03-01"),
                   event.duration = c(30, 30),
                   medication.class = c("A", "A")
                   )

cma0 <- CMA0(data = data,
             ID.colname = "ID",
             event.date.colname = "event.date",
             event.duration.colname = "event.duration",
             medication.class.colname = "medication.class",
             date.format = "%Y-%m-%d")

plot(cma0)

episodes <- compute.treatment.episodes(data = data,
                                       ID.colname = "ID",
                                       event.date.colname = "event.date",
                                       event.duration.colname = "event.duration",
                                       medication.class.colname = "medication.class",
                                       maximum.permissible.gap = 1,
                                       maximum.permissible.gap.unit = "days",
                                       followup.window.duration = 100,
                                       date.format = "%Y-%m-%d",
                                       return.data.table = TRUE)

maximum.permissible.gap <- 0.5
new.episodes <- NULL

cumSumReset <- function(x, thresh) {
  ans    <- numeric()
  i      <- 0

  while(length(x) > 0) {
    cs_over <- cumsum(x)
    ntimes <- sum( cs_over <= thresh )

    if(ntimes==0) {
      ans <- c(ans, rep(i, length(x)))
      break
    }

    x      <- x[-(1:ntimes)]
    ans <- c(ans, rep(i, ntimes))
    i   <- i + 1
  }
  return(ans)
}

episodes[,cma := episode.duration/(episode.duration + end.episode.gap.days)]
episodes[,g := cumSumReset(cma, maximum.permissible.gap)]

new_episodes <- unique(episodes[, .(ID,
                             episode.start = first(episode.start),
                             total.episode.duration = sum(episode.duration),
                             total.gap.days = sum(end.episode.gap.days),
                             end.episode.gap.days = last(end.episode.gap.days),
                             permissible.gap.days = (sum(episode.duration) - sum(episode.duration)*maximum.permissible.gap)/maximum.permissible.gap,
                             episode.end = last(episode.end)),
                             by = g])


for(i in episodes$episode.ID){

  episode <- episodes[episode.ID == i]

  if(episode$episode.duration/(episode$episode.duration + episode$end.episode.gap.days) <= maximum.permissible.gap){

    permissible.gap.days <- (episode$episode.duration - episode$episode.duration*maximum.permissible.gap)/maximum.permissible.gap

    episode$episode.duration <- episode$episode.duration + permissible.gap.days
    episode$episode.end <- episode$episode.end + permissible.gap.days
    episode$end.episode.gap.days <- episode$end.episode.gap.days - permissible.gap.days

    new.episodes <- rbind(new.episodes, episode)

  } else if(i == tail(episodes$episode.ID,1)){

    new.episodes <- rbind(new.episodes, episode)

  } else {

    episodes[episode.ID == i + 1, `:=` (episode.start = episodes[episode.ID == i,episode.start],
                                        episode.duration = episode.duration + episodes[episode.ID == i,episode.duration],
                                        end.episode.gap.days = end.episode.gap.days + episodes[episode.ID == i,end.episode.gap.days])]
  }
}