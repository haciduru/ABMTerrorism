
setwd('/Users/hd/OneDrive - SUNY Brockport/Documents/ProjectBox/ABM Terrorism')
n_agents = 1000
p_mil = .02
p_ins = .005
p_soc = .02

mil = 'dodgerblue1'
ins = 'firebrick3'
soc = 'darkorchid1'

results = data.frame(
  n_agents = NA,
  p_mil = NA,
  p_ins = NA,
  p_soc = NA,
  coef_intercept = NA,
  coef_tx = NA,
  se_intercept = NA,
  se_tx = NA,
  t_intercept = NA,
  t_tx = NA,
  n_ter_attack = NA,
  n_ter_kills = NA,
  n_ins_captur = NA,
  n_pol_missed = NA,
  niter = NA,
  n_civ1 = NA,
  n_soc1 = NA,
  n_mil1 = NA,
  n_ins1 = NA,
  n_civ2 = NA,
  n_soc2 = NA,
  n_mil2 = NA,
  n_ins2 = NA
)

r = 0
while (r < 100) {
  r = r + 1
  
  n_ter_attack = 0
  n_ter_kills = 0
  n_ins_captur = 0
  n_pol_missed = 0
  
  agents = init.agents(n_agents)
  invisible(lapply(sample(agents, ceiling(n_agents * p_mil)), function(a) { class(a) = mil; a$sup = .02; a$res = 1 }))
  invisible(lapply(sample(agents, ceiling(n_agents * p_ins)), function(a) { class(a) = ins; a$sup = .98 }))
  invisible(lapply(sample(agents, ceiling(n_agents * p_soc)), function(a) { class(a) = soc; a$sup = .02; a$res = 1 }))
  init.intel(agents)
  
  pre_sup = unlist(lapply(agents, function(a) if (class(a) == civ) a$sup else NULL))
  
  n_civ1 = sum(unlist(lapply(agents, function(a) if (class(a) == civ) 1 else 0)))
  n_soc1 = sum(unlist(lapply(agents, function(a) if (class(a) == soc) 1 else 0)))
  n_mil1 = sum(unlist(lapply(agents, function(a) if (class(a) == mil) 1 else 0)))
  n_ins1 = sum(unlist(lapply(agents, function(a) if (class(a) == ins) 1 else 0)))
  
  pre_nums = table(unlist(lapply(agents, function(a) class(a))))
  
  nins = sum(unlist(lapply(agents, function(a) if (class(a) == ins) 1 else 0)))
  j = 0
  while (j < 10 & nins > 0) {
    tic(paste('Run ', r, ', Macro iteration ', j, sep=''))
    j = j + 1
    for (i in 1:365) {
      # tic(i)
      iterate()
      # toc()
    }
    agents = prune(agents)
    nins = sum(unlist(lapply(agents, function(a) if (class(a) == ins) 1 else 0)))
    
    # print(table(unlist(lapply(agents, function(a) length(a$nei)))))
    # print(table(unlist(lapply(agents, function(a) class(a)))))
    # cat('\n# of terrorists captured:\t\t', n_ins_captur,
    #     '\n# of times police missed target:\t', n_pol_missed,
    #     '\n# of terrorist attacks:\t\t\t', n_ter_attack,
    #     '\n# of deadly terrorist attacks:\t\t', n_ter_kills,'\n\n')
    # hist(unlist(lapply(agents, function(a) if (class(a) != mil & class(a) != soc) a$sup)), xlab = 'support', main = 'support levels', breaks = 20)
    # display(agents)
    prune(agents)
    toc()
  }
  
  post_sup = unlist(lapply(agents, function(a) if (class(a) == civ) a$sup))
  
  n_civ2 = sum(unlist(lapply(agents, function(a) if (class(a) == civ) 1 else 0)))
  n_soc2 = sum(unlist(lapply(agents, function(a) if (class(a) == soc) 1 else 0)))
  n_mil2 = sum(unlist(lapply(agents, function(a) if (class(a) == mil) 1 else 0)))
  n_ins2 = sum(unlist(lapply(agents, function(a) if (class(a) == ins) 1 else 0)))
  
  df_sup = rbind(data.frame(sup = pre_sup, tx = 0), data.frame(sup = post_sup, tx = 1))
  model = summary(lm(sup ~ tx, data = df_sup))
  
  results = 
    rbind(results,
          data.frame(
            n_agents = n_agents,
            p_mil = p_mil,
            p_ins = p_ins,
            p_soc = p_soc,
            coef_intercept = model$coefficients[1,1],
            coef_tx = model$coefficients[2,1],
            se_intercept = model$coefficients[1,2],
            se_tx = model$coefficients[2,2],
            t_intercept = model$coefficients[1,3],
            t_tx = model$coefficients[2,3],
            n_ter_attack = n_ter_attack,
            n_ter_kills = n_ter_kills,
            n_ins_captur = n_ins_captur,
            n_pol_missed = n_pol_missed,
            niter = j,
            n_civ1 = n_civ1,
            n_soc1 = n_soc1,
            n_mil1 = n_mil1,
            n_ins1 = n_ins1,
            n_civ2 = n_civ2,
            n_soc2 = n_soc2,
            n_mil2 = n_mil2,
            n_ins2 = n_ins2
          )
        )
  row.names(results) = NULL
  results = results[!is.na(results$n_agents), ]
  dt = paste(Sys.time())
  dt = substr(dt, 1, 13)
  write.csv(results, paste('results', dt, '.csv', sep = ''), row.names = F)
}

dt = paste(Sys.time())
dt = gsub(':', '.', dt)
write.csv(results, paste('results', dt, '.csv', sep = ''), row.names = F)


