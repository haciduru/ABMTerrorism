
rm(list=ls())
library(igraph)
library(pryr)
library(tictoc)

'%!in%' = function(x,y)!('%in%'(x,y))

# ------------------------------------------------------------------------------

civ = 'darkgrey'
new_agent = function(class=civ) {
  e = new.env(parent = globalenv())
  e$id = address(e)
  class(e) = class
  e$sup = 1/(1+exp(-rnorm(1)))
  e$res = 0
  return(e)
}

wire = function(a,b) {
  if (a$id != b$id) {
    a$nei = unique(append(a$nei, b))
    b$nei = unique(append(b$nei, a))
  }
}

unwire = function(a,b) {
  nei = unlist(lapply(a$nei, function(x) x$id))
  a$nei = a$nei[-which(nei == b$id)]
  nei = unlist(lapply(b$nei, function(x) x$id))
  b$nei = b$nei[-which(nei == a$id)]
}

wire_all = function(agents, n=4) {
  for (i in 1:n) {
    m = matrix(
      c(sample(agents, round(length(agents) / 2)),
        sample(agents, round(length(agents) / 2))),
      ncol = 2)
    apply(m, 1, function(x) {
      a = x[[1]]
      b = x[[2]]
      wire(a,b)
    })
  }
  agents = unlist(lapply(agents, function(x) if (length(x$nei) > 0) x else NULL))
  return(agents)
}

init.agents = function(N=100) {
  agents = lapply(c(1:N), function(x) {
    a = new_agent()
    return(a)
  })
  agents = wire_all(agents)
  return(agents)
}

init.intel = function(agents) {
  invisible(lapply(agents, function(a) {
    if (class(a) == mil | class(a) == ins) nint = 7 else nint = 2
    a$int = rep(list(list(a = a, sup = err(a$sup))), length(a$nei)*nint)
  }))
}

display = function(agents) {
  agents = unlist(lapply(agents, function(a) if (length(a$nei) > 0) a else NULL))
  G = graph_from_data_frame(  
    data.frame(t(matrix(unlist(lapply(agents, function(x) {
      unlist(t(data.frame(u = x$id, y = unlist(lapply(x$nei, function(y) y$id)))))
    })), nrow=2)))
  )
  G = as.undirected(G)
  V(G)$class = unlist(lapply(agents, function(a) class(a)))
  V(G)$sup = unlist(lapply(agents, function(a) abs(a$sup - .5)))
  plot(G, vertex.label='', vertex.size=V(G)$sup*5, vertex.color = V(G)$class,
       vertex.frame.color = V(G)$class, edge.width = .5, edge.color = 'gainsboro')
}

# ------------------------------------------------------------------------------

err = function(x) if (x > 0 & x < 1) rnorm(1, x, sqrt(x * (1 - x)) / 99) else x

# ------------------------------------------------------------------------------

find.target.to.push.intel = function(a) {
  if (length(a$nei) > 1) {
    per = unlist(lapply(a$nei, function(n) err(n$sup)))
    per = abs(per - err(a$sup))
    per = 1 - per
    per = per / sum(per)
    per = cumsum(per)
    target = a$nei[[sum(per < runif(1)) + 1]]
    return(target)
  } else {
    return(a$nei[1][[1]])
  }
}

push.intel = function(a) {
  target = find.target.to.push.intel(a)
  if (runif(1) > .5) {
    int = sample(a$int, 1)[[1]]
  } else {
    int = list(a = a, sup = err(a$sup))
  }
  int$sup = err(int$sup)
  target$int[sample(length(target$int), 1)][[1]] = int
}

# ------------------------------------------------------------------------------

check.friendships = function(a, t=.5) {
  if (length(a$nei) < length(a$int) / 2 ) {
    wire(a, sample(agents, 1)[[1]])
  } else {
    per = unlist(lapply(a$nei, function(n) err(n$sup)))
    per = abs(per - err(a$sup))
    if (max(per) > t) {
      unwire(a, a$nei[per == max(per)][1][[1]])
      wire(a, sample(agents, 1)[[1]])
    }
  }
}

# ------------------------------------------------------------------------------

find.target.to.attack = function(a, t=.9) {
  targets = c(a$int, a$nei)
  sups = unlist(lapply(targets, function(a) a$sup))
  sups = abs(a$sup - sups)
  target = targets[sups > t]
  if (length(target)) target = target[[1]]$a
  return(target)
}

mil.attack = function(a, t=.9) {
  target = find.target.to.attack(a, t)
  if (length(target)) {
    if (class(target) == ins) {
      die(target)
      n_ins_captur <<- n_ins_captur + 1
    } else {
      target$sup = target$sup + (target$sup * (1 - target$sup))**2
      n_pol_missed <<- n_pol_missed + 1
    }
  }
}

ins.attack = function(a, t=.9) {
  target = find.target.to.attack(a, t)
  if (length(target)) {
    if (class(target) == mil) { # target is military/police
      if (runif(1) > .5) {
        die(target)
        recruit.mil()
        n_ter_kills <<- n_ter_kills + 1
      } else {
        die(a)
      }
    } else if (class(target) == soc) { # target is social worker
      die(target)
      recruit.soc()
      n_ter_kills <<- n_ter_kills + 1
    } else { # target is civilian
      if (runif(1) < .05) {
        die(target)
        n_ter_kills <<- n_ter_kills + 1
      } else {
        target$sup = .5
      }
    }
    n_ter_attack <<- n_ter_attack + 1
  }
}

die = function(a) {
  invisible(lapply(a$nei, function(n) {
    simi = 1 - abs(a$sup - n$sup)
    slop = (n$sup * (1 - n$sup))**2
    coef = a$sup - a$sup
    n$sup = n$sup + simi * slop * coef
  }))
  invisible(lapply(a$nei, function(b) unwire(a,b)))
  invisible(lapply(sample(agents, 4), function(b) wire(a,b)))
  a$int = rep(list(list(a = a, sup = err(a$sup))), length(a$nei)*2)
  class(a) = civ
  a$sup = 1/(1+exp(-rnorm(1)))
  a$res = 0
}

# ------------------------------------------------------------------------------

recruit.mil = function() {
  a = unlist(lapply(agents, function(a) if (class(a) == civ & a$sup < .1) a else NULL))[[1]]
  if (length(a)) {
    class(a) = mil
    a$sup = .02
    a$res = 1
    a$int = rep(list(list(a = a, sup = err(a$sup))), length(a$nei)*7)
  }
}

recruit.soc = function() {
  a = unlist(lapply(agents, function(a) if (class(a) == civ & a$sup < .1) a else NULL))[[1]]
  if (length(a)) {
    class(a) = soc
    a$sup = .02
    a$res = 1
  }
}

# ------------------------------------------------------------------------------

share = function(a) {
  nei = unlist(lapply(a$nei, function(a) if (class(a) != mil & class(a) != soc) a else NULL))
  if (length(nei)) {
    res = unlist(lapply(nei, function(n) n$res))
    nei = nei[res < a$res]
    if (length(nei)) {
      target = sample(nei, 1)[[1]]
      target$res = target$res + log(1+a$res)
      if (class(a) != soc) a$res = 0
    } else {
      if (class(a) != soc) a$res = log(1+a$res)
    }
  } else {
    if (class(a) != soc) a$res = log(1+a$res)
  }
}

check.clients = function(a, t=.3) {
  sup = unlist(lapply(a$nei, function(n) n$sup))
  nei = a$nei[sup < t]
  if (length(nei)) {
    unwire(a, nei[[1]])
    wire(a, sample(agents, 1)[[1]])
  }
}

adj.sup = function(a) {
  if (a$res > 1) a$res = .99
  if (a$sup - a$res**2 > 0) a$sup = a$sup - a$res**2
  if (a$sup > .98) class(a) = ins
}

# ------------------------------------------------------------------------------

iterate = function() {
  invisible(lapply(agents, function(a) {
    push.intel(a)
    if (class(a) == civ) {
      check.friendships(a)
      share(a)
      adj.sup(a)
    }
    if (class(a) == soc) {
      check.clients(a, .2)
      share(a)
    }
    if (class(a) == mil) {
      mil.attack(a)
    }
    if (class(a) == ins) {
      ins.attack(a)
    }
  }))
}

prune = function(agents) {
  agents = unlist(lapply(agents, function(a) if (length(a$nei) > 0) a else NULL))
  G = graph_from_data_frame(  
    data.frame(t(matrix(unlist(lapply(agents, function(x) {
      unlist(t(data.frame(u = x$id, y = unlist(lapply(x$nei, function(y) y$id)))))
    })), nrow=2)))
  )
  G = as.undirected(G)
  agents = agents[unlist(lapply(agents, function(a) a$id)) %in% V(G)$name]
  return(agents)
}


