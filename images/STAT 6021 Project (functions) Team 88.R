# -------------------------------------------------------+
# R function for Stepwise selection                      |
# -------------------------------------------------------+

get.model.str <- function(var.in, resp.name, reg.names) {
  var.in.idx <- which(var.in)
  model.str <- paste(resp.name, "~")
  first.in <- TRUE
  for (iVAR in var.in.idx) {
    if (first.in) {
      model.str <- paste(model.str, reg.names[iVAR])
      first.in <- FALSE
    } else {
      model.str <- paste(model.str, "+", reg.names[iVAR])
    }
  }
  return(model.str)
}

eval.lm <- function(model.str, data.name) {
  lm.call.str <- paste("reg.lm <- lm(", model.str, ", data=", data.name, ")")
  eval(parse(text=lm.call.str))
  return(reg.lm)
}

forward.step <- function(curr.var.in, alpha.in, resp.name, reg.names, data.name) {
  curr.var.out.idx <- which(!curr.var.in)
  enter.idx <- NA
  if (length(curr.var.out.idx) > 0) {
    k <- length(reg.names)
    pval.seq <- rep(x=Inf, times=k)
    for (iVAR in curr.var.out.idx) {
      cand.var.in <- curr.var.in
      cand.var.in[iVAR] <- TRUE
      cand.model.str <- get.model.str(cand.var.in, resp.name, reg.names)
      cand.model.lm <- eval.lm(cand.model.str, data.name)
      iROW <- which(row.names(summary(cand.model.lm)$coefficients) == reg.names[iVAR])
      pval.seq[iVAR] <- summary(cand.model.lm)$coefficients[iROW,4]
    }
    enter.idx <- which.min(pval.seq)
    if (pval.seq[enter.idx] < alpha.in) {
      print(paste("Variable ", reg.names[enter.idx], " enters the model (pval=", sprintf("%6.4f", pval.seq[enter.idx]), ")", sep=""))
    } else {
      print("No variables enter the model")
      enter.idx <- NA
    }
  } else {
    print("No variables available to enter the model")
  }
  return(enter.idx)
}

backward.step <- function(curr.var.in, alpha.out, resp.name, reg.names, data.name) {
  curr.var.in.idx <- which(curr.var.in)
  leave.idx <- NA
  if (length(curr.var.in.idx) > 0) {
    k <- length(reg.names)
    pval.seq <- rep(x=-Inf, times=k)
    curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
    curr.model.lm <- eval.lm(curr.model.str, data.name)
    for (iVAR in curr.var.in.idx) {
      iROW <- which(row.names(summary(curr.model.lm)$coefficients) == reg.names[iVAR])
      pval.seq[iVAR] <- summary(curr.model.lm)$coefficients[iROW,4]
    }
    leave.idx <- which.max(pval.seq)
    if (pval.seq[leave.idx] >= alpha.out) {
      print(paste("Variable ", reg.names[leave.idx], " leaves the model (pval=", sprintf("%6.4f", pval.seq[leave.idx]), ")", sep=""))
    } else {
      print("No variables leave the model")
      leave.idx <- NA
    }
  } else {
    print("No variables available to leave the model")
  }
  return(leave.idx)
}

stepwise.selection <- function(alpha.in, alpha.out, resp.name, reg.names, data.name) {
  k <- length(reg.names)
  curr.var.in <- rep(x=FALSE, times=k)
  stop <- FALSE
  while(!stop) {
    enter.idx <- forward.step(curr.var.in, alpha.in, resp.name, reg.names, data.name)
    if (is.na(enter.idx)) {
      stop <- TRUE
    } else {
      curr.var.in[enter.idx] <- TRUE
      leave.idx <- backward.step(curr.var.in, alpha.out, resp.name, reg.names, data.name)
      if (!is.na(leave.idx)) {
        curr.var.in[leave.idx] <- FALSE
        if (leave.idx == enter.idx) {
          stop <- TRUE
        }
      }
    }
  }
  curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
  print(paste("Final model: ", curr.model.str, sep=""))
  curr.model.lm <- eval.lm(curr.model.str, data.name)
  return(curr.model.lm)
}