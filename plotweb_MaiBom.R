# function plotweb, but we changed the parameter "mai" from "par" function
plotweb_MaiBom <- function (web, method = "cca", empty = TRUE, labsize = 1, ybig = 1, 
                        y.width.low = 0.1, y.width.high = 0.1, low.spacing = NULL, 
                        high.spacing = NULL, arrow = "no", col.interaction = "grey80", 
                        col.high = "grey10", col.low = "grey10", bor.col.interaction = "black", 
                        bor.col.high = "black", bor.col.low = "black", high.lablength = NULL, 
                        low.lablength = NULL, sequence = NULL, low.abun = NULL, 
                        low.abun.col = "green", bor.low.abun.col = "black", high.abun = NULL, 
                        high.abun.col = "red", bor.high.abun.col = "black", text.rot = 0, 
                        text.high.col = "black", text.low.col = "black", adj.high = NULL, 
                        adj.low = NULL, plot.axes = FALSE, low.y = 0.5, high.y = 1.5, 
                        add = FALSE, y.lim = NULL, x.lim = NULL, low.plot = TRUE, 
                        high.plot = TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, 
                        low.lab.dis = NULL, abuns.type = "additional") 
{
  if (abuns.type %in% c("additional", "none")) {
    if (empty) 
      web <- empty(web)
    else method <- "normal"
    web <- as.matrix(web)
    low.order <- 1:dim(web)[1]
    high.order <- 1:dim(web)[2]
    if (length(colnames(web)) == 0) 
      colnames(web) <- colnames(web, do.NULL = FALSE)
    if (length(rownames(web)) == 0) 
      rownames(web) <- rownames(web, do.NULL = FALSE)
    if (NROW(web) == 1 | NCOL(web) == 1) {
      xlim <- low.order
      ylim <- high.order
      sequence <- NULL
      method = "normal"
    }
    meths <- c("normal", "cca")
    meths.match <- pmatch(method, meths)
    if (is.na(meths.match)) 
      stop("Choose plot-method: normal/cca.\n")
    if (length(unique(as.vector(web))) == 1) {
      meths.match <- 1
      warning("CCA-sorting does not work with same value in each cell. Uses method='normal' instead.")
    }
    if (meths.match == 2) {
      co <- compart(web)
      if (co$n.compart > 1) {
        row.seq <- NULL
        col.seq <- NULL
        for (m in 1:co$n.compart) {
          comp.member <- which(abs(co$cweb) == m, arr.ind = TRUE)
          rs <- unique(comp.member[, 1])
          cs <- unique(comp.member[, 2])
          if (length(rs) < 3 | length(cs) < 3) {
            row.seq <- c(row.seq, rs)
            col.seq <- c(col.seq, cs)
          }
          else {
            ca <- cca(web[rs, cs])
            row.seq <- c(row.seq, rs[order(summary(ca)$sites[, 
                                                             1], decreasing = TRUE)])
            col.seq <- c(col.seq, cs[order(summary(ca)$species[, 
                                                               1], decreasing = TRUE)])
          }
        }
        web <- web[row.seq, col.seq]
        low.order <- row.seq
        high.order <- col.seq
      }
      else {
        ca <- cca(web)
        web <- web[order(summary(ca)$sites[, 1], decreasing = TRUE), 
                   order(summary(ca)$species[, 1], decreasing = TRUE)]
        low.order <- order(summary(ca)$sites[, 1], decreasing = TRUE)
        high.order <- order(summary(ca)$species[, 1], 
                            decreasing = TRUE)
      }
    }
    if (!is.null(sequence)) {
      cs <- which(sequence$seq.high %in% colnames(web))
      rs <- which(sequence$seq.low %in% rownames(web))
      for (i in 1:dim(web)[2]) high.order[i] <- which(sequence$seq.high[i] == 
                                                        colnames(web))
      for (i in 1:dim(web)[1]) low.order[i] <- which(sequence$seq.low[i] == 
                                                       rownames(web))
      web <- web[sequence$seq.low[rs], sequence$seq.high[cs], 
                 drop = FALSE]
    }
    websum <- sum(web)
    difff <- diffh <- 0
    if (!is.null(low.abun)) {
      lowfreq = rowSums(web)
      dummy <- lowfreq
      for (i in 1:length(low.abun)) {
        ind <- which(names(low.abun)[i] == names(dummy))
        lowfreq[ind] <- lowfreq[ind] + low.abun[i]
      }
      difff = (lowfreq - rowSums(web))/websum
    }
    if (!is.null(high.abun)) {
      highfreq = colSums(web)
      dummy <- highfreq
      for (i in 1:length(high.abun)) {
        ind <- which(names(high.abun)[i] == names(dummy))
        highfreq[ind] <- highfreq[ind] + high.abun[i]
      }
      diffh = (highfreq - colSums(web))/websum
    }
    if (is.null(high.abun)) 
      high_prop <- colSums(web)/websum
    else high_prop <- highfreq/websum
    if (is.null(low.abun)) 
      low_prop <- rowSums(web)/websum
    else low_prop <- lowfreq/websum
    n.high <- length(high_prop)
    n.low <- length(low_prop)
    high_x <- 0
    high_xold <- -1
    high_versatz <- 0
    low_x <- 0
    low_xold <- -1
    low_versatz <- 0
    if (!is.null(high.lablength)) 
      colnames(web) <- substr(colnames(web), 1, high.lablength)
    if (!is.null(low.lablength)) 
      rownames(web) <- substr(rownames(web), 1, low.lablength)
    par(mai = c(0.1, 0.1, 0.1, 0.1))
    high_spacing = (n.low - 1)/(n.high - 1)
    low_spacing = (n.high - 1)/(n.low - 1)
    high_spacing <- high_spacing * 0.05
    low_spacing <- low_spacing * 0.05
    if (n.high > n.low) 
      low_spacing <- high_spacing * (n.high - 1)/(n.low - 
                                                    1)
    else high_spacing <- low_spacing * (n.low - 1)/(n.high - 
                                                      1)
    if (n.high == 1) 
      high_spacing <- 1
    if (n.low == 1) 
      low_spacing <- 1
    if (!is.null(low.abun)) 
      high_spacing <- high_spacing + sum(difff)/n.high
    if (!is.null(high.abun)) 
      low_spacing <- low_spacing + sum(diffh)/n.low
    wleft = 0
    if (!is.null(high.spacing)) 
      high_spacing <- high.spacing
    if (!is.null(low.spacing)) 
      low_spacing <- low.spacing
    wright = (max(n.high, n.low)) * min(low_spacing, high_spacing) + 
      1 + max(sum(diffh), sum(difff))
    wup <- 1.6 + y.width.high + 0.05
    wdown <- 0.4 - y.width.low - 0.05
    if (is.null(y.lim)) 
      y.lim <- range(wdown/ybig, wup * ybig)
    if (is.null(x.lim)) 
      x.lim <- range(wleft, wright)
    if (add == FALSE) 
      plot(0, type = "n", xlim = x.lim, ylim = y.lim, 
           axes = plot.axes, xlab = "", ylab = "")
    if (high.plot) {
      high_x = 0
      hoffset <- 0
      links <- 0
      rechts <- 0
      hoehe <- strheight(colnames(web)[1], cex = 0.6)
      if (!is.null(high.lab.dis)) 
        hoehe = high.lab.dis
      for (i in 1:n.high) {
        rect(high_x + high.xoff, high.y - y.width.high, 
             high_x + high.xoff + high_prop[i], high.y + 
               y.width.high, col = col.high[(high.order[i] - 
                                               1)%%(length(col.high)) + 1], border = bor.col.high[(high.order[i] - 
                                                                                                     1)%%(length(bor.col.high)) + 1])
        if (!is.null(high.abun)) {
          rect(high_x + high.xoff + high_prop[i] - diffh[i], 
               high.y - y.width.high, high_x + high.xoff + 
                 high_prop[i], high.y + y.width.high, col = high.abun.col[(high.order[i] - 
                                                                             1)%%(length(high.abun.col)) + 1], border = bor.high.abun.col[(high.order[i] - 
                                                                                                                                             1)%%(length(bor.high.abun.col)) + 1])
        }
        breite <- strwidth(colnames(web)[i], cex = 0.6 * 
                             labsize)
        links <- high_x + high_prop[i]/2 - breite/2
        if (links < rechts && i > 1) 
          hoffset = hoffset + hoehe
        else {
          rechts <- high_x + high_prop[i]/2 + breite/2
          hoffset <- 0
        }
        if (text.rot == 90) {
          hoffset = 0
          ad = c(0, 0.3)
        }
        else ad = c(0.5, 0.4)
        if (!is.null(adj.high)) 
          ad = adj.high
        text(high_x + high.xoff + high_prop[i]/2, high.y + 
               y.width.high + hoehe + hoffset, colnames(web)[i], 
             cex = 0.6 * labsize, offset = 0, srt = text.rot, 
             adj = ad, col = text.high.col[(high.order[i] - 
                                              1)%%(length(text.high.col)) + 1])
        high_x <- high_x + high_prop[i] + high_spacing
      }
    }
    if (low.plot) {
      low_x <- 0
      links <- 0
      rechts <- 0
      hoehe <- strheight(rownames(web)[1], cex = 0.6)
      if (!is.null(low.lab.dis)) 
        hoehe = low.lab.dis
      hoffset <- hoehe
      for (i in 1:n.low) {
        rect(low_x + low.xoff, low.y - y.width.low, 
             low_x + low.xoff + low_prop[i], low.y + y.width.low, 
             col = col.low[(low.order[i] - 1)%%(length(col.low)) + 
                             1], border = bor.col.low[(low.order[i] - 
                                                         1)%%(length(bor.col.low)) + 1])
        if (!is.null(low.abun)) {
          rect(low_x + low.xoff + low_prop[i] - difff[i], 
               low.y - y.width.low, low_x + low.xoff + 
                 low_prop[i], low.y + y.width.low, col = low.abun.col[(low.order[i] - 
                                                                         1)%%(length(low.abun.col)) + 1], border = bor.low.abun.col[(low.order[i] - 
                                                                                                                                       1)%%(length(bor.low.abun.col)) + 1])
        }
        breite <- strwidth(rownames(web)[i], cex = 0.6 * 
                             labsize)
        links <- low_x + low_prop[i]/2 - breite/2
        if (links < rechts && i > 1) 
          hoffset = hoffset + hoehe
        else {
          rechts <- low_x + low_prop[i]/2 + breite/2
          hoffset <- hoehe
        }
        if (text.rot == 90) {
          hoffset = hoehe
          ad = c(1, 0.3)
        }
        else ad = c(0.5, 0.4)
        if (!is.null(adj.low)) 
          ad = adj.low
        text(low_x + low.xoff + low_prop[i]/2, low.y - 
               y.width.low - hoffset, rownames(web)[i], cex = 0.6 * 
               labsize, offset = 0, srt = text.rot, adj = ad, 
             col = text.low.col[(low.order[i] - 1)%%(length(text.low.col)) + 
                                  1])
        low_x <- low_x + low_prop[i] + low_spacing
      }
    }
    px <- c(0, 0, 0, 0)
    py <- c(0, 0, 0, 0)
    high_x <- 0
    web.df <- data.frame(row = rep(1:n.low, n.high), col = rep(1:n.high, 
                                                               each = n.low), dat = c(web))
    web.df <- web.df[order(-web.df$dat), ]
    XYcoords <- as.matrix(web.df[, 1:2])
    y1 <- high.y - y.width.high
    y2 <- y1
    y3 <- low.y + y.width.low
    y4 <- y3
    for (p in 1:sum(web > 0)) {
      i <- XYcoords[p, 1]
      j <- XYcoords[p, 2]
      if (j == 1 & i == 1) 
        x1 <- 0
      else x1 <- (j - 1) * high_spacing + cumsum(web)[(j - 
                                                         1) * nrow(web) + (i - 1)]/websum
      if (!is.null(high.abun) && j > 1) 
        x1 <- x1 + cumsum(diffh)[j - 1]
      x2 <- x1 + web[i, j]/websum
      if (arrow == "up" || arrow == "both") {
        x2 <- (x1 + x2)/2
        x1 <- x2
      }
      if (arrow == "up.center" || arrow == "both.center") {
        if (j != 1) {
          x2 <- (j - 1) * high_spacing + cumsum(web)[(j - 
                                                        1) * nrow(web)]/websum + colSums(web)[j]/websum/2
          if (!is.null(high.abun)) 
            x2 <- x2 + cumsum(diffh)[j - 1]
          x1 <- x2
        }
        else {
          x2 = colSums(web)[j]/websum/2
          x1 <- x2
        }
      }
      tweb <- t(web)
      if (j == 1 & i == 1) 
        x3 <- 0
      else x3 <- (i - 1) * low_spacing + cumsum(tweb)[(i - 
                                                         1) * nrow(tweb) + (j - 1)]/websum
      if (!is.null(low.abun) && i > 1) 
        x3 <- x3 + cumsum(difff)[i - 1]
      x4 <- x3 + tweb[j, i]/websum
      if (arrow == "down" || arrow == "both") {
        x4 <- (x3 + x4)/2
        x3 <- x4
      }
      if (arrow == "down.center" || arrow == "both.center") {
        if (i != 1) {
          x3 <- (i - 1) * low_spacing + cumsum(tweb)[(i - 
                                                        1) * nrow(tweb)]/websum + colSums(tweb)[i]/websum/2
          if (!is.null(low.abun)) 
            x3 <- x3 + cumsum(difff)[i - 1]
          x4 <- x3
        }
        else {
          x3 = colSums(tweb)[i]/websum/2
          x4 = x3
        }
      }
      icol <- col.interaction[((low.order[XYcoords[p, 
                                                   1]] - 1) * (length(high.order)) + (high.order[XYcoords[p, 
                                                                                                          2]] - 1))%%(length(col.interaction)) + 1]
      bicol <- bor.col.interaction[((low.order[XYcoords[p, 
                                                        1]] - 1) * (length(high.order)) + (high.order[XYcoords[p, 
                                                                                                               2]] - 1))%%(length(bor.col.interaction)) + 1]
      polygon(c(x1 + high.xoff, x2 + high.xoff, x4 + low.xoff, 
                x3 + low.xoff), c(y1, y2, y4, y3), col = icol, 
              border = bicol)
    }
  }
  if (abuns.type == "independent") {
    if (empty) 
      web <- empty(web)
    else method <- "normal"
    web <- as.matrix(web)
    low.order <- 1:dim(web)[1]
    high.order <- 1:dim(web)[2]
    if (length(colnames(web)) == 0) 
      colnames(web) <- colnames(web, do.NULL = FALSE)
    if (length(rownames(web)) == 0) 
      rownames(web) <- rownames(web, do.NULL = FALSE)
    if (NROW(web) == 1 | NCOL(web) == 1) {
      xlim <- low.order
      ylim <- high.order
      sequence <- NULL
      method = "normal"
    }
    meths <- c("normal", "cca")
    meths.match <- pmatch(method, meths)
    if (is.na(meths.match)) 
      stop("Choose plot-method: normal/cca.\n")
    if (length(unique(as.vector(web))) == 1) {
      meths.match <- 1
      warning("CCA-sorting does not work with same value in each cell. Uses method='normal' instead.")
    }
    if (meths.match == 2) {
      co <- compart(web)
      if (co$n.compart > 1) {
        row.seq <- NULL
        col.seq <- NULL
        for (m in 1:co$n.compart) {
          comp.member <- which(abs(co$cweb) == m, arr.ind = TRUE)
          rs <- unique(comp.member[, 1])
          cs <- unique(comp.member[, 2])
          if (length(rs) < 3 | length(cs) < 3) {
            row.seq <- c(row.seq, rs)
            col.seq <- c(col.seq, cs)
          }
          else {
            ca <- cca(web[rs, cs])
            row.seq <- c(row.seq, rs[order(summary(ca)$sites[, 
                                                             1], decreasing = TRUE)])
            col.seq <- c(col.seq, cs[order(summary(ca)$species[, 
                                                               1], decreasing = TRUE)])
          }
        }
        web <- web[row.seq, col.seq]
        low.order <- row.seq
        high.order <- col.seq
      }
      else {
        ca <- cca(web)
        web <- web[order(summary(ca)$sites[, 1], decreasing = TRUE), 
                   order(summary(ca)$species[, 1], decreasing = TRUE)]
        low.order <- order(summary(ca)$sites[, 1], decreasing = TRUE)
        high.order <- order(summary(ca)$species[, 1], 
                            decreasing = TRUE)
      }
    }
    if (!is.null(sequence)) {
      cs <- which(sequence$seq.high %in% colnames(web))
      rs <- which(sequence$seq.low %in% rownames(web))
      for (i in 1:dim(web)[2]) high.order[i] <- which(sequence$seq.high[i] == 
                                                        colnames(web))
      for (i in 1:dim(web)[1]) low.order[i] <- which(sequence$seq.low[i] == 
                                                       rownames(web))
      web <- web[sequence$seq.low[rs], sequence$seq.high[cs], 
                 drop = FALSE]
    }
    websum <- sum(web)
    if (!is.null(high.abun)) {
      highfreq <- high.abun[colnames(web)]
    }
    else {
      highfreq <- colSums(web)
    }
    if (!is.null(low.abun)) {
      lowfreq <- low.abun[rownames(web)]
    }
    else {
      lowfreq <- rowSums(web)
    }
    high_prop <- highfreq/sum(highfreq)
    low_prop <- lowfreq/sum(lowfreq)
    n.high <- length(high_prop)
    n.low <- length(low_prop)
    high_x <- 0
    high_xold <- -1
    high_versatz <- 0
    low_x <- 0
    low_xold <- -1
    low_versatz <- 0
    if (!is.null(high.lablength)) 
      colnames(web) <- substr(colnames(web), 1, high.lablength)
    if (!is.null(low.lablength)) 
      rownames(web) <- substr(rownames(web), 1, low.lablength)
    par(mai = c(0.3, 0.3, 0.3, 0.3))
    high_spacing = (n.low - 1)/(n.high - 1)
    low_spacing = (n.high - 1)/(n.low - 1)
    high_spacing <- high_spacing * 0.05
    low_spacing <- low_spacing * 0.05
    if (n.high > n.low) 
      low_spacing <- high_spacing * (n.high - 1)/(n.low - 
                                                    1)
    else high_spacing <- low_spacing * (n.low - 1)/(n.high - 
                                                      1)
    if (n.high == 1) 
      high_spacing <- 1
    if (n.low == 1) 
      low_spacing <- 1
    wleft = 0
    if (!is.null(high.spacing)) 
      high_spacing <- high.spacing
    if (!is.null(low.spacing)) 
      low_spacing <- low.spacing
    wright = (max(n.high, n.low)) * min(low_spacing, high_spacing) + 
      1
    wup <- 1.6 + y.width.high + 0.05
    wdown <- 0.4 - y.width.low - 0.05
    if (is.null(y.lim)) 
      y.lim <- range(wdown/ybig, wup * ybig)
    if (is.null(x.lim)) 
      x.lim <- range(wleft, wright)
    if (add == FALSE) 
      plot(0, type = "n", xlim = x.lim, ylim = y.lim, 
           axes = plot.axes, xlab = "", ylab = "")
    rightmost_h <- 1
    x2_vals <- (cumsum(high_prop) + (1:n.high) * high_spacing - 
                  high_spacing)
    x1_vals <- c(0, x2_vals[1:(n.high - 1)] + high_spacing)
    names(x1_vals) <- names(x2_vals)
    x2_vals <- x2_vals/rightmost_h
    x1_vals <- x1_vals/rightmost_h
    rightmost_l <- 1
    x4_vals <- (cumsum(low_prop) + (1:n.low) * low_spacing - 
                  low_spacing)
    x3_vals <- c(0, x4_vals[1:(n.low - 1)] + low_spacing)
    names(x3_vals) <- names(x4_vals)
    x4_vals <- x4_vals/rightmost_l
    x3_vals <- x3_vals/rightmost_l
    if (high.plot) {
      hoffset <- 0
      links <- 0
      rechts <- 0
      hoehe <- strheight(colnames(web)[1], cex = 0.6)
      if (!is.null(high.lab.dis)) 
        hoehe = high.lab.dis
      for (i in 1:n.high) {
        rect(x1_vals[i] + high.xoff, high.y - y.width.high, 
             x2_vals[i] + high.xoff, high.y + y.width.high, 
             col = high.abun.col[(high.order[i] - 1)%%(length(high.abun.col)) + 
                                   1], border = bor.high.abun.col[(high.order[i] - 
                                                                     1)%%(length(bor.high.abun.col)) + 1])
        high_x <- x1_vals[i]
        breite <- strwidth(colnames(web)[i], cex = 0.6 * 
                             labsize)
        links <- high_x + high_prop[i]/2 - breite/2
        if (links < rechts && i > 1) {
          hoffset = hoffset + hoehe
        }
        else {
          rechts <- high_x + high_prop[i]/2 + breite/2
          hoffset <- 0
        }
        if (text.rot == 90) {
          hoffset = 0
          ad = c(0, 0.3)
        }
        else ad = c(0.5, 0.4)
        if (!is.null(adj.high)) 
          ad = adj.high
        text(high_x + high.xoff + high_prop[i]/2, high.y + 
               y.width.high + hoehe + hoffset, colnames(web)[i], 
             cex = 0.6 * labsize, offset = 0, srt = text.rot, 
             adj = ad, col = text.high.col[(high.order[i] - 
                                              1)%%(length(text.high.col)) + 1])
      }
    }
    if (low.plot) {
      links <- 0
      rechts <- 0
      hoehe <- strheight(rownames(web)[1], cex = 0.6)
      if (!is.null(low.lab.dis)) 
        hoehe = low.lab.dis
      hoffset <- hoehe
      for (i in 1:n.low) {
        rect(x3_vals[i] + low.xoff, low.y - y.width.low, 
             x4_vals[i] + low.xoff, low.y + y.width.low, 
             col = low.abun.col[(low.order[i] - 1)%%(length(low.abun.col)) + 
                                  1], border = bor.low.abun.col[(low.order[i] - 
                                                                   1)%%(length(bor.low.abun.col)) + 1])
        low_x <- x3_vals[i]
        breite <- strwidth(rownames(web)[i], cex = 0.6 * 
                             labsize)
        links <- low_x + low_prop[i]/2 - breite/2
        if (links < rechts && i > 1) 
          hoffset = hoffset + hoehe
        else {
          rechts <- low_x + low_prop[i]/2 + breite/2
          hoffset <- hoehe
        }
        if (text.rot == 90) {
          hoffset = hoehe
          ad = c(1, 0.3)
        }
        else ad = c(0.5, 0.4)
        if (!is.null(adj.low)) 
          ad = adj.low
        text(low_x + low.xoff + low_prop[i]/2, low.y - 
               y.width.low - hoffset, rownames(web)[i], cex = 0.6 * 
               labsize, offset = 0, srt = text.rot, adj = ad, 
             col = text.low.col[(low.order[i] - 1)%%(length(text.low.col)) + 
                                  1])
      }
    }
    px <- c(0, 0, 0, 0)
    py <- c(0, 0, 0, 0)
    high_x <- 0
    web.df <- data.frame(row = rep(1:n.low, n.high), col = rep(1:n.high, 
                                                               each = n.low), dat = c(web))
    web.df <- web.df[order(-web.df$dat), ]
    XYcoords <- as.matrix(web.df[1:sum(web > 0), 1:2])
    y1 <- high.y - y.width.high
    y2 <- y1
    y3 <- low.y + y.width.low
    y4 <- y3
    if (!is.null(low.abun) & !(arrow %in% c("both.center", 
                                            "down.center"))) 
      warning("interaction width is influenced by abundance vector and possibly not proportional to link weight")
    if (!is.null(high.abun) & !(arrow %in% c("both.center", 
                                             "up.center"))) 
      warning("interaction width is influenced by abundance vector and possibly not proportional to link weight")
    if (arrow %in% c("both", "down", "up")) 
      warning("this arrow type is currently not supported, using \"no\"")
    for (p in 1:sum(web > 0)) {
      i <- XYcoords[p, 1]
      j <- XYcoords[p, 2]
      if (i == 1) {
        x1 <- x1_vals[j]
      }
      else {
        x1 <- x1_vals[j] + (x2_vals[j] - x1_vals[j]) * 
          cumsum(web[, j])[i - 1]/sum(web[, j])
      }
      x2 <- x1_vals[j] + (x2_vals[j] - x1_vals[j]) * cumsum(web[, 
                                                                j])[i]/sum(web[, j])
      if (j == 1) {
        x3 <- x3_vals[i]
      }
      else {
        x3 <- x3_vals[i] + (x4_vals[i] - x3_vals[i]) * 
          cumsum(web[i, ])[j - 1]/sum(web[i, ])
      }
      x4 <- x3_vals[i] + (x4_vals[i] - x3_vals[i]) * cumsum(web[i, 
                                                                ])[j]/sum(web[i, ])
      if (arrow == "up.center" || arrow == "both.center") {
        x1 <- x2 <- (x1_vals[j] + x2_vals[j])/2
      }
      if (arrow == "down.center" || arrow == "both.center") {
        x3 <- x4 <- (x3_vals[i] + x4_vals[i])/2
      }
      icol <- col.interaction[((low.order[XYcoords[p, 
                                                   1]] - 1) * (length(high.order)) + (high.order[XYcoords[p, 
                                                                                                          2]] - 1))%%(length(col.interaction)) + 1]
      bicol <- bor.col.interaction[((low.order[XYcoords[p, 
                                                        1]] - 1) * (length(high.order)) + (high.order[XYcoords[p, 
                                                                                                               2]] - 1))%%(length(bor.col.interaction)) + 1]
      polygon(c(x1 + high.xoff, x2 + high.xoff, x4 + low.xoff, 
                x3 + low.xoff), c(y1, y2, y4, y3), col = icol, 
              border = bicol)
    }
  }
}
