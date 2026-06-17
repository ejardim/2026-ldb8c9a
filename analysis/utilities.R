flq_3d <- function(object, flq="harvest", cscl = "Greys"){

	if(is(object, "a4aFit")) {
		df0 <- as.data.frame(slot(object, flq))
	} else {
		df0 <- as.data.frame(object)
	}
	z_matrix <- acast(df0, age ~ year, value.var = "data")

	p <- plot_ly(x = as.numeric(colnames(z_matrix)),
		y = as.numeric(rownames(z_matrix)),
		z = z_matrix,
		type = "surface",
		colorscale = list(c(0,"white"), c(0.5,"grey60"), c(1,"black")),
		contours = list(y = list(show = TRUE, color = "white", width = 1), opacity = 0.95, x = list(show = TRUE, color = "white", width = 1))
	)

	layout(p,
		paper_bgcolor = "white",
		scene = list(
			bgcolor = "white",
			aspectmode = "manual",
			aspectratio = list(x = 2, y = 1, z = 0.7),
			camera = list(eye = list(x = 1.5, y = -1.5, z = 0.8)),
			xaxis = list(title = "Year", gridcolor = "lightgrey", zeroline = FALSE),
			yaxis = list(title = "Age",  gridcolor = "lightgrey", zeroline = FALSE),
			zaxis = list(title = "F",    gridcolor = "lightgrey", zeroline = FALSE)
			)
		)
}

