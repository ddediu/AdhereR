// The JavaScript associated with the SVG

function svg_change_bkg_color(c) {
  svg = document.getElementById('adherence_plot').contentDocument;
  x = svg.getElementsByClassName("plotting-area-background");
  for(i=0; i < x.length; i++) x[i].style.fill = c;
}

function svg_change_size(w="auto", h="auto") {
  svg = document.getElementById('adherence_plot');
  svg.style.height = w; svg.style.width = h;
}

