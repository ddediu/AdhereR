/**
 * The JavaScript API for interacting with the AdhereR SVG plot (assumed embedded in an HTML document)
 *
 * Using the documentation standard described here:
 * https://make.wordpress.org/core/handbook/best-practices/inline-documentation-standards/javascript/
 *
 * (c) Dan Dediu [ddediu@gmail.com], 2019
 */

// The SVG plot's ID:
var svg_plot_id = 'adherence_plot';


/**
 * Change the SVG image's backgound color.
 *
 * Change the *whole* SVG image bacgound color (i.e., not just the actual plot).
 *
 * @access     public
 *
 * @param {String}   c  The new backgroundcolor.
 *
 * @return {None}
 */
 function svg_change_bkg_color(c) {
  svg = document.getElementById(svg_plot_id);
  x = svg.getElementsByClassName("plotting-area-background");
  for(i=0; i < x.length; i++) x[i].style.fill = c;
}


/**
 * Change the SVG image's size.
 *
 * Change the *whole* SVG image size.
 *
 * @access     public
 *
 * @param {String}   w  The new width (follows the rules for CSS width).
 * @param {String}   h  The new height (follows the rules for CSS height).
 *
 * @return {None}
 */
function svg_change_plot_size(w="auto", h="auto") {
  svg = document.getElementById(svg_plot_id);
  svg.style.height = w; svg.style.width = h;
}

