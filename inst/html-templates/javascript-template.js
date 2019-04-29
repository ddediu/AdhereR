/**
 * The JavaScript API for interacting with the AdhereR SVG plot (assumed embedded in an HTML document)
 *
 * Using the documentation standard described here:
 * https://make.wordpress.org/core/handbook/best-practices/inline-documentation-standards/javascript/
 *
 * (c) Dan Dediu [ddediu@gmail.com], 2019
 */

// Simulate a namespace 'adh_svg' to avoid potential conflicts with other JavaScript libraries:
var adh_svg = { // begin namespace

  // The SVG plot's ID:
  plot_id : 'adherence_plot',


  /**
   * Get the SVG image's backgound color.
   * @return {String}   the background color (or None).
   */
  get_bkg_color : function() {
    svg = document.getElementById(adh_svg.plot_id);
    plotting_areas = svg.getElementsByClassName("plotting-area-background");
    return (plotting_areas.length > 0) ? plotting_areas[0].style.fill : None; // there should be only one plotting area
  },


  /**
   * Change the SVG image's backgound color.
   * @param {String} c  The new background color.
   * @return {None}
   */
  set_bkg_color : function(c) {
    svg = document.getElementById(adh_svg.plot_id);
    plotting_areas = svg.getElementsByClassName("plotting-area-background");
    if( plotting_areas.length > 0 ) plotting_areas[0].style.fill = c; // there should be only one plotting area
  },


  /**
   * Change the SVG image's size.
   * @param {String} w  The new width (follows the rules for CSS width).
   * @param {String} h  The new height (follows the rules for CSS height).
   * @return {None}
   */
  change_plot_size : function(w="auto", h="auto") {
    svg = document.getElementById(adh_svg.plot_id);
    svg.style.height = w; svg.style.width = h;
  },


  /**
   * Show/hide the alternating bands.
   * @param {Boolean} show  if true, show the bands, otherwise hide them.
   * @return {None}
   */
  show_alternating_bands : function(show=true) {
    svg = document.getElementById(adh_svg.plot_id);
    alt_bands = svg.querySelectorAll('[class^="alternating-bands-"]');
    for(i=0; i < alt_bands.length; i++) alt_bands[i].style.visibility = show ? "visible" : "hidden";
  },


  /**
   * Show/hide the axis lables.
   * @param {Boolean} show  if true, show the bands, otherwise hide them.
   * @param {String}  axis  the axis to act on; can be "x", "y" or "both".
   * @return {None}
   */
  show_axis_labels : function(show=true, axis="x") {
    svg = document.getElementById(adh_svg.plot_id);
    if( axis == "x" || axis == "both" ) {
      x = svg.getElementsByClassName("axis-label-x")
      for(i=0; i < x.length; i++) x[i].style.visibility = show ? "visible" : "hidden"
    }
    if( axis == "y" || axis == "both" ) {
      y = svg.getElementsByClassName("axis-label-y")
      for(i=0; i < y.length; i++) y[i].style.visibility = show ? "visible" : "hidden"
    }
  }

}; // end namespace


// Initialisation stuff:
window.onload = function() {
  adh_svg.set_bkg_color("white");
}







