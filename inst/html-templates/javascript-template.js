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
   * Get the SVG image's size.
   * @return {Dictionary{w,h}}  dictionary of current width (w) and height (h)
   */
  get_plot_size : function() {
    svg = document.getElementById(adh_svg.plot_id);
    return {"w" : (svg.style.width) ? svg.style.width : "auto",
            "h" : (svg.style.height) ? svg.style.height : "auto"};
  },

  /**
   * Set the SVG image's size.
   * @param {String} w  The new width (follows the rules for CSS width).
   * @param {String} h  The new height (follows the rules for CSS height).
   * @return {None}
   */
  set_plot_size : function(w="auto", h="auto") {
    svg = document.getElementById(adh_svg.plot_id);
    svg.style.width = w; svg.style.height = h;
  },


  /**
   * Are the alternating bands visible?
   * @return {Boolean}  true if they are visible
   */
  is_visible_alternating_bands : function() {
    svg = document.getElementById(adh_svg.plot_id);
    alt_bands = svg.querySelectorAll('[class^="alternating-bands-"]');
    if( alt_bands.length > 0 ) {
      return alt_bands[0].style.visibility == "visible";
    } else {
      return false;
    }
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
   * Are the axis names visible?
   * @return {Dictionary{x,y}} true if the corresponding axis name is visible
   */
  is_visible_axis_names : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":false, "y":false}; // the return value
    x = svg.getElementsByClassName("axis-name-x");
    if(x.length > 0) ret_val["x"] = (x[0].style.visibility == "visible"); // there should be only one y-axis name
    y = svg.getElementsByClassName("axis-name-y");
    if(y.length > 0) ret_val["y"] = (y[0].style.visibility == "visible"); // there should be only one y-axis name
    return ret_val;
  },

  /**
   * Show/hide the axis names.
   * @param {Boolean} show_x  show x axis name if true, otherwise hide it.
   * @param {Boolean} show_y  show y axis name if true, otherwise hide it.
   * @return {None}
   */
  show_axis_names : function(show_x=true, show_y=true) {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementsByClassName("axis-name-x");
    if(x.length > 0) x[0].style.visibility = show_x ? "visible" : "hidden"; // there should be only one x-axis name
    y = svg.getElementsByClassName("axis-name-y");
    if(y.length > 0) y[0].style.visibility = show_y ? "visible" : "hidden"; // there should be only one y-axis name
  },


  /**
   * Are the axis labels visible?
   * @return {Dictionary{x,y}} true if the corresponding axis label is visible
   */
  is_visible_axis_labels : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":false, "y":false}; // the return value
    x = svg.getElementsByClassName("axis-labels-x");
    if(x.length > 0) ret_val["x"] = (x[0].style.visibility == "visible"); // just for the first label (all should be the same)
    y = svg.getElementsByClassName("axis-labels-y");
    if(y.length > 0) ret_val["y"] = (y[0].style.visibility == "visible"); // just for the first label (all should be the same)
    return ret_val;
  },

  /**
   * Show/hide the axis lables.
   * @param {Boolean} show_x  show x axis label if true, otherwise hide it.
   * @param {Boolean} show_y  show y axis label if true, otherwise hide it.
   * @return {None}
   */
  show_axis_labels : function(show_x=true, show_y=true) {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementsByClassName("axis-labels-x");
    ticks = svg.getElementsByClassName("axis-ticks-x");
    for(i=0; i < x.length; i++) {
      x[i].style.visibility = show_x ? "visible" : "hidden"; // for each label
      ticks[i].style.visibility = show_x ? "visible" : "hidden"; // there should be exactly one tick for each label
    }
    y = svg.getElementsByClassName("axis-labels-y");
    for(i=0; i < y.length; i++) y[i].style.visibility = show_y ? "visible" : "hidden"; // for each label
  },


  /**
   * Is the legend visible?
   * @return {Boolean} true if the legend is visible
   */
  is_visible_legend : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementById("legend");
    if(!x) {
      return false; // no legend, so it's not visible!
    } else {
      return x.style.visibility == "visible"; // is it visible? // there's a legend
    }
  },

  /**
   * Show/hide the legend.
   * @param {Boolean} show  show legend if true, otherwise hide it.
   * @return {None}
   */
  show_legend : function(show=true) {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementById("legend");
    if(x) { // there's a legend
      x.style.visibility = show ? "visible" : "hidden"; // for each label
    }
    // otherwise, nothing to do...
  },

  /**
   * Get the legend's position.
   * @return {Dictionary{x,y}}  the position of the legend as a pair of numbers between 0 (leftmost/bottommost) and 1 (rightmost/topmost).
   */
  get_legend_position : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":None, "y":None}; // the resutn value (None = undefined)
    x = svg.getElementById("legend");
    if(x) { // there's a legend
      x.style.visibility = show ? "visible" : "hidden"; // for each label
    }
  },

  /**
   * Move the legend to a given position.
   * @param {Numeric} x  horizontal location of the legend between 0 (leftmost) and 1 (rightmost).
   * @param {Numeric} y  vertical location of the legend between 0 (bottommost) and 1 (topmost).
   * @return {None}
   */
  set_legend_position : function(x=0.5, y=0.5) {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementById("legend");
    if(x) { // there's a legend
      x.style.visibility = show ? "visible" : "hidden"; // for each label
    }
    // otherwise, nothing to do...
  }

}; // end namespace


// Initialisation stuff:
window.onload = function() {
  adh_svg.set_bkg_color("white");
  //adh_svg.set_plot_size();
  adh_svg.show_alternating_bands(true);
  adh_svg.show_axis_names(true, true);
  adh_svg.show_axis_labels(true, true);
  adh_svg.show_legend(true);
}







