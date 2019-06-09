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
    if( plotting_areas.length > 0 ) { // there should be only one plotting area
      if( plotting_areas[0].hasAttribute("fill") ) {
        return plotting_areas[0].getAttribute("fill");
      } else
      {
        return plotting_areas[0].style.fill;
      }
    } else
    {
      return undefined;
    }
  },

  /**
   * Change the SVG image's backgound color.
   * @param {String} c  The new background color.
   * @return {None}
   */
  set_bkg_color : function(c) {
    svg = document.getElementById(adh_svg.plot_id);
    plotting_areas = svg.getElementsByClassName("plotting-area-background");
    if( plotting_areas.length > 0 ) {
      if( plotting_areas[0].hasAttribute("fill") ) { // there should be only one plotting area
        plotting_areas[0].setAttribute("fill", c);
      } else
      {
        plotting_areas[0].style.fill = c; // there should be only one plotting area
      }
    }
  },


  /**
   * Get the SVG image's size.
   * @return {Dictionary{w,h}}  dictionary of current width (w) and height (h)
   */
  get_plot_size : function() {
    svg = document.getElementById(adh_svg.plot_id);
    return {"w" : (svg.style.width) ? svg.style.width : "auto",
            "h" : (svg.style.height) ? svg.style.height : "auto"}; // this is special: we go for CSS attributes directly
  },

  /**
   * Set the SVG image's size.
   * @param {String} w  The new width (follows the rules for CSS width).
   * @param {String} h  The new height (follows the rules for CSS height).
   * @return {None}
   */
  set_plot_size : function(w="auto", h="auto") {
    svg = document.getElementById(adh_svg.plot_id);
    svg.style.width = w; svg.style.height = h; // this is special: we go for CSS attributes directly
  },


  /**
   * Are the alternating bands visible?
   * @return {Boolean}  true if they are visible
   */
  is_visible_alternating_bands : function() {
    svg = document.getElementById(adh_svg.plot_id);
    alt_bands = svg.querySelectorAll('[class^="alternating-bands-"]');
    if( alt_bands.length > 0 ) {
      return alt_bands[0].style.visibility ? alt_bands[0].style.visibility == "visible" : true; // if CSS visibility is not set, assume it is visible
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
    if(x.length > 0) ret_val["x"] = (x[0].style.visibility ? x[0].style.visibility == "visible" : true); // if CSS visibility is not set, assume it is visible
    y = svg.getElementsByClassName("axis-name-y");
    if(y.length > 0) ret_val["y"] = (y[0].style.visibility ? y[0].style.visibility == "visible" : true); // if CSS visibility is not set, assume it is visible
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
    if(x.length > 0) ret_val["x"] = (x[0].style.visibility ? x[0].style.visibility == "visible" : true); // if CSS visibility is not set, assume it is visible
    y = svg.getElementsByClassName("axis-labels-y");
    if(y.length > 0) ret_val["y"] = (y[0].style.visibility ? y[0].style.visibility == "visible" : true); // if CSS visibility is not set, assume it is visible
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
      return x.style.visibility ? x.style.visibility == "visible" : true; // if CSS visibility is not set, assume it is visible
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
   * Is the title visible?
   * @return {Boolean} true if the title is visible
   */
  is_visible_title : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementsByClassName("main-title");
    if( x.length > 0 ) {
      return x[0].style.visibility ? x[0].style.visibility == "visible" : true; // if CSS visibility is not set, assume it is visible
    } else {
      return false;
    }
  },

  /**
   * Show/hide the title.
   * @param {Boolean} show  show title if true, otherwise hide it.
   * @return {None}
   */
  show_title : function(show=true) {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementsByClassName("main-title");
    for(i=0; i < x.length; i++) x[i].style.visibility = show ? "visible" : "hidden";
  },


  /**
   * Get font size for title
   * @return {Numeric} the font size
   */
  get_font_size_title : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementsByClassName("main-title");
    if( x.length > 0 ) {
      if( x[0].hasAttribute("font-size") )  {
        return x[0].getAttribute("font-size");
      } else if( x[0].style.fontSize ) {
        return x[0].style.fontSize;
      } else {
        return undefined;
      }
    } else {
      return undefined;
    }
  },

  /**
   * Set font size for title
   * @param {String} s the new font size
   * @return {None}
   */
  set_font_size_title : function(s="16px") {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementsByClassName("main-title");
    for(i=0; i < x.length; i++) {
      if( x[i].hasAttribute("font-size") ) {
        x[i].setAttribute("font-size", s);
      } else {
        x[i].style.fontSize = s;
      }
    }
  },


  /**
   * Are there medication classes defined?
   * @return {Boolean} true if there are medication classes, false otherwise
   */
  are_medication_classes_defined : function() {
    return (typeof adh_svg.medication_classes !== 'undefined');
  },

  /**
   * Get the list of all medication classes
   * @return {Vector} the medication classes' names; null means that no classes are defined
   */
  get_medication_classes : function() {
    if( !adh_svg.are_medication_classes_defined() ) {
      return null;
    } else {
      return Object.keys(adh_svg.medication_classes);
    }
  },

  /**
   * For a medication class name, get the corresponding internal id
   * @param {String} the medication class name or null for all classes (or no class, if classes are undefined)
   * @return {String} the medication classe id
   */
  get_id_for_medication_class : function(m=null) {
    if( !adh_svg.are_medication_classes_defined() || m == null || Array.isArray(m) ) {
      return null;
    } else {
      return adh_svg.medication_classes[m];
    }
  },

  /**
   * Is a given medication class visible?
   * @param {String} the medication class name; null means there are no medication classes defined
   * @return {Boolean} true if visible
   */
  is_visible_medication_class : function(m=null) {
    svg = document.getElementById(adh_svg.plot_id);

    if( !adh_svg.are_medication_classes_defined() ) {
      // No medication classes defined
      x_start = svg.getElementsByClassName("event-start");
      //x_end = svg.getElementsByClassName("event-end"); // not used for the getter
      //x_covered = svg.getElementsByClassName("event-interval-covered"); // not used for the getter
      //x_notcovered = svg.getElementsByClassName("event-interval-not-covered"); // not used for the getter
      //x_segment = svg.getElementsByClassName("event-segment"); // not used for the getter
      //x_dose = svg.getElementsByClassName("event-dose-text"); // not used for the getter
      //x_continuation = svg.getElementsByClassName("continuation-line"); // not used for the getter
      //x_legend_rect = svg.getElementsByClassName("legend-medication-class-rect"); // not used for the getter
      //x_legend_text = svg.getElementsByClassName("legend-medication-class-label"); // not used for the getter
    } else {
      // Get the given medication class
      m_id = adh_svg.get_id_for_medication_class(m);
      if( !m_id ) return false; // cannot get the ID, so it's not visible by definition
      x_start = svg.getElementsByClassName("event-start-" + m_id);
      //x_end = svg.getElementsByClassName("event-end-" + m_id); // not used for the getter
      //x_covered = svg.getElementsByClassName("event-interval-covered-" + m_id); // not used for the getter
      //x_notcovered = svg.getElementsByClassName("event-interval-not-covered-" + m_id); // not used for the getter
      //x_segment = svg.getElementsByClassName("event-segment-" + m_id); // not used for the getter
      //x_dose = svg.getElementsByClassName("event-dose-text-" + m_id); // not used for the getter
      //x_continuation = svg.getElementsByClassName("continuation-line-" + m_id); // not used for the getter
      //x_legend_rect = svg.getElementsByClassName("legend-medication-class-rect-" + m_id); // not used for the getter
      //x_legend_text = svg.getElementsByClassName("legend-medication-class-label-" + m_id); // not used for the getter
    }

    if( x_start.length > 0 ) {
      return x_start[0].style.visibility ? x_start[0].style.visibility == "visible" : true; // if CSS visibility is not set, assume it is visible
    } else {
      return false;
    }
  },

  /**
   * Show/hide a given medication class.
   * @param {String} the medication class name; null means there are no medication classes defined
   * @param {Boolean} show  show title if true, otherwise hide it.
   * @return {None}
   */
  show_medication_class : function(m=null, show=true) {
    svg = document.getElementById(adh_svg.plot_id);

    if( !adh_svg.are_medication_classes_defined() ) {
      // No medication classes defined:
      x_start = svg.getElementsByClassName("event-start");
      x_end = svg.getElementsByClassName("event-end");
      x_covered = svg.getElementsByClassName("event-interval-covered");
      x_notcovered = svg.getElementsByClassName("event-interval-not-covered");
      x_segment = svg.getElementsByClassName("event-segment");
      x_dose = svg.getElementsByClassName("event-dose-text");
      x_continuation = svg.getElementsByClassName("continuation-line");
      x_legend_rect = svg.getElementsByClassName("legend-medication-class-rect");
      x_legend_text = svg.getElementsByClassName("legend-medication-class-label");
    } else {
      // Get the given medication class:
      m_id = adh_svg.get_id_for_medication_class(m);
      if( !m_id ) return false; // cannot get the ID, so it's not visible by definition
      x_start = svg.getElementsByClassName("event-start-" + m_id);
      x_end = svg.getElementsByClassName("event-end-" + m_id);
      x_covered = svg.getElementsByClassName("event-interval-covered-" + m_id);
      x_notcovered = svg.getElementsByClassName("event-interval-not-covered-" + m_id);
      x_segment = svg.getElementsByClassName("event-segment-" + m_id);
      x_dose = svg.getElementsByClassName("event-dose-text-" + m_id);
      x_continuation = svg.getElementsByClassName("continuation-line-" + m_id);
      x_legend_rect = svg.getElementsByClassName("legend-medication-class-rect-" + m_id);
      x_legend_text = svg.getElementsByClassName("legend-medication-class-label-" + m_id);
    }

    for(i=0; i < x_start.length;        i++) x_start[i].style.visibility        = show ? "visible" : "hidden";
    for(i=0; i < x_end.length;          i++) x_end[i].style.visibility          = show ? "visible" : "hidden";
    for(i=0; i < x_covered.length;      i++) x_covered[i].style.visibility      = show ? "visible" : "hidden";
    for(i=0; i < x_notcovered.length;   i++) x_notcovered[i].style.visibility   = show ? "visible" : "hidden";
    for(i=0; i < x_segment.length;      i++) x_segment[i].style.visibility      = show ? "visible" : "hidden";
    for(i=0; i < x_dose.length;         i++) x_dose[i].style.visibility         = show ? "visible" : "hidden";
    for(i=0; i < x_continuation.length; i++) x_continuation[i].style.visibility = show ? "visible" : "hidden";
    for(i=0; i < x_legend_rect.length;  i++) x_legend_rect[i].style.stroke      = show ? "Black"   : "LightGray";
    for(i=0; i < x_legend_text.length;  i++) x_legend_text[i].style.fill        = show ? "Black"   : "LightGray";
  }

}; // end namespace


// Initialisation stuff:
window.onload = function() {
  //adh_svg.set_bkg_color("white");
  //adh_svg.set_plot_size();
  //adh_svg.show_alternating_bands(true);
  //adh_svg.show_axis_names(true, true);
  //adh_svg.show_axis_labels(true, true);
  //adh_svg.show_legend(true);
  //adh_svg.show_title(true);
}







