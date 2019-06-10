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
   * Given an SVG element and an attribute, return the attribute's value
   * @param {String} elem  The SVG element.
   * @param {String} attr  The SVG attribute name.
   * @param {String} elem_type  Some types of elements require a special mapping to CSS (e.g., fonts).
   * @return {String}   the attribute value.
   */
  get_svg_attribute : function(elem, attr, elem_type=null) {
    if( !elem ) {
      return undefined;
    } else {
      if( elem.length > 0 ) elem = elem[0]; //assume that for arrays the first element is enough

      if( elem.hasAttribute(attr) ) {
        return elem.getAttribute(attr);
      } else
      {
        // SVG attribute may require translation to CSS:
        switch(attr) {
          case "fill":
            if( elem_type == "font" ) {
              return !elem.style.color ? undefined : elem.style.color;
            } else {
              return !elem.style.fill ? undefined : elem.style.fill;
            }
            break;
          case "font-size":
            return !elem.style.fontSize ? undefined : elem.style.fontSize;
            break;
          case "visibility":
            return !elem.style.visibility ? undefined : elem.style.visibility;
            break;
          default:
            return undefined;
        }
      }
    }
  },

  /**
   * Given an SVG element, an attribute and a vlue, set the attribute's value
   * @param {String} elem  The SVG element.
   * @param {String} attr  The SVG attribute name.
   * @param {String} val   The attribute's new value.
   * @param {String} elem_type  Some types of elements require a special mapping to CSS (e.g., fonts).
   * @param {Boolean} force_svg_attr  If true, set the SVG attribute avan if not yet defined (needed in some cases for some browsers).
   */
  set_svg_attribute : function(elem, attr, val, elem_type=null, force_svg_attr=true) {
    if( !elem ) {
      return;
    } else {
      // Local function dealing with a single element at a time:
      function _set_svg_attribute_for_element(elem, attr, val, elem_type=null) {
        if( force_svg_attr || elem.hasAttribute(attr) ) {
          elem.setAttribute(attr, val);
        } else
        {
          // SVG attribute may require translation to CSS:
          switch(attr) {
            case "fill":
              if( elem_type == "font" ) {
                elem.style.color = val;
              } else {
                elem.style.fill = val;
              }
              break;
            case "font-size":
              elem.style.fontSize = val;
              break;
            case "visibility":
              elem.style.visibility = val;
              break;
          }
        }
      }

      if( elem.length > 0 ) {
        for(i=0; i<elem.length; i++) _set_svg_attribute_for_element(elem[i], attr, val, elem_type);
      } else {
        _set_svg_attribute_for_element(elem, attr, val, elem_type);
      }
    }
  },


  /**
   * Is an SVG element (or array thereof) visible?
   * @param {String}  elem  The SVG element.
   * @return {Boolean} True if visible.
   */
  is_visible_svg_element : function(elem) {
    return adh_svg.get_svg_attribute(elem, "visibility") != "hidden"; //  assume visible unless explicitely hidden...
  },

  /**
   * Show/hide one (or more) SVG element(s).
   * @param {String}  elem  The SVG element(s).
   * @param {Boolean} show  If true, show it, otherwise hide it.
   */
  show_svg_element : function(elem, show=true) {
    adh_svg.set_svg_attribute(elem, "visibility", show ? "visible" : "hidden");
  },


  /**
   * Get the SVG image's backgound color.
   * @return {String}   the background color (or None).
   */
  get_bkg_color : function() {
    svg = document.getElementById(adh_svg.plot_id);
    plotting_areas = svg.getElementsByClassName("plotting-area-background");
    return adh_svg.get_svg_attribute(plotting_areas, "fill");
  },

  /**
   * Change the SVG image's backgound color.
   * @param {String} c  The new background color.
   * @return {None}
   */
  set_bkg_color : function(c) {
    svg = document.getElementById(adh_svg.plot_id);
    plotting_areas = svg.getElementsByClassName("plotting-area-background");
    adh_svg.set_svg_attribute(plotting_areas, "fill", c);
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
    return adh_svg.is_visible_svg_element(alt_bands);
  },

  /**
   * Show/hide the alternating bands.
   * @param {Boolean} show  if true, show the bands, otherwise hide them.
   * @return {None}
   */
  show_alternating_bands : function(show=true) {
    svg = document.getElementById(adh_svg.plot_id);
    alt_bands = svg.querySelectorAll('[class^="alternating-bands-"]');
    adh_svg.show_svg_element(alt_bands, show);
  },


  /**
   * Are the axis names visible?
   * @return {Dictionary{x,y}} true if the corresponding axis name is visible
   */
  is_visible_axis_names : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":false, "y":false}; // the return value
    x = svg.getElementsByClassName("axis-name-x"); ret_val["x"] = adh_svg.is_visible_svg_element(x);
    y = svg.getElementsByClassName("axis-name-y"); ret_val["y"] = adh_svg.is_visible_svg_element(y);
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
    x = svg.getElementsByClassName("axis-name-x"); adh_svg.show_svg_element(x, show_x);
    y = svg.getElementsByClassName("axis-name-y"); adh_svg.show_svg_element(y, show_y);
  },


  /**
   * Are the axis labels visible?
   * @return {Dictionary{x,y}} true if the corresponding axis label is visible
   */
  is_visible_axis_labels : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":false, "y":false}; // the return value
    x = svg.getElementsByClassName("axis-labels-x"); ret_val["x"] = adh_svg.is_visible_svg_element(x);
    y = svg.getElementsByClassName("axis-labels-y"); ret_val["y"] = adh_svg.is_visible_svg_element(y);
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
    x = svg.getElementsByClassName("axis-labels-x");    adh_svg.show_svg_element(x, show_x);
    ticks = svg.getElementsByClassName("axis-ticks-x"); adh_svg.show_svg_element(ticks, show_x);
    y = svg.getElementsByClassName("axis-labels-y");    adh_svg.show_svg_element(y, show_y);
  },


  /**
   * Is the legend visible?
   * @return {Boolean} true if the legend is visible
   */
  is_visible_legend : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementById("legend");
    return adh_svg.is_visible_svg_element(x);
  },

  /**
   * Show/hide the legend.
   * @param {Boolean} show  show legend if true, otherwise hide it.
   * @return {None}
   */
  show_legend : function(show=true) {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementById("legend");
    adh_svg.show_svg_element(x, show);
  },


  /**
   * Is the title visible?
   * @return {Boolean} true if the title is visible
   */
  is_visible_title : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementsByClassName("main-title");
    return adh_svg.is_visible_svg_element(x);
  },

  /**
   * Show/hide the title.
   * @param {Boolean} show  show title if true, otherwise hide it.
   * @return {None}
   */
  show_title : function(show=true) {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementsByClassName("main-title");
    adh_svg.show_svg_element(x, show);
  },


  /**
   * Get font size for title
   * @return {Numeric} the font size
   */
  get_font_size_title : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementsByClassName("main-title");
    return adh_svg.get_svg_attribute(x[0], "font-size");
  },

  /**
   * Set font size for title
   * @param {String} s the new font size
   * @return {None}
   */
  set_font_size_title : function(s="16px") {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementsByClassName("main-title");
    adh_svg.set_svg_attribute(x, "font-size", s);
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
    } else {
      // Get the given medication class
      m_id = adh_svg.get_id_for_medication_class(m);
      if( !m_id ) return false; // cannot get the ID, so it's not visible by definition
      x_start = svg.getElementsByClassName("event-start-" + m_id);
    }

    return adh_svg.is_visible_svg_element(x_start);
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

    adh_svg.show_svg_element(x_start, show);
    adh_svg.show_svg_element(x_end, show);
    adh_svg.show_svg_element(x_covered, show);
    adh_svg.show_svg_element(x_notcovered, show);
    adh_svg.show_svg_element(x_segment, show);
    adh_svg.show_svg_element(x_dose, show);
    adh_svg.show_svg_element(x_continuation, show);

    adh_svg.set_svg_attribute(x_legend_rect, "stroke", show ? "Black" : "LightGray");
    adh_svg.set_svg_attribute(x_legend_text, "fill",   show ? "Black" : "LightGray", elem_type="font");
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







