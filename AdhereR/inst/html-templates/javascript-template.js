/**
 * The JavaScript API for interacting with the AdhereR SVG plot (assumed embedded in an HTML document)
 *
 * Using the documentation standard described here:
 * https://make.wordpress.org/core/handbook/best-practices/inline-documentation-standards/javascript/
 *
 * (c) Dan Dediu [ddediu@gmail.com], 2019
 */

// Comments about specific browsers/platforms:
//  - IE11 on Windows 10:
//      - does not allow function arguments with default values -> use === undefined as a replacement in the function body
//      - does not implement getElementsByClassName() so use: https://stackoverflow.com/questions/7410949/javascript-document-getelementsbyclassname-compatibility-with-ie
//      - does not implement hasAttribute() so use: https://andrewdupont.net/2007/01/10/code-hasattribute-for-ie/


// Simulate a namespace 'adh_svg' to avoid potential conflicts with other JavaScript libraries:
var adh_svg = { // begin namespace

  // The SVG plot's ID:
  plot_id : 'adherence_plot',


  /**
   * Check if browser supports embedded SVG
   * using: https://css-tricks.com/a-complete-guide-to-svg-fallbacks/
   * @param {HTMLElement} node  The element.
   * @param {String} classname  The class name.
   * @return {Array} the child elements of the given class.
   */
  is_embedded_SVG_supported : function() {
    var div = document.createElement('div');
    div.innerHTML = '<svg/>';
    return (div.firstChild && div.firstChild.namespaceURI) == 'http://www.w3.org/2000/svg';
  },


  /**
   * IE does not implement getElementsByClassName()
   * so re-implement it using: https://stackoverflow.com/questions/7410949/javascript-document-getelementsbyclassname-compatibility-with-ie
   * @param {HTMLElement} node  The element.
   * @param {String} classname  The class name.
   * @return {Array} the child elements of the given class.
   */
  _getElementsByClassName : function(node, classname) {
    if(!node.getElementsByClassName) {
      // getElementsByClassName() is not implemennted, so fake it:
      var a = [];
      var re = new RegExp('(^| )'+classname+'( |$)');
      var els = node.getElementsByTagName("*");
      for(var i=0,j=els.length; i<j; i++)
          if(re.test(els[i].getAttribute('class'))) a.push(els[i]);
      return a;
    } else
    {
      // getElementsByClassName() is implemented, so use it:
      return node.getElementsByClassName(classname);
    }
  },

  /**
   * IE does not implement hasAttribute()
   * so re-implement it using: https://andrewdupont.net/2007/01/10/code-hasattribute-for-ie/
   * @param {HTMLElement} node  The element.
   * @param {String} attrname  The attribute name.
   * @return {Boolean} true if node has the attribute.
   */
  _hasAttribute : function(node, attrname) {
    if(!node.hasAttribute) {
      // hasAttribute() is not implemennted, so fake it:
      var x = node.attributes[attrname];
      return (typeof x != "undefined");
    } else
    {
      // getElementsByClassName() is implemented, so use it:
      return node.hasAttribute(attrname);
    }
  },


  /**
   * Given an SVG element and an attribute, return the attribute's value
   * @param {String} elem  The SVG element.
   * @param {String} attr  The SVG attribute name.
   * @param {String} elem_type  Some types of elements require a special mapping to CSS (e.g., fonts).
   * @return {String}   the attribute value.
   */
  get_svg_attribute : function(elem, attr, elem_type) {
    if( !elem || elem.length == 0 ) {
      return undefined;
    } else {
      if( elem.length > 1 ) elem = elem[0]; //assume that for arrays the first element is enough

      if( adh_svg._hasAttribute(elem, attr) ) {
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
   * @param {Boolean} force_svg_attr  If undefined or true, set the SVG attribute avan if not yet defined (needed in some cases for some browsers).
   */
  set_svg_attribute : function(elem, attr, val, elem_type, force_svg_attr) {
    if( !elem || elem.length == 0 ) {
      return;
    } else {
      // Local function dealing with a single element at a time:
      function _set_svg_attribute_for_element(elem, attr, val, elem_type) {
        if( force_svg_attr === undefined || force_svg_attr || adh_svg._hasAttribute(elem, attr) ) {
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
   * @param {Boolean} show  If undefined or true, show it, otherwise hide it.
   */
  show_svg_element : function(elem, show) {
    adh_svg.set_svg_attribute(elem, "visibility", (show === undefined || show) ? "visible" : "hidden");
  },


  /**
   * Get the SVG image's backgound color.
   * @return {String}   the background color (or None).
   */
  get_bkg_color : function() {
    svg = document.getElementById(adh_svg.plot_id);
    plotting_areas = adh_svg._getElementsByClassName(svg, "plotting-area-background");
    return adh_svg.get_svg_attribute(plotting_areas, "fill");
  },

  /**
   * Change the SVG image's backgound color.
   * @param {String} c  The new background color.
   * @return {None}
   */
  set_bkg_color : function(c) {
    svg = document.getElementById(adh_svg.plot_id);
    plotting_areas = adh_svg._getElementsByClassName(svg, "plotting-area-background");
    adh_svg.set_svg_attribute(plotting_areas, "fill", c);
  },


  /**
   * Get the SVG image's size.
   * @return {Dictionary{w,h}}  dictionary of current width (w) and height (h)
   */
  get_plot_size : function() {
    svg = document.getElementById(adh_svg.plot_id);
    /*return {"w" : (svg.style.width) ? svg.style.width : "auto",
            "h" : (svg.style.height) ? svg.style.height : "auto"}; // this is special: we go for CSS attributes directly*/
    return {"w" : (svg.clientWidth) ? svg.clientWidth : "auto",
            "h" : (svg.clientHeight) ? svg.clientHeight : "auto"}; // get the actual size in pixels
  },

  /**
   * Set the SVG image's size.
   * @param {String} w  The new width (follows the rules for CSS width).
   * @param {String} h  The new height (follows the rules for CSS height).
   * @return {None}
   */
  set_plot_size : function(w, h) {
    svg = document.getElementById(adh_svg.plot_id);
    svg.style.width = (w === undefined) ? "auto" : w; svg.style.height = (h === undefined) ? "auto" : h; // this is special: we go for CSS attributes directly
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
  show_alternating_bands : function(show) {
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
    x = adh_svg._getElementsByClassName(svg, "axis-name-x"); ret_val["x"] = adh_svg.is_visible_svg_element(x);
    y = adh_svg._getElementsByClassName(svg, "axis-name-y"); ret_val["y"] = adh_svg.is_visible_svg_element(y);
    return ret_val;
  },

  /**
   * Show/hide the axis names.
   * @param {Boolean} show_x  show x axis name if true, otherwise hide it.
   * @param {Boolean} show_y  show y axis name if true, otherwise hide it.
   * @return {None}
   */
  show_axis_names : function(show_x, show_y) {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "axis-name-x"); adh_svg.show_svg_element(x, show_x);
    y = adh_svg._getElementsByClassName(svg, "axis-name-y"); adh_svg.show_svg_element(y, show_y);
  },


  /**
   * Are the axis labels visible?
   * @return {Dictionary{x,y}} true if the corresponding axis label is visible
   */
  is_visible_axis_labels : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":false, "y":false}; // the return value
    x = adh_svg._getElementsByClassName(svg, "axis-labels-x"); ret_val["x"] = adh_svg.is_visible_svg_element(x);
    y = adh_svg._getElementsByClassName(svg, "axis-labels-y"); ret_val["y"] = adh_svg.is_visible_svg_element(y);
    return ret_val;
  },

  /**
   * Show/hide the axis lables.
   * @param {Boolean} show_x  show x axis label if true, otherwise hide it.
   * @param {Boolean} show_y  show y axis label if true, otherwise hide it.
   * @return {None}
   */
  show_axis_labels : function(show_x, show_y) {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "axis-labels-x");    adh_svg.show_svg_element(x, show_x);
    ticks = adh_svg._getElementsByClassName(svg, "axis-ticks-x"); adh_svg.show_svg_element(ticks, show_x);
    y = adh_svg._getElementsByClassName(svg, "axis-labels-y");    adh_svg.show_svg_element(y, show_y);
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
  show_legend : function(show) {
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
    x = adh_svg._getElementsByClassName(svg, "main-title");
    return adh_svg.is_visible_svg_element(x);
  },

  /**
   * Show/hide the title.
   * @param {Boolean} show  show title if true, otherwise hide it.
   * @return {None}
   */
  show_title : function(show) {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "main-title");
    adh_svg.show_svg_element(x, show);
  },


  /**
   * Get font size for title
   * @return {Numeric} the font size
   */
  get_font_size_title : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "main-title");
    return adh_svg.get_svg_attribute(x[0], "font-size");
  },

  /**
   * Set font size for title
   * @param {String} s the new font size
   * @return {None}
   */
  set_font_size_title : function(s) {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "main-title");
    adh_svg.set_svg_attribute(x, "font-size", (s === undefined) ? "16px" : s);
  },


  /**
   * Is the follow-up window (FUW) visible?
   * @return {Boolean} true if the FUW is visible
   */
  is_visible_fuw : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "fuw");
    return adh_svg.is_visible_svg_element(x);
  },

  /**
   * Show/hide the follow-up window (FUW).
   * @param {Boolean} show  show FUW if true, otherwise hide it.
   * @return {None}
   */
  show_fuw : function(show) {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "fuw");
    //x_legend_rect = adh_svg._getElementsByClassName(svg, "legend-fuw-rect");
    x_legend_text = adh_svg._getElementsByClassName(svg, "legend-fuw-label");

    adh_svg.show_svg_element(x, show);
    //adh_svg.set_svg_attribute(x_legend_rect, "stroke", (show === undefined || show) ? "Black" : "LightGray");
    adh_svg.set_svg_attribute(x_legend_text, "fill",   (show === undefined || show) ? "Black" : "LightGray", elem_type="font");
  },


  /**
   * Is the observation window (OW) visible?
   * @return {Boolean} true if the OW is visible
   */
  is_visible_ow : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "ow");
    return adh_svg.is_visible_svg_element(x);
  },

  /**
   * Show/hide the observation window (OW).
   * @param {Boolean} show  show OW if true, otherwise hide it.
   * @return {None}
   */
  show_ow : function(show) {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "ow");
    //x_legend_rect = adh_svg._getElementsByClassName(svg, "legend-ow-rect");
    x_legend_text = adh_svg._getElementsByClassName(svg, "legend-ow-label");

    adh_svg.show_svg_element(x, show);
    //adh_svg.set_svg_attribute(x_legend_rect, "stroke", (show === undefined || show) ? "Black" : "LightGray");
    adh_svg.set_svg_attribute(x_legend_text, "fill",   (show === undefined || show) ? "Black" : "LightGray", elem_type="font");
  },


  /**
   * Is the "real" observation window (CMA8 only) visible?
   * @return {Boolean} true if the "real" observation window is visible
   */
  is_visible_ow_real : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "ow-real");
    return adh_svg.is_visible_svg_element(x);
  },

  /**
   * Show/hide the "real" observation window (OW).
   * @param {Boolean} show  the "real" OW if true, otherwise hide it.
   * @return {None}
   */
  show_ow_real : function(show) {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "ow-real");
    //x_legend_rect = adh_svg._getElementsByClassName(svg, "legend-ow-real-rect");
    x_legend_text = adh_svg._getElementsByClassName(svg, "legend-ow-real-label");

    adh_svg.show_svg_element(x, show);
    //adh_svg.set_svg_attribute(x_legend_rect, "stroke", (show === undefined || show) ? "Black" : "LightGray");
    adh_svg.set_svg_attribute(x_legend_text, "fill",   (show === undefined || show) ? "Black" : "LightGray", elem_type="font");
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
   * @param {String} the medication class name or undefined for all classes (or no class, if classes are undefined)
   * @return {String} the medication classe id
   */
  get_id_for_medication_class : function(m) {
    if( !adh_svg.are_medication_classes_defined() || m === null || m === undefined || Array.isArray(m) ) {
      return null;
    } else {
      return adh_svg.medication_classes[m];
    }
  },

  /**
   * Is a given medication class visible?
   * @param {String} the medication class name; null (or undefined) means there are no medication classes defined
   * @return {Boolean} true if visible
   */
  is_visible_medication_class : function(m) {
    svg = document.getElementById(adh_svg.plot_id);

    if( !adh_svg.are_medication_classes_defined() ) {
      // No medication classes defined
      x_start = adh_svg._getElementsByClassName(svg, "event-start");
    } else {
      // Get the given medication class
      m_id = adh_svg.get_id_for_medication_class(m);
      if( !m_id ) return false; // cannot get the ID, so it's not visible by definition
      x_start = adh_svg._getElementsByClassName(svg, "event-start-" + m_id);
    }

    return adh_svg.is_visible_svg_element(x_start);
  },

  /**
   * Show/hide a given medication class.
   * @param {String} the medication class name; null (or undefined) means there are no medication classes defined
   * @param {Boolean} show  show title if true, otherwise hide it.
   * @return {None}
   */
  show_medication_class : function(m, show) {
    svg = document.getElementById(adh_svg.plot_id);

    if( !adh_svg.are_medication_classes_defined() ) {
      // No medication classes defined:
      x_start = adh_svg._getElementsByClassName(svg, "event-start");
      x_end = adh_svg._getElementsByClassName(svg, "event-end");
      x_covered = adh_svg._getElementsByClassName(svg, "event-interval-covered");
      x_notcovered = adh_svg._getElementsByClassName(svg, "event-interval-not-covered");
      x_segment = adh_svg._getElementsByClassName(svg, "event-segment");
      x_dose = adh_svg._getElementsByClassName(svg, "event-dose-text");
      x_continuation = adh_svg._getElementsByClassName(svg, "continuation-line");
      x_legend_rect = adh_svg._getElementsByClassName(svg, "legend-medication-class-rect");
      x_legend_text = adh_svg._getElementsByClassName(svg, "legend-medication-class-label");
    } else {
      // Get the given medication class:
      m_id = adh_svg.get_id_for_medication_class(m);
      if( !m_id ) return false; // cannot get the ID, so it's not visible by definition
      x_start = adh_svg._getElementsByClassName(svg, "event-start-" + m_id);
      x_end = adh_svg._getElementsByClassName(svg, "event-end-" + m_id);
      x_covered = adh_svg._getElementsByClassName(svg, "event-interval-covered-" + m_id);
      x_notcovered = adh_svg._getElementsByClassName(svg, "event-interval-not-covered-" + m_id);
      x_segment = adh_svg._getElementsByClassName(svg, "event-segment-" + m_id);
      x_dose = adh_svg._getElementsByClassName(svg, "event-dose-text-" + m_id);
      x_continuation = adh_svg._getElementsByClassName(svg, "continuation-line-" + m_id);
      x_legend_rect = adh_svg._getElementsByClassName(svg, "legend-medication-class-rect-" + m_id);
      x_legend_text = adh_svg._getElementsByClassName(svg, "legend-medication-class-label-" + m_id);
    }

    adh_svg.show_svg_element(x_start, show);
    adh_svg.show_svg_element(x_end, show);
    adh_svg.show_svg_element(x_covered, show);
    adh_svg.show_svg_element(x_notcovered, show);
    adh_svg.show_svg_element(x_segment, show);
    adh_svg.show_svg_element(x_dose, show);
    adh_svg.show_svg_element(x_continuation, show);

    adh_svg.set_svg_attribute(x_legend_rect, "stroke", (show === undefined || show) ? "Black" : "LightGray");
    adh_svg.set_svg_attribute(x_legend_text, "fill",   (show === undefined || show) ? "Black" : "LightGray", elem_type="font");
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

  // Check if browser supports embedded SVG:
  if( !adh_svg.is_embedded_SVG_supported() ) {
    // No SVG support:
    document.documentElemement.classList.add("no-svg");
  }

  // Various set-up things:
  svg = document.getElementById(adh_svg.plot_id);

  // Make (parts of) the legend clickable:
  // The medication classes (if any):
  m = adh_svg.get_medication_classes();
  if(m) {
    for(i=0; i<m.length; i++) {
      l_rect = adh_svg._getElementsByClassName(svg, "legend-medication-class-rect-" + adh_svg.get_id_for_medication_class(m[i]));
      for(j=0; j<l_rect.length; j++) {
        l_rect[j].style.cursor = "pointer";
        l_rect[j].addEventListener("click", (function(x){ return function() { /*console.log('AddEvent()!');*/ adh_svg.show_medication_class(x, !adh_svg.is_visible_medication_class(x)); }; })(m[i]), false);
      }
      l_label = adh_svg._getElementsByClassName(svg, "legend-medication-class-label-" + adh_svg.get_id_for_medication_class(m[i]));
      for(j=0; j<l_label.length; j++) {
        l_label[j].style.cursor = "pointer";
        l_label[j].addEventListener("click", (function(x){ return function() { adh_svg.show_medication_class(x, !adh_svg.is_visible_medication_class(x)); }; })(m[i]), false);
      }
    }
  }
  // The FUW (if any):
  l_rect = adh_svg._getElementsByClassName(svg, "legend-fuw-rect");
  for(j=0; j<l_rect.length; j++) {
    l_rect[j].style.cursor = "pointer";
    l_rect[j].addEventListener("click", function(e){ adh_svg.show_fuw(!adh_svg.is_visible_fuw()); }, false);
  }
  l_label = adh_svg._getElementsByClassName(svg, "legend-fuw-label");
  for(j=0; j<l_label.length; j++) {
    l_label[j].style.cursor = "pointer";
    l_label[j].addEventListener("click", function(e){ adh_svg.show_fuw(!adh_svg.is_visible_fuw()); }, false);
  }
  // The OW (if any):
  l_rect = adh_svg._getElementsByClassName(svg, "legend-ow-rect");
  for(j=0; j<l_rect.length; j++) {
    l_rect[j].style.cursor = "pointer";
    l_rect[j].addEventListener("click", function(e){ adh_svg.show_ow(!adh_svg.is_visible_ow()); }, false);
  }
  l_label = adh_svg._getElementsByClassName(svg, "legend-ow-label");
  for(j=0; j<l_label.length; j++) {
    l_label[j].style.cursor = "pointer";
    l_label[j].addEventListener("click", function(e){ adh_svg.show_ow(!adh_svg.is_visible_ow()); }, false);
  }
  // The "real" OW [CMA8] (if any):
  l_rect = adh_svg._getElementsByClassName(svg, "legend-ow-real-rect");
  for(j=0; j<l_rect.length; j++) {
    l_rect[j].style.cursor = "pointer";
    l_rect[j].addEventListener("click", function(e){ adh_svg.show_ow_real(!adh_svg.is_visible_ow_real()); }, false);
  }
  l_label = adh_svg._getElementsByClassName(svg, "legend-ow-real-label");
  for(j=0; j<l_label.length; j++) {
    l_label[j].style.cursor = "pointer";
    l_label[j].addEventListener("click", function(e){ adh_svg.show_ow_real(!adh_svg.is_visible_ow_real()); }, false);
  }

  /*// TEST:
  e1 = adh_svg._getElementsByClassName(svg, "main-title")[0];
  e1.style.cursor = "pointer"; e1.addEventListener("click", (function(x){ return function() { console.log('AddEvent()!'); adh_svg.show_medication_class(x, !adh_svg.is_visible_medication_class(x)); }; })("medA"), false);*/
}







