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

// From https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/keys
if (!Object.keys) {
  Object.keys = (function() {
    'use strict';
    var hasOwnProperty = Object.prototype.hasOwnProperty,
        hasDontEnumBug = !({ toString: null }).propertyIsEnumerable('toString'),
        dontEnums = [
          'toString',
          'toLocaleString',
          'valueOf',
          'hasOwnProperty',
          'isPrototypeOf',
          'propertyIsEnumerable',
          'constructor'
        ],
        dontEnumsLength = dontEnums.length;

    return function(obj) {
      if (typeof obj !== 'function' && (typeof obj !== 'object' || obj === null)) {
        throw new TypeError('Object.keys called on non-object');
      }

      var result = [], prop, i;

      for (prop in obj) {
        if (hasOwnProperty.call(obj, prop)) {
          result.push(prop);
        }
      }

      if (hasDontEnumBug) {
        for (i = 0; i < dontEnumsLength; i++) {
          if (hasOwnProperty.call(obj, dontEnums[i])) {
            result.push(dontEnums[i]);
          }
        }
      }
      return result;
    };
  }());
}

// From: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/isArray
if (!Array.isArray) {
  Array.isArray = function(arg) {
    return Object.prototype.toString.call(arg) === '[object Array]';
  };
}


// Simulate a namespace 'adh_svg' to avoid potential conflicts with other JavaScript libraries:
var adh_svg = { // begin namespace

  // The SVG plot's ID:
  plot_id : 'adherence_plot',

  // Default values so we are able to restore them later if need be:
  label_style_default : "color: black", // the default lablel CSS style
  label_style_disabled : "color: #aaa;", // the disabled label look
  default_svg_width : "auto", default_svg_height : "auto", // default SVG size
  default_font_size_title : "15px", // default axes font sizes
  default_font_size_axis_names : {"x":"10px", "y":"10px"}, // default axes names font sizes
  default_font_size_axis_labels : {"x":"8px", "y":"8px"}, // default axes labels font sizes

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
   * Are there alternating bands visible in the document?
   * @return {Boolean}  true if they do exist
   */
  exists_alternating_bands : function() {
    svg = document.getElementById(adh_svg.plot_id);
    alt_bands = svg.querySelectorAll('[class^="alternating-bands-"]');
    return !(!alt_bands || alt_bands.length < 1);
  },

  /**
   * Are the alternating bands visible?
   * @return {Boolean}  true if they are visible
   */
  is_visible_alternating_bands : function() {
    svg = document.getElementById(adh_svg.plot_id);
    alt_bands = svg.querySelectorAll('[class^="alternating-bands-"]');
    if(!alt_bands || alt_bands.length < 1) return undefined;
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
    if(!(!alt_bands || alt_bands.length < 1)) adh_svg.show_svg_element(alt_bands, show);
  },


  /**
   * Are the axis names defined?
   * @return {Dictionary{x,y}} true if the corresponding axis name is defined
   */
  exists_axis_names : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":false, "y":false}; // the return value
    x = adh_svg._getElementsByClassName(svg, "axis-name-x"); ret_val["x"] = !(!x || x.length < 1);
    y = adh_svg._getElementsByClassName(svg, "axis-name-y"); ret_val["y"] = !(!y || y.length < 1);
    return ret_val;
  },

  /**
   * Are the axis names visible?
   * @return {Dictionary{x,y}} true if the corresponding axis name is visible
   */
  is_visible_axis_names : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":false, "y":false}; // the return value
    x = adh_svg._getElementsByClassName(svg, "axis-name-x"); ret_val["x"] = adh_svg.is_visible_svg_element(x[0]);
    y = adh_svg._getElementsByClassName(svg, "axis-name-y"); ret_val["y"] = adh_svg.is_visible_svg_element(y[0]);
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
    x = adh_svg._getElementsByClassName(svg, "axis-name-x"); adh_svg.show_svg_element(x[0], show_x);
    y = adh_svg._getElementsByClassName(svg, "axis-name-y"); adh_svg.show_svg_element(y[0], show_y);
  },

  /**
   * Get font size for axis names.
   * @return {Dictionary{x,y}} the font sizes
   */
  get_font_size_axis_names : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":false, "y":false}; // the return value
    x = adh_svg._getElementsByClassName(svg, "axis-name-x"); ret_val["x"] = adh_svg.get_svg_attribute(x[0], "font-size");
    y = adh_svg._getElementsByClassName(svg, "axis-name-y"); ret_val["y"] = adh_svg.get_svg_attribute(y[0], "font-size");
    return ret_val;
  },

  /**
   * Set font size for axis names.
   * @param {String} sx the new font size for x axis
   * @param {String} sy the new font size for y axis
   * @return {None}
   */
  set_font_size_axis_names : function(sx, sy) {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "axis-name-x"); adh_svg.set_svg_attribute(x[0], "font-size", (sx === undefined) ? adh_svg.default_font_size_axis_names["x"] : sx);
    y = adh_svg._getElementsByClassName(svg, "axis-name-y"); adh_svg.set_svg_attribute(y[0], "font-size", (sy === undefined) ? adh_svg.default_font_size_axis_names["y"] : sy);
  },


  /**
   * Are the axis labels defined?
   * @return {Dictionary{x,y}} true if the corresponding axis label is defined
   */
  exists_axis_labels : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":false, "y":false}; // the return value
    x = adh_svg._getElementsByClassName(svg, "axis-labels-x"); ret_val["x"] = !(!x || x.length < 1);
    y = adh_svg._getElementsByClassName(svg, "axis-labels-y"); ret_val["y"] = !(!y || y.length < 1);
    return ret_val;
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
   * Get font size for axis labels.
   * @return {Dictionary{x,y}} the font sizes
   */
  get_font_size_axis_labels : function() {
    svg = document.getElementById(adh_svg.plot_id);
    ret_val = {"x":false, "y":false}; // the return value
    x = adh_svg._getElementsByClassName(svg, "axis-labels-x"); ret_val["x"] = adh_svg.get_svg_attribute(x[0], "font-size");
    y = adh_svg._getElementsByClassName(svg, "axis-labels-y"); ret_val["y"] = adh_svg.get_svg_attribute(y[0], "font-size");
    return ret_val;
  },

  /**
   * Set font size for axis labels.
   * @param {String} sx the new font size for x axis
   * @param {String} sy the new font size for y axis
   * @return {None}
   */
  set_font_size_axis_labels : function(sx, sy) {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "axis-labels-x"); adh_svg.set_svg_attribute(x, "font-size", (sx === undefined) ? adh_svg.default_font_size_axis_labels["x"] : sx);
    y = adh_svg._getElementsByClassName(svg, "axis-labels-y"); adh_svg.set_svg_attribute(y, "font-size", (sy === undefined) ? adh_svg.default_font_size_axis_labels["y"] : sy);
  },


  /**
   * Does the legend exist?
   * @return {Boolean} true if the legend exists
   */
  exists_legend : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementById("legend");
    return !(!x || x.length < 1);
  },

  /**
   * Is the legend visible?
   * @return {Boolean} true if the legend is visible
   */
  is_visible_legend : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = svg.getElementById("legend");
    if(!x || x.length < 1) return undefined;
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
    if(x) adh_svg.show_svg_element(x, show);
  },


  /**
   * Does the title exist?
   * @return {Boolean} true if the title exists
   */
  exists_title : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "main-title");
    return !(!x || x.length < 1);
  },

  /**
   * Is the title visible?
   * @return {Boolean} true if the title is visible
   */
  is_visible_title : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "main-title");
    if(!x || x.length < 1) return undefined;
    return adh_svg.is_visible_svg_element(x[0]);
  },

  /**
   * Show/hide the title.
   * @param {Boolean} show  show title if true, otherwise hide it.
   * @return {None}
   */
  show_title : function(show) {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "main-title");
    if(x) adh_svg.show_svg_element(x[0], show);
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
   * Does the follow-up window (FUW) exist?
   * @return {Boolean} true if the FUW exists
   */
  exists_fuw : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "fuw");
    return !(!x || x.length < 1);
  },

  /**
   * Is the follow-up window (FUW) visible?
   * @return {Boolean} true if the FUW is visible
   */
  is_visible_fuw : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "fuw");
    if(!x || x.length < 1) return undefined;
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
    //x_fuw_rect = adh_svg._getElementsByClassName(svg, "legend-fuw-rect");
    x_fuw_text = adh_svg._getElementsByClassName(svg, "legend-fuw-label");

    if(x) adh_svg.show_svg_element(x, show);
    //adh_svg.set_svg_attribute(x_fuw_rect, "stroke", (show === undefined || show) ? "Black" : "LightGray");
    if(x_fuw_text) adh_svg.set_svg_attribute(x_fuw_text, "fill",   (show === undefined || show) ? "Black" : "LightGray", elem_type="font");
  },


  /**
   * Does the observation window (OW) exist?
   * @return {Boolean} true if the OW exists
   */
  exists_ow : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "ow");
    return !(!x || x.length < 1);
  },

  /**
   * Is the observation window (OW) visible?
   * @return {Boolean} true if the OW is visible
   */
  is_visible_ow : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "ow");
    if(!x || x.length < 1) return undefined;
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
    //x_ow_rect = adh_svg._getElementsByClassName(svg, "legend-ow-rect");
    x_ow_text = adh_svg._getElementsByClassName(svg, "legend-ow-label");

    if(x) adh_svg.show_svg_element(x, show);
    //adh_svg.set_svg_attribute(x_ow_rect, "stroke", (show === undefined || show) ? "Black" : "LightGray");
    if(x_ow_text) adh_svg.set_svg_attribute(x_ow_text, "fill",   (show === undefined || show) ? "Black" : "LightGray", elem_type="font");
  },


  /**
   * Is there a "real" observation window (CMA8 only) defined?
   * @return {Boolean} true if the "real" observation window is defined
   */
  exists_ow_real : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "ow-real");
    return !(!x || x.length < 1);
  },

  /**
   * Is the "real" observation window (CMA8 only) visible?
   * @return {Boolean} true if the "real" observation window is visible
   */
  is_visible_ow_real : function() {
    svg = document.getElementById(adh_svg.plot_id);
    x = adh_svg._getElementsByClassName(svg, "ow-real");
    if(!x || x.length < 1) return undefined;
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

    if(x) adh_svg.show_svg_element(x, show);
    //adh_svg.set_svg_attribute(x_legend_rect, "stroke", (show === undefined || show) ? "Black" : "LightGray");
    if(x_legend_text) adh_svg.set_svg_attribute(x_legend_text, "fill",   (show === undefined || show) ? "Black" : "LightGray", elem_type="font");
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


// Other functions used for HTML interactions

// Change the SVG image size up and down by a given multiplier:
function image_change_size(ds) {
  img_dims = adh_svg.get_plot_size(); // get the current image dimensions

  new_w = img_dims.w * ds; new_h = img_dims.h * ds; // the new size
  if(new_w < 1 || new_h < 1) return; // can't go below 1!

  adh_svg.set_plot_size(new_w + 'px', new_h + 'px'); // set the new dimension
}


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

  // Save default values so we are able to restore them later if need be:
  tmp = document.getElementById("button_toggle_alt_bands");
  adh_svg.label_style_default = tmp ? tmp.style : "none"; // save the default lablel CSS style
  adh_svg.label_style_disabled = "color: #aaa;" // and this is the disabled lable look
  img_dims = adh_svg.get_plot_size(); adh_svg.default_svg_width = (img_dims.w === undefined) ? "auto" : img_dims.w; adh_svg.default_svg_height = (img_dims.h === undefined) ? "auto" : img_dims.h; // default SVG size
  adh_svg.default_font_size_title = adh_svg.get_font_size_title(); // default title font sizes
  adh_svg.default_font_size_axis_names = adh_svg.get_font_size_axis_names(); // default axes names font sizes
  adh_svg.default_font_size_axis_labels = adh_svg.get_font_size_axis_labels(); // default axes labels font sizes

  // Make (parts of) the legend clickable:
  // The medication classes (if any):
  m = adh_svg.get_medication_classes();
  if(m) {
    // Hide the medication classes controls:
    tmp = document.getElementById("medication_classes_div"); if(tmp) { tmp.style.display = 'block'; }

    // Iterate through all medication classes:
    for(i=0; i<m.length; i++) {
      l_rect = adh_svg._getElementsByClassName(svg, "legend-medication-class-rect-" + adh_svg.get_id_for_medication_class(m[i]));
      for(j=0; j<l_rect.length; j++) {
        l_rect[j].style.cursor = "pointer";
        l_rect[j].addEventListener("click", (function(x){ return function() { adh_svg.show_medication_class(x, !adh_svg.is_visible_medication_class(x)); tmp = document.getElementById("button_toggle_class_" + adh_svg.get_id_for_medication_class(x)); if(tmp) { tmp.checked = !tmp.checked; } }; })(m[i]), false);
      }
      l_label = adh_svg._getElementsByClassName(svg, "legend-medication-class-label-" + adh_svg.get_id_for_medication_class(m[i]));
      for(j=0; j<l_label.length; j++) {
        l_label[j].style.cursor = "pointer";
        l_label[j].addEventListener("click", (function(x){ return function() { adh_svg.show_medication_class(x, !adh_svg.is_visible_medication_class(x)); tmp = document.getElementById("button_toggle_class_" + adh_svg.get_id_for_medication_class(x)); if(tmp) { tmp.checked = !tmp.checked; } }; })(m[i]), false);
      }
      // Add the HTML elements as well:
      node = document.createElement('span'); // the contaning <span>
      node.title = "Show/hide " + m[i]; // the tooltip (title)
      node.innerHTML = '<label id="label_toggle_class_' + adh_svg.get_id_for_medication_class(m[i]) + '"><input id="button_toggle_class_' + adh_svg.get_id_for_medication_class(m[i]) + '" type="checkbox" onclick=\'adh_svg.show_medication_class("' + m[i] + '", !adh_svg.is_visible_medication_class("' + m[i] + '"))\' checked="checked">' + m[i] + '</label> &nbsp;'; // the HTML content
      tmp.appendChild(node); // ad it to the document
    }
  } else
  {
    // Hide the medication classes controls:
    tmp = document.getElementById("medication_classes_div"); if(tmp) { tmp.style.display = 'none'; }
  }
  // The FUW (if any):
  l_rect = adh_svg._getElementsByClassName(svg, "legend-fuw-rect");
  for(j=0; j<l_rect.length; j++) {
    l_rect[j].style.cursor = "pointer";
    l_rect[j].addEventListener("click", function(e){ adh_svg.show_fuw(!adh_svg.is_visible_fuw()); tmp = document.getElementById("button_toggle_fuw"); if(tmp) { tmp.checked = !tmp.checked; } }, false);
  }
  l_label = adh_svg._getElementsByClassName(svg, "legend-fuw-label");
  for(j=0; j<l_label.length; j++) {
    l_label[j].style.cursor = "pointer";
    l_label[j].addEventListener("click", function(e){ adh_svg.show_fuw(!adh_svg.is_visible_fuw()); tmp = document.getElementById("button_toggle_fuw"); if(tmp) { tmp.checked = !tmp.checked; } }, false);
  }
  // The OW (if any):
  l_rect = adh_svg._getElementsByClassName(svg, "legend-ow-rect");
  for(j=0; j<l_rect.length; j++) {
    l_rect[j].style.cursor = "pointer";
    l_rect[j].addEventListener("click", function(e){ adh_svg.show_ow(!adh_svg.is_visible_ow()); tmp = document.getElementById("button_toggle_ow"); if(tmp) { tmp.checked = !tmp.checked; } }, false);
  }
  l_label = adh_svg._getElementsByClassName(svg, "legend-ow-label");
  for(j=0; j<l_label.length; j++) {
    l_label[j].style.cursor = "pointer";
    l_label[j].addEventListener("click", function(e){ adh_svg.show_ow(!adh_svg.is_visible_ow()); tmp = document.getElementById("button_toggle_ow"); if(tmp) { tmp.checked = !tmp.checked; } }, false);
  }
  // The "real" OW [CMA8] (if any):
  l_rect = adh_svg._getElementsByClassName(svg, "legend-ow-real-rect");
  for(j=0; j<l_rect.length; j++) {
    l_rect[j].style.cursor = "pointer";
    l_rect[j].addEventListener("click", function(e){ adh_svg.show_ow_real(!adh_svg.is_visible_ow_real()); tmp = document.getElementById("button_toggle_ow_real"); if(tmp) { tmp.checked = !tmp.checked; } }, false);
  }
  l_label = adh_svg._getElementsByClassName(svg, "legend-ow-real-label");
  for(j=0; j<l_label.length; j++) {
    l_label[j].style.cursor = "pointer";
    l_label[j].addEventListener("click", function(e){ adh_svg.show_ow_real(!adh_svg.is_visible_ow_real()); tmp = document.getElementById("button_toggle_ow_real"); if(tmp) { tmp.checked = !tmp.checked; } }, false);
  }

  // (Un)check and (dis)able various components in the HTML document
  // the idea is to disable the check button and the label if the element does not exist in the SVG, and to enable it if the element exists and is visible...
  if(adh_svg.exists_alternating_bands()) {
    tmp = document.getElementById("button_toggle_alt_bands"); if(tmp) { tmp.disabled = false; tmp.checked = adh_svg.is_visible_alternating_bands(); }
    tmp = document.getElementById("label_toggle_alt_bands"); if(tmp) { tmp.disabled = false; tmp.style = adh_svg.label_style_default; }
  } else {
    tmp = document.getElementById("button_toggle_alt_bands"); if(tmp) { tmp.disabled = true; tmp.checked = false; }
    tmp = document.getElementById("label_toggle_alt_bands"); if(tmp) { tmp.disabled = true; tmp.style = adh_svg.label_style_disabled; }
  }

  if(adh_svg.exists_axis_names()["x"]) {
    tmp = document.getElementById("button_toggle_x_axis_name"); if(tmp) { tmp.disabled = false; tmp.checked = adh_svg.is_visible_axis_names()["x"]; }
    tmp = document.getElementById("label_toggle_x_axis_name"); if(tmp) { tmp.disabled = false; tmp.style = adh_svg.label_style_default; }
  } else {
    tmp = document.getElementById("button_toggle_x_axis_name"); if(tmp) { tmp.disabled = true; tmp.checked = false; }
    tmp = document.getElementById("label_toggle_x_axis_name"); if(tmp) { tmp.disabled = true; tmp.style = adh_svg.label_style_disabled; }
  }

  if(adh_svg.exists_axis_labels()["x"]) {
    tmp = document.getElementById("button_toggle_x_axis_labels"); if(tmp) { tmp.disabled = false; tmp.checked = adh_svg.is_visible_axis_labels()["x"]; }
    tmp = document.getElementById("label_toggle_x_axis_labels"); if(tmp) { tmp.disabled = false; tmp.style = adh_svg.label_style_default; }
  } else {
    tmp = document.getElementById("button_toggle_x_axis_labels"); if(tmp) { tmp.disabled = true; tmp.checked = false; }
    tmp = document.getElementById("label_toggle_x_axis_labels"); if(tmp) { tmp.disabled = true; tmp.style = adh_svg.label_style_disabled; }
  }

  if(adh_svg.exists_axis_names()["y"]) {
    tmp = document.getElementById("button_toggle_y_axis_name"); if(tmp) { tmp.disabled = false; tmp.checked = adh_svg.is_visible_axis_names()["y"]; }
    tmp = document.getElementById("label_toggle_y_axis_name"); if(tmp) { tmp.disabled = false; tmp.style = adh_svg.label_style_default; }
  } else {
    tmp = document.getElementById("button_toggle_y_axis_name"); if(tmp) { tmp.disabled = true; tmp.checked = false; }
    tmp = document.getElementById("label_toggle_y_axis_name"); if(tmp) { tmp.disabled = true; tmp.style = adh_svg.label_style_disabled; }
  }

  if(adh_svg.exists_axis_labels()["y"]) {
    tmp = document.getElementById("button_toggle_y_axis_labels"); if(tmp) { tmp.disabled = false; tmp.checked = adh_svg.is_visible_axis_labels()["y"]; }
    tmp = document.getElementById("label_toggle_y_axis_labels"); if(tmp) { tmp.disabled = false; tmp.style = adh_svg.label_style_default; }
  } else {
    tmp = document.getElementById("button_toggle_y_axis_labels"); if(tmp) { tmp.disabled = true; tmp.checked = false; }
    tmp = document.getElementById("label_toggle_y_axis_labels"); if(tmp) { tmp.disabled = true; tmp.style = adh_svg.label_style_disabled; }
  }

  if(adh_svg.exists_title()) {
    tmp = document.getElementById("button_toggle_title"); if(tmp) { tmp.disabled = false; tmp.checked = adh_svg.is_visible_title(); }
    tmp = document.getElementById("label_toggle_title"); if(tmp) { tmp.disabled = false; tmp.style = adh_svg.label_style_default; }
  } else {
    tmp = document.getElementById("button_toggle_title"); if(tmp) { tmp.disabled = true; tmp.checked = false; }
    tmp = document.getElementById("label_toggle_title"); if(tmp) { tmp.disabled = true; tmp.style = adh_svg.label_style_disabled; }
  }

  if(adh_svg.exists_legend()) {
    tmp = document.getElementById("button_toggle_legend"); if(tmp) { tmp.disabled = false; tmp.checked = adh_svg.is_visible_legend(); }
    tmp = document.getElementById("label_toggle_legend"); if(tmp) { tmp.disabled = false; tmp.style = adh_svg.label_style_default; }
  } else {
    tmp = document.getElementById("button_toggle_legend"); if(tmp) { tmp.disabled = true; tmp.checked = false; }
    tmp = document.getElementById("label_toggle_legend"); if(tmp) { tmp.disabled = true; tmp.style = adh_svg.label_style_disabled; }
  }

  if(adh_svg.exists_fuw()) {
    tmp = document.getElementById("button_toggle_fuw"); if(tmp) { tmp.disabled = false; tmp.checked = adh_svg.is_visible_fuw(); }
    tmp = document.getElementById("label_toggle_fuw"); if(tmp) { tmp.disabled = false; tmp.style = adh_svg.label_style_default; }
  } else {
    tmp = document.getElementById("button_toggle_fuw"); if(tmp) { tmp.disabled = true; tmp.checked = false; }
    tmp = document.getElementById("label_toggle_fuw"); if(tmp) { tmp.disabled = true; tmp.style = adh_svg.label_style_disabled; }
  }

  if(adh_svg.exists_ow()) {
    tmp = document.getElementById("button_toggle_ow"); if(tmp) { tmp.disabled = false; tmp.checked = adh_svg.is_visible_ow(); }
    tmp = document.getElementById("label_toggle_ow"); if(tmp) { tmp.disabled = false; tmp.style = adh_svg.label_style_default; }
  } else {
    tmp = document.getElementById("button_toggle_ow"); if(tmp) { tmp.disabled = true; tmp.checked = false; }
    tmp = document.getElementById("label_toggle_ow"); if(tmp) { tmp.disabled = true; tmp.style = adh_svg.label_style_disabled; }
  }

  if(adh_svg.exists_ow_real()) {
    tmp = document.getElementById("button_toggle_ow_real"); if(tmp) { tmp.disabled = false; tmp.checked = adh_svg.is_visible_ow_real(); }
    tmp = document.getElementById("label_toggle_ow_real"); if(tmp) { tmp.disabled = false; tmp.style = adh_svg.label_style_default; }
  } else {
    tmp = document.getElementById("button_toggle_ow_real"); if(tmp) { tmp.disabled = true; tmp.checked = false; }
    tmp = document.getElementById("label_toggle_ow_real"); if(tmp) { tmp.disabled = true; tmp.style = adh_svg.label_style_disabled; }
  }

  /*// TEST:
  e1 = adh_svg._getElementsByClassName(svg, "main-title")[0];
  e1.style.cursor = "pointer"; e1.addEventListener("click", (function(x){ return function() { console.log('AddEvent()!'); adh_svg.show_medication_class(x, !adh_svg.is_visible_medication_class(x)); }; })("medA"), false);*/
}







