exports.cssEscape = typeof window === "undefined" || !window.CSS || !window.CSS.escape ? require("css.escape") : window.CSS.escape;
