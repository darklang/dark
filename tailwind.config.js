/** @type {import('tailwindcss').Config} */
module.exports = {
  important: true, // tailwind should always win
  content: ["./client/**/*.res"],
  theme: {
    // override builtin tailwind colors
    colors: {
      transparent: "transparent",
      current: "currentColor",
      inherit: "inherit",
      black1: "#181818",
      black2: "#282828",
      black3: "#383838",

      grey1: "#484848",
      grey2: "#585858",
      grey3: "#686868",
      grey4: "#787878",
      grey5: "#888888",
      grey6: "#989898",
      grey7: "#a8a8a8",
      grey8: "#b8b8b8",
      grey9: "#c8c8c8",

      white1: "#d8d8d8",
      white2: "#e8e8e8",
      white3: "#f8f8f8",

      red: "#ab4642",
      orange: "#dc9656",
      yellow: "#f7ca88",
      yellow1: "#fadfb8", //lighten($yellow, 10%)
      green: "#a1b56c",
      cyan: "#86c1b9",
      cyan1: "#3cbfad", //darken($tooltip-color, 25%)
      blue: "#7cafc2",
      purple: "#b18bba",
      purple1: "#c7abcd", // lighten($purple, 10%)
      pink: "#d5839d",
      magenta: "magenta",

      "sidebar-bg": "var(--sidebarBgColor)",
      "sidebar-hover": "var(--sidebarHoverColor)",
      "sidebar-primary": "var(--sidebarPrimaryColor)",
      "sidebar-secondary": "var(--sidebarSecondaryColor)",
      "http-get": "var(--httpGetColor)",
      "http-post": "var(--httpPostColor)",
      "http-put": "var(--httpPutColor)",
      "http-delete": "var(--httpDeleteColor)",
      "http-patch": "var(--httpPatchColor)",
      "http-options": "var(--httpOptionsColor)",
      "http-head": "var(--httpHeadColor)",
      cron: "var(--cronColor)",
      queue: "var(--queueColor)",
      repl: "var(--replColor)",
      "default-toplevel": "var(--defaultToplevelColor)",
      "user-functions": "var(--userFunctionsColor)",
      db: "var(--dbColor)",
      tooltip: "var(--tooltipColor)",
    },
    extend: {
      borderWidth: {
        3: "3px",
      },
      fontSize: {
        xxxs: "0.5rem",
        xxs: "0.625rem",
        md: "1.0625rem", // 17px - between base and lg
      },
      margin: {
        0.25: "0.0625rem",
        1.25: "0.3125rem",
      },
      maxWidth: {
        "3.5xl": "50rem",
      },
      padding: {
        0.25: "0.0625rem",
        1.25: "0.3125rem",
      },
      width: {
        "17/20": "85%",
      },
      zIndex: {
        100: "100",
      },
    },
    fontFamily: {
      body: ['"Fira Code"', "sans-serif"],
      heading: ['"Source Sans Pro"', "sans-serif"],
      accents: ['"Fira Code"', "sans-serif"],
      code: ['"Fira Code"', "mono"],
      text: ['"Source Sans Pro"', "sans-serif"],
      brands: ['"Font Awesome 6 Brands"', "sans-serif"],
    },
  },
  corePlugins: {
    // The preflight (https://tailwindcss.com/docs/preflight) options added by
    // default by tailwind knock everything a little bit off center, probably due to
    // the other reset css used, so disable it.
    preflight: false,
  },
  plugins: [require("tailwind-scrollbar")({ nocompatible: true })],
};
