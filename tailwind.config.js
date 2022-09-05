/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./client/**/*.res"],
  theme: {
    // override builtin tailwind colors
    colors: {
      transparent: "transparent",
      current: "currentColor",
      black1: "#181818",
      black2: "#282828",
      black3: "#383838",

      grey1: "#484848",
      grey2: "#585858",
      grey3: "#b8b8b8",

      white1: "#d8d8d8",
      white2: "#e8e8e8",
      white3: "#f8f8f8",

      red: "#ab4642",
      orange: "#dc9656",
      yellow: "#f7ca88",
      green: "#a1b56c",
      cyan: "#86c1b9",
      blue: "#7cafc2",
      purple: "#b18bba",
      pink: "#d5839d",
      magenta: "magenta",
    },
    extend: {
      borderWidth: {
        3: "3px",
      },
      fontSize: {
        xxs: "0.625rem",
      },
      margin: {
        1.25: "0.3125rem",
      },
      padding: {
        1.25: "0.3125rem",
      },
    },
  },
  corePlugins: {
    // The preflight (https://tailwindcss.com/docs/preflight) options added by
    // default by tailwind knock everything a little bit off center, probably due to
    // the other reset css used, so disable it.
    preflight: false,
  },
  plugins: [],
};
