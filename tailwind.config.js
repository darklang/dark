/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./client/**/*.res"],
  theme: {
    extend: {
      borderWidth: {
        3: "3px",
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
