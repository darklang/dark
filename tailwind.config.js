/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./client/**/*.res"],
  theme: {
    extend: {},
  },
  corePlugins: {
    // The preflight (https://tailwindcss.com/docs/preflight) options added by
    // default by tailwind knock everything a little bit off center, probably due to
    // the other reset css used, so disable it.
    preflight: false,
  },
  plugins: [],
};
