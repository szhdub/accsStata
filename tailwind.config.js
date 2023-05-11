module.exports = {
  content: [
    './index.html',
    './src/js/main.js',
    './src/css/styles.css',
    './src/elm/**/*.elm'
  ],
  theme: {
    extend: {},
    fontFamily: {
      sans: ['Lexend Deca', 'sans-serif']
    }
  },
  plugins: [ // https://tailwindcss.com/docs/plugins#official-plugins
    require('daisyui') // https://daisyui.com/docs/install/
  ],
  daisyui: { // daisyUI config (optional)
    themes: [
      {
      cyberpunk: {
        ...require("daisyui/src/colors/themes")["[data-theme=cyberpunk]"],
        "base-100": "#F7F7F8",
        "primary":"#48bc25",
        "info":"#5486E8",
        "error": "#FC645F"
      },
    }, 'dark']
  }
}
