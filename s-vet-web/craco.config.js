const CracoLessPlugin = require("craco-less");

module.exports = {
  devServer: {
    proxy: {
      "/api": {
        target: "http://127.0.0.1:1234",
        secure: false,
        changeOrigin: true,
      },
    },
  },
  plugins: [
    {
      plugin: CracoLessPlugin,
      options: {
        lessLoaderOptions: {
          lessOptions: {
            javascriptEnabled: true,
          },
        },
      },
    },
  ],
};
