const path = require('path');

module.exports = {
  entry: './src/Index.bs.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist'),
  },
};