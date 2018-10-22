import resolve from 'rollup-plugin-node-resolve';

export default {
    input: './lib/es6/src/Main.bs.js',
    output: {
          file: '../server/static/bsmain.js',
          format: 'iife',
          name: 'buckle'
        },
    plugins: [
          resolve()
        ]
};
