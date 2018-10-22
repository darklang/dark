import resolve from 'rollup-plugin-node-resolve';

export default {
    input: './lib/es6/src/Main.bs.js',
    output: {
          file: './release/main.js',
          format: 'iife',
          name: 'starter'
        },
    plugins: [
          resolve()
        ]
};
