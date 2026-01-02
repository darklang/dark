/**
 * TypeScript declaration for SVG imports.
 *
 * Tells the compiler that .svg files export a string (the SVG content).
 * The actual inlining is handled by esbuild's SVG plugin at build time.
 */
declare module "*.svg" {
  const content: string;
  export default content;
}
