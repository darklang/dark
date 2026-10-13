const esbuild = require("esbuild");
const fs = require("fs");

const production = process.argv.includes("--production");
const watch = process.argv.includes("--watch");

/**
 * Plugin to load .svg files as inline strings
 */
const svgPlugin = {
  name: "svg-loader",
  setup(build) {
    build.onLoad({ filter: /\.svg$/ }, async (args) => {
      try {
        const content = await fs.promises.readFile(args.path, "utf8");
        const trimmed = content.trim();

        // Validate SVG content
        if (!trimmed.startsWith("<svg")) {
          return {
            errors: [
              {
                text: "File does not appear to be a valid SVG (must start with <svg)",
                location: { file: args.path },
              },
            ],
          };
        }
        if (!trimmed.endsWith("</svg>")) {
          return {
            errors: [
              {
                text: "File does not appear to be a valid SVG (missing closing </svg> tag)",
                location: { file: args.path },
              },
            ],
          };
        }

        // Normalize whitespace for cleaner output
        const normalized = trimmed.replace(/\n\s*/g, " ");
        return {
          contents: `export default ${JSON.stringify(normalized)};`,
          loader: "js",
        };
      } catch (error) {
        return {
          errors: [
            {
              text: `Failed to load SVG file: ${error.message}`,
              location: { file: args.path },
            },
          ],
        };
      }
    });
  },
};

/**
 * Plugin to log build status
 */
const statusPlugin = {
  name: "status",
  setup(build) {
    build.onStart(() => {
      console.log("[watch] build started");
    });
    build.onEnd((result) => {
      if (result.errors.length > 0) {
        console.error(`[watch] build failed with ${result.errors.length} errors`);
      } else {
        console.log("[watch] build finished");
      }
    });
  },
};

async function main() {
  const ctx = await esbuild.context({
    entryPoints: ["client/src/extension.ts"],
    bundle: true,
    format: "cjs",
    minify: production,
    sourcemap: !production,
    sourcesContent: !production,
    platform: "node",
    outfile: "client/out/extension.js",
    external: ["vscode"],
    logLevel: "warning",
    plugins: [svgPlugin, statusPlugin],
  });

  if (watch) {
    await ctx.watch();
    console.log("[esbuild] Watching for changes...");
  } else {
    await ctx.rebuild();
    await ctx.dispose();
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
