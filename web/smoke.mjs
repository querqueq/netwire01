// Boots the GHC wasm reactor outside a browser and steps a few frames to
// prove the RTS initialises and the netwire game loop produces sane output.
// Run from a directory containing netwire01-web.wasm and ghc_wasm_jsffi.js,
// with @bjorn3/browser_wasi_shim installed (see .github/workflows/web.yml).
import { WASI, OpenFile, File, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import { readFile } from "node:fs/promises";
import { pathToFileURL } from "node:url";

const TARGET_FRAMES = 10;
let frames = 0;
let last = null;

const fail = (msg) => { console.error("SMOKE FAIL:", msg); process.exit(1); };
process.on("unhandledRejection", (e) => fail(e));
setTimeout(() => fail(`timed out after 30s with ${frames} frames`), 30000);

// Stubs for the five browser interop points used by web/Main.hs.
globalThis.setStars = (s) => {
  const n = String(s).split(";").length;
  if (n !== 150) fail(`expected 150 stars, got ${n}`);
  console.log("setStars: 150 stars received");
};
globalThis.controlBits = () => 4; // hold forward thrust so the ship moves
globalThis.drawFrame = (cx, cy, poly) => {
  if (!Number.isFinite(cx) || !Number.isFinite(cy)) fail("non-finite camera position");
  const pts = String(poly).split(";");
  if (pts.length !== 7) fail(`expected 7 polygon points, got ${pts.length}`);
  frames++;
  last = { cx, cy };
};
globalThis.requestAnimationFrame = (f) => {
  if (frames >= TARGET_FRAMES) {
    if (!(Math.hypot(last.cx, last.cy) > 0)) fail("ship did not move under thrust");
    console.log(`SMOKE OK: ${frames} frames, cam=(${last.cx}, ${last.cy})`);
    process.exit(0);
  }
  setTimeout(f, 16);
};

const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((m) => console.log("[wasi stdout]", m)),
  ConsoleStdout.lineBuffered((m) => console.warn("[wasi stderr]", m)),
];
const wasi = new WASI([], [], fds);
const { default: ghc_wasm_jsffi } = await import(pathToFileURL("ghc_wasm_jsffi.js"));
const wasm = await WebAssembly.compile(await readFile("netwire01-web.wasm"));
const instance_exports = {};
const instance = await WebAssembly.instantiate(wasm, {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);
wasi.initialize(instance);
await instance.exports.hs_start();
console.log("hs_start returned; stepping frames…");
