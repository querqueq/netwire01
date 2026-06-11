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
globalThis.drawFrame = (cx, cy, poly, particles) => {
  if (!Number.isFinite(cx) || !Number.isFinite(cy)) fail("non-finite camera position");
  const pts = String(poly).split(";");
  if (pts.length !== 7) fail(`expected 7 polygon points, got ${pts.length}`);
  const ps = String(particles).split(";").filter(Boolean);
  for (const p of ps) {
    const [x, y] = p.split(",").map(Number);
    if (!Number.isFinite(x) || !Number.isFinite(y)) fail(`non-finite particle: ${p}`);
  }
  frames++;
  last = { cx, cy, nParticles: ps.length };
};
globalThis.requestAnimationFrame = (f) => {
  if (frames >= TARGET_FRAMES) {
    if (!(Math.hypot(last.cx, last.cy) > 0)) fail("ship did not move under thrust");
    // The front thruster expels 3 particles per frame with ~1s lifetimes, so
    // by now a trail must exist.
    if (!(last.nParticles > 0)) fail("no exhaust particles under thrust");
    console.log(`SMOKE OK: ${frames} frames, cam=(${last.cx}, ${last.cy}), ${last.nParticles} particles`);
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
