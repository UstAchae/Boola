import {
 ANIM,
 TT_DEBOUNCE_MS,
 BDD_DEBOUNCE_MS
} from "./config.js";

import { createState } from "./state.js";
import { getDom } from "./dom.js";

import {
  createCy,
  setGraphAnimated,
  clearGraph,
  setGraphInstant,
  snapNodesToLayers
} from "./graph/cy.js";

import {
  renderLayers,
  renderLayerAxis,
  syncLayerAxis
} from "./graph/layerAxis.js";

import {
  buildDefMap,
  parseDefinition,
  expandExpr,
  syncOrder,
  shouldRequest
} from "./expr/defs.js";

import * as expr from "./expr/exprUI.js";

import {
  fetchTruthTable,
  fetchBdd,
  fetchReduceTerminalsTrace,
  fetchReduceRedundantTrace,
  fetchReduceMergeTrace
} from "./net/api.js";

import { playReduceTerminalsTrace } from "./reduce/terminals.js";
import { playReduceRedundantTrace } from "./reduce/redundant.js";
import { playReduceMergeTrace } from "./reduce/merge.js";

import { setupKeyboard } from "./keyboard/keyboard.js";

const state = createState();
const dom = getDom();
const cy = createCy(dom.cyContainer);

const axis = {
  render(vars) {
    renderLayerAxis(dom.layerAxisEl, vars, cy);
  },
  sync() {
    syncLayerAxis(dom.layerAxisEl, cy);
  }
};

function setReduceButtonsEnabled(enabled) {
  expr.setReduceButtonsEnabled(dom, enabled);
}

function scheduleTruthTable() {
  if (state.ttTimer) clearTimeout(state.ttTimer);
  state.ttTimer = setTimeout(() => updateTruthTableForActive(true), TT_DEBOUNCE_MS);
}

function scheduleBdd() {
  if (state.bddTimer) clearTimeout(state.bddTimer);
  state.bddTimer = setTimeout(() => updateBddForActive(true), BDD_DEBOUNCE_MS);
}

function prepareActiveExpr() {
  const active = state.expressions[state.activeIndex];
  const raw = (active?.text || "").trim();
  if (!raw) return { ok: false, reason: "empty" };

  try {
    const defs = buildDefMap(state.expressions);
    const d = parseDefinition(raw);
    let expanded = d ? d.rhs : raw;
    expanded = expandExpr(expanded, defs);

    const vars = syncOrder(expanded, active.order);
    active.order = vars;

    expr.updateOrderBarOnly(ctx, state.activeIndex);
    renderLayers(dom.layersListEl, vars);
    axis.render(vars);

    return { ok: true, active, raw, expanded, vars };
  } catch (e) {
    return { ok: false, reason: "expand_failed", error: e };
  }
}

async function updateTruthTableForActive(isLive = true) {
  if (state.isReducing) return;
  const prep = prepareActiveExpr();
  if (!prep.ok) return;

  const { expanded, vars } = prep;
  if (isLive && !shouldRequest(expanded)) return;

  try {
    const ttResp = await fetchTruthTable(expanded, vars);
    if (!ttResp.ok) {
      if (isLive && ttResp.status === 400) return;
      return;
    }
    await ttResp.json();
  } catch {
    return;
  }
}

async function updateBddForActive(isLive = true) {
  if (state.isReducing) return;

  const prep = prepareActiveExpr();
  if (!prep.ok) {
    if (prep.reason === "empty") {
      clearGraph(cy);
      axis.sync();
      setReduceButtonsEnabled(false);
      state.lastRequestedExpr = null;
      // new graph is gone => clear reduce state
      state.appliedReduce.length = 0;
      state.lastBddPayload = null;
      state.lastBddElements = null;
    }
    return;
  }

  const { expanded, vars } = prep;
  if (isLive && !shouldRequest(expanded)) return;

  const normalized = expanded.replace(/\s+/g, "");
  if (state.lastRequestedExpr === normalized) {
    return; // same expression, skip rebuild
  }

  state.lastRequestedExpr = normalized;


  const mySeq = ++state.bddReqSeq;

  try {
    const resp = await fetchBdd(expanded, vars);
    if (mySeq !== state.bddReqSeq) return;

    if (!resp.ok) {
      if (isLive && resp.status === 400) return;
      clearGraph(cy);
      setReduceButtonsEnabled(false);
      axis.sync();
      return;
    }

    const data = await resp.json();
    if (mySeq !== state.bddReqSeq) return;

    if (data?.elements?.nodes && data?.elements?.edges) {
      // IMPORTANT: payload vars must be a copy (avoid aliasing active.order)
      const varsCopy = [...vars];
      const nextPayload = { expr: expanded, vars: varsCopy };

      const prev = state.lastBddPayload;
      const prevVars = prev?.vars ?? [];

      const payloadChanged =
        !prev ||
        prev.expr !== nextPayload.expr ||
        prevVars.length !== varsCopy.length ||
        prevVars.some((v, i) => v !== varsCopy[i]);

      // if expr or vars order changed => clear applied reductions
      if (payloadChanged) state.appliedReduce.length = 0;

      // update payload/elements only after successful response
      state.lastBddPayload = nextPayload;
      state.lastBddElements = data.elements;

      setGraphAnimated(cy, data.elements, ANIM, {
        onAfterLayout: () => {
          snapNodesToLayers(cy);
          axis.sync();
        }
      });

      setReduceButtonsEnabled(true);
    } else {
      clearGraph(cy);
      setReduceButtonsEnabled(false);
      axis.sync();
    }
  } catch (err) {
    console.error("BDD fetch failed:", err);
    if (mySeq === state.bddReqSeq) {
      clearGraph(cy);
      setReduceButtonsEnabled(false);
      axis.sync();
    }
  }
}

const ctx = {
  state,
  dom,
  cy,
  axis,
  expr,
  callbacks: {
    onExprChanged() {
      scheduleTruthTable();
      scheduleBdd();
    }
  }
};

async function setGraphSnapshot(elements) {
  setGraphInstant(cy, elements, { keepViewport: true, onAfterLayout: () => axis.sync() });
  snapNodesToLayers(cy);
  axis.sync();
}

async function runReduceTrace({ kind, fetchTrace, playTrace }) {
  if (!state.lastBddPayload) return false;
  const applied = state.appliedReduce.slice();
  console.log("appliedReduce(front) before =", applied);

  const { expr, vars } = state.lastBddPayload;

  const resp = await fetchTrace(expr, vars, applied);
  if (!resp.ok) return false;

  const trace = await resp.json();
  const stepsCount = trace?.steps?.length || 0;
  if (!stepsCount) return false;

  await playTrace(cy, trace, {
    setGraph: async (els) => setGraphSnapshot(els),
    onAfterEach: () => axis.sync()
  });

  state.appliedReduce.push(kind);
  state.lastBddElements = trace.steps.at(-1)?.snapshot ?? state.lastBddElements;

  console.log("appliedReduce(front) after  =", state.appliedReduce);
  return true;
}

function wireButtons() {
  dom.btnAdd?.addEventListener("click", () => {
    expr.addLine(ctx, "");
    expr.focusActiveInputSoon(dom);
  });

  dom.btnClearAll?.addEventListener("click", () => {
    expr.clearAll(ctx);
    clearGraph(cy);
    setReduceButtonsEnabled(false);
    axis.sync();
  });

  // Step 1: terminals (snapshot trace)
  let terminalsBusy = false;
  dom.btnReduceTerminals?.addEventListener("click", async () => {
    if (!state.lastBddPayload || terminalsBusy) return;
    terminalsBusy = true;
    state.isReducing = true;
    try {
      await runReduceTrace({
        kind: "terminals",
        fetchTrace: fetchReduceTerminalsTrace,
        playTrace: playReduceTerminalsTrace
      });
    } finally {
      state.isReducing = false;
      terminalsBusy = false;
    }
  });

  // Step 2: redundant (snapshot trace)
  let redundantBusy = false;
  dom.btnReduceRedundant?.addEventListener("click", async () => {
    if (!state.lastBddPayload || redundantBusy) return;
    redundantBusy = true;
    state.isReducing = true;
    try {
      await runReduceTrace({
        kind: "redundant",
        fetchTrace: fetchReduceRedundantTrace,
        playTrace: playReduceRedundantTrace
      });
    } finally {
      state.isReducing = false;
      redundantBusy = false;
    }
  });

  // Step 3: merge (snapshot trace)
  let mergeBusy = false;
  dom.btnReduceMerge?.addEventListener("click", async () => {
    if (!state.lastBddPayload || mergeBusy) return;
    mergeBusy = true;
    state.isReducing = true;
    try {
      await runReduceTrace({
        kind: "merge",
        fetchTrace: fetchReduceMergeTrace,
        playTrace: playReduceMergeTrace
      });
    } finally {
      state.isReducing = false;
      mergeBusy = false;
    }
  });

  dom.btnFit?.addEventListener("click", () => {
    // 如果你之后想彻底禁用 fit，这里就直接留空或删掉
    cy.fit(undefined, 30);
    axis.sync();
  });

  dom.btnResetExample?.addEventListener("click", () => {
    // 你项目里原本怎么做 example 就怎么保留；这里不改
  });
}

function wireCyAxisSync() {
  cy.on("zoom pan", () => axis.sync());
  cy.on("resize", () => axis.sync());
  window.addEventListener("resize", () => axis.sync());
}

function init() {
  setReduceButtonsEnabled(false);

  // input bar
  expr.renderExprList(ctx);
  expr.setActiveIndex(state, dom, 0);

  clearGraph(cy);
  axis.sync();

  setupKeyboard({ ...ctx, expr });
  wireButtons();
  wireCyAxisSync();

  dom.backendInfo && (dom.backendInfo.textContent = "Backend: /api/bdd");
}

init();