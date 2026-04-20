// frontend/js/main.js

import { ANIM, BDD_DEBOUNCE_MS } from "./config.js";

import { createState, defaultBddPane } from "./state.js";
import { getDom } from "./dom.js";

import {
  createCy,
  enableHorizontalDragOnly,
  disableUserZoom,
  autoFitOnResize,
  setGraphAnimated,
  clearGraph,
  setGraphInstant,
  cancelAllGraphAnims,
  smoothFit,
  syncApplyAuxDragHandles
} from "./graph/cy.js";

import { renderLayerAxis, syncLayerAxis } from "./graph/layerAxis.js";
import {
  animateInterframeTransition,
  captureNodePositions as capturePositions,
  restoreNodePositions as restorePositions
} from "./graph/positions.js";

import {
  buildDefMap,
  formatApplyCall,
  inferVars,
  isValidBooleanExpression,
  makeRestrictDerivedName,
  isDerivedExprName,
  isRestrictDerivedAliasName,
  parseDefinitionSlot,
  parseApplyCall,
  parseRestrictCall,
  syncOrder,
  shouldRequest
} from "./expr/defs.js";

import * as expr from "./expr/exprUI.js";

import {
  fetchBdd,
  fetchApplyTrace,
  fetchApplyReduceTrace,
  fetchRestrictTrace,
  fetchReduceStateTrace,
  fetchFormattedExpr,
  fetchAnalyzeLine,
  fetchBddApply,
  fetchBddReduced
} from "./net/api.js";

import { playRestrictTrace } from "./bddTraces/restriction/play_restrict_trace.js";
import { REDUCE_TRACE_BY_KIND } from "./bddTraces/reduction/reduceTraceKinds.js";
import { createServerReduceTraceRunner } from "./bddTraces/serverReduceTrace.js";
import { createBddAutoLayout } from "./graph/bddAutoLayout.js";

import { setupKeyboard } from "./keyboard/keyboard.js";
import { setupCanvasResizeObserver, setupLeftDrawer } from "./panes/leftDrawer.js";
import { uniqueIds } from "./bddTraces/apply/ui/helpers.js";
import { createApplyExpandController } from "./bddTraces/apply/ui/expand.js";
import { createApplyHighlightController } from "./bddTraces/apply/ui/highlight.js";
import { createBddBarController } from "./bddTraces/apply/ui/bddBar.js";
import { storeApplyCurrentResultState, storeApplyFinalLayout } from "./bddTraces/apply/ui/sessionState.js";
import { createLocalLineAnalyzer } from "./analysis/localLineAnalysis.js";
import { createLineAnalysisSync } from "./analysis/lineAnalysisSync.js";
import { createPaneStateController } from "./panes/paneState.js";
import { createPaneLifecycleController } from "./panes/paneLifecycle.js";
import { createBddRequestPrep } from "./bddTraces/orchestration/bddRequestPrep.js";
import { createBddScheduler } from "./bddTraces/orchestration/bddScheduler.js";
import { createBddGraphSnapshot } from "./graph/bddGraphSnapshot.js";
import { createBddUpdateController } from "./bddTraces/orchestration/bddUpdate.js";

const state = createState();
const dom = getDom();

const cy = createCy(dom.cyContainer);
window.cy = cy;

const BOOLA_DEBUG =
  (typeof window !== "undefined" && (
    window.localStorage?.getItem("boola:debug") === "1" ||
    /(?:^|[?&])debug=1(?:&|$)/.test(window.location?.search || "")
  ));
const boolaDebugLog = [];

function boolaStateSnapshot() {
  return {
    activeIndex: state.activeIndex,
    isReducing: state.isReducing,
    skipReductionApplied: state.skipReductionApplied,
    hasApplySession: Boolean(state.applyTraceSession),
    applyIdx: state.applyTraceSession?.idx ?? null,
    hasRestrictSession: Boolean(state.restrictTraceSession),
    restrictIdx: state.restrictTraceSession?.idx ?? null,
    lastRequestedKey: state.lastRequestedKey
  };
}

function debugLog(event, payload = {}) {
  if (!BOOLA_DEBUG) return;
  const row = {
    t: new Date().toISOString(),
    event,
    ...payload,
    state: boolaStateSnapshot()
  };
  boolaDebugLog.push(row);
  if (boolaDebugLog.length > 500) boolaDebugLog.shift();
  // Keep console output concise and searchable.
  // eslint-disable-next-line no-console
  console.debug("[boola-debug]", event, row);
}

if (typeof window !== "undefined") {
  window.boolaDebugDump = () => [...boolaDebugLog];
  window.boolaDebugClear = () => {
    boolaDebugLog.length = 0;
    return true;
  };
  window.addEventListener("error", (e) => {
    debugLog("window.error", { message: e?.message || "", source: e?.filename || "" });
  });
  window.addEventListener("unhandledrejection", (e) => {
    debugLog("window.unhandledrejection", { reason: String(e?.reason || "") });
  });
}

// Trace global state flips; this is crucial for diagnosing "UI frozen" cases.
let reducingValue = Boolean(state.isReducing);
Object.defineProperty(state, "isReducing", {
  configurable: true,
  enumerable: true,
  get() {
    return reducingValue;
  },
  set(next) {
    const normalized = Boolean(next);
    if (normalized === reducingValue) return;
    reducingValue = normalized;
    debugLog("state.isReducing.changed", { next: normalized });
  }
});

let activeIndexValue = Number(state.activeIndex) || 0;
Object.defineProperty(state, "activeIndex", {
  configurable: true,
  enumerable: true,
  get() {
    return activeIndexValue;
  },
  set(next) {
    const normalized = Number(next);
    if (!Number.isFinite(normalized) || normalized === activeIndexValue) return;
    activeIndexValue = normalized;
    debugLog("state.activeIndex.changed", { next: normalized });
  }
});

disableUserZoom(cy);
enableHorizontalDragOnly(cy, { layerGap: 120 });

const axis = {
  render(vars) {
    renderLayerAxis(dom.layerAxisEl, vars, cy, { onReorder: handleAxisReorder });
  },
  sync() {
    syncLayerAxis(dom.layerAxisEl, cy);
  }
};

function applyStageDebug(event, payload = {}) {
  return;
}

function applyCompareDebug(event, payload = {}) {
  return;
}

function applyPositionDebug(event, payload = {}) {
  return;
}

const applyExpand = createApplyExpandController({
  cy,
  refreshNodeDraggability,
  applyStageDebug,
  applyPositionDebug,
  sampleNodePositions,
  collectApplyPositionSnapshot
});
const {
  setApplyHiddenIds,
  clearApplyExpandEdgeStaging,
  resetApplyExpandEdgeStaging,
  targetNodeIdsForEdges,
  autoExpandApplyPathsForNodeIds,
  repositionApplyStagedStubTargetsForNode,
  setupApplyExpandEdgeStaging,
  reapplyAllExpandEdgeStaging,
  stageApplyExpandPathState,
  revealApplyExpandSideByEdge
} = applyExpand;

const applyHighlight = createApplyHighlightController({
  cy,
  applyCompareDebug,
  targetNodeIdsForEdges
});
const {
  clearApplyPendingCompareHighlight,
  clearApplyCompareHighlight,
  setApplyPendingCompareHighlightFromReveal,
  setApplyCompareHighlightFromBranch,
  syncApplyCompareHighlight,
  ensureApplyCompareHighlight
} = applyHighlight;

const bddBar = createBddBarController({
  state,
  dom,
  cy,
  analyzeLine,
  canUndoSkipReduction,
  clearApplyPendingCompareHighlight,
  clearApplyCompareHighlight
});
const {
  getVisibleApplyReduceKinds,
  getVisibleApplyResultNodeIds,
  getCanvasReduceKinds,
  getLayoutVarsForCurrentCanvas,
  isApplyTraceFullyReducedObdd,
  isRestrictTraceInProgress,
  canAutoLayoutCurrentBdd,
  refreshCanvasReduceButtons,
  refreshBddBarPrimaryButtons
} = bddBar;

const localLineAnalyzer = createLocalLineAnalyzer({
  getExpressions: () => state.expressions,
  parseDefinitionSlot,
  isDerivedExprName,
  isRestrictDerivedAliasName,
  makeRestrictDerivedName,
  parseApplyCall,
  parseRestrictCall,
  buildDefMap,
  inferVars,
  isValidBooleanExpression
});

const lineAnalysisSync = createLineAnalysisSync({
  state,
  fetchAnalyzeLine,
  localAnalyzeLine: (idx) => localLineAnalyzer.analyzeLineLocal(idx),
  refreshLineUi: () => state.expressions.forEach((_, idx) => expr.refreshExprUiOnly(ctx, idx)),
  refreshPrimaryButtons: () => refreshBddBarPrimaryButtons()
});

const paneState = createPaneStateController({ state, defaultBddPane });
const {
  ensureBddPane,
  cloneElements,
  clonePayload,
  applyGlobalsFromPane,
  persistBddPane
} = paneState;

/** Late-bound: `bddUpdate` installs `updateBddForActive` here after `ctx` exists. */
const bddApis = {};

const bddGraphSnapshot = createBddGraphSnapshot({
  state,
  cy,
  axis,
  setGraphInstant,
  cancelAllGraphAnims,
  smoothFit,
  setDraggingEnabled,
  persistBddPane,
  refreshCanvasReduceButtons,
  cloneElements
});
const { pruneUserX, setGraphSnapshot, restoreBaseGraphIfAvailable } = bddGraphSnapshot;

const paneLifecycle = createPaneLifecycleController({
  state,
  cy,
  axis,
  defaultBddPane,
  ensureBddPane,
  applyGlobalsFromPane,
  setReduceButtonsEnabled,
  clearGraph,
  clearApplyPendingCompareHighlight,
  clearApplyCompareHighlight,
  clearRestrictInteractiveFocus,
  cancelAllGraphAnims,
  prepareActiveExpr: () => prepareActiveExpr(),
  setGraphInstant,
  pruneUserX,
  setDraggingEnabled,
  smoothFit,
  updateBddForActive: (isLive) => bddApis.updateBddForActive(isLive),
  refreshBddBarPrimaryButtons,
  scheduleLineAnalysisRefresh
});
const {
  clearActivePaneState,
  restoreBddPaneForIndex,
  onExpressionsReset
} = paneLifecycle;

function applyZoneNodeIds(zoneClass = "") {
  return cy
    .nodes(`.${zoneClass}`)
    .filter((n) => !n.hasClass("apply-hidden-step") && !n.hasClass("apply-ghost") && !n.hasClass("apply-drag-handle"))
    .map((n) => n.id());
}

function bboxForNodeIds(nodeIds = []) {
  if (!nodeIds.length) return { count: 0 };
  const xs = [];
  nodeIds.forEach((id) => {
    const n = cy.getElementById(id);
    if (!n || n.empty?.()) return;
    const x = Number(n.position("x"));
    if (Number.isFinite(x)) xs.push(x);
  });
  if (!xs.length) return { count: nodeIds.length };
  const left = Math.min(...xs);
  const right = Math.max(...xs);
  return {
    count: nodeIds.length,
    left,
    right,
    center: (left + right) / 2,
    width: right - left
  };
}

function sampleNodePositions(ids = [], limit = 6) {
  const out = {};
  for (const id of ids.slice(0, limit)) {
    const n = cy.getElementById(id);
    if (!n || n.empty?.()) continue;
    out[id] = { x: Number(n.position("x")), y: Number(n.position("y")) };
  }
  return out;
}

function collectApplyPositionSnapshot(session = state.applyTraceSession) {
  const leftIds = applyZoneNodeIds("apply-left");
  const rightIds = applyZoneNodeIds("apply-right");
  const stagedL = [];
  const stagedR = [];
  if (session?.expandNodeOriginalPos instanceof Map) {
    for (const [id, pos] of session.expandNodeOriginalPos.entries()) {
      if (!pos) continue;
      if (String(id).startsWith("L-")) stagedL.push(id);
      if (String(id).startsWith("R-")) stagedR.push(id);
    }
  }
  return {
    offsets: {
      L: Number(session?.applyAuxZoneOffsets?.L ?? 0),
      R: Number(session?.applyAuxZoneOffsets?.R ?? 0)
    },
    leftBox: bboxForNodeIds(leftIds),
    rightBox: bboxForNodeIds(rightIds),
    leftSample: sampleNodePositions(leftIds),
    rightSample: sampleNodePositions(rightIds),
    stagedLeftCount: stagedL.length,
    stagedRightCount: stagedR.length
  };
}

function isSameLevelApplyBranch(branch) {
  return String(branch?.caseKey ?? "") === "case2";
}

function applyPairAnchorId(nodeId = "") {
  const m = /^P-m_(.+)-[LR]$/.exec(String(nodeId));
  return m ? `M-m_${m[1]}` : null;
}

function storeApplyBranches(session, data) {
  session.branchMap = new Map();
  session.branchByPath = session.branchByPath instanceof Map ? session.branchByPath : new Map();
  for (const br of data?.branches ?? []) {
    if (!br?.path) continue;
    session.branchByPath.set(br.path, br);
    for (const id of br.nodeIds ?? []) {
      session.branchMap.set(id, br.path);
    }
  }
}

const APPLY_REDUCE_PROBE_KINDS = ["terminals", "redundant", "merge"];

async function refreshApplyReduceAvailability(session) {
  if (!session) return;
  const probeSeq = (session.reduceAvailabilitySeq ?? 0) + 1;
  session.reduceAvailabilitySeq = probeSeq;
  const revealed = [...(session.revealed ?? [])];
  const resolved = [...(session.resolved ?? [])];
  const expanded = [...(session.expanded ?? [])];
  const { op, expr1, expr2, vars } = session;
  const visibleResultNodeIds = getVisibleApplyResultNodeIds();
  if (!visibleResultNodeIds.length || !session.currentResultState) {
    session.availableReduceKinds = [];
    refreshCanvasReduceButtons();
    return;
  }

  // Do not rely on local visible-graph heuristics here; only backend probe may
  // decide whether T / NT / R are valid for the current visible apply subtree.
  session.availableReduceKinds = [];
  refreshCanvasReduceButtons();

  const controller = new AbortController();
  const t = setTimeout(() => controller.abort(), 12000);
  try {
    const picked = await Promise.all(
      APPLY_REDUCE_PROBE_KINDS.map(async (kind) => {
        try {
          const resp = await fetchApplyReduceTrace(
            kind,
            op,
            expr1,
            expr2,
            vars,
            revealed,
            resolved,
            expanded,
            [],
            session.currentResultState,
            visibleResultNodeIds,
            controller.signal
          );
          if (resp.status === 204) return null;
          if (!resp.ok) return null;
          const trace = await resp.json().catch(() => null);
          return trace?.initial ? kind : null;
        } catch (e) {
          if (e?.name !== "AbortError") console.error("[apply-reduce-availability]", kind, e);
          return null;
        }
      })
    );
    if (session.reduceAvailabilitySeq !== probeSeq) return;
    const available = new Set(picked.filter(Boolean));
    session.availableReduceKinds = APPLY_REDUCE_PROBE_KINDS.filter((kind) => available.has(kind));
  } finally {
    clearTimeout(t);
    if (session.reduceAvailabilitySeq === probeSeq) refreshCanvasReduceButtons();
  }
}

function captureNodePositions() {
  return capturePositions(cy);
}

function restoreNodePositions(posMap) {
  restorePositions(cy, posMap);
}

async function animateRestrictInterframeTransition(fromPosMap, opts = {}) {
  await animateInterframeTransition(cy, fromPosMap, opts);
}

function analyzeLine(idx) {
  return lineAnalysisSync.analyzeLine(idx);
}

async function ensureLineAnalysis(idx) {
  await lineAnalysisSync.ensureLineAnalysis(idx);
}

function scheduleLineAnalysisRefresh() {
  lineAnalysisSync.scheduleLineAnalysisRefresh();
}

const ctx = {
  state,
  dom,
  cy,
  axis,
  expr,
  callbacks: {
    onExprChanged() {
      debugLog("callbacks.onExprChanged");
      refreshBddBarPrimaryButtons();
      scheduleLineAnalysisRefresh();
      scheduleBdd();
    },
    onActiveIndexChanged() {
      refreshBddBarPrimaryButtons();
    },
    onExprBlur(idx) {
      return (async () => {
        await formatExpressionLine(idx);
        await ensureLineAnalysis(idx);
        expr.refreshExprUiOnly(ctx, idx);
        refreshBddBarPrimaryButtons();
      })();
    },
    async onPlayTrace(idx) {
      const meta = analyzeLine(idx);
      if (!meta.ok) return false;
      if (meta.kind === "apply") {
        if (state.applyTraceSession?.idx === idx) return stopApplyTraceForIndex(idx);
        return runApplyTraceForIndex(idx);
      }
      if (meta.kind === "restrict") {
        return runRestrictTraceForIndex(idx);
      }
      return false;
    },
    getLineUiState(idx) {
      const isApplyTracePlaying = state.applyTraceSession?.idx === idx;
      const meta = analyzeLine(idx);
      return {
        ...meta,
        isApplyTracePlaying,
        applyReduceKinds: isApplyTracePlaying ? (state.applyTraceSession?.availableReduceKinds ?? []) : []
      };
    },
    onApplyReduce(idx, kind) {
      if (idx !== state.activeIndex) return false;
      return runApplyReduceTrace(kind);
    },
    onExpressionSwitchPersist(fromIdx) {
      persistBddPane(fromIdx);
    },
    onExpressionSwitchRestore(toIdx) {
      refreshBddBarPrimaryButtons();
      void restoreBddPaneForIndex(toIdx);
    },
    onExpressionsReset
  }
};

const bddRequestPrep = createBddRequestPrep({
  state,
  expr,
  ctx,
  axis,
  syncOrder,
  analyzeLine
});
const { prepareActiveExpr } = bddRequestPrep;

function setReduceButtonsEnabled(enabled) {
  expr.setReduceButtonsEnabled(dom, enabled);
  refreshCanvasReduceButtons();
}

function handleAxisReorder(nextVars) {
  const active = state.expressions[state.activeIndex];
  if (!active || !Array.isArray(nextVars) || nextVars.length === 0) return;
  void handleAxisReorderAsync(nextVars);
}

async function restartRestrictTraceFromOrder(nextVars) {
  const session = state.restrictTraceSession;
  if (!session || session.idx !== state.activeIndex || state.isReducing) return false;

  await ensureLineAnalysis(state.activeIndex);
  const analysis = analyzeLine(state.activeIndex);
  if (!analysis.ok || analysis.kind !== "restrict") return false;

  const owner = state.expressions[state.activeIndex];
  if (!owner) return false;

  const vars = syncOrder(analysis.baseExpr || analysis.expr, nextVars);
  owner.order = vars;
  axis.render(vars);

  const trace = await fetchRestrictTraceData(analysis, vars);
  if (!trace?.initial) return false;
  const initialStepIndex = getRestrictInitialStepIndex(trace);

  const requestKey =
    `restrict-trace|${analysis.restrict.bit}|${analysis.restrict.atomName}|${analysis.restrict.bddName}|` +
    `${(analysis.expr || "").replace(/\s+/g, "")}|${vars.join(",")}`;

  clearRestrictInteractiveFocus();
  state.userX.clear();
  await setGraphSnapshot(trace.initial, vars, { bddLayoutKind: "aux_sugiyama" });
  await smoothFit(cy, undefined, { padding: 30, duration: 360 });

  session.vars = [...vars];
  session.expr = analysis.expr;
  session.trace = trace;
  session.nextStepIndex = initialStepIndex;
  session.completed = false;
  session.active = true;
  session.stepLayoutPos = captureNodePositions();
  session.currentResultState = trace.initialResultState ?? null;
  session.requestKey = requestKey;

  state.lastBddElements = trace.initial;
  state.baseBddElements = trace.initial;
  state.lastBddPayload = { expr: analysis.expr, vars: [...vars] };
  state.lastRequestedKey = requestKey;
  state.appliedReduce.length = 0;
  state.skipReductionApplied = false;
  state.bddLayoutKind = "aux_sugiyama";
  setDraggingEnabled(false);
  persistBddPane(session.idx);
  expr.refreshExprUiOnly(ctx, session.idx);
  refreshCanvasReduceButtons();
  refreshBddBarPrimaryButtons();
  return true;
}

async function handleAxisReorderAsync(nextVars) {
  const active = state.expressions[state.activeIndex];
  if (!active || !Array.isArray(nextVars) || nextVars.length === 0) return;

  active.order = nextVars.slice();
  axis.render(nextVars);

  if (state.restrictTraceSession?.idx === state.activeIndex) {
    if (state.isReducing) return;
    const restarted = await restartRestrictTraceFromOrder(nextVars);
    if (restarted) return;
  }

  scheduleBdd();
}

async function runApplyTraceForIndex(idx) {
  if (state.isReducing) return false;

  if (idx !== state.activeIndex) {
    persistBddPane(state.activeIndex);
    state.activeIndex = idx;
    expr.renderExprList(ctx);
    await restoreBddPaneForIndex(idx);
  }

  await ensureLineAnalysis(idx);
  const analysis = analyzeLine(idx);
  if (!analysis.ok || analysis.kind !== "apply") return false;

  const ownerIdx = state.activeIndex;
  const owner = state.expressions[ownerIdx];
  if (!owner) return false;

  const vars = syncOrder(`${analysis.expr1} ${analysis.expr2}`, owner.order);
  owner.order = vars;
  axis.render(vars);

  clearRestrictInteractiveFocus();
  state.restrictTraceSession = null;
  state.isRestrictTracing = false;
  state.isReducing = true;
  try {
    const session = {
      idx: ownerIdx,
      op: analysis.apply.op,
      expr1: analysis.expr1,
      expr2: analysis.expr2,
      vars: [...vars],
      revealed: new Set(),
      resolved: new Set(),
      expanded: new Set(),
      appliedReductions: [],
      currentResultState: null,
      branchMap: new Map(),
      branchByPath: new Map(),
      expandEdgeById: new Map(),
      expandRevealByPath: new Map(),
      expandNodeOriginalPos: new Map(),
      reduceAvailabilitySeq: 0,
      finalResultPositions: new Map(),
      finalResultCenterX: null,
      applyAuxZoneOffsets: { L: 0, R: 0 },
      zoneLayoutCache: {},
      availableReduceKinds: [],
      compareHighlightPath: "",
      compareHighlightIds: [],
      pendingCompareHighlightPath: null,
      pendingCompareHighlightIds: [],
      pendingCompareFocus: null,
      suspendCompareHighlight: false
    };
    state.applyTraceSession = session;
    await refreshInteractiveApplyScene({ fit: true, keepViewport: false });
    return true;
  } catch (e) {
    if (e?.name !== "AbortError") console.error("[apply-trace] failed", e);
    return false;
  } finally {
    state.isReducing = false;
    expr.refreshExprUiOnly(ctx, ownerIdx);
    refreshCanvasReduceButtons();
    refreshBddBarPrimaryButtons();
  }
}

async function stopApplyTraceForIndex(idx) {
  const session = state.applyTraceSession;
  if (!session || session.idx !== idx || state.isReducing) return false;

  clearApplyPendingCompareHighlight(session);
  state.applyTraceSession = null;
  clearApplyCompareHighlight();
  expr.refreshExprUiOnly(ctx, idx);

  if (idx === state.activeIndex) {
    await restoreBddPaneForIndex(idx);
    expr.refreshExprUiOnly(ctx, idx);
  }
  refreshCanvasReduceButtons();
  refreshBddBarPrimaryButtons();
  return true;
}

async function fetchRestrictTraceData(analysis, vars) {
  const controller = new AbortController();
  const t = setTimeout(() => controller.abort(), 12000);
  try {
    const resp = await fetchRestrictTrace(
      analysis.baseExpr,
      vars,
      analysis.restrict.atomName,
      analysis.restrict.bit,
      controller.signal
    );
    if (resp.status === 204) return null;
    if (!resp.ok) {
      const txt = await resp.text().catch(() => "");
      console.error("[restrict-trace] HTTP not ok", resp.status, txt);
      return null;
    }
    return await resp.json().catch((e) => {
      console.error("[restrict-trace] JSON parse failed", e);
      return null;
    });
  } finally {
    clearTimeout(t);
  }
}

function clearRestrictInteractiveFocus() {
  cy.batch(() => {
    cy.elements(".focus").removeClass("focus");
  });
}

function getRestrictInitialStepIndex(trace) {
  const first = trace?.steps?.[0];
  if (!first) return 0;
  const hasIncomingFocusEdge = (first.focus ?? []).some((id) => String(id).startsWith("e_"));
  return hasIncomingFocusEdge ? 0 : 1;
}

async function startRestrictTraceSession(ownerIdx, analysis, vars) {
  const trace = await fetchRestrictTraceData(analysis, vars);
  if (!trace?.initial) return false;
  const initialStepIndex = getRestrictInitialStepIndex(trace);

  const requestKey =
    `restrict-trace|${analysis.restrict.bit}|${analysis.restrict.atomName}|${analysis.restrict.bddName}|` +
    `${(analysis.expr || "").replace(/\s+/g, "")}|${vars.join(",")}`;

  state.userX.clear();
  await setGraphSnapshot(trace.initial, vars, { bddLayoutKind: "aux_sugiyama" });
  await smoothFit(cy, undefined, { padding: 30, duration: 420 });

  state.restrictTraceSession = {
    idx: ownerIdx,
    vars: [...vars],
    expr: analysis.expr,
    trace,
    nextStepIndex: initialStepIndex,
    completed: false,
    active: true,
    stepLayoutPos: captureNodePositions(),
    currentResultState: trace.initialResultState ?? null,
    requestKey
  };
  state.lastBddElements = trace.initial;
  state.baseBddElements = trace.initial;
  state.lastBddPayload = { expr: analysis.expr, vars: [...vars] };
  state.lastRequestedKey = requestKey;
  state.appliedReduce.length = 0;
  state.bddLayoutKind = "aux_sugiyama";
  setDraggingEnabled(false);
  persistBddPane(ownerIdx);
  expr.refreshExprUiOnly(ctx, ownerIdx);
  refreshCanvasReduceButtons();
  refreshBddBarPrimaryButtons();
  return true;
}

async function stopRestrictTraceForIndex(idx) {
  const session = state.restrictTraceSession;
  if (!session || session.idx !== idx || state.isReducing) return false;

  clearRestrictInteractiveFocus();
  state.restrictTraceSession = null;
  state.isRestrictTracing = false;
  expr.refreshExprUiOnly(ctx, idx);
  await bddApis.updateBddForActive(false);
  refreshCanvasReduceButtons();
  refreshBddBarPrimaryButtons();
  return true;
}

async function enterRestrictFlow(session) {
  clearRestrictInteractiveFocus();
  const initialStepIndex = getRestrictInitialStepIndex(session.trace);
  session.active = true;
  session.completed = false;
  session.nextStepIndex = initialStepIndex;
  session.currentResultState = session.trace?.initialResultState ?? session.currentResultState;
  state.userX.clear();
  await setGraphSnapshot(session.trace.initial, session.vars, { bddLayoutKind: "aux_sugiyama" });
  await smoothFit(cy, undefined, { padding: 30, duration: 360 });
  state.lastBddElements = session.trace.initial;
  state.baseBddElements = session.trace.initial;
  state.lastBddPayload = { expr: session.expr, vars: [...session.vars] };
  state.lastRequestedKey = session.requestKey;
  state.bddLayoutKind = "aux_sugiyama";
  session.stepLayoutPos = captureNodePositions();
  persistBddPane(session.idx);
  return true;
}

async function advanceRestrictTraceSession(session) {
  if (!session?.active) return false;
  const step = session?.trace?.steps?.[session.nextStepIndex];
  if (!step?.snapshot) return false;

  clearRestrictInteractiveFocus();
  state.isReducing = true;
  state.isRestrictTracing = true;
  refreshCanvasReduceButtons();
  refreshBddBarPrimaryButtons();
  try {
    await playRestrictTrace(cy, step, {
      setGraph: async (els) => {
        const prevPos = session.stepLayoutPos instanceof Map ? session.stepLayoutPos : captureNodePositions();
        await setGraphSnapshot(els, session.vars, { bddLayoutKind: "aux_sugiyama" });
        await animateRestrictInterframeTransition(prevPos);
        session.stepLayoutPos = captureNodePositions();
      },
      onAfterEach: () => axis.sync(),
      stepIndex: session.nextStepIndex,
      stepsLen: session.trace?.steps?.length ?? 0,
      isFinalStep: session.nextStepIndex === (session.trace?.steps?.length ?? 1) - 1,
      skipIntroHighlight: true
    });

    session.nextStepIndex += 1;
    session.completed = session.nextStepIndex >= (session.trace?.steps?.length ?? 0);
    while (!session.completed) {
      const upcoming = session?.trace?.steps?.[session.nextStepIndex];
      if (!upcoming?.snapshot) break;
      const hasUpcomingFocus = uniqueIds(upcoming.focus ?? []).length > 0;
      if (hasUpcomingFocus) break;
      if (upcoming?.resultState) session.currentResultState = upcoming.resultState;
      state.lastBddElements = upcoming.snapshot;
      state.baseBddElements = session.trace?.initial ?? upcoming.snapshot;
      state.lastBddPayload = { expr: session.expr, vars: [...session.vars] };
      state.lastRequestedKey = session.requestKey;
      session.nextStepIndex += 1;
      session.completed = session.nextStepIndex >= (session.trace?.steps?.length ?? 0);
    }
    if (session.completed) {
      session.active = false;
      clearRestrictInteractiveFocus();
    }
    if (step?.resultState) session.currentResultState = step.resultState;
    state.lastBddElements = step.snapshot;
    state.baseBddElements = session.trace?.initial ?? step.snapshot;
    state.lastBddPayload = { expr: session.expr, vars: [...session.vars] };
    state.lastRequestedKey = session.requestKey;
    persistBddPane(session.idx);
    expr.refreshExprUiOnly(ctx, session.idx);
    return true;
  } finally {
    state.isRestrictTracing = false;
    state.isReducing = false;
    refreshCanvasReduceButtons();
    refreshBddBarPrimaryButtons();
  }
}

async function runRestrictTraceForIndex(idx) {
  if (state.isReducing) return false;

  if (idx !== state.activeIndex) {
    persistBddPane(state.activeIndex);
    state.activeIndex = idx;
    expr.renderExprList(ctx);
    await restoreBddPaneForIndex(idx);
  }

  await ensureLineAnalysis(idx);
  const analysis = analyzeLine(idx);
  if (!analysis.ok || analysis.kind !== "restrict") return false;

  const ownerIdx = state.activeIndex;
  const owner = state.expressions[ownerIdx];
  if (!owner) return false;

  const vars = syncOrder(analysis.baseExpr || analysis.expr, owner.order);
  owner.order = vars;
  axis.render(vars);

  state.applyTraceSession = null;
  clearApplyCompareHighlight();
  clearApplyPendingCompareHighlight(null);

  const session = state.restrictTraceSession;
  if (session && session.idx === ownerIdx) {
    if (session.active) {
      return stopRestrictTraceForIndex(ownerIdx);
    }
    if (session.completed) {
      const entered = await enterRestrictFlow(session);
      if (!entered) return false;
      return advanceRestrictTraceSession(session);
    }
    return enterRestrictFlow(session);
  }

  state.restrictTraceSession = null;
  state.isRestrictTracing = false;
  return startRestrictTraceSession(ownerIdx, analysis, vars);
}

async function refreshInteractiveApplyScene({ fit = false, keepViewport = true } = {}) {
  const session = state.applyTraceSession;
  if (!session) return false;

  const data = await fetchInteractiveApplySceneData(session, session.expanded);
  if (!data?.snapshot) return false;
  storeApplyBranches(session, data);
  storeApplyFinalLayout(session, data);
  storeApplyCurrentResultState(session, data);
  clearApplyExpandEdgeStaging(session);
  await renderInteractiveApplyScene(data.snapshot, session.vars, {
    fit,
    keepViewport,
    pinnedApplyResultPositions: session.finalResultPositions,
    pinnedApplyResultCenterX: session.finalResultCenterX,
    zoneLayoutCache: session.zoneLayoutCache
  });
  await refreshApplyReduceAvailability(session);
  return true;
}

async function fetchInteractiveApplySceneData(
  session,
  expandedSet = session.expanded,
  { advancePath = null, advancePhase = null } = {}
) {
  const revealedSet = session.revealed ?? new Set();
  const resolvedSet = session.resolved ?? new Set();
  const controller = new AbortController();
  const t = setTimeout(() => controller.abort(), 12000);
  let resp;
  try {
    resp = await fetchApplyTrace(
      session.op,
      session.expr1,
      session.expr2,
      session.vars,
      [...revealedSet],
      [...resolvedSet],
      [...expandedSet],
      [...(session.appliedReductions ?? [])],
      session.currentResultState,
      advancePath,
      advancePhase,
      controller.signal
    );
  } finally {
    clearTimeout(t);
  }

  if (!resp.ok) {
    const txt = await resp.text().catch(() => "");
    console.error("[apply-trace] HTTP not ok", resp.status, txt);
    return null;
  }

  const data = await resp.json().catch((e) => {
    console.error("[apply-trace] JSON parse failed", e);
    return null;
  });
  if (!data?.snapshot) {
    console.error("[apply-trace] missing snapshot", data);
    return null;
  }
  return data;
}

function getApplyFitMotion() {
  const visibleNodes = cy
    .nodes(".apply-zone")
    .filter((n) => !n.hasClass("apply-hidden-step") && !n.hasClass("apply-ghost"));

  const nodeCount = visibleNodes.length;
  const bb = nodeCount
    ? visibleNodes.boundingBox({ includeLabels: false, includeOverlays: false })
    : { w: 0, h: 0 };
  const container = cy.container?.();
  const viewW = Math.max(1, container?.clientWidth ?? 1);
  const viewH = Math.max(1, container?.clientHeight ?? 1);
  const spanPressure = Math.max(bb.w / viewW, bb.h / viewH);

  const duration = Math.round(
    Math.max(
      460,
      Math.min(
        760,
        420 +
          Math.min(180, nodeCount * 9) +
          Math.max(0, Math.min(160, (spanPressure - 0.55) * 170))
      )
    )
  );

  return {
    padding: 30,
    duration,
    easing: "ease-in-out"
  };
}

function isApplyFrameTransitionNode(n) {
  if (!n || n.empty?.()) return false;
  if (!n.hasClass("apply-zone")) return false;
  return (
    !n.hasClass("apply-hidden-step") &&
    !n.hasClass("apply-ghost") &&
    !n.hasClass("apply-slot") &&
    !n.hasClass("apply-drag-handle") &&
    !n.hasClass("apply-pair")
  );
}

function captureApplyFrameNodePositions() {
  const pos = new Map();
  cy.nodes().forEach((n) => {
    if (!isApplyFrameTransitionNode(n)) return;
    const p = n.position();
    pos.set(n.id(), { x: p.x, y: p.y });
  });
  return pos;
}

async function animateApplyFrameTransition(fromPosMap, { duration = 280, minDelta = 1.2 } = {}) {
  if (!(fromPosMap instanceof Map) || fromPosMap.size === 0) return;
  const animations = [];
  cy.batch(() => {
    cy.nodes().forEach((n) => {
      if (!isApplyFrameTransitionNode(n)) return;
      const from = fromPosMap.get(n.id());
      if (!from) return;
      const to = n.position();
      if (!Number.isFinite(from.x) || !Number.isFinite(from.y)) return;
      if (!Number.isFinite(to.x) || !Number.isFinite(to.y)) return;
      if (Math.abs(to.x - from.x) + Math.abs(to.y - from.y) < minDelta) return;
      n.position({ x: from.x, y: from.y });
      animations.push(
        n.animation(
          { position: { x: to.x, y: to.y } },
          { duration, easing: "ease-in-out" }
        ).play().promise()
      );
    });
  });
  if (!animations.length) return;
  await Promise.allSettled(animations);
}

async function renderInteractiveApplyScene(
  snapshot,
  vars,
  {
    fit = false,
    keepViewport = true,
    preserveApplyResultPositions = false,
    pinnedApplyResultPositions = null,
    pinnedApplyResultCenterX = null,
    zoneLayoutCache = null
  } = {}
) {
  const prevFramePos = captureApplyFrameNodePositions();
  applyPositionDebug("render-scene:before-setGraphInstant", {
    fit,
    keepViewport,
    snapshot: collectApplyPositionSnapshot(state.applyTraceSession)
  });
  cancelAllGraphAnims(cy);
  cy.nodes().ungrabify();
  const shouldAnimateFit = Boolean(fit);
  // Preserve dragged x positions across apply frame updates.
  await setGraphInstant(cy, snapshot, vars, state.userX, {
    keepViewport,
    fit: false,
    preserveApplyResultPositions,
    pinnedApplyResultPositions,
    pinnedApplyResultCenterX,
    zoneLayoutCache,
    applyAuxZoneOffsets: state.applyTraceSession?.applyAuxZoneOffsets ?? null,
    onAfterLayout: () => axis.sync()
  });
  applyPositionDebug("render-scene:after-setGraphInstant", {
    snapshot: collectApplyPositionSnapshot(state.applyTraceSession)
  });
  reapplyAllExpandEdgeStaging(state.applyTraceSession);
  applyPositionDebug("render-scene:after-reapply-staging", {
    snapshot: collectApplyPositionSnapshot(state.applyTraceSession)
  });
  syncApplyAuxDragHandles(cy);
  if (shouldAnimateFit) {
    await animateApplyFrameTransition(prevFramePos);
    syncApplyAuxDragHandles(cy);
  }
  pruneUserX(cy);
  ensureApplyCompareHighlight(state.applyTraceSession);
  if (shouldAnimateFit) {
    void smoothFit(cy, undefined, getApplyFitMotion()).then(() => {
      applyPositionDebug("render-scene:after-smoothFit", {
        snapshot: collectApplyPositionSnapshot(state.applyTraceSession)
      });
      syncApplyAuxDragHandles(cy);
      axis.sync();
    });
  }
  axis.sync();
}

async function expandApplyPath(path, { fit = true, keepViewport = false, updateComparePath = false } = {}) {
  const session = state.applyTraceSession;
  if (!session || state.isReducing) return false;
  clearApplyPendingCompareHighlight(session);
  state.isReducing = true;
  try {
    const branch = session.branchByPath.get(path);
    if (!branch) return false;

    const nextRevealed = new Set(session.revealed);
    const nextResolved = new Set(session.resolved);
    const nextExpanded = new Set(session.expanded);
    if (branch.phase === "reveal") {
      if (session.revealed.has(path)) return false;
      nextRevealed.add(path);
      if (isSameLevelApplyBranch(branch)) {
        // Same-level non-terminal previews should land directly on the
        // clickable lo/hi stub state instead of requiring one more node tap.
        nextExpanded.add(path);
      }
    } else if (branch.phase === "resolve") {
      if (session.resolved.has(path)) return false;
      nextRevealed.add(path);
      nextResolved.add(path);
      // Clicking a pair should immediately merge it into the node and expose
      // the next lo/hi edges, avoiding an extra click on the merged node.
      nextExpanded.add(path);
    } else {
      if (session.expanded.has(path)) return false;
      nextRevealed.add(path);
      nextResolved.add(path);
      nextExpanded.add(path);
    }

    const nextSession = {
      ...session,
      revealed: nextRevealed,
      resolved: nextResolved,
      expanded: nextExpanded
    };
    const data = await fetchInteractiveApplySceneData(nextSession, nextExpanded, {
      advancePath: path,
      advancePhase: branch.phase
    });
    if (!data?.snapshot) return false;
    storeApplyFinalLayout(session, data);
    storeApplyCurrentResultState(session, data);
    clearApplyExpandEdgeStaging(session);
    const nextBranch = (data.branches ?? []).find((br) => br?.path === path) ?? null;
    storeApplyBranches(session, data);
    const autoExpandedReveal = branch.phase === "reveal" && isSameLevelApplyBranch(branch) && nextExpanded.has(path);
    const stagedBranch =
      branch.phase === "resolve" && !nextBranch
        ? { ...branch, phase: "expand" }
        : (autoExpandedReveal && !nextBranch)
          ? { ...branch, phase: "expand" }
          : (nextBranch ?? branch);
    const stagedPhase = String(stagedBranch?.phase ?? "");
    applyStageDebug("after-fetch", {
      clickedPath: path,
      clickedPhase: branch?.phase,
      nextBranchPhase: nextBranch?.phase,
      stagedPhase,
      stagedBranch
    });
    stageApplyExpandPathState(session, path, stagedBranch);
    await renderInteractiveApplyScene(data.snapshot, session.vars, {
      fit,
      keepViewport,
      preserveApplyResultPositions: true,
      pinnedApplyResultPositions: session.finalResultPositions,
      pinnedApplyResultCenterX: session.finalResultCenterX,
      zoneLayoutCache: session.zoneLayoutCache
    });

    session.revealed = nextRevealed;
    session.resolved = nextResolved;
    session.expanded = nextExpanded;
    if (updateComparePath) {
      setApplyCompareHighlightFromBranch(session, nextBranch ?? branch);
      syncApplyCompareHighlight(session);
    } else {
      ensureApplyCompareHighlight(session);
    }
    await refreshApplyReduceAvailability(session);
    expr.refreshExprUiOnly(ctx, session.idx);
    return true;
  } finally {
    state.isReducing = false;
    refreshCanvasReduceButtons();
  }
}

async function runApplyReduceTrace(kind) {
  const session = state.applyTraceSession;
  if (!session || state.isReducing) return false;
  const cfg = REDUCE_TRACE_BY_KIND[kind];
  if (!cfg) return false;

  clearApplyPendingCompareHighlight(session);
  session.compareHighlightPath = "";
  session.compareHighlightIds = [];
  session.suspendCompareHighlight = true;
  clearApplyCompareHighlight();

  state.isReducing = true;
  try {
    const requestReduceTrace = async (resultStateOverride) => {
      const visibleResultNodeIds = getVisibleApplyResultNodeIds();
      const controller = new AbortController();
      const t = setTimeout(() => controller.abort(), 12000);
      try {
        return await fetchApplyReduceTrace(
          kind,
          session.op,
          session.expr1,
          session.expr2,
          session.vars,
          [...(session.revealed ?? [])],
          [...(session.resolved ?? [])],
          [...(session.expanded ?? [])],
          [],
          resultStateOverride,
          visibleResultNodeIds,
          controller.signal
        );
      } finally {
        clearTimeout(t);
      }
    };

    let resp = await requestReduceTrace(session.currentResultState);
    if (resp.status === 204) {
      session.suspendCompareHighlight = false;
      await refreshApplyReduceAvailability(session);
      console.warn("[apply-reduce] no trace returned", { kind, visibleKinds: getVisibleApplyReduceKinds() });
      return false;
    }
    if (!resp.ok) {
      session.suspendCompareHighlight = false;
      const txt = await resp.text().catch(() => "");
      console.error("[apply-reduce] HTTP not ok", resp.status, txt);
      return false;
    }

    const trace = await resp.json().catch((e) => {
      session.suspendCompareHighlight = false;
      console.error("[apply-reduce] JSON parse failed", e);
      return null;
    });
    if (!trace?.initial) {
      session.suspendCompareHighlight = false;
      console.error("[apply-reduce] missing trace.initial", trace);
      return false;
    }

    storeApplyBranches(session, { branches: trace.initialBranches ?? [] });
    storeApplyCurrentResultState(session, trace);
    const applyReduceLayoutOptions = {
      preserveApplyResultPositions: true,
      pinnedApplyResultPositions: session.finalResultPositions,
      pinnedApplyResultCenterX: session.finalResultCenterX,
      zoneLayoutCache: session.zoneLayoutCache,
      applyAuxZoneOffsets: session.applyAuxZoneOffsets ?? null
    };
    await renderInteractiveApplyScene(trace.initial, session.vars, {
      fit: true,
      keepViewport: false,
      ...applyReduceLayoutOptions
    });

    await cfg.playTrace(cy, trace, {
      setGraph: async (snapshot, step = null) => {
        if (step?.branches) storeApplyBranches(session, { branches: step.branches });
        if (step?.resultState) session.currentResultState = step.resultState;
        await renderInteractiveApplyScene(snapshot, session.vars, {
          fit: true,
          keepViewport: false,
          ...applyReduceLayoutOptions
        });
      },
      onAfterEach: () => axis.sync(),
      vars: session.vars,
      ctx: { vars: session.vars, state },
      scope: "apply-result",
      applyLayout: applyReduceLayoutOptions
    });

    session.suspendCompareHighlight = false;
    await refreshInteractiveApplyScene({ fit: true, keepViewport: false });
    expr.refreshExprUiOnly(ctx, session.idx);
    return true;
  } finally {
    state.isReducing = false;
    refreshCanvasReduceButtons();
  }
}

async function runRestrictStateReduceTrace(kind) {
  const session = state.restrictTraceSession;
  if (!session || !session.completed || !session.currentResultState || state.isReducing) return false;
  const cfg = REDUCE_TRACE_BY_KIND[kind];
  if (!cfg) return false;

  state.isReducing = true;
  try {
    const controller = new AbortController();
    const t = setTimeout(() => controller.abort(), 12000);
    let resp;
    try {
      resp = await fetchReduceStateTrace(kind, session.vars, session.currentResultState, controller.signal);
    } finally {
      clearTimeout(t);
    }

    if (resp.status === 204) {
      refreshCanvasReduceButtons();
      return false;
    }
    if (!resp.ok) {
      const txt = await resp.text().catch(() => "");
      console.error("[restrict-reduce] HTTP not ok", resp.status, txt);
      return false;
    }

    const trace = await resp.json().catch((e) => {
      console.error("[restrict-reduce] JSON parse failed", e);
      return null;
    });
    if (!trace?.initial) {
      console.error("[restrict-reduce] missing trace.initial", trace);
      return false;
    }

    const reduceLayoutOptions = { bddLayoutKind: "aux_sugiyama" };
    const setSnapshotKeepingExistingNodePositions = async (elements) => {
      const prevPos = captureNodePositions();
      await setGraphSnapshot(elements, session.vars, reduceLayoutOptions);
      // Keep coordinates of existing nodes stable across reduction frames;
      // only truly new nodes use freshly computed layout coordinates.
      cy.batch(() => {
        prevPos.forEach((p, id) => {
          const n = cy.getElementById(id);
          if (!n || n.empty?.() || n.isEdge?.()) return;
          n.position({ x: p.x, y: p.y });
        });
      });
      axis.sync();
    };
    // Do not re-render trace.initial here: the canvas is already at the current
    // restrict-result frame. Re-rendering would trigger a pre-animation layout
    // jump and cause visible flashing before reduction animations start.
    await cfg.playTrace(cy, trace, {
      setGraph: async (els, step = null) => {
        if (step?.resultState) session.currentResultState = step.resultState;
        await setSnapshotKeepingExistingNodePositions(els);
      },
      onAfterEach: () => axis.sync(),
      vars: session.vars,
      ctx: { vars: session.vars, state },
      applyLayout: reduceLayoutOptions
    });

    session.currentResultState = trace.steps?.at?.(-1)?.resultState ?? session.currentResultState;
    const finalSnap = trace.steps?.at?.(-1)?.snapshot ?? trace.initial;
    state.lastBddElements = finalSnap;
    state.lastBddPayload = { expr: session.expr, vars: [...session.vars] };
    // Keep the exact final animation frame as-is. Do not auto-layout here;
    // layout should only happen when the user explicitly clicks Layout.
    persistBddPane(session.idx);
    expr.refreshExprUiOnly(ctx, session.idx);
    axis.sync();
    return true;
  } finally {
    state.isReducing = false;
    refreshCanvasReduceButtons();
  }
}

async function formatExpressionLine(idx) {
  const line = state.expressions[idx];
  if (!line) return;

  const raw = (line.text || "").trim();
  if (!raw) return;

  const slot = parseDefinitionSlot(raw);
  if (!slot && /=/.test(raw)) return;
  const body = slot ? slot.rhs : raw;
  if (!body) return;

  const formattedApply = formatApplyCall(body);
  if (formattedApply) {
    const nextText = slot ? `${slot.name} = ${formattedApply}` : formattedApply;
    if (nextText !== line.text) {
      line.text = nextText;
      const item = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"]`);
      const input = item?.querySelector(".expr-input");
      if (input) input.value = nextText;
      expr.refreshExprUiOnly(ctx, idx);
      scheduleBdd();
    }
    return;
  }

  const parsedRestrict = parseRestrictCall(body);
  if (parsedRestrict) {
    const derivedName = makeRestrictDerivedName(
      parsedRestrict.bddName,
      parsedRestrict.bit,
      parsedRestrict.atomName
    );
    const formattedRestrict =
      `restrict(${parsedRestrict.bit}, ${parsedRestrict.atomName}, ${parsedRestrict.bddName})`;

    // Keep custom names (e.g. G = restrict(...)) untouched.
    // But if user uses restrict-derived alias format, always sync it with parameters.
    if (slot) {
      if (!isRestrictDerivedAliasName(slot.name)) return;
      if (!derivedName) return;
      const nextText = `${derivedName} = ${formattedRestrict}`;
      if (nextText !== line.text) {
        line.text = nextText;
        const item = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"]`);
        const input = item?.querySelector(".expr-input");
        if (input) input.value = nextText;
        expr.refreshExprUiOnly(ctx, idx);
        scheduleBdd();
      }
      return;
    }

    if (!derivedName) return;
    const nextText = `${derivedName} = ${formattedRestrict}`;
    if (nextText !== line.text) {
      line.text = nextText;
      const item = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"]`);
      const input = item?.querySelector(".expr-input");
      if (input) input.value = nextText;
      expr.refreshExprUiOnly(ctx, idx);
      scheduleBdd();
    }
    return;
  }

  if (/^apply\s*\(/i.test(body) || /^restrict\s*\(/i.test(body)) return;

  try {
    const resp = await fetchFormattedExpr(body);
    if (!resp.ok) return;
    const data = await resp.json().catch(() => null);
    const formatted = (data?.expr || "").trim();
    if (!formatted) return;

    const nextText = slot ? `${slot.name} = ${formatted}` : formatted;
    if (nextText === line.text) return;

    line.text = nextText;
    const item = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"]`);
    const input = item?.querySelector(".expr-input");
    if (input) input.value = nextText;
    expr.refreshExprUiOnly(ctx, idx);
    scheduleBdd();
  } catch {
    // Leave the user's original input untouched if formatting fails.
  }
}

function refreshNodeDraggability() {
  const enabled = Boolean(state.panelDragEnabled);
  if (!enabled) {
    cy.nodes().ungrabify();
    return;
  }
  cy.nodes().forEach((n) => {
    const hiddenLike = n.hasClass("apply-hidden-step") || n.hasClass("apply-ghost");
    const inAuxZone = n.hasClass("apply-left") || n.hasClass("apply-right");
    const dragHandle = n.hasClass("apply-drag-handle");
    const pairNode = n.hasClass("apply-pair");
    if (hiddenLike) {
      n.ungrabify();
      return;
    }
    // For apply auxiliary zones, keep drag ownership on handles/pairs only.
    // This prevents invisible/covered nodes from being draggable and desyncing the hit-area.
    if (inAuxZone && !dragHandle && !pairNode) {
      n.ungrabify();
      return;
    }
    n.grabify();
  });
}

function setDraggingEnabled(enabled) {
  state.panelDragEnabled = !!enabled;
  refreshNodeDraggability();
}

Object.assign(
  bddApis,
  createBddUpdateController({
    state,
    ctx,
    shouldRequest,
    prepareActiveExpr,
    ensureLineAnalysis,
    clearRestrictInteractiveFocus,
    clearActivePaneState,
    ensureBddPane,
    cloneElements,
    applyGlobalsFromPane,
    expr,
    cy,
    axis,
    setGraphInstant,
    setGraphAnimated,
    smoothFit,
    fetchBddApply,
    fetchBddReduced,
    fetchBdd,
    clearGraph,
    setReduceButtonsEnabled,
    setDraggingEnabled
  })
);

const bddScheduler = createBddScheduler({
  state,
  updateBddForActive: (isLive) => bddApis.updateBddForActive(isLive),
  debounceMs: BDD_DEBOUNCE_MS
});
const { scheduleBdd } = bddScheduler;

function hasAnyReduceApplied() {
  return (state.appliedReduce?.length ?? 0) > 0;
}

const { runReduceTrace } = createServerReduceTraceRunner({
  state,
  cy,
  axis,
  smoothFit,
  setGraphSnapshot,
  setDraggingEnabled,
  hasAnyReduceApplied,
  persistBddPane,
  expr,
  ctx,
  refreshCanvasReduceButtons
});

const { runAutoLayoutForCurrentBdd } = createBddAutoLayout({
  state,
  cy,
  axis,
  canAutoLayoutCurrentBdd,
  getLayoutVarsForCurrentCanvas,
  persistBddPane,
  refreshCanvasReduceButtons
});

function canUndoSkipReduction() {
  if (state.isReducing) return false;
  if (!state.skipReductionApplied) return false;
  if (!state.lastBddPayload) return false;
  return Boolean(state.baseBddElements?.nodes?.length);
}

async function runReduceForCurrentCanvas(kind) {
  if (state.isReducing) return false;
  const activeMeta = analyzeLine(state.activeIndex);
  if (state.applyTraceSession) {
    return runApplyReduceTrace(kind);
  }
  if (
    activeMeta?.kind === "restrict" &&
    state.restrictTraceSession?.completed &&
    state.restrictTraceSession?.idx === state.activeIndex
  ) {
    return runRestrictStateReduceTrace(kind);
  }
  if (!state.lastBddPayload) return false;
  const cfg = REDUCE_TRACE_BY_KIND[kind];
  if (!cfg) return false;
  return runReduceTrace(cfg);
}

function wireButtons() {
  dom.btnAdd?.addEventListener("click", () => {
    expr.addLine(ctx, "");
    expr.focusActiveInputSoon(dom);
  });

  dom.btnApplyToggle?.addEventListener("click", async () => {
    if (dom.btnApplyToggle.hidden || dom.btnApplyToggle.disabled) return;
    await ctx.callbacks.onPlayTrace(state.activeIndex);
    expr.refreshExprUiOnly(ctx, state.activeIndex);
    refreshBddBarPrimaryButtons();
  });

  // Primary: full Bryant reduce (OBDD) from server.
  // If skip-reduced view is active, clicking again restores pre-reduction BDD.
  dom.btnReduce?.addEventListener("click", async (ev) => {
    debugLog("btnReduce.click", {
      shiftKey: Boolean(ev?.shiftKey),
      canUndo: canUndoSkipReduction()
    });
    if (state.isReducing) return;
    // Skip/undo belongs to plain-expression canvas mode. Force-exit any stale
    // apply/restrict trace session so reducible-step buttons and callbacks
    // cannot keep reading old interactive session state.
    clearApplyPendingCompareHighlight(state.applyTraceSession);
    clearApplyCompareHighlight();
    clearRestrictInteractiveFocus();
    state.applyTraceSession = null;
    state.restrictTraceSession = null;
    state.isRestrictTracing = false;

    if (ev.shiftKey || canUndoSkipReduction()) {
      debugLog("btnReduce.undo.start");
      await restoreBaseGraphIfAvailable();
      debugLog("btnReduce.undo.done");
      return;
    }
    if (!state.lastBddPayload) return;

    // Invalidate any in-flight normal BDD fetch for this row so a late
    // response cannot overwrite the just-applied skip-reduction snapshot.
    const activeExpr = state.expressions[state.activeIndex];
    if (activeExpr) activeExpr.bddFetchTicket = (activeExpr.bddFetchTicket || 0) + 1;

    state.isReducing = true;
    const ownerIdx = state.activeIndex;
    const reducingGuard = setTimeout(() => {
      if (!state.isReducing) return;
      debugLog("btnReduce.skip.watchdog.force_unlock", { ownerIdx });
      state.isReducing = false;
      refreshCanvasReduceButtons();
      refreshBddBarPrimaryButtons();
    }, 20000);
    try {
      debugLog("btnReduce.skip.start", { ownerIdx });
      const { expr: exprStr, vars } = state.lastBddPayload;
      const controller = new AbortController();
      const t = setTimeout(() => controller.abort(), 12000);
      let resp;
      try {
        debugLog("btnReduce.skip.fetch.start", { varsCount: Array.isArray(vars) ? vars.length : -1 });
        resp = await fetchBddReduced(exprStr, vars, controller.signal);
        debugLog("btnReduce.skip.fetch.done", { status: resp?.status });
      } finally {
        clearTimeout(t);
      }

      if (!resp.ok) {
        const txt = await resp.text().catch(() => "");
        console.error("[reduce-full] HTTP not ok", resp.status, txt);
        debugLog("btnReduce.skip.http_not_ok", { status: resp.status, body: txt.slice(0, 200) });
        return;
      }

      const data = await resp.json().catch((e) => {
        console.error("[reduce-full] JSON parse failed", e);
        debugLog("btnReduce.skip.json.error", { message: String(e?.message || e) });
        return null;
      });
      if (!data?.elements?.nodes || !data?.elements?.edges) {
        console.error("[reduce-full] bad payload", data);
        debugLog("btnReduce.skip.bad_payload");
        return;
      }

      state.appliedReduce.length = 0;
      state.skipReductionApplied = true;
      state.userX.clear();
      state.lastBddElements = data.elements;
      state.bddLayoutKind = "aux_sugiyama";

      cancelAllGraphAnims(cy);
      // Use instant snapshot here instead of animated replace. In production
      // latency/jitter conditions, animated replacement can occasionally stall
      // and keep the UI in reducing mode.
      await setGraphSnapshot(data.elements, vars, { bddLayoutKind: state.bddLayoutKind });
      debugLog("btnReduce.skip.graph_snapshot.done");
      setDraggingEnabled(true);
      const fitOutcome = await Promise.race([
        smoothFit(cy, undefined, { padding: 30, duration: 260 })
          .then(() => "ok")
          .catch((e) => {
            debugLog("btnReduce.skip.smooth_fit.error", { message: String(e?.message || e) });
            return "error";
          }),
        new Promise((resolve) => setTimeout(() => resolve("timeout"), 1200))
      ]);
      debugLog("btnReduce.skip.smooth_fit.done", { outcome: fitOutcome });
      axis.sync();
      persistBddPane(ownerIdx);
      debugLog("btnReduce.skip.done");
    } catch (e) {
      if (e?.name !== "AbortError") console.error("[reduce-full] failed", e);
      debugLog("btnReduce.skip.error", { name: String(e?.name || ""), message: String(e?.message || e) });
    } finally {
      clearTimeout(reducingGuard);
      state.isReducing = false;
      debugLog("btnReduce.skip.finally");
      refreshCanvasReduceButtons();
    }
  });

  const reduceTraceButtons = [
    { kind: "terminals", btn: dom.btnReduceTerminals },
    { kind: "redundant", btn: dom.btnReduceRedundant },
    { kind: "merge", btn: dom.btnReduceMerge }
  ];
  reduceTraceButtons.forEach(({ kind, btn }) => {
    btn?.addEventListener("click", async () => {
      if (btn.disabled) return;
      const restrictSession = state.restrictTraceSession;
      const restrictStepIndex =
        btn === dom.btnReduceTerminals ? 0 :
        btn === dom.btnReduceRedundant ? 1 :
        btn === dom.btnReduceMerge ? 2 : -1;
      if (
        restrictSession?.active &&
        !restrictSession.completed &&
        restrictSession.idx === state.activeIndex
      ) {
        if (restrictStepIndex !== restrictSession.nextStepIndex) return;
        await advanceRestrictTraceSession(restrictSession);
        expr.refreshExprUiOnly(ctx, restrictSession.idx);
        refreshCanvasReduceButtons();
        return;
      }
      await runReduceForCurrentCanvas(kind);
    });
  });

  dom.btnLayout?.addEventListener("click", async () => {
    if (dom.btnLayout.disabled) return;
    await runAutoLayoutForCurrentBdd();
  });

  refreshCanvasReduceButtons();
  refreshBddBarPrimaryButtons();
}

function wireCyAxisSync() {
  cy.on("zoom pan", () => axis.sync());
  cy.on("resize", () => axis.sync());

  let fitTimer = null;
  function scheduleFitAfterDrag() {
    if (fitTimer) clearTimeout(fitTimer);
    fitTimer = setTimeout(() => {
      fitTimer = null;
      void smoothFit(cy, undefined, { padding: 30, duration: 220 }).then(() => axis.sync());
    }, 60);
  }

  cy.on("dragfree", "node", (evt) => {
    const n = evt.target;
    const applyZoneDrag = n.scratch("_applyZoneDrag");
    if (applyZoneDrag?.zoneKey) {
      const zone = applyZoneDrag.zoneKey === "apply-left" ? "L" : applyZoneDrag.zoneKey === "apply-right" ? "R" : null;
      const session = state.applyTraceSession;
      const movedNodeIds = (applyZoneDrag.nodeIds ?? []).filter((id) => id && id !== n.id());
      let zoneDx = Number(n.position("x")) - Number(applyZoneDrag.dragStartX ?? n.position("x"));
      if (movedNodeIds.length) {
        const deltas = [];
        for (const nodeId of movedNodeIds) {
          const node = cy.getElementById(nodeId);
          const base = applyZoneDrag.basePositions?.[nodeId];
          if (!node || node.empty?.() || !base) continue;
          const dx = Number(node.position("x")) - Number(base.x);
          if (Number.isFinite(dx)) deltas.push(dx);
        }
        if (deltas.length) zoneDx = deltas.reduce((a, b) => a + b, 0) / deltas.length;
      }
      if (zone && session?.applyAuxZoneOffsets) {
        session.applyAuxZoneOffsets[zone] = Number(session.applyAuxZoneOffsets[zone] ?? 0) + zoneDx;
      }
      // Keep staged hidden-target "original positions" in sync with zone drags.
      // Otherwise, clicking a staged lo/hi edge can restore nodes to pre-drag
      // coordinates and cause a visible snap-back.
      if (zone && session?.expandNodeOriginalPos instanceof Map && Number.isFinite(zoneDx)) {
        const zonePrefix = zone === "L" ? "L-" : zone === "R" ? "R-" : "";
        if (zonePrefix) {
          for (const [nodeId, pos] of session.expandNodeOriginalPos.entries()) {
            if (!String(nodeId).startsWith(zonePrefix) || !pos) continue;
            session.expandNodeOriginalPos.set(nodeId, { ...pos, x: Number(pos.x) + zoneDx });
          }
        }
      }
      // Keep userX aligned with zone-drag result; otherwise next frame render
      // can be overridden by stale userX and visually "snap back".
      for (const nodeId of applyZoneDrag.nodeIds ?? []) {
        const node = cy.getElementById(nodeId);
        if (!node || node.empty?.() || node.hasClass("apply-drag-handle")) continue;
        state.userX.set(nodeId, node.position("x"));
      }
      applyPositionDebug("zone-dragfree", {
        zone,
        zoneDx,
        dragNodeId: n.id(),
        movedNodeCount: movedNodeIds.length,
        snapshot: collectApplyPositionSnapshot(session)
      });
      scheduleFitAfterDrag();
      return;
    }
    const anchorId = n.hasClass("apply-pair") ? applyPairAnchorId(n.id()) : null;
    if (anchorId) {
      const anchor = cy.getElementById(anchorId);
      if (anchor && !anchor.empty?.()) state.userX.set(anchorId, anchor.position("x"));
      state.userX.set(n.id(), n.position("x"));
      const siblingId = n.id().endsWith("-L") ? n.id().replace(/-L$/, "-R") : n.id().replace(/-R$/, "-L");
      const sibling = cy.getElementById(siblingId);
      if (sibling && !sibling.empty?.()) state.userX.set(siblingId, sibling.position("x"));
    } else {
      state.userX.set(n.id(), n.position("x"));
    }
    scheduleFitAfterDrag();
  });

  cy.on("drag", "node", (evt) => {
    const session = state.applyTraceSession;
    if (!session || state.isReducing) return;
    const nodeId = evt.target?.id?.();
    if (!nodeId) return;
    if (repositionApplyStagedStubTargetsForNode(session, nodeId)) axis.sync();
  });

  cy.on("tap", "node", async (evt) => {
    const session = state.applyTraceSession;
    if (!session || state.isReducing) return;

    const id = evt.target.id();
    const path = session.branchMap.get(id);
    if (!path || session.expanded.has(path)) return;

    try {
      await expandApplyPath(path, {
        fit: true,
        keepViewport: false,
        updateComparePath: path !== session.compareHighlightPath
      });
    } finally {
      axis.sync();
    }
  });

  cy.on("tap", "edge", async (evt) => {
    const session = state.applyTraceSession;
    if (!session || state.isReducing) return;
    const id = evt.target.id();
    const revealResult = revealApplyExpandSideByEdge(session, id);
    if (!revealResult) return;

    setApplyPendingCompareHighlightFromReveal(session, revealResult);
    syncApplyCompareHighlight(session);
    axis.sync();

    const autoExpandPaths = autoExpandApplyPathsForNodeIds(session, revealResult.revealedNodeIds);
    for (const path of autoExpandPaths) {
      try {
        await expandApplyPath(path, {
          fit: false,
          keepViewport: true,
          updateComparePath: path !== session.compareHighlightPath
        });
      } finally {
        axis.sync();
      }
    }
    if (state.applyTraceSession === session) await refreshApplyReduceAvailability(session);
  });

  window.addEventListener("resize", () => axis.sync());
}

function init() {
  setReduceButtonsEnabled(false);
  refreshBddBarPrimaryButtons();

  expr.renderExprList(ctx);
  expr.setActiveIndex(ctx, 0);
  scheduleLineAnalysisRefresh();

  clearGraph(cy);
  state.lastRequestedKey = null;
  axis.sync();

  setupKeyboard({ ...ctx, expr });
  setupLeftDrawer({ dom, state, cy, axis, smoothFit });
  setupCanvasResizeObserver({ dom, state, cy, axis, smoothFit });
  wireButtons();
  wireCyAxisSync();

  autoFitOnResize(cy, () => axis.sync(), { padding: 30, debounceMs: 60 });
}

init();