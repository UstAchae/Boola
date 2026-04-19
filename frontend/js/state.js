/** Per-expression right-panel state (BDD graph, reduce history, layout). */
export function defaultBddPane() {
  return {
    lastBddPayload: null,
    baseBddElements: null,
    lastBddElements: null,
    appliedReduce: [],
    skipReductionApplied: false,
    lastRequestedKey: null,
    userX: {},
    panelDragEnabled: false,
    layoutKind: "tree"
  };
}

export function createState() {
  const prefersTouchKeyboard =
    typeof window !== "undefined" &&
    (
      window.matchMedia?.("(pointer: coarse)")?.matches ||
      navigator.maxTouchPoints > 0
    );

  return {
    apList: ["p", "q", "r", "s"],
    expressions: [{ id: crypto.randomUUID(), text: "", order: [], bddPane: defaultBddPane() }],
    activeIndex: 0,
    selectedForApply: new Set(),
    focusedInput: null,
    prefersTouchKeyboard,
    bddTimer: null,
    lastBddPayload: null,

    baseBddElements: null, // active tab mirror (from expressions[i].bddPane)
    lastBddElements: null,

    appliedReduce: [],
    skipReductionApplied: false,
    isReducing: false,
    lastRequestedKey: null,
    userX: new Map(),
    /** Mirrors active tab bddPane while it is selected */
    panelDragEnabled: false,
    bddLayoutKind: "tree",
    applyTraceSession: null,
    restrictTraceSession: null,
    isRestrictTracing: false,
    lineAnalysisCache: new Map(),
    lineAnalysisVersion: 0
  };
}
