import { syncOrder } from "./defs.js";

export function setReduceButtonsEnabled(dom, enabled) {
  [dom.btnReduce, dom.btnReduceTerminals, dom.btnReduceRedundant, dom.btnReduceMerge].forEach((b) => {
    if (!b) return;
    b.disabled = !enabled;
  });
}

export function updateSelectedInfo(state, dom) {
  const active = state.expressions[state.activeIndex];
  if (!active) {
    dom.selectedInfo.textContent = "No selection";
    return;
  }
  const applyPick = [...state.selectedForApply].map((i) => i + 1).join(", ");
  dom.selectedInfo.textContent =
    `Selected: #${state.activeIndex + 1}` + (applyPick ? ` | Apply: [${applyPick}]` : "");
}

function updateActiveClass(state, dom) {
  const items = dom.exprListEl.querySelectorAll(".expr-item");
  items.forEach((el) => {
    const idx = Number(el.dataset.index);
    el.classList.toggle("active", idx === state.activeIndex);
  });
}

function focusIndex(state, dom, idx, placeCursorAtEnd = true) {
  queueMicrotask(() => {
    const item = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"]`);
    const input = item?.querySelector(".expr-input");
    if (!input) return;

    input.focus();
    state.focusedInput = input;

    if (placeCursorAtEnd) {
      const n = input.value.length;
      input.setSelectionRange(n, n);
    }
  });
}

export function setActiveIndex(state, dom, idx) {
  if (idx < 0 || idx >= state.expressions.length) return;
  state.activeIndex = idx;
  updateActiveClass(state, dom);
  updateSelectedInfo(state, dom);
  focusIndex(state, dom, idx);
}

export function updateOrderBarOnly(ctx, idx) {
  const { state, dom, callbacks } = ctx;
  const item = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"]`);
  if (!item) return;

  const bar = item.querySelector(".order-bar");
  if (!bar) return;

  bar.innerHTML = "";
  const expr = state.expressions[idx];
  const order = expr.order || [];

  if (order.length === 0) {
    bar.style.display = "none";
    return;
  }
  bar.style.display = "flex";

  function clearDropTargets(container) {
    const targets = container.querySelectorAll(".chip.drop-target");
    targets.forEach((t) => t.classList.remove("drop-target"));
  }

  for (const v of order) {
    const chip = document.createElement("div");
    chip.className = "chip";
    chip.draggable = true;
    chip.dataset.var = v;

    const label = document.createElement("span");
    label.textContent = v;
    chip.appendChild(label);

    chip.addEventListener("dragstart", (e) => {
      chip.classList.add("dragging");
      e.dataTransfer.effectAllowed = "move";
      e.dataTransfer.setData("text/plain", v);
      bar.dataset.dragVar = v;
    });

    chip.addEventListener("dragend", () => {
      chip.classList.remove("dragging");
      clearDropTargets(bar);
      delete bar.dataset.dragVar;
    });

    chip.addEventListener("dragover", (e) => {
      e.preventDefault();
      const dragVar = bar.dataset.dragVar;
      if (!dragVar || dragVar === v) return;
      chip.classList.add("drop-target");
      e.dataTransfer.dropEffect = "move";
    });

    chip.addEventListener("dragleave", () => {
      chip.classList.remove("drop-target");
    });

    chip.addEventListener("drop", (e) => {
      e.preventDefault();
      const dragVar = e.dataTransfer.getData("text/plain") || bar.dataset.dragVar;
      if (!dragVar || dragVar === v) return;

      if (state.activeIndex !== idx) setActiveIndex(state, dom, idx);

      const ord = state.expressions[idx].order || [];
      const from = ord.indexOf(dragVar);
      const to = ord.indexOf(v);
      if (from !== -1 && to !== -1 && from !== to) {
        ord.splice(from, 1);
        ord.splice(to, 0, dragVar);
      }

      updateOrderBarOnly(ctx, idx);
      callbacks.onExprChanged?.();
      clearDropTargets(bar);
    });

    bar.appendChild(chip);
  }
}

export function onLineChanged(ctx, idx, inputEl, { runNetwork = true } = {}) {
  const { state, dom, callbacks } = ctx;
  const e = state.expressions[idx];
  e.text = inputEl.value;

  if (!e.text.trim()) e.order = [];
  else e.order = syncOrder(e.text, e.order);

  state.activeIndex = idx;
  updateActiveClass(state, dom);
  updateSelectedInfo(state, dom);
  updateOrderBarOnly(ctx, idx);

  if (runNetwork) callbacks.onExprChanged?.();
}

export function clearLineAt(ctx, idx) {
  const { dom } = ctx;
  const item = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"]`);
  const input = item?.querySelector(".expr-input");
  if (!input) return;
  input.value = "";
  onLineChanged(ctx, idx, input);
}

export function addLine(ctx, text = "") {
  const { state } = ctx;
  const order = syncOrder(text, []);
  state.expressions.push({ id: crypto.randomUUID(), text, order });
  renderExprList(ctx);
  setActiveIndex(state, ctx.dom, state.expressions.length - 1);
}

export function clearAll(ctx) {
  const { state } = ctx;
  state.expressions = [{ id: crypto.randomUUID(), text: "", order: [] }];
  state.activeIndex = 0;
  state.selectedForApply.clear();
  renderExprList(ctx);
}

export function toggleSelectForApply(ctx, idx) {
  const { state } = ctx;

  if (state.selectedForApply.has(idx)) state.selectedForApply.delete(idx);
  else {
    if (state.selectedForApply.size >= 2) {
      const first = state.selectedForApply.values().next().value;
      state.selectedForApply.delete(first);
    }
    state.selectedForApply.add(idx);
  }

  renderExprList(ctx);
}

export function focusActiveInputSoon(dom) {
  queueMicrotask(() => {
    const active = dom.exprListEl.querySelector(".expr-item.active .expr-input");
    if (active) active.focus();
  });
}

export function renderExprList(ctx) {
  const { state, dom } = ctx;
  dom.exprListEl.innerHTML = "";

  state.expressions.forEach((expr, idx) => {
    const item = document.createElement("div");
    item.className = "expr-item" + (idx === state.activeIndex ? " active" : "");
    item.dataset.index = String(idx);

    const index = document.createElement("div");
    index.className = "expr-index";
    index.textContent = String(idx + 1);

    const mid = document.createElement("div");
    mid.className = "expr-mid";

    const input = document.createElement("input");
    input.className = "expr-input";
    input.id = `expr-${idx}`;
    input.name = `expr-${idx}`;
    input.value = expr.text || "";
    input.placeholder = "";

    input.addEventListener("focus", () => {
      state.focusedInput = input;
      if (state.activeIndex !== idx) {
        state.activeIndex = idx;
        updateActiveClass(state, dom);
        updateSelectedInfo(state, dom);
      }
    });

    input.addEventListener("blur", () => {
      if (state.focusedInput === input) state.focusedInput = null;
    });

    input.addEventListener("input", () => {
      onLineChanged(ctx, idx, input);
    });

    mid.appendChild(input);

    const orderBar = document.createElement("div");
    orderBar.className = "order-bar";
    mid.appendChild(orderBar);

    const del = document.createElement("button");
    del.className = "expr-del";
    del.textContent = "×";
    del.title = "Delete";
    del.addEventListener("click", (ev) => {
      ev.stopPropagation();
      clearLineAt(ctx, idx);
    });

    item.addEventListener("click", () => setActiveIndex(state, dom, idx));
    item.addEventListener("dblclick", () => toggleSelectForApply(ctx, idx));

    item.appendChild(index);
    item.appendChild(mid);
    item.appendChild(del);

    dom.exprListEl.appendChild(item);

    updateOrderBarOnly(ctx, idx);
  });

  updateSelectedInfo(state, dom);
}