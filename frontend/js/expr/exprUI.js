import {
  canonicalApplyOperator,
  inferVars,
  isDerivedExprName,
  parseApplyCall,
  parseRestrictCall,
  parseDefinitionSlot,
  sanitizeApplyOperatorDraft,
  syncOrder,
  trimVariablesToLimit
} from "./defs.js";
import { topLevelOperatorLabel } from "./labels.js";
import { defaultBddPane } from "../state.js";

const MAX_BOOLEAN_VARS = 5;
const VAR_LIMIT_MESSAGE = "This version does not support too many boolean expression variables (max 5).";

let varLimitDialogRoot = null;
let varLimitDialogOkBtn = null;
let varLimitDialogPendingFocus = null;

export function setReduceButtonsEnabled(dom, enabled) {
  if (!dom.btnReduce) return;
  const canUse = Boolean(enabled);
  dom.btnReduce.disabled = !canUse;
  dom.btnReduce.hidden = !canUse;
}

export function updateSelectedInfo(state, dom) {
  if (!dom.selectedInfo) return;
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

function applyMobileKeyboardMode(state, input) {
  if (!input) return;

  const customKeyboardOnly = Boolean(state.prefersTouchKeyboard);
  input.readOnly = customKeyboardOnly;
  input.setAttribute("inputmode", customKeyboardOnly ? "none" : "text");
  input.setAttribute("autocomplete", "off");
  input.setAttribute("autocorrect", "off");
  input.setAttribute("autocapitalize", "off");
  input.setAttribute("spellcheck", "false");
  input.classList.toggle("expr-input--custom-kbd", customKeyboardOnly);
}

let dragExprFromIdx = -1;

function ensureVarLimitDialog() {
  if (varLimitDialogRoot && document.body.contains(varLimitDialogRoot)) {
    return varLimitDialogRoot;
  }

  const root = document.createElement("div");
  root.className = "var-limit-dialog";
  root.hidden = true;
  root.innerHTML = `
    <div class="var-limit-dialog__backdrop" aria-hidden="true"></div>
    <div class="var-limit-dialog__panel" role="dialog" aria-modal="true" aria-labelledby="varLimitDialogTitle">
      <h3 class="var-limit-dialog__title" id="varLimitDialogTitle">Input limit</h3>
      <p class="var-limit-dialog__message">${VAR_LIMIT_MESSAGE}</p>
      <div class="var-limit-dialog__actions">
        <button type="button" class="btn primary var-limit-dialog__ok">OK</button>
      </div>
    </div>
  `;

  const okBtn = root.querySelector(".var-limit-dialog__ok");
  const close = () => {
    root.hidden = true;
    root.classList.remove("is-open");
    const focusTarget = varLimitDialogPendingFocus;
    varLimitDialogPendingFocus = null;
    if (focusTarget?.input && document.contains(focusTarget.input)) {
      const safeCaret = Math.max(0, Math.min(focusTarget.input.value.length, focusTarget.caret ?? focusTarget.input.value.length));
      focusTarget.input.focus();
      focusTarget.input.setSelectionRange(safeCaret, safeCaret);
    }
  };

  okBtn?.addEventListener("click", close);
  root.addEventListener("keydown", (ev) => {
    if (ev.key !== "Enter" && ev.key !== "Escape") return;
    ev.preventDefault();
    close();
  });
  root.addEventListener("pointerdown", (ev) => {
    if (!ev.target.closest(".var-limit-dialog__panel")) {
      ev.preventDefault();
      close();
    }
  });

  document.body.appendChild(root);
  varLimitDialogRoot = root;
  varLimitDialogOkBtn = okBtn;
  return root;
}

function openVarLimitDialog(input, caret) {
  const root = ensureVarLimitDialog();
  varLimitDialogPendingFocus = { input, caret };
  if (!root.hidden) return;
  root.hidden = false;
  root.classList.add("is-open");
  queueMicrotask(() => {
    varLimitDialogOkBtn?.focus();
  });
}

function remapIndexAfterMove(index, fromIdx, toIdx) {
  if (index === fromIdx) return toIdx;
  if (fromIdx < toIdx && index > fromIdx && index <= toIdx) return index - 1;
  if (fromIdx > toIdx && index >= toIdx && index < fromIdx) return index + 1;
  return index;
}

function remapIndexAfterRemoval(index, removedIdx) {
  if (index === removedIdx) return -1;
  if (index > removedIdx) return index - 1;
  return index;
}

function moveExpressionRow(ctx, fromIdx, toIdx) {
  const { state, callbacks } = ctx;
  if (fromIdx === toIdx) return;
  if (fromIdx < 0 || toIdx < 0) return;
  if (fromIdx >= state.expressions.length || toIdx >= state.expressions.length) return;

  const [moved] = state.expressions.splice(fromIdx, 1);
  if (!moved) return;
  state.expressions.splice(toIdx, 0, moved);

  state.activeIndex = remapIndexAfterMove(state.activeIndex, fromIdx, toIdx);
  state.selectedForApply = new Set(
    [...state.selectedForApply].map((idx) => remapIndexAfterMove(idx, fromIdx, toIdx))
  );

  renderExprList(ctx);
  callbacks.onExprChanged?.();
}

function deleteExpressionRow(ctx, idx) {
  const { state, callbacks } = ctx;
  if (idx < 0 || idx >= state.expressions.length) return;

  state.expressions.splice(idx, 1);

  if (state.expressions.length === 0) {
    state.expressions = [{ id: crypto.randomUUID(), text: "", order: [], bddPane: defaultBddPane() }];
    state.activeIndex = 0;
    state.selectedForApply.clear();
    callbacks.onExpressionsReset?.();
    renderExprList(ctx);
    return;
  }

  state.activeIndex = Math.max(0, Math.min(state.expressions.length - 1, remapIndexAfterRemoval(state.activeIndex, idx)));
  state.selectedForApply = new Set(
    [...state.selectedForApply]
      .map((lineIdx) => remapIndexAfterRemoval(lineIdx, idx))
      .filter((lineIdx) => lineIdx >= 0)
  );

  renderExprList(ctx);
  setActiveIndex(ctx, state.activeIndex, { focusInput: false });
  callbacks.onExprChanged?.();
}

function findApplySlotDelimiters(value) {
  const text = value || "";
  const applyStart = text.search(/apply\s*\(/i);
  if (applyStart < 0) return null;

  const prefix = text.slice(0, applyStart);
  if (prefix.trim()) {
    const prefixSlot = parseDefinitionSlot(prefix);
    if (!prefixSlot || prefixSlot.rhs) return null;
  }

  const firstComma = text.indexOf(",", applyStart);
  if (firstComma < 0) return null;
  const secondComma = text.indexOf(",", firstComma + 1);
  if (secondComma < 0) return null;
  const close = text.lastIndexOf(")");
  if (close < 0 || close < secondComma) return null;

  const firstLt = text.indexOf("<", applyStart);
  const lastGtBeforeFirstComma = text.lastIndexOf(">", firstComma);
  if (firstLt < 0 || lastGtBeforeFirstComma < firstLt) return null;

  return {
    applyStart,
    firstComma,
    secondComma,
    close,
    firstLt,
    lastGtBeforeFirstComma
  };
}

function findRestrictSlotDelimiters(value) {
  const text = value || "";
  const start = text.search(/restrict\s*\(/i);
  if (start < 0) return null;

  const prefix = text.slice(0, start);
  if (prefix.trim()) {
    const prefixSlot = parseDefinitionSlot(prefix);
    if (!prefixSlot || prefixSlot.rhs) return null;
  }

  const firstComma = text.indexOf(",", start);
  if (firstComma < 0) return null;
  const secondComma = text.indexOf(",", firstComma + 1);
  if (secondComma < 0) return null;
  const close = text.lastIndexOf(")");
  if (close < 0 || close < secondComma) return null;

  return { start, firstComma, secondComma, close };
}

function trimSlotRange(value, rawStart, rawEnd) {
  let start = rawStart;
  let end = rawEnd;
  while (start < end && /\s/.test(value[start])) start += 1;
  while (end > start && /\s/.test(value[end - 1])) end -= 1;
  return { start, end };
}

function selectApplySlot(input, rawStart, rawEnd) {
  const { start, end } = trimSlotRange(input.value, rawStart, rawEnd);
  const isSlotDelimiter = (ch) => /[\s,()<>]/.test(ch || "");
  let cleanStart = start;
  let cleanEnd = end;
  while (cleanStart < cleanEnd && isSlotDelimiter(input.value[cleanStart])) cleanStart += 1;
  while (cleanEnd > cleanStart && isSlotDelimiter(input.value[cleanEnd - 1])) cleanEnd -= 1;

  input.focus();
  if (cleanStart === cleanEnd) {
    const safeStart = Math.max(0, rawStart);
    let caret = safeStart;
    // Never land on template delimiters (comma/parens/angle brackets).
    while (caret < input.value.length && /[,()<>]/.test(input.value[caret])) caret += 1;
    if (caret > rawEnd) caret = Math.max(0, rawEnd);
    input.setSelectionRange(caret, caret);
    return;
  }
  input.setSelectionRange(cleanStart, cleanEnd);
}

function normalizeTemplateSpacing(input) {
  if (!input || !document.contains(input)) return false;
  const original = String(input.value || "");
  const selStart = input.selectionStart ?? 0;
  const selEnd = input.selectionEnd ?? selStart;
  const wasCollapsed = selStart === selEnd;
  const metaBefore = getTemplateSlotMeta(input);
  const activeSlotIndex = findSlotIndexBySelection(metaBefore, selStart, selEnd);

  const apply = findApplySlotDelimiters(original);
  if (apply) {
    const prefix = original.slice(0, apply.applyStart);
    const suffix = original.slice(apply.close + 1);
    const op = original.slice(apply.firstLt + 1, apply.lastGtBeforeFirstComma).trim();
    const left = original.slice(apply.firstComma + 1, apply.secondComma).trim();
    const right = original.slice(apply.secondComma + 1, apply.close).trim();
    const rebuilt = `${prefix}apply(<${op}>, ${left}, ${right})${suffix}`;
    if (rebuilt === original) return false;
    input.value = rebuilt;
    const d2 = findApplySlotDelimiters(rebuilt);
    if (d2 && activeSlotIndex >= 0) {
      const slotByIndex = [
        [d2.firstLt + 1, d2.lastGtBeforeFirstComma],
        [d2.firstComma + 1, d2.secondComma],
        [d2.secondComma + 1, d2.close]
      ][activeSlotIndex];
      if (slotByIndex) {
        if (wasCollapsed) {
          const slotStart = Math.max(0, slotByIndex[0]);
          const slotEnd = Math.max(slotStart, slotByIndex[1]);
          const mapped = Math.min(slotEnd, Math.max(slotStart, selStart));
          input.setSelectionRange(mapped, mapped);
        } else {
          selectApplySlot(input, slotByIndex[0], slotByIndex[1]);
        }
        return true;
      }
    }
    const nextCaret = Math.min(rebuilt.length, selStart);
    input.setSelectionRange(nextCaret, nextCaret);
    return true;
  }

  const restrict = findRestrictSlotDelimiters(original);
  if (!restrict) return false;
  const prefix = original.slice(0, restrict.start);
  const suffix = original.slice(restrict.close + 1);
  const bit = original.slice(restrict.start + "restrict(".length, restrict.firstComma).trim();
  const atom = original.slice(restrict.firstComma + 1, restrict.secondComma).trim();
  const bdd = original.slice(restrict.secondComma + 1, restrict.close).trim();
  const rebuilt = `${prefix}restrict(${bit}, ${atom}, ${bdd})${suffix}`;
  if (rebuilt === original) return false;
  input.value = rebuilt;
  const d2 = findRestrictSlotDelimiters(rebuilt);
  if (d2 && activeSlotIndex >= 0) {
    const slotByIndex = [
      [d2.start + "restrict(".length, d2.firstComma],
      [d2.firstComma + 1, d2.secondComma],
      [d2.secondComma + 1, d2.close]
    ][activeSlotIndex];
    if (slotByIndex) {
      if (wasCollapsed) {
        const slotStart = Math.max(0, slotByIndex[0]);
        const slotEnd = Math.max(slotStart, slotByIndex[1]);
        const mapped = Math.min(slotEnd, Math.max(slotStart, selStart));
        input.setSelectionRange(mapped, mapped);
      } else {
        selectApplySlot(input, slotByIndex[0], slotByIndex[1]);
      }
      return true;
    }
  }
  const nextCaret = Math.min(rebuilt.length, selStart);
  input.setSelectionRange(nextCaret, nextCaret);
  return true;
}

function replaceSlotText(input, rawStart, rawEnd, text) {
  const { start, end } = trimSlotRange(input.value, rawStart, rawEnd);
  input.value = input.value.slice(0, start) + text + input.value.slice(end);
  input.focus();
  const caret = start + text.length;
  input.setSelectionRange(caret, caret);
  return { start, end, caret };
}

function isApplyLikeText(text) {
  const slot = parseDefinitionSlot(text || "");
  const body = slot ? slot.rhs : (text || "").trim();
  return /^apply\s*\(/i.test(body) || /^restrict\s*\(/i.test(body);
}

function isApplyTemplateOnlyText(text) {
  const slot = parseDefinitionSlot(text || "");
  const body = slot ? slot.rhs : (text || "").trim();
  return (
    /^apply\s*\(\s*<\s*>\s*,\s*,\s*\)\s*$/i.test(body) ||
    /^restrict\s*\(\s*,\s*,\s*\)\s*$/i.test(body)
  );
}

function syncApplyCodeStyle(input) {
  if (!input) return;
  input.classList.toggle("expr-input--code", isApplyTemplateOnlyText(input.value));
  input.classList.toggle("expr-input--apply", isApplyLikeText(input.value));
}

export function handleProtectedApplyBackspace(input) {
  if (!input || !document.contains(input)) return { handled: false, changed: false };

  const caretStart = input.selectionStart ?? 0;
  const caretEnd = input.selectionEnd ?? caretStart;
  if (caretStart !== caretEnd) {
    const selected = String(input.value || "").slice(caretStart, caretEnd);
    const protectedChars = /[,\(\)<>]/.test(selected);
    if (!protectedChars) return { handled: false, changed: false };
    // Block deleting template scaffold characters selected accidentally.
    input.focus();
    input.setSelectionRange(caretStart, caretStart);
    normalizeTemplateCaret(input);
    return { handled: true, changed: false };
  }

  const delims = findApplySlotDelimiters(input.value);
  if (!delims) {
    const r = findRestrictSlotDelimiters(input.value);
    if (!r) return { handled: false, changed: false };
    if (caretStart === r.start && r.start > 0) return { handled: false, changed: false };

    const bitRange = trimSlotRange(input.value, r.start + "restrict(".length, r.firstComma);
    const atomRange = trimSlotRange(input.value, r.firstComma + 1, r.secondComma);
    const bddRange = trimSlotRange(input.value, r.secondComma + 1, r.close);
    const bitText = input.value.slice(bitRange.start, bitRange.end);
    const atomText = input.value.slice(atomRange.start, atomRange.end);
    const bddText = input.value.slice(bddRange.start, bddRange.end);
    const inFrame = caretStart >= r.start && caretStart <= r.close + 1;
    if (!inFrame) return { handled: false, changed: false };

    // restrict(_, , F) <-backspace- restrict(, _, F) <-backspace- restrict(, , F)
    if (caretStart <= bitRange.start) {
      selectApplySlot(input, r.firstComma + 1, r.secondComma);
      return { handled: true, changed: false };
    }
    if (caretStart <= atomRange.start) {
      selectApplySlot(input, r.secondComma + 1, r.close);
      return { handled: true, changed: false };
    }
    if (caretStart <= bddRange.start || (!bddText && caretStart <= r.close + 1)) {
      if (bddText) return { handled: false, changed: false };
      const nextValue = input.value.slice(0, r.start) + input.value.slice(r.close + 1);
      input.value = nextValue;
      input.focus();
      input.setSelectionRange(r.start, r.start);
      return { handled: true, changed: true };
    }
    return { handled: false, changed: false };
  }

  const opRange = trimSlotRange(input.value, delims.firstLt + 1, delims.lastGtBeforeFirstComma);
  const leftRange = trimSlotRange(input.value, delims.firstComma + 1, delims.secondComma);
  const rightRange = trimSlotRange(input.value, delims.secondComma + 1, delims.close);
  const opText = input.value.slice(opRange.start, opRange.end);
  const leftText = input.value.slice(leftRange.start, leftRange.end);
  const rightText = input.value.slice(rightRange.start, rightRange.end);
  const inFrame = caretStart >= delims.applyStart && caretStart <= delims.close + 1;
  if (!inFrame) return { handled: false, changed: false };
  if (caretStart === delims.applyStart && delims.applyStart > 0) return { handled: false, changed: false };

  if (caretStart <= opRange.start) {
    if (opText || leftText || rightText) return { handled: true, changed: false };
    const nextValue = input.value.slice(0, delims.applyStart) + input.value.slice(delims.close + 1);
    input.value = nextValue;
    input.focus();
    input.setSelectionRange(delims.applyStart, delims.applyStart);
    return { handled: true, changed: true };
  }

  if (caretStart <= leftRange.start) {
    selectApplySlot(input, delims.firstLt + 1, delims.lastGtBeforeFirstComma);
    return { handled: true, changed: false };
  }

  if (caretStart <= rightRange.start || (!rightText && caretStart <= delims.close + 1)) {
    // At the right-slot boundary, Backspace should navigate to left slot
    // instead of deleting template scaffold chars (comma/space).
    selectApplySlot(input, delims.firstComma + 1, delims.secondComma);
    return { handled: true, changed: false };
  }

  return { handled: false, changed: false };
}

function findDerivedAliasSpanAt(value, pos) {
  const text = String(value || "");
  const index = Number(pos);
  if (!Number.isFinite(index) || index < 0 || index >= text.length) return null;
  const re = /[A-Za-z_][A-Za-z0-9_]*(?:\[[01]\/[A-Za-z_][A-Za-z0-9_]*\])+/g;
  let m = re.exec(text);
  while (m) {
    const start = m.index;
    const end = start + m[0].length;
    if (index >= start && index < end) return { start, end };
    m = re.exec(text);
  }
  return null;
}

export function handleProtectedDerivedNameDelete(input, key = "Backspace") {
  if (!input || !document.contains(input)) return { handled: false, changed: false };
  if (key !== "Backspace" && key !== "Delete") return { handled: false, changed: false };

  const start = input.selectionStart ?? 0;
  const end = input.selectionEnd ?? start;
  if (start !== end) return { handled: false, changed: false };

  const probe = key === "Backspace" ? start - 1 : start;
  const span = findDerivedAliasSpanAt(input.value, probe);
  if (!span) return { handled: false, changed: false };

  input.value = input.value.slice(0, span.start) + input.value.slice(span.end);
  input.focus();
  input.setSelectionRange(span.start, span.start);
  return { handled: true, changed: true };
}

export function sanitizeApplyOperatorInput(input) {
  if (!input || !document.contains(input)) return false;

  const delims = findApplySlotDelimiters(input.value);
  if (!delims) return false;

  const rawOp = input.value.slice(delims.firstLt + 1, delims.lastGtBeforeFirstComma);
  const sanitized = sanitizeApplyOperatorDraft(rawOp);
  if (sanitized === rawOp) return false;

  const nextValue =
    input.value.slice(0, delims.firstLt + 1) +
    sanitized +
    input.value.slice(delims.lastGtBeforeFirstComma);

  const caretStart = input.selectionStart ?? 0;
  const caretEnd = input.selectionEnd ?? caretStart;
  const delta = sanitized.length - rawOp.length;

  input.value = nextValue;

  let nextStart = caretStart;
  let nextEnd = caretEnd;
  if (caretStart <= delims.lastGtBeforeFirstComma) {
    nextStart = Math.min(delims.firstLt + 1 + sanitized.length, delims.firstLt + 1 + sanitized.length);
    nextEnd = nextStart;
  } else {
    nextStart = Math.max(delims.firstLt + 1, caretStart + delta);
    nextEnd = Math.max(delims.firstLt + 1, caretEnd + delta);
  }

  input.setSelectionRange(nextStart, nextEnd);
  return true;
}

function slotMetaForApply(input, delims) {
  const opRange = trimSlotRange(input.value, delims.firstLt + 1, delims.lastGtBeforeFirstComma);
  const leftRange = trimSlotRange(input.value, delims.firstComma + 1, delims.secondComma);
  const rightRange = trimSlotRange(input.value, delims.secondComma + 1, delims.close);
  return [
    {
      key: "op",
      priority: 0,
      rawStart: delims.firstLt + 1,
      rawEnd: delims.lastGtBeforeFirstComma,
      text: input.value.slice(opRange.start, opRange.end)
    },
    {
      key: "left",
      priority: 1,
      rawStart: delims.firstComma + 1,
      rawEnd: delims.secondComma,
      text: input.value.slice(leftRange.start, leftRange.end)
    },
    {
      key: "right",
      priority: 2,
      rawStart: delims.secondComma + 1,
      rawEnd: delims.close,
      text: input.value.slice(rightRange.start, rightRange.end)
    }
  ];
}

function slotMetaForRestrict(input, delims) {
  const bitRange = trimSlotRange(input.value, delims.start + "restrict(".length, delims.firstComma);
  const atomRange = trimSlotRange(input.value, delims.firstComma + 1, delims.secondComma);
  const bddRange = trimSlotRange(input.value, delims.secondComma + 1, delims.close);
  return [
    {
      key: "bdd",
      priority: 0,
      rawStart: delims.secondComma + 1,
      rawEnd: delims.close,
      text: input.value.slice(bddRange.start, bddRange.end)
    },
    {
      key: "atom",
      priority: 1,
      rawStart: delims.firstComma + 1,
      rawEnd: delims.secondComma,
      text: input.value.slice(atomRange.start, atomRange.end)
    },
    {
      key: "bit",
      priority: 2,
      rawStart: delims.start + "restrict(".length,
      rawEnd: delims.firstComma,
      text: input.value.slice(bitRange.start, bitRange.end)
    }
  ];
}

function locateSlotIndex(slots, selectionStart, selectionEnd) {
  const s = selectionStart ?? 0;
  const e = selectionEnd ?? s;
  for (let i = 0; i < slots.length; i++) {
    const slot = slots[i];
    const a = Math.max(0, slot.rawStart);
    const b = Math.max(a, slot.rawEnd);
    if (s >= a && s <= b && e >= a && e <= b) return i;
  }
  return -1;
}

function firstEmptySlotByPriority(slots) {
  return slots
    .filter((slot) => !String(slot.text || "").trim())
    .sort((a, b) => a.priority - b.priority)[0] ?? null;
}

function selectSlotRaw(input, slot) {
  if (!slot) return false;
  selectApplySlot(input, slot.rawStart, slot.rawEnd);
  return true;
}

export function maybeAdvanceApplyInput(input) {
  if (!input || !document.contains(input)) return false;

  const completeAndBlur = () => {
    const slot = parseDefinitionSlot(input.value);
    const body = slot ? slot.rhs : String(input.value || "").trim();
    if (!parseApplyCall(body) && !parseRestrictCall(body)) return false;
    queueMicrotask(() => {
      if (!document.contains(input)) return;
      if (document.activeElement === input) input.blur();
    });
    return true;
  };

  const caretStart = input.selectionStart ?? 0;
  const caretEnd = input.selectionEnd ?? caretStart;
  const applyDelims = findApplySlotDelimiters(input.value);
  if (applyDelims) {
    if (caretStart !== caretEnd) return false;
    const slots = slotMetaForApply(input, applyDelims);
    const currentIdx = locateSlotIndex(slots, caretStart, caretEnd);
    if (currentIdx < 0) return false;

    const opCanonical = canonicalApplyOperator(slots[0].text);
    slots[0].text = opCanonical ?? slots[0].text;
    const targetEmpty = firstEmptySlotByPriority(slots);
    if (!targetEmpty) return completeAndBlur();

    const currentSlot = slots[currentIdx];
    if (String(currentSlot.text || "").trim()) {
      return selectSlotRaw(input, targetEmpty);
    }
    if (targetEmpty.key !== currentSlot.key && targetEmpty.priority < currentSlot.priority) {
      return selectSlotRaw(input, targetEmpty);
    }
    return false;
  }

  const restrictDelims = findRestrictSlotDelimiters(input.value);
  if (!restrictDelims) return false;
  if (caretStart !== caretEnd) return false;

  const slots = slotMetaForRestrict(input, restrictDelims);
  const currentIdx = locateSlotIndex(slots, caretStart, caretEnd);
  if (currentIdx < 0) return false;
  const targetEmpty = firstEmptySlotByPriority(slots);
  if (!targetEmpty) return completeAndBlur();

  const currentSlot = slots[currentIdx];
  if (String(currentSlot.text || "").trim()) {
    return selectSlotRaw(input, targetEmpty);
  }
  if (targetEmpty.key !== currentSlot.key && targetEmpty.priority < currentSlot.priority) {
    return selectSlotRaw(input, targetEmpty);
  }
  return completeAndBlur();
}

function collectNamedExpressions(state, currentIdx) {
  const out = [];
  for (let i = 0; i < (state?.expressions?.length ?? 0); i++) {
    if (i === currentIdx) continue;
    const slot = parseDefinitionSlot(state.expressions[i]?.text || "");
    if (!slot?.name || !slot?.rhs) continue;
    if (!isDerivedExprName(slot.name)) continue;
    out.push({ name: slot.name, rhs: slot.rhs });
  }
  return out;
}

function collectDefinitionBodies(state, currentIdx, currentText = null) {
  const defs = new Map();
  for (let i = 0; i < (state?.expressions?.length ?? 0); i++) {
    const raw = i === currentIdx ? currentText : state.expressions[i]?.text;
    if (i === currentIdx && currentText == null) continue;
    const slot = parseDefinitionSlot(raw || "");
    if (!slot?.name || !slot?.rhs) continue;
    if (!isDerivedExprName(slot.name)) continue;
    defs.set(slot.name, slot.rhs);
  }
  return defs;
}

function applyRestrictToExprText(exprText, atomName, bit) {
  const src = String(exprText || "");
  const atom = String(atomName || "");
  if (!src.trim() || !atom) return src;
  const replacement = Number(bit) === 1 ? "true" : "false";
  return src.replace(/[A-Za-z_][A-Za-z0-9_]*/g, (m) => (m === atom ? replacement : m));
}

function expandNamedExprForVars(name, defs, visiting = new Set()) {
  const key = String(name || "").trim();
  if (!key || !defs.has(key)) return null;
  if (visiting.has(key)) return null;
  const raw = String(defs.get(key) || "").trim();
  if (!raw) return null;

  const next = new Set(visiting);
  next.add(key);

  const parsedApply = parseApplyCall(raw);
  if (parsedApply) {
    const left = expandNamedExprForVars(parsedApply.leftName, defs, next);
    const right = expandNamedExprForVars(parsedApply.rightName, defs, next);
    if (!left || !right) return null;
    return `((${left}) ${parsedApply.op} (${right}))`;
  }

  const parsedRestrict = parseRestrictCall(raw);
  if (parsedRestrict) {
    const base = expandNamedExprForVars(parsedRestrict.bddName, defs, next);
    if (!base) return null;
    return applyRestrictToExprText(base, parsedRestrict.atomName, parsedRestrict.bit);
  }

  return raw;
}

function inferExprVarsForLimit(rawBody, defs) {
  const body = String(rawBody || "").trim();
  if (!body) return [];

  let expanded = body;
  const parsedApply = parseApplyCall(body);
  if (parsedApply) {
    const left = expandNamedExprForVars(parsedApply.leftName, defs);
    const right = expandNamedExprForVars(parsedApply.rightName, defs);
    if (left && right) expanded = `((${left}) ${parsedApply.op} (${right}))`;
  } else {
    const parsedRestrict = parseRestrictCall(body);
    if (parsedRestrict) {
      const base = expandNamedExprForVars(parsedRestrict.bddName, defs);
      if (base) expanded = applyRestrictToExprText(base, parsedRestrict.atomName, parsedRestrict.bit);
    }
  }

  const reserved = new Set(["apply", "restrict", "true", "false"]);
  return inferVars(expanded).filter((v) => !reserved.has(String(v || "").toLowerCase()) && !defs.has(v));
}

function collectVarsFromNamedExpr(state, currentIdx, name) {
  const defs = collectDefinitionBodies(state, currentIdx);
  const expanded = expandNamedExprForVars(name, defs);
  const source = expanded || defs.get(name) || "";
  const rawVars = inferVars(source);
  const reserved = new Set(["apply", "restrict", "true", "false"]);
  return rawVars.filter((v) => !reserved.has(String(v || "").toLowerCase()) && !defs.has(v));
}

function isConstantNamedExpr(state, currentIdx, name) {
  const defs = collectDefinitionBodies(state, currentIdx);
  const expanded = expandNamedExprForVars(name, defs);
  const source = String(expanded || defs.get(name) || "").trim();
  if (!source) return false;
  const compact = source.replace(/\s+/g, "").toLowerCase();
  if (compact === "true" || compact === "false" || compact === "1" || compact === "0") return true;
  return inferVars(source).length === 0;
}

function collectDefinitionNames(state, currentIdx) {
  const out = new Set();
  for (let i = 0; i < (state?.expressions?.length ?? 0); i++) {
    if (i === currentIdx) continue;
    const slot = parseDefinitionSlot(state.expressions[i]?.text || "");
    if (!slot?.name || !isDerivedExprName(slot.name)) continue;
    out.add(slot.name);
  }
  return out;
}

function currentSlotHint(input) {
  if (!input) return { type: "none" };
  const caret = input.selectionStart ?? 0;
  const inRawRange = (rawStart, rawEnd) => caret >= rawStart && caret <= rawEnd;
  const a = findApplySlotDelimiters(input.value);
  if (a) {
    const op = trimSlotRange(input.value, a.firstLt + 1, a.lastGtBeforeFirstComma);
    const left = trimSlotRange(input.value, a.firstComma + 1, a.secondComma);
    const right = trimSlotRange(input.value, a.secondComma + 1, a.close);
    if ((caret >= op.start && caret <= a.lastGtBeforeFirstComma) || inRawRange(a.firstLt + 1, a.lastGtBeforeFirstComma)) {
      return { type: "apply-op", delims: a, range: op };
    }
    if ((caret >= left.start && caret <= a.secondComma) || inRawRange(a.firstComma + 1, a.secondComma)) {
      return { type: "apply-left", delims: a, range: left };
    }
    if ((caret >= right.start && caret <= a.close + 1) || inRawRange(a.secondComma + 1, a.close)) {
      return { type: "apply-right", delims: a, range: right };
    }
    return { type: "none" };
  }
  const r = findRestrictSlotDelimiters(input.value);
  if (!r) return { type: "none" };
  const bit = trimSlotRange(input.value, r.start + "restrict(".length, r.firstComma);
  const atom = trimSlotRange(input.value, r.firstComma + 1, r.secondComma);
  const bdd = trimSlotRange(input.value, r.secondComma + 1, r.close);
  if ((caret >= bit.start && caret <= r.firstComma) || inRawRange(r.start + "restrict(".length, r.firstComma)) {
    return { type: "restrict-bit", delims: r, range: bit };
  }
  if ((caret >= atom.start && caret <= r.secondComma) || inRawRange(r.firstComma + 1, r.secondComma)) {
    return { type: "restrict-atom", delims: r, range: atom };
  }
  if ((caret >= bdd.start && caret <= r.close + 1) || inRawRange(r.secondComma + 1, r.close)) {
    return { type: "restrict-bdd", delims: r, range: bdd };
  }
  return { type: "none" };
}

function buildRestrictAliasAutoDefinition(raw) {
  const t = String(raw || "").trim();
  const m = t.match(/^([A-Za-z_][A-Za-z0-9_]*)(\[[01]\/[A-Za-z_][A-Za-z0-9_]*\])+$/);
  if (!m) return null;
  const base = m[1];
  const suffixes = [...t.matchAll(/\[([01])\/([A-Za-z_][A-Za-z0-9_]*)\]/g)];
  if (!suffixes.length) return null;
  let rhs = base;
  for (const suffix of suffixes) {
    const bit = suffix[1];
    const atom = suffix[2];
    rhs = `restrict(${bit}, ${atom}, ${rhs})`;
  }
  return `${t} = ${rhs}`;
}

function getTemplateSlotMeta(input) {
  if (!input) return null;
  const apply = findApplySlotDelimiters(input.value);
  if (apply) {
    const opTrim = trimSlotRange(input.value, apply.firstLt + 1, apply.lastGtBeforeFirstComma);
    const leftTrim = trimSlotRange(input.value, apply.firstComma + 1, apply.secondComma);
    const rightTrim = trimSlotRange(input.value, apply.secondComma + 1, apply.close);
    const op = {
      start: opTrim.start,
      end: opTrim.end,
      rawStart: apply.firstLt + 1,
      rawEnd: apply.lastGtBeforeFirstComma
    };
    const left = {
      start: leftTrim.start,
      end: leftTrim.end,
      rawStart: apply.firstComma + 1,
      rawEnd: apply.secondComma
    };
    const right = {
      start: rightTrim.start,
      end: rightTrim.end,
      rawStart: apply.secondComma + 1,
      rawEnd: apply.close
    };
    return {
      frameStart: apply.applyStart,
      frameEnd: apply.close + 1,
      anchors: [apply.applyStart, op.start, left.start, right.start],
      ranges: [op, left, right]
    };
  }
  const restrict = findRestrictSlotDelimiters(input.value);
  if (!restrict) return null;
  const bitTrim = trimSlotRange(input.value, restrict.start + "restrict(".length, restrict.firstComma);
  const atomTrim = trimSlotRange(input.value, restrict.firstComma + 1, restrict.secondComma);
  const bddTrim = trimSlotRange(input.value, restrict.secondComma + 1, restrict.close);
  const bit = {
    start: bitTrim.start,
    end: bitTrim.end,
    rawStart: restrict.start + "restrict(".length,
    rawEnd: restrict.firstComma
  };
  const atom = {
    start: atomTrim.start,
    end: atomTrim.end,
    rawStart: restrict.firstComma + 1,
    rawEnd: restrict.secondComma
  };
  const bdd = {
    start: bddTrim.start,
    end: bddTrim.end,
    rawStart: restrict.secondComma + 1,
    rawEnd: restrict.close
  };
  return {
    frameStart: restrict.start,
    frameEnd: restrict.close + 1,
    anchors: [restrict.start, bit.start, atom.start, bdd.start],
    ranges: [bit, atom, bdd]
  };
}

function isCaretInAnyRange(caret, ranges) {
  return ranges.some(({ start, end }) => caret >= start && caret <= end);
}

function nearestAnchor(caret, anchors) {
  let target = anchors[0];
  let best = Math.abs(caret - target);
  for (let i = 1; i < anchors.length; i++) {
    const d = Math.abs(caret - anchors[i]);
    if (d < best) {
      best = d;
      target = anchors[i];
    }
  }
  return target;
}

function slotSelectionBounds(slot) {
  if (!slot) return { start: 0, end: 0 };
  if (slot.start < slot.end) return { start: slot.start, end: slot.end };
  if (Number.isFinite(slot.rawStart) && Number.isFinite(slot.rawEnd) && slot.rawStart < slot.rawEnd) {
    return { start: slot.rawStart, end: slot.rawEnd };
  }
  return { start: slot.start, end: slot.end };
}

function findSlotIndexBySelection(meta, start, end) {
  if (!meta?.ranges?.length) return -1;
  for (let i = 0; i < meta.ranges.length; i++) {
    const r = meta.ranges[i];
    if (start === r.start && end === r.end) return i;
    if (start === r.rawStart && end === r.rawEnd) return i;
    if (start === end && start >= r.start && start <= r.end) return i;
    if (start === end && Number.isFinite(r.rawStart) && Number.isFinite(r.rawEnd) && start >= r.rawStart && start <= r.rawEnd) return i;
  }
  return -1;
}

function selectTemplateSlot(meta, input, slotIndex) {
  const slot = meta?.ranges?.[slotIndex];
  if (!slot) return false;
  const rawStart = Number.isFinite(slot.rawStart) ? slot.rawStart : slot.start;
  const rawEnd = Number.isFinite(slot.rawEnd) ? slot.rawEnd : slot.end;
  selectApplySlot(input, rawStart, rawEnd);
  return true;
}

export function normalizeTemplateCaret(input) {
  if (!input || !document.contains(input)) return false;
  const meta = getTemplateSlotMeta(input);
  if (!meta) return false;
  const start = input.selectionStart ?? 0;
  const end = input.selectionEnd ?? start;
  if (start !== end) return false;
  if (start < meta.frameStart || start > meta.frameEnd) return false;
  if (start === meta.frameStart) return false;
  const inSlot = findSlotIndexBySelection(meta, start, end);
  if (inSlot >= 0) {
    return selectTemplateSlot(meta, input, inSlot);
  }
  const target = nearestAnchor(start, meta.anchors);
  const slotIndex = findSlotIndexBySelection(meta, target, target);
  if (slotIndex >= 0) return selectTemplateSlot(meta, input, slotIndex);
  input.setSelectionRange(target, target);
  return true;
}

export function handleProtectedTemplateArrow(input, key) {
  if (!input || !document.contains(input)) return false;
  if (key !== "ArrowLeft" && key !== "ArrowRight") return false;
  const meta = getTemplateSlotMeta(input);
  if (!meta) return false;
  const start = input.selectionStart ?? 0;
  const end = input.selectionEnd ?? start;
  const hasSelection = start !== end;
  if (start < meta.frameStart || start > meta.frameEnd) return false;

  const anchors = meta.anchors;
  let target = start;
  const slotIndex = findSlotIndexBySelection(meta, start, end);
  const inRange = slotIndex >= 0;

  if (inRange) {
    if (key === "ArrowRight") {
      const next = slotIndex + 1;
      if (next < meta.ranges.length) return selectTemplateSlot(meta, input, next);
      const lastAnchor = anchors[anchors.length - 1];
      if (start === lastAnchor && hasSelection) return false;
      input.setSelectionRange(lastAnchor, lastAnchor);
      return true;
    }
    if (key === "ArrowLeft") {
      const prev = slotIndex - 1;
      if (prev >= 0) return selectTemplateSlot(meta, input, prev);
      input.setSelectionRange(meta.frameStart, meta.frameStart);
      return true;
    }
  }

  if (!inRange) {
    if (key === "ArrowRight") {
      target = anchors.find((a) => a > start) ?? anchors[anchors.length - 1];
    } else {
      for (let i = anchors.length - 1; i >= 0; i--) {
        if (anchors[i] < start) {
          target = anchors[i];
          break;
        }
      }
      if (target === start) target = anchors[0];
    }
    if (target === start) target = nearestAnchor(start, anchors);
  }

  if (target === start) return false;
  const targetSlot = findSlotIndexBySelection(meta, target, target);
  if (targetSlot >= 0) return selectTemplateSlot(meta, input, targetSlot);
  input.setSelectionRange(target, target);
  return true;
}

function updateIndexBadgeOnly(ctx, idx) {
  const { state, dom } = ctx;
  const item = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"]`);
  const index = item?.querySelector(".expr-index");
  const line = state.expressions[idx];
  if (!index || !line) return;

  const slot = parseDefinitionSlot(line.text);
  const hasName = Boolean(slot?.name);
  const body = slot ? slot.rhs : (line.text || "").trim();
  index.textContent = hasName ? slot.name : topLevelOperatorLabel(body);

  index.style.background = "";
  index.style.borderColor = "";
  index.style.color = "";
}

function updateApplyUiOnly(ctx, idx) {
  const { dom, callbacks, state } = ctx;
  const item = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"]`);
  const box = item?.querySelector(".expr-apply-ui");
  const input = item?.querySelector(".expr-input");
  if (!box) return;

  box.innerHTML = "";
  const meta = callbacks.getLineUiState?.(idx);
  const isRowFocused = Boolean(input && state.activeIndex === idx && document.activeElement === input);
  const errorByReason = {
    invalid_name: "invalid name",
    invalid_boolean_expression: "invalid Boolean expression",
    duplicate_name: "duplicate name",
    self_reference_definition: "definition cannot reference itself"
  };
  let errorText = meta ? errorByReason[meta.reason] ?? "" : "";
  if ((meta?.reason === "apply_missing_defs" || meta?.reason === "restrict_missing_defs") && (meta?.missingNames?.length ?? 0) > 0) {
    const name = String(meta.missingNames[0] ?? "").trim();
    if (name) errorText = `${name} does not exist`;
  }
  if (meta?.reason === "restrict_atom_missing_in_expr") {
    const bddName = String(meta?.restrict?.bddName ?? "").trim();
    const atomName = String(meta?.restrict?.atomName ?? "").trim();
    if (bddName && atomName) {
      errorText = `${bddName} does not contain variable ${atomName}`;
    } else {
      errorText = "restrict variable is not present in the selected expression";
    }
  }
  const hasError = Boolean(errorText);
  const shouldShowError = hasError && !isRowFocused;
  const duplicateName = Boolean(meta && meta.reason === "duplicate_name");
  if (input) {
    input.classList.toggle("expr-input--invalid", shouldShowError);
    input.setAttribute("aria-invalid", shouldShowError ? "true" : "false");
    input.title = shouldShowError ? errorText : "";
  }
  if (item) {
    item.classList.toggle("expr-item--duplicate", duplicateName && !isRowFocused);
  }

  if (shouldShowError) {
    const warn = document.createElement("div");
    warn.className = "expr-invalid-hint";
    warn.textContent = errorText;
    box.style.display = "flex";
    box.appendChild(warn);
    return;
  }

  const slotHint = currentSlotHint(input);
  const isApplyOrRestrict = meta && (meta.kind === "apply" || meta.kind === "restrict");
  if (!isApplyOrRestrict || !isRowFocused) {
    box.style.display = "none";
    return;
  }

  const actions = document.createElement("div");
  actions.className = "expr-apply-actions";

  const quick = document.createElement("div");
  quick.className = "expr-apply-actions";

  function addQuickBtn(label, onClick, title = label) {
    const btn = document.createElement("button");
    btn.className = "btn";
    btn.type = "button";
    btn.textContent = label;
    btn.title = title;
    btn.addEventListener(
      "pointerdown",
      (ev) => {
        // Keep input focus stable so blur-driven rerender does not
        // destroy this button before its click handler runs.
        ev.preventDefault();
        ev.stopPropagation();
      },
      true
    );
    btn.addEventListener("click", (ev) => {
      ev.stopPropagation();
      onClick();
    });
    quick.appendChild(btn);
  }

  if (input && document.activeElement === input) {
    if (slotHint.type === "apply-op") {
      ["∧", "↑", "∨", "↓", "⊕", "→", "↔"].forEach((op) => {
        addQuickBtn(op, () => {
          replaceSlotText(input, slotHint.range.start, slotHint.range.end, op);
          const d = slotHint.delims;
          selectApplySlot(input, d.firstComma + 1, d.secondComma);
          onLineChanged(ctx, idx, input);
        }, `Use operator ${op}`);
      });
    }

    if (slotHint.type === "apply-left" || slotHint.type === "apply-right") {
      const named = collectNamedExpressions(state, idx);
      named.forEach(({ name }) => {
        addQuickBtn(name, () => {
          replaceSlotText(input, slotHint.range.start, slotHint.range.end, name);
          if (slotHint.type === "apply-left") {
            const d = slotHint.delims;
            selectApplySlot(input, d.secondComma + 1, d.close);
          }
          onLineChanged(ctx, idx, input);
        }, `Insert ${name}`);
      });
    }

    if (slotHint.type === "restrict-bdd") {
      const named = collectNamedExpressions(state, idx);
      named
        .filter(({ name }) => !isConstantNamedExpr(state, idx, name))
        .forEach(({ name }) => {
        addQuickBtn(name, () => {
          replaceSlotText(input, slotHint.range.start, slotHint.range.end, name);
          const d = slotHint.delims;
          selectApplySlot(input, d.firstComma + 1, d.secondComma);
          onLineChanged(ctx, idx, input);
        }, `Insert ${name}`);
        });
    }

    if (slotHint.type === "restrict-atom") {
      const d = slotHint.delims;
      const bddName = input.value.slice(d.secondComma + 1, d.close).trim();
      const vars = collectVarsFromNamedExpr(state, idx, bddName);
      vars.forEach((v) => {
        addQuickBtn(v, () => {
          replaceSlotText(input, slotHint.range.start, slotHint.range.end, v);
          selectApplySlot(input, d.start + "restrict(".length, d.firstComma);
          onLineChanged(ctx, idx, input);
        }, `Use variable ${v}`);
      });
    }

    if (slotHint.type === "restrict-bit") {
      ["0", "1"].forEach((bit) => {
        addQuickBtn(bit, () => {
          replaceSlotText(input, slotHint.range.start, slotHint.range.end, bit);
          onLineChanged(ctx, idx, input);
        }, `Set bit ${bit}`);
      });
    }
  }

  const fallbackCreatableNames = [];
  if (meta?.kind === "restrict" && input) {
    const d = findRestrictSlotDelimiters(input.value);
    if (d) {
      const token = input.value.slice(d.secondComma + 1, d.close).trim();
      if (isDerivedExprName(token)) {
        const existingDefs = collectDefinitionNames(state, idx);
        if (!existingDefs.has(token)) fallbackCreatableNames.push(token);
      }
    }
  }
  const creatableNames = (meta.creatableNames?.length ?? 0) > 0 ? meta.creatableNames : fallbackCreatableNames;

  if (creatableNames.length > 0) {
    const createBtn = document.createElement("button");
    createBtn.className = "btn";
    createBtn.type = "button";
    createBtn.textContent = `Create: ${creatableNames.map((name) => `${name} =`).join(", ")}`;
    createBtn.addEventListener(
      "pointerdown",
      (ev) => {
        // Same focus guard as quick buttons.
        ev.preventDefault();
        ev.stopPropagation();
      },
      true
    );
    createBtn.addEventListener("click", (ev) => {
      ev.stopPropagation();
      const inserts = creatableNames.map((name) => ({
        id: crypto.randomUUID(),
        text: `${name} = `,
        order: [],
        bddPane: defaultBddPane()
      }));
      if (inserts.length === 0) return;
      const targetName = creatableNames[0] ?? "";
      ctx.state.expressions.splice(idx + 1, 0, ...inserts);
      renderExprList(ctx);
      setActiveIndex(ctx, idx + 1);
      queueMicrotask(() => {
        // For restrict(..., , F) creation flow: after creating F, return to
        // original line and jump to restrict(, _, F).
        if (!targetName || !input || slotHint.type !== "restrict-bdd") return;
        const origin = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"] .expr-input`);
        if (!origin) return;
        const d = findRestrictSlotDelimiters(origin.value);
        if (!d) return;
        const bdd = origin.value.slice(d.secondComma + 1, d.close).trim();
        if (bdd && bdd !== targetName) return;
        replaceSlotText(origin, d.secondComma + 1, d.close, targetName);
        selectApplySlot(origin, d.firstComma + 1, d.secondComma);
        onLineChanged(ctx, idx, origin);
      });
    });
    actions.appendChild(createBtn);
  }

  const isPlaying = Boolean(meta.isApplyTracePlaying);
  if (meta.ok && isPlaying) {
      const enabledKinds = Array.isArray(meta.applyReduceKinds) ? meta.applyReduceKinds : null;
      const reduceKinds = [
        { kind: "terminals", label: "T", title: "Reduce terminals in apply result" },
        { kind: "redundant", label: "R", title: "Reduce redundant test in apply result" },
        { kind: "merge", label: "NT", title: "Reduce non-terminals in apply result" }
      ].filter(({ kind }) => !enabledKinds || enabledKinds.includes(kind));
    reduceKinds.forEach(({ kind, label, title }) => {
      const btn = document.createElement("button");
      btn.className = "expr-reduce-btn";
      btn.type = "button";
      btn.textContent = label;
      btn.title = title;
      btn.addEventListener(
        "pointerdown",
        (ev) => {
          ev.preventDefault();
          ev.stopPropagation();
        },
        true
      );
      btn.addEventListener("click", async (ev) => {
        ev.stopPropagation();
        await callbacks.onApplyReduce?.(idx, kind);
      });
      actions.appendChild(btn);
    });
  }

  if (quick.childElementCount > 0) box.appendChild(quick);

  if (actions.childElementCount === 0 && quick.childElementCount === 0) {
    box.style.display = "none";
    return;
  }

  box.style.display = "flex";
  if (actions.childElementCount > 0) box.appendChild(actions);
}

export function setActiveIndex(ctx, idx, { placeCursorAtEnd = true, focusInput = true } = {}) {
  const { state, dom, callbacks } = ctx;
  if (idx < 0 || idx >= state.expressions.length) return;
  if (state.isReducing) return;
  const from = state.activeIndex;
  if (from !== idx) {
    callbacks.onExpressionSwitchPersist?.(from);
    state.activeIndex = idx;
    void callbacks.onExpressionSwitchRestore?.(idx);
  } else {
    state.activeIndex = idx;
  }
  updateActiveClass(state, dom);
  updateSelectedInfo(state, dom);
  callbacks.onActiveIndexChanged?.(idx, from);
  if (focusInput) focusIndex(state, dom, idx, placeCursorAtEnd);
}

export function updateOrderBarOnly(ctx, idx) {
  const { dom } = ctx;
  const item = dom.exprListEl.querySelector(`.expr-item[data-index="${idx}"]`);
  if (!item) return;

  const bar = item.querySelector(".order-bar");
  if (!bar) return;
  bar.innerHTML = "";
  bar.style.display = "none";
}

export function onLineChanged(ctx, idx, inputEl, { runNetwork = true } = {}) {
  const { state, dom, callbacks } = ctx;
  const e = state.expressions[idx];
  const previousText = String(e.text ?? "");
  let rawText = inputEl.value ?? "";
  let caret = inputEl.selectionStart ?? rawText.length;
  let shouldShowLimitDialog = false;

  const autoAliasDef = buildRestrictAliasAutoDefinition(rawText);
  if (autoAliasDef && !rawText.includes("=")) {
    rawText = autoAliasDef;
    caret = rawText.length;
    inputEl.value = rawText;
    inputEl.setSelectionRange(caret, caret);
  }

  const slotDraft = parseDefinitionSlot(rawText);
  const bodyDraft = slotDraft
    ? rawText.slice(rawText.indexOf("=") + 1)
    : rawText;
  const trimmedBody = bodyDraft.trim();
  const isApplyLike = /^apply\s*\(/i.test(trimmedBody);
  const isRestrictLike = /^restrict\s*\(/i.test(trimmedBody);

  if (trimmedBody && !isApplyLike && !isRestrictLike) {
    const bodyStart = slotDraft ? rawText.indexOf("=") + 1 : 0;
    let bodyText = bodyDraft;
    let bodyCaret = Math.max(0, caret - bodyStart);
    let hitLimit = false;
    while (true) {
      const limited = trimVariablesToLimit(bodyText, bodyCaret, MAX_BOOLEAN_VARS);
      if (!limited.exceeded) break;
      hitLimit = true;
      bodyText = limited.text;
      bodyCaret = limited.caret;
    }

    if (hitLimit) {
      rawText = rawText.slice(0, bodyStart) + bodyText;
      caret = bodyStart + bodyCaret;
      inputEl.value = rawText;
      inputEl.setSelectionRange(caret, caret);
      shouldShowLimitDialog = true;
    }
  }

  const nextSlot = parseDefinitionSlot(rawText);
  const nextBody = nextSlot ? nextSlot.rhs : String(rawText || "").trim();
  if (nextBody) {
    const defs = collectDefinitionBodies(state, idx, rawText);
    const varsInExpr = inferExprVarsForLimit(nextBody, defs);
    if (varsInExpr.length > MAX_BOOLEAN_VARS) {
      rawText = previousText;
      const fallbackCaret = Math.max(0, Math.min(rawText.length, caret - 1));
      caret = fallbackCaret;
      inputEl.value = rawText;
      inputEl.setSelectionRange(caret, caret);
      shouldShowLimitDialog = true;
    }
  }

  e.text = rawText;
  syncApplyCodeStyle(inputEl);

  const slot = parseDefinitionSlot(e.text);
  const body = slot ? slot.rhs : (e.text || "").trim();
  const applyLike = /^apply\s*\(/i.test(body);
  const restrictLike = /^restrict\s*\(/i.test(body);
  if (!body || applyLike || restrictLike || parseApplyCall(body) || parseRestrictCall(body)) e.order = [];
  else e.order = syncOrder(body, e.order);

  if (state.activeIndex !== idx) {
    setActiveIndex(ctx, idx);
  } else {
    updateActiveClass(state, dom);
    updateSelectedInfo(state, dom);
  }
  updateIndexBadgeOnly(ctx, idx);
  updateOrderBarOnly(ctx, idx);
  updateApplyUiOnly(ctx, idx);

  if (shouldShowLimitDialog) {
    openVarLimitDialog(inputEl, caret);
  }

  if (runNetwork) callbacks.onExprChanged?.();
}

export function clearLineAt(ctx, idx) {
  deleteExpressionRow(ctx, idx);
}

export function addLine(ctx, text = "") {
  const { state } = ctx;
  const order = syncOrder(text, []);
  state.expressions.push({ id: crypto.randomUUID(), text, order, bddPane: defaultBddPane() });
  renderExprList(ctx);
  setActiveIndex(ctx, state.expressions.length - 1);
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

export function refreshExprUiOnly(ctx, idx) {
  updateIndexBadgeOnly(ctx, idx);
  updateOrderBarOnly(ctx, idx);
  updateApplyUiOnly(ctx, idx);
}

export function renderExprList(ctx) {
  const { state, dom, callbacks } = ctx;
  dom.exprListEl.innerHTML = "";

  state.expressions.forEach((expr, idx) => {
    const item = document.createElement("div");
    item.className = "expr-item" + (idx === state.activeIndex ? " active" : "");
    item.dataset.index = String(idx);

    const index = document.createElement("div");
    index.className = "expr-index";
    index.textContent = "?";
    index.draggable = true;
    index.title = "Drag to reorder";
    index.addEventListener("click", (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      setActiveIndex(ctx, idx, { focusInput: false });
    });
    index.addEventListener("dragstart", (ev) => {
      dragExprFromIdx = idx;
      item.classList.add("expr-item--dragging");
      if (ev.dataTransfer) {
        ev.dataTransfer.effectAllowed = "move";
        ev.dataTransfer.setData("text/plain", String(idx));
      }
    });
    index.addEventListener("dragend", () => {
      dragExprFromIdx = -1;
      item.classList.remove("expr-item--dragging");
      dom.exprListEl
        .querySelectorAll(".expr-item--drop-target")
        .forEach((el) => el.classList.remove("expr-item--drop-target"));
    });

    const mid = document.createElement("div");
    mid.className = "expr-mid";

    const input = document.createElement("input");
    input.className = "expr-input";
    input.id = `expr-${idx}`;
    input.name = `expr-${idx}`;
    input.value = expr.text || "";
    input.placeholder = "";
    applyMobileKeyboardMode(state, input);
    syncApplyCodeStyle(input);

    input.addEventListener("focus", () => {
      state.focusedInput = input;
      if (state.activeIndex !== idx) {
        setActiveIndex(ctx, idx, { placeCursorAtEnd: false });
      } else {
        updateApplyUiOnly(ctx, idx);
      }
    });

    input.addEventListener("blur", () => {
      if (state.focusedInput === input) state.focusedInput = null;
      void callbacks.onExprBlur?.(idx);
    });

    input.addEventListener("input", () => {
      sanitizeApplyOperatorInput(input);
      normalizeTemplateSpacing(input);
      onLineChanged(ctx, idx, input);
      maybeAdvanceApplyInput(input);
    });

    input.addEventListener("click", () => {
      normalizeTemplateCaret(input);
      updateApplyUiOnly(ctx, idx);
    });
    input.addEventListener("keyup", () => updateApplyUiOnly(ctx, idx));
    input.addEventListener("select", () => updateApplyUiOnly(ctx, idx));

    input.addEventListener("keydown", (ev) => {
      if (ev.key === "ArrowLeft" || ev.key === "ArrowRight") {
        const moved = handleProtectedTemplateArrow(input, ev.key);
        if (!moved) return;
        ev.preventDefault();
        updateApplyUiOnly(ctx, idx);
        return;
      }
      if (ev.key !== "Backspace" && ev.key !== "Delete") return;
      const nameDeleteResult = handleProtectedDerivedNameDelete(input, ev.key);
      if (nameDeleteResult.handled) {
        ev.preventDefault();
        if (nameDeleteResult.changed) onLineChanged(ctx, idx, input);
        else updateApplyUiOnly(ctx, idx);
        return;
      }
      if (ev.key !== "Backspace") return;
      const protectedResult = handleProtectedApplyBackspace(input);
      if (!protectedResult.handled) return;
      ev.preventDefault();
      if (protectedResult.changed) onLineChanged(ctx, idx, input);
      else updateApplyUiOnly(ctx, idx);
    });

    mid.appendChild(input);

    const orderBar = document.createElement("div");
    orderBar.className = "order-bar";
    mid.appendChild(orderBar);

    const applyUi = document.createElement("div");
    applyUi.className = "expr-apply-ui";
    mid.appendChild(applyUi);

    const del = document.createElement("button");
    del.className = "expr-del";
    del.textContent = "×";
    del.title = "Delete";
    del.addEventListener(
      "pointerdown",
      (ev) => {
        // Keep this click reliable even if blur triggers rerender.
        ev.preventDefault();
        ev.stopPropagation();
      },
      true
    );
    del.addEventListener("click", (ev) => {
      ev.stopPropagation();
      clearLineAt(ctx, idx);
    });

    item.addEventListener("click", (ev) => {
      if (ev.target.closest(".expr-input")) return;
      setActiveIndex(ctx, idx);
    });
    item.addEventListener("dragover", (ev) => {
      if (dragExprFromIdx < 0) return;
      ev.preventDefault();
      ev.dataTransfer && (ev.dataTransfer.dropEffect = "move");
      item.classList.add("expr-item--drop-target");
    });
    item.addEventListener("dragleave", () => {
      item.classList.remove("expr-item--drop-target");
    });
    item.addEventListener("drop", (ev) => {
      if (dragExprFromIdx < 0) return;
      ev.preventDefault();
      ev.stopPropagation();
      item.classList.remove("expr-item--drop-target");
      const toIdx = idx;
      const fromIdx = dragExprFromIdx;
      dragExprFromIdx = -1;
      moveExpressionRow(ctx, fromIdx, toIdx);
    });
    item.addEventListener("dblclick", () => toggleSelectForApply(ctx, idx));

    item.appendChild(index);
    item.appendChild(mid);
    item.appendChild(del);

    dom.exprListEl.appendChild(item);

    refreshExprUiOnly(ctx, idx);
  });

  updateSelectedInfo(state, dom);
}
