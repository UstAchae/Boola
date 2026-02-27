export function setupKeyboard(ctx) {
  const { state, dom, cy, axis } = ctx;

  function setKeyboardCollapsed(collapsed) {
    if (!dom.keyboardRoot) return;
    dom.keyboardRoot.classList.toggle("is-collapsed", collapsed);
    if (dom.btnKbdToggle) dom.btnKbdToggle.setAttribute("aria-expanded", String(!collapsed));
  }

  dom.btnKbdToggle?.addEventListener("click", () => {
    const collapsed = dom.keyboardRoot.classList.contains("is-collapsed");
    setKeyboardCollapsed(!collapsed);
  });

  const keyboardEl = document.querySelector(".keyboard");
  if (!keyboardEl) return;

  function insertToFocused(str) {
    const input = state.focusedInput;
    if (!input || !document.contains(input)) return;

    input.focus();
    const start = input.selectionStart ?? input.value.length;
    const end = input.selectionEnd ?? input.value.length;
    input.value = input.value.slice(0, start) + str + input.value.slice(end);

    const pos = start + str.length;
    input.setSelectionRange(pos, pos);

    ctx.expr.onLineChanged(ctx, state.activeIndex, input);
  }

  function backspaceFocused() {
    const input = state.focusedInput;
    if (!input || !document.contains(input)) return;

    input.focus();
    const start = input.selectionStart ?? input.value.length;
    const end = input.selectionEnd ?? input.value.length;

    if (start !== end) {
      input.value = input.value.slice(0, start) + input.value.slice(end);
      input.setSelectionRange(start, start);
    } else if (start > 0) {
      input.value = input.value.slice(0, start - 1) + input.value.slice(start);
      input.setSelectionRange(start - 1, start - 1);
    }

    ctx.expr.onLineChanged(ctx, state.activeIndex, input);
  }

  function addAP() {
    const next = prompt("New AP name (e.g., a or x1):");
    if (!next) return;
    const clean = next.trim();
    if (!clean) return;
    if (state.apList.includes(clean)) return;
    state.apList.push(clean);

    const row = document.getElementById("rowAP");
    if (!row) return;
    const key = document.createElement("div");
    key.className = "key";
    key.dataset.insert = clean;
    key.textContent = clean;
    row.insertBefore(key, row.lastElementChild);
  }

  keyboardEl.addEventListener("click", (e) => {
    const key = e.target.closest(".key");
    if (!key) return;

    if (key.dataset.insert) {
      insertToFocused(key.dataset.insert);
      return;
    }

    const action = key.dataset.action;
    if (!action) return;

    if (action === "backspace") backspaceFocused();
    else if (action === "space") insertToFocused(" ");
    else if (action === "newLine") {
      ctx.expr.addLine(ctx, "");
      ctx.expr.focusActiveInputSoon(dom);
    } else if (action === "addAP") addAP();
  });

  keyboardEl.addEventListener(
    "pointerdown",
    (e) => {
      const key = e.target.closest(".key");
      if (!key) return;
      e.preventDefault();
    },
    true
  );

  document.addEventListener(
    "pointerdown",
    (e) => {
      const insideExpr = e.target.closest(".expr-item");
      const insideKeyboard = e.target.closest(".keyboard");
      const insideCy = e.target.closest("#cy");
      const insideRight = e.target.closest(".right");
      if (insideExpr || insideKeyboard || insideCy || insideRight) return;

      if (state.focusedInput && document.contains(state.focusedInput)) {
        state.focusedInput.blur();
      }
      state.focusedInput = null;
    },
    true
  );
}