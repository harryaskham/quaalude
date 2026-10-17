// Filter the reference: show declarations whose text contains the query; open modules with matches.
(() => {
  const input = document.getElementById("filter"), count = document.getElementById("filter-count");
  const mods = [...document.querySelectorAll("#ref details.mod")];
  const run = () => {
    const q = input.value.trim().toLowerCase();
    let shown = 0;
    for (const m of mods) {
      let hits = 0;
      for (const e of m.querySelectorAll(".e")) {
        const ok = !q || e.dataset.k.includes(q);
        e.hidden = !ok;
        hits += ok;
      }
      m.hidden = q && !hits;
      m.open = !!q && hits > 0 && hits <= 40;
      shown += hits;
    }
    count.textContent = q ? `${shown} declaration${shown === 1 ? "" : "s"} match` : "";
  };
  input.addEventListener("input", run);
})();
