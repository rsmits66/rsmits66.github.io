/* ============================================================
   Robert G. Smits — site scripts
   Handles: mobile nav toggle, tabset switching.
   (Same behavior as Georgia Smits's site script.)
   ============================================================ */

(function () {
  // ---- mobile nav toggle ----
  document.querySelectorAll('.nav-toggle').forEach(function (btn) {
    btn.addEventListener('click', function () {
      var container = btn.closest('.nav-container');
      if (!container) return;
      var open = container.classList.toggle('is-open');
      btn.setAttribute('aria-expanded', open ? 'true' : 'false');
    });
  });

  // ---- tabsets ----
  document.querySelectorAll('.tabset').forEach(function (set) {
    var buttons = set.querySelectorAll('.tabset-tabs button');
    var panels  = set.querySelectorAll('.tabset-panel');

    buttons.forEach(function (btn, i) {
      btn.addEventListener('click', function () {
        buttons.forEach(function (b) { b.classList.remove('active'); });
        panels.forEach(function (p)  { p.classList.remove('active'); });
        btn.classList.add('active');
        if (panels[i]) panels[i].classList.add('active');
      });
    });

    // ensure at least one tab is active on load
    if (buttons.length && !set.querySelector('.tabset-tabs button.active')) {
      buttons[0].classList.add('active');
      if (panels[0]) panels[0].classList.add('active');
    }
  });
})();
