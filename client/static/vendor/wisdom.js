/*  -- Wisdom -- Start of Snippet v1.0 (www.GetWisdom.io) -- */
!(function (W, I, S, D, O, M) {
  "use strict";
  if (((S.__WISDOM__INIT__OBJECT_REFERENCE = W), !(W in S))) {
    var f = (S[W] = function () {
      arguments.length && f.q.push({ args: arguments, ts: new Date() });
    });
    (f.ts = new Date()),
      (f.q = []),
      (f.flag = function () {
        return null;
      });
    var g = D.createElement("script");
    (g.src = "https://script.getwisdom.io/v" + (I | 0)),
      (g.type = "text/javascript"),
      (g.charset = "UTF-8"),
      (g.crossOrigin = "anonymous"),
      (g.async = !0);
    var h = D.getElementsByTagName("script")[0];
    h.parentNode.insertBefore(g, h);
  }
})("wisdom", 1, window, document);
