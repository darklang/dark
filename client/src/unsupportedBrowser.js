function unsupportedBrowser() {
  var isChrome =
    /Chrome/.test(navigator.userAgent) && /Google Inc/.test(navigator.vendor);
  var isMobile = /Android|BlackBerry|iPhone|iPad|iPod|Opera Mini|IEMobile/.test(
    navigator.userAgent,
  );
  var isDesktopApp = /DarkLang\/Editor/.test(navigator.userAgent);
  var isSupported = isDesktopApp || (isChrome && !isMobile);
  return !isSupported;
}
document.addEventListener("DOMContentLoaded", event => {
  if (unsupportedBrowser()) {
    let htmlString =
      "<div class='modal-overlay' id='unsupportedBrowser'><div class='modal'><div class='warning'><p class='title'>Dark is only fully supported on Desktop Chrome right now. We have a desktop client you can use if you prefer.</p><p class='title'>Unfortunately, as a small team, we don’t have the capacity to support other browsers at the moment. Once we’ve made the experience excellent on Chrome, we'll address cross-browser compatibility issues to support Firefox, Safari, and mobile devices.</p><p class='title'>Thanks for understanding ❤️</p></div></div></div>";
    var div = document.createElement("div");
    div.innerHTML = htmlString;
    document.body.append(div);
  }
});
