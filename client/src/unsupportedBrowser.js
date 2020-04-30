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
    document.body.innerHTML +=
      "<div class='modal-overlay' id='unsupportedBrowser'><div class='modal'><div class='warning'><p class='title'>Unfortunately we only support Dark on desktop Chrome right now. Between browser different input models, differences in scripting and rendering performance, and differing web platform support, we don't have the capacity to support other browsers at the moment. We hope to support Firefox, Safari, and mobile use once we've really nailed the experience on Chrome. Thanks for understanding!</p></div></div></div>";
  }
});
