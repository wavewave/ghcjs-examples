window.onload = function(){
  goog.debug.Console.autoInstall();
  $hs_loadPath = "../../bin/multiple-pages.jsexe/";

  // Must be called first
  $hs_init();
  $hs_consoleInitAndRunIO([$$ZCMain_main]);
}

