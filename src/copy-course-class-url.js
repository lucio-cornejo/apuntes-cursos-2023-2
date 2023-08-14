function copyTextArgument(text) {
  const textArea = document.createElement("textarea");
  textArea.value = text;

  globalThis["clases"].prepend(textArea);
  textArea.focus();
  textArea.select();

  document.execCommand('copy');
  textArea.remove();
}


function copyCourseClassURL() {
  // Get course class
  const coursePath = document
    .querySelector(".selected")
    .innerText
    .toLowerCase()
    .replace(/\s/g, "-");
  
  const classNumberText = document
    .querySelector(".controls :not(.control)")
    .innerText;
  
  // Get URL
  const url = globalThis.location.href
    // Remove possible parameters/arguments in URL
    .split("?").at(0)

  const courseClassURL = url + "?" + 
    coursePath + "&" + classNumberText;
  copyTextArgument(courseClassURL);
}