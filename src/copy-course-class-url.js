function copyTextArgument(text) {
  const textArea = document.createElement("textarea");
  textArea.value = text;

  document.body.prepend(textArea);
  textArea.focus();
  textArea.select();

  document.execCommand('copy');
  textArea.remove();
}


function copyCourseClassURL() {
  // Get course class
  const coursePath = document
    .querySelector(".selected")
    .dataset.path;
  
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