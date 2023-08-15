function showLastClass() {
  // Get selected course
  const coursePath = document
    .querySelector(".selected")
    .dataset.path;

  // Get last class
  const lastClassIndex = 
    globalThis.availableClasses[coursePath]
    .at(-1);

  // Select last class
  const classNumber = document.querySelector("#clase");
  classNumber.value = lastClassIndex;
  classNumber.dispatchEvent(new Event('input', { bubbles: true }));
}
