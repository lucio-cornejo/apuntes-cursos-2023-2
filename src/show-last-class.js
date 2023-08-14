document.addEventListener(
  'DOMContentLoaded', function() {
    const lastClass = document.querySelector("#last-class");
    lastClass.onclick = function () {
      // Get selected course
      const coursePath = document
        .querySelector(".selected")
        .innerText
        .toLowerCase()
        .replace(/\s/g, "-");

      // Get last class
      const lastClassIndex = 
        globalThis.availableClasses[coursePath]
        .at(-1);

      // Select last class
      const classNumber = document.querySelector("#clase");
      classNumber.value = lastClassIndex;
      clase.dispatchEvent(new Event('input', { bubbles: true }));
    }
  }
);
