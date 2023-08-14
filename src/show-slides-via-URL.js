// Read URL to show (or not) a specific course class
document.addEventListener(
  "DOMContentLoaded",
  function () {
    /*
      Expected URL pattern type:
      "..." + "index.html?" + coursePath + "&" + classNumberText
    */
    setTimeout(function () {
      let url = globalThis.location.href;
    
      // No course or class was referenced
      if (!url.includes("?")) return;
      const reference = url.split("?").at(-1).split("&");
      
      // Retrieve course name as in website
      const courseName = reference[0]
        .split("-")
        .map(text => {
          const possibleNumber = parseInt(text);
          if (isNaN(possibleNumber)) {
            // Capitalize first letter only
            return text[0].toUpperCase() + text.slice(1);
          }
          // Return number as text after removing
          // possible '0' in '01', for example.
          return String(possibleNumber);
        })
        .reduce((a, b) => a + " " + b);
      
      // Set selected course
      document.querySelectorAll("#cursos li").forEach(
        course => {
          if (course.innerText === courseName) {
            course.classList.add("selected");
          } else {
            course.classList.remove("selected");
          }
        }
      );

      // Set selected class
      document.querySelector(".controls :not(.control)")
        .innerText = reference[1];

      // Show referenced course class
      removeAllChildren(globalThis["clases"]);
      showSlides(reference[0], reference[1]);
    }, 1000);
  }
)
