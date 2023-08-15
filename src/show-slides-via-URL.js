/*
Read URL to show (or not) a specific course class

Expected URL pattern type:
"..." + "index.html?" + coursePath + "&" + classNumberText
*/

// Wait one second
await new Promise(r => setTimeout(r, 1000));

(function () {
  let url = globalThis.location.href;
  
  // No course or class was referenced
  if (!url.includes("?")) return;
  
  const reference = url.split("?").at(-1).split("&");
  
  const coursePath = reference[0];
  const courseClassPath = `./courses/${coursePath}/apuntes/clase-`;
  
  // Set selected course
  document.querySelectorAll("#cursos li").forEach(
    course => {
      if (course.dataset.path === coursePath) {
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
  showPDF(courseClassPath + reference[1] + ".pdf");
});
