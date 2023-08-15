// Load JSON of available classes
fetch("./available-classes.json")
  .then(response => { return response.json() })
  .then(jsondata => { globalThis.availableClasses = jsondata });


function addCourseSelection() {
  // Add interactive selection of courses
  const courses = [...document.querySelectorAll("#cursos li")];
  courses.forEach(function(course) {
    // Trigger course selection via click or space keydown
    const events = ["click", "keydown"];
    events.forEach(event => {
      course.addEventListener(event, function(evt) {
        if (event === 'keydown') {
          if (evt.repeat || (evt.key !== ' ')) return;
          evt.preventDefault();
        }
        
        // Undo previous course selection
        courses.forEach(e => e.classList.remove("selected"));
        // Update course selection
        this.classList.add("selected");

        // Trigger input event for slides update
        classNumber.dispatchEvent(new Event('input', { bubbles: true }));
      })
    })
  });

  // Choose class number to show slides
  const classNumber = document.querySelector("#clase");
  classNumber.addEventListener(
    "input",
    function () {
      const classNumberValue = parseInt(classNumber.value);

      // Do nothing when no class number is selected
      if (isNaN(classNumberValue)) return;

      const classNumberText = numberToText(classNumberValue);

      const coursePath = document
        .querySelector(".selected")
        .dataset.path;

      // Update text where class number is displayed
      const classText = classNumber
        .previousElementSibling
        .querySelector("p:not(.control)");
      
      classText.innerText = classNumberText;
      
      // Tell user if course class is not available
      if (
        !globalThis
          .availableClasses[coursePath]
          .includes(classNumberText)
      ) {
        classText.classList.add("missing-class");
        return;
      }
      
      classText.classList.remove("missing-class");
      showPDF(`courses/${coursePath}/apuntes/clase-${classNumberText}.pdf`);
    }
  )

  // Increase or decrease class number by one
  const [decrease, increase] = document.querySelectorAll(".control");
  increase.onclick = function() {
    classNumber.valueAsNumber += 1;
    classNumber.dispatchEvent(new Event('input', { bubbles: true }));
  }
  decrease.onclick = function() {
    classNumber.valueAsNumber -= 1;
    classNumber.dispatchEvent(new Event('input', { bubbles: true }));
  }
  // Trigger click via space key
  increase.addEventListener('keydown', function(evt) {
    if (evt.repeat || (evt.key !== ' ')) return;
    evt.preventDefault();
    this.click();
  });
  decrease.addEventListener('keydown', function(evt) {
    if (evt.repeat || (evt.key !== ' ')) return;
    evt.preventDefault();
    this.click();
  });

  // On course click, show respective class slides
  const showAllSlides = document.querySelector("#all-classes");
  showAllSlides.onclick = function () {
    const coursePath = document
      .querySelector(".selected")
      .dataset.path;
      
    showPDF(`courses/${coursePath}/merged.pdf`)
  }
}


function numberToText(num) {
  return num < 10 ? `0${num}` : `${num}`;
}


function showPDF(pdfPath) {
  const pdfContainer = document.querySelector("#pdf");
  pdfContainer.remove();

  // Create new embed element
  const newPDFContainer = document.createElement("embed");
  const pdfSettings = "#page=1&view=Fit"

  newPDFContainer.id = "pdf";
  newPDFContainer.classList.add("pdf-container");
  
  newPDFContainer.type = "application/pdf";

  // If user is using a mobile device, 
  // use Google Drive to visualize the pdf,
  // in order to avoid downloading the pdf file.
  const isMobile = navigator.userAgentData.mobile;
  const googleViewURL = "https://docs.google.com/gview?embedded=true&url=";
  const personalWebsiteURL = "https://2023-2-apuntes-cursos-pucp.netlify.app/";

  newPDFContainer.src = !isMobile 
    ? pdfPath + pdfSettings
    : googleViewURL + personalWebsiteURL + pdfPath;

  document.body.appendChild(newPDFContainer);
}
