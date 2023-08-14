// Load JSON of available classes
fetch("./available-classes.json")
  .then(response => { return response.json() })
  .then(jsondata => { globalThis.availableClasses = jsondata });

document.addEventListener(
  'DOMContentLoaded',
  function() {
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
          .innerText
          .toLowerCase()
          .replace(/\s/g, "-");
          
        // Update text where class number is displayed
        classNumber
          .previousElementSibling
          .querySelector("p:not(.control)")
          .innerText = classNumberText;
        
        removeAllChildren(globalThis["clases"]);

        // Tell user if course class is not available
        if (
          !globalThis
            .availableClasses[coursePath]
            .includes(classNumberText)
        ) {
          const message = document.createElement("h1");
          message.innerText = 'Selected class not found.';
          globalThis["clases"].appendChild(message);
          return;
        }
        showSlides(coursePath, classNumberText);
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
    const showAllSlides = document.querySelector("input[type='button']");
    showAllSlides.onclick = function () {
      // Clear class container
      const classesContainer = document.querySelector("#clases");
      while (classesContainer.firstChild) {
        classesContainer.removeChild(classesContainer.lastChild);
      }

      const coursePath = document
        .querySelector(".selected")
        .innerText
        .toLowerCase()
        .replace(/\s/g, "-");
        
      // To add the slides of all the course's classes,
      // let's assume that there are at most 30 classes.
      for (let classNum = 1; classNum<31; classNum++) {
        showSlides(coursePath, numberToText(classNum))
      }

      // Activate slides navigation via header slides
      clickH1ForNextClass();
    }
  }
)

function numberToText(num) {
  return num < 10 ? `0${num}` : `${num}`;
}

function removeAllChildren(element) {
  while (element.firstChild) {
    element.removeChild(element.lastChild);
  }
}

function showSlides(coursePath, classNumberText) {
  const classesContainer = document.querySelector("#clases");

  // Add title slide
  const titleSlide = document.createElement("h1");

  // If the course class is not available, display
  // some error message and end function execution
  if (
    !globalThis
      .availableClasses[coursePath]
      .includes(classNumberText)
  ) {
    return;
  }

  titleSlide.innerText = "CLASE " + classNumberText;
  classesContainer.appendChild(titleSlide);

  // To add all slides from the respective class,
  // let's assume that there are at most 20 slides.
  for (let slideNum = 1; slideNum<21; slideNum++) {
    const slideContainer = document.createElement("img");
    
    // Delete extra containers for the slides
    slideContainer.onerror = function() { this.remove() }

    slideContainer.src = 
      `./_apuntes/${coursePath}/${classNumberText}/Page ${slideNum}.webp`;
    
    classesContainer.appendChild(slideContainer);
  }
}
