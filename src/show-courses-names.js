// Load JSON of available classes
fetch("./courses.json")
  .then(response => { return response.json() })
  .then(jsondata => { 
    globalThis.courses = jsondata;
    addCoursesNames();
  });

function addCoursesNames() { 
  // Add courses as li elements in ul#cursos
  coursesContainer = document.querySelector("#cursos");
  inputsContainer = document.querySelector("#inputs");
  
  Object.values(globalThis.courses)
    .forEach(courseName => {
      item = document.createElement("li");
      item.textContent = courseName;
      item.tabIndex = 0;

      coursesContainer.insertBefore(item, inputsContainer);
    });

  // Select first course
  document.querySelector("#cursos li")
    .classList.add("selected");
}