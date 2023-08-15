// Load JSON of available classes
fetch("./courses.json")
  .then(response => { return response.json() })
  .then(jsondata => { 
    globalThis.courses = jsondata;
    addCoursesNames();
    addCourseSelection();
  });

function addCoursesNames() { 
  // Add courses as li elements in ul#cursos
  coursesContainer = document.querySelector("#cursos");
  inputsContainer = document.querySelector("#inputs");
  
  Object.entries(globalThis.courses)
    .forEach(arr => {
      const coursePath = arr[0];
      const courseName = arr[1];
      const item = document.createElement("li");
      
      item.tabIndex = 0;
      item.textContent = courseName;
      item.dataset.path = coursePath;

      coursesContainer.insertBefore(item, inputsContainer);
    });

  // Select first course
  document.querySelector("#cursos li")
    .classList.add("selected");
}