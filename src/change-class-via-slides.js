function clickH1ForNextClass() {
  const headers = [...document.querySelectorAll("h1")];

  // Only when showing at least two classes 
  if (headers.length < 2) return;

  // Get width of slides (constant for all)
  const slidesHalfWidth = headers[0]
    .getBoundingClientRect()
    .width / 2;

  headers.forEach((h, index) => {
    h.onclick = function(evt) {
      if (index === 0) {
        // Show second class
        headers.at(1).scrollIntoView();
        return;
      }
      if (index === headers.length - 1) {
        // Show penultimate class
        headers.at(-2).scrollIntoView();
        return;
      }

      if (evt.offsetX < slidesHalfWidth) {
        headers[index - 1].scrollIntoView();
      } else {
        headers[index + 1].scrollIntoView();
      }
    }
  })  
}