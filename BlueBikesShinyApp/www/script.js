$(document).on(
  "shiny:inputchanged",
  function(event) {
    if (event.name === 'bostonMap_marker_mouseout') {
      Shiny.onInputChange("bostonMap_marker_mouseover", null);
    }
  }
);
