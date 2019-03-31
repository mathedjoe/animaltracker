shinyjs.removePolygon = function() {
      var event = document.createEvent("Event");
      event.initEvent("click", true, true);
      var trashButton = document.getElementsByClassName("leaflet-draw-edit-remove");
      !trashButton[0].dispatchEvent(event);
      var clearButton = document.querySelector('[title="Clear all layers"]');
      !clearButton.dispatchEvent(event);
}
