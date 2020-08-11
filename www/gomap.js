// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".ana-streq-gomap", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  Shiny.setInputValue("ana_streq_gotomappoint", {
    lat: lat,
    lng: long,
    nonce: Math.random()
  });
});
