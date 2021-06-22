// All page modals, button images and close buttons
var modals = document.querySelectorAll('.modal');
var buttonImgs = document.querySelectorAll('.buttonImg');
var spans = document.getElementsByClassName("close");

// When the user clicks the button, open the modal
for (var i = 0; i < buttonImgs.length; i++) {
 buttonImgs[i].onclick = function(e) {
    e.preventDefault();
    modal = document.querySelector(e.target.getAttribute("href"));
    modal.style.display = "block";
 }
}


// When the user clicks on <span> (x), close the modal
for (var i = 0; i < spans.length; i++) {
 spans[i].onclick = function() {
    for (var index in modals) {
      if (typeof modals[index].style !== 'undefined') modals[index].style.display = "none";    
    }
 }
}

window.onclick = function(event) {
       if (event.target.class != "modal-content") {
          $("#modal").hide();
       }
    }