let mybutton = document.getElementById("myBtn");

// When the user scrolls down 20px from the top of the document, show the button
window.onscroll = function() {scrollFunction()};

function scrollFunction() {
  if (document.documentElement.scrollTop > 5000) {
      mybutton.style.display = "block";
  } else {
      mybutton.style.display = "none";
  }
}

// When the user clicks on the button, scroll to the top of the document
function topFunction() {
  document.body.scrollTop = 0; // For Safari
  document.documentElement.scrollTop = 0; // For Chrome, Firefox, IE and Opera
}




let tocHead = document.querySelector(".toc-heading")
let toc = document.querySelector(".toc")


tocHead.addEventListener("click", (e) => {
    e.preventDefault();
    if ((toc.style.getPropertyValue("display")) === "block") {
        toc.style.display = "none";
    } else {
        toc.style.display = "block";
    }


})



// document.addEventListener('DOMContentLoaded', () => {
  const hamburgerMenu = document.getElementById('hamburger-menu');
  const navList = document.getElementById('nav-list');

  hamburgerMenu.addEventListener('click', () => {
    console.log("hello")
    navList.classList.toggle('open');
    hamburgerMenu.querySelector('.bar1').classList.toggle('change');
    hamburgerMenu.querySelector('.bar2').classList.toggle('change');
    hamburgerMenu.querySelector('.bar3').classList.toggle('change');
  });
// });
