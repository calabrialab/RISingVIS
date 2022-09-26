function slideToNext(currentId, nextId) {
  $(`#${nextId}`).css({
    opacity: 0,
  });
  $(`#${nextId}`).show();
  $("html, body").animate({
    scrollTop: $(`#${nextId}`).offset().top,
  }, {
    duration: 500,
    start: function() {
      $(`#${currentId}`).animate({
        opacity: 0,
      }, {
        duration: 500,
        done: function() {
          $(`#${currentId}`).hide();
        }
      });
      $(`#${nextId}`).animate({
        opacity: 100,
      }, {
        duration: 500,
      })
    }
  });
}

function attachSlider() {
  $(".node.clickable").click(function() {
      var pageId = $(this).closest(".page").attr("id");
      var nextPageId = $(`#${pageId}`).next(".page").attr("id");
      slideToNext(pageId, nextPageId);
    })
}


