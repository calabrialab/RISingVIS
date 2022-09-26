$(document).ready(
  $("#isa_config").parent().css({
    "background-color": "grey",
  })
)

function styleFileInput(id) {
  $(`#${id}`).parent().removeClass('btn-default').addClass('btn-file-custom');
}
