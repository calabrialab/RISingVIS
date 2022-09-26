function disablePseudoInput(containerId, btnsIds, txtIds) {
  btnsIds.forEach(
    function(value, index, array) {
      $(`#${value}`).prop("disabled", true);
    }
  );
  txtIds.forEach(
    function(value, index, array) {
      $(`#${value}`).addClass("text-out-mock-disabled").removeClass("text-out-mock-enabled")
    }
  );
}

function enablePseudoInput(containerId, btnsIds, txtIds) {
  btnsIds.forEach(
    function(value, index, array) {
      $(`#${value}`).prop("disabled", false);
    }
  );
    txtIds.forEach(
    function(value, index, array) {
      $(`#${value}`).addClass("text-out-mock-enabled").removeClass("text-out-mock-disabled")
    }
  );
}
