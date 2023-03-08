const nspaceURI = 'http://www.w3.org/2000/svg';
const radius = 27.561001;
const cx1 = 34.722301;
const cy1 = 34.722301;
const distance = 2.5*radius
const arcRadius = 31.708;
const arcStartY = 3.0143021;
const deltaRad = arcRadius - radius;
const arcDist = distance - 2*deltaRad;
const conGap = 12;

function calcCY(nodeIndex) {
  if (nodeIndex == 1) {
    return cy1;
  }
  if (nodeIndex > 1) {
    var cy = cy1 + 2*radius + distance*(nodeIndex - 1) + (nodeIndex - 2)*2*radius;
    return cy;
  }
}

function calcArcY(nodeIndex) {
  var arcY = arcStartY + (nodeIndex - 1)*(2*arcRadius + arcDist);
  return arcY;
}

function adjustViewBox(svgId, nodeIndex) {
  var totHeight = 2*arcStartY + nodeIndex*2*arcRadius + (nodeIndex-1)*arcDist;
  var svgObj = document.getElementById(svgId);
  const newVB = `0 0 72.021 ${totHeight}`;
  svgObj.setAttribute("viewBox", newVB);
}

function createNode(nodeIndex, uniqueId) {
  // Node group
  var group = document.createElementNS(nspaceURI, "g");
  group.setAttribute("id", uniqueId);
  // Main node
  var mainNode = document.createElementNS(nspaceURI, "circle");
  mainNode.setAttribute("cx", cx1);
  mainNode.setAttribute("cy", calcCY(nodeIndex));
  mainNode.setAttribute("r", radius);
  mainNode.setAttribute("id", `${uniqueId}-main-node`);
  mainNode.classList.add("mini-node-main");
  // Node arc
  var arc = document.createElementNS(nspaceURI, "path");
  var clockwise = nodeIndex % 2 == 0 ? 0 : 1;
  var arcPath = `m ${cx1}, ${calcArcY(nodeIndex)} a ${arcRadius}, ${arcRadius} 0 1 ${clockwise} 0,63.4159999`;
  arc.setAttribute("d", arcPath);
  arc.setAttribute("id", `${uniqueId}-node-arc`);
  arc.classList.add("mini-node-arc");
  // compose
  group.appendChild(mainNode);
  group.appendChild(arc);
  return group
}

function addNewNode(nodeIndex, uniqueId, svgId) {
  var newNode = createNode(nodeIndex, uniqueId);
  var svgObj = document.getElementById(svgId);
  return svgObj.appendChild(newNode)
}

function addNewConnector(nodeIndex, uniqueId, svgId) {
  var line = document.createElementNS(nspaceURI, "line");
  line.setAttribute("id", uniqueId);
  line.setAttribute("x1", cx1);
  line.setAttribute("x2", cx1);
  var y1 = arcStartY + nodeIndex*2*arcRadius + (nodeIndex - 1)*arcDist + conGap;
  var y2 = y1 + (arcDist - 2*conGap);
  line.setAttribute("y1", y1);
  line.setAttribute("y2", y2);
  line.classList.add("mini-node-connector");
  var svgObj = document.getElementById(svgId);
  return svgObj.appendChild(line)
}

$( document ).ready(function() {
  Shiny.addCustomMessageHandler('addSideNavNode', function(arg) {
    if (arg.nodeIndex > 1) {
      addNewConnector(arg.nodeIndex - 1,
      `connector-${arg.nodeIndex - 1}-${arg.nodeIndex}`, arg.svgId);
    }
    addNewNode(arg.nodeIndex, arg.uniqueNodeId, arg.svgId);
    adjustViewBox(arg.svgId, arg.nodeIndex);
  })
})


function changeStatusClasses(nodeId, status, processed) {
  nodeMain = document.getElementById(`${nodeId}-main-node`);
  nodeArc = document.getElementById(`${nodeId}-node-arc`);
  if (status == "skipped") {
    nodeMain.classList.remove("mini-node-main-processed");
    nodeMain.classList.remove("mini-node-main-active");
    nodeMain.classList.add("mini-node-main-skipped");
    nodeArc.classList.remove("mini-node-arc-processed");
    nodeArc.classList.remove("mini-node-arc-active");
    nodeArc.classList.add("mini-node-arc-skipped");
    return
  }
  if (status == "active") {
    nodeMain.classList.remove("mini-node-main-processed");
    nodeMain.classList.remove("mini-node-main-skipped");
    nodeMain.classList.add("mini-node-main-active");
    nodeArc.classList.remove("mini-node-arc-processed");
    nodeArc.classList.remove("mini-node-arc-skipped");
    nodeArc.classList.add("mini-node-arc-active");
    return
  }
  if (status == "inactive" && processed === true) {
    nodeMain.classList.remove("mini-node-main-active");
    nodeMain.classList.remove("mini-node-main-skipped");
    nodeMain.classList.add("mini-node-main-processed");
    nodeArc.classList.remove("mini-node-arc-active");
    nodeArc.classList.remove("mini-node-arc-skipped");
    nodeArc.classList.add("mini-node-arc-processed");
    return
  }
  nodeMain.classList.remove("mini-node-main-active");
  nodeMain.classList.remove("mini-node-main-skipped");
  nodeMain.classList.remove("mini-node-main-processed");
  nodeArc.classList.remove("mini-node-arc-processed");
  nodeArc.classList.remove("mini-node-arc-active");
  nodeArc.classList.remove("mini-node-arc-skipped");
  return
}

$( document ).ready(function() {
  Shiny.addCustomMessageHandler('changeStatus', function(arg) {
    changeStatusClasses(arg.nodeId, arg.nodeStatus, arg.processed);
  })
})

