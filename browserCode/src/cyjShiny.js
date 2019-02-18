// following http://www.htmlwidgets.org/develop_intro.html
"use strict";

var cytoscape = require('cytoscape');
//----------------------------------------------------------------------------------------------------
// add layout extensions
var cola = require('cytoscape-cola');
cytoscape.use(cola);

let dagre = require('cytoscape-dagre');
cytoscape.use(dagre);

let coseBilkent = require('cytoscape-cose-bilkent');
cytoscape.use(coseBilkent);

let popper = require('cytoscape-popper');
//cytoscape.use(popper);

$ = require('jquery');
require('jquery-ui-bundle');
// apparently two version of jquery loaded: by shiny, and just above
// see https://api.jquery.com/jquery.noconflict/ and
// this stackoverflow discussion: https://stackoverflow.com/questions/31227844/typeerror-datatable-is-not-a-function
$.noConflict();
//----------------------------------------------------------------------------------------------------
var executionMode = "devel";
const log = function(msg)
{
  if(executionMode == "devel")
      console.log(msg);
}
//----------------------------------------------------------------------------------------------------
HTMLWidgets.widget({

    name: 'cyjShiny',
    type: 'output',

    factory: function(el, allocatedWidth, allocatedHeight) {
        log("---- entering factory, initial dimensions: " + allocatedWidth + ", " + allocatedHeight);
	var cyj;
	return {
	    renderValue: function(x, instance) {
		log("---- ~/github/cyjsShiny/inst/browserCode/src/cyjShiny.js, renderValue")
		var data = JSON.parse(x.graph);
        var layoutName = x.layoutName;
        var style = x.style;
		log(data);
		var cyDiv = el;
		cyj = cytoscape({
		    container: cyDiv,
		    elements: data.elements,
		    layout: {name: layoutName},
		    style:  [{selector: 'node', css: {
                        'text-valign': 'center',
                        'text-halign': 'center',
                        'content': 'data(id)',
                        'border-color': 'red',
                        'background-color': 'white',
                        'border-width': 1,
                        'height': 60,
                        'width': 60
                        }},
                     {selector: 'node:selected', css: {
                        'overlay-color': 'gray',
                        'overlay-opacity': 0.4,
                         }},
                     {selector: 'edge', css: {
                          'width': '1px',
                          'line-color': 'black',
                           'target-arrow-shape': 'triangle',
                           'target-arrow-color': 'black',
                           'curve-style': 'bezier'
                           }},
                     {selector: 'edge:selected', css: {
                        'overlay-color': 'gray',
                        'overlay-opacity': 0.4
                        }}
                        ],
		    ready: function(){
                        log("cyjShiny cyjs ready");
			//$("#cyjShiny").height(0.95*window.innerHeight);
                        log("cyjShiny widget, initial dimensions: " + allocatedWidth + ", " + allocatedHeight)
			$("#cyjShiny").height(allocatedHeight)
			$("#cyjShiny").width(allocatedWidth)
			var cyj = this;
			window.cyj = this;   // terrible hack.  but gives us a simple way to call cytosacpe functions
			//If given a style, this is the place to set it!
			if (style != null) {
				cyj.style(style.json);
			}
			log("small cyjs network ready, with " + cyj.nodes().length + " nodes.");
		        log("  initial widget dimensions: " +
                            $("#cyjShiny").width() + ", " +
                            $("#cyjShiny").height());

			cyj.nodes().map(function(node){node.data({degree: node.degree()})});
			
			//Do we have a div for selection?
			  if (data.elements.edges[0].data.selectionDiv) {
					cyj.on('select', 'edge', function(event) {
						let edge = event.target;
						let idd = edge.data().id;
						let popper = edge.popper({
						  content: () => {
							let div = document.createElement('div');
							div.setAttribute('id', 'tooltipSelect'+idd);
							div.innerHTML = edge.data().selectionDiv;
							document.body.appendChild(div);
							return div;

						  },
						  popper: {}
						});

						let closeEdge = () => {
							let div = document.getElementById('tooltipSelect'+idd);
							if (div) {
								document.body.removeChild(div);
							}
							popper.destroy();
						};

						let updateEdge = () => {
							popper.scheduleUpdate();
						};

						edge.on("unselect", closeEdge);
						edge.on('position', updateEdge);
				  cyj.nodes().on('position',updateEdge);
				  cyj.on('pan zoom resize', updateEdge);
					});
			  }

			  //Do we have a div for selection?
			  if (data.elements.nodes[0].data.selectionDiv) {
					cyj.on('select', 'node', function(event) {
						let node = event.target;
						let idd = node.data().id;
						let popper = node.popper({
						  content: () => {
							let div = document.createElement('div');
							div.setAttribute('id', 'tooltipSelect'+idd);
							div.innerHTML = node.data().selectionDiv;
							document.body.appendChild(div);
							return div;

						  },
						  popper: {}
						});

						let closeNode = () => {
							let div = document.getElementById('tooltipSelect'+idd);
							if (div) {
								document.body.removeChild(div);
							}
							popper.destroy();
						};

						let updateNode = () => {
							popper.scheduleUpdate();
						};

						node.on("unselect", closeNode);
						node.on('position', updateNode);
						cyj.on('pan zoom resize', updateNode);
					});
			  }

				cyj.on('mouseover', 'node', function(event) {
						let node = event.target;

						let popper = node.popper({
						  content: () => {
							let div = document.createElement('div');
							div.setAttribute('id', 'tooltipNode');
							div.innerHTML = node.data().tooltip;
							div.style.backgroundColor='white';
							document.body.appendChild(div);
							return div;

						  },
						  popper: {}
						});

						let close = () => {
							let div = document.getElementById('tooltipNode');
							if (div) {
								document.body.removeChild(div);
							}
							popper.destroy();
						};

						let update = () => {
							popper.scheduleUpdate();
						};

						node.on("mouseout", close);
						node.on('position', update);
					});
			//setTimeout(function() {
			//    cyj.fit(10)
			//}, 600);
		    } // ready
		}) // cytoscape()
            }, // renderValue
            resize: function(newWidth, newHeight, instance){
		log("cyjShiny widget, resize: " + newWidth + ", " + newHeight)
		//$("#cyjShiny").height(0.95 * window.innerHeight);
		$("#cyjShiny").height(newHeight);
		cyj.resize()
		log("  after resize, widget dimensions: " +
                            $("#cyjShiny").width() + ", " +
                            $("#cyjShiny").height());
            },
            cyjWidget: cyj
        }; // return
    } // factory
});  // widget
//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("doLayout", function(message){

    var strategy = message.strategy;
    self.cyj.layout({name: strategy}).run()
    })

//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("getNodePositions", function(message){

    log("--- entering getNodePositions, customMessangeHandler")
    var tbl = JSON.stringify(self.cyj.nodes().map(function(n){return{id: n.id(),
                                                                     x: n.position().x,
                                                                     y: n.position().y}}));

    log(tbl)
    Shiny.onInputChange("tbl.nodePositions", tbl)
    })

//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("setNodePositions", function(message){

    log("--- entering setNodePositions, customMessangeHandler")
    log(message.tbl)

    var tbl = message.tbl; // JSON.parse(message.tbl)

    console.log("calling setPosition map");
    tbl.map(function(e){
       var tag="[id='" + e.id + "']";
       self.cyj.$(tag).position({x: e.x, y:e.y});
       });

    })

//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("removeGraph", function(message){

    self.cyj.elements().remove();
    })

//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("addGraph", function(message){

    var jsonString = message.graph;
    var g = JSON.parse(jsonString);
    self.cyj.add(g.elements);
    self.cyj.fit(50);
    })

//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("redraw", function(message){

    log("redraw requested");
    self.cyj.style().update();
    })

//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("setNodeAttributes", function(message){

    log("setNodeAttributes requested")

    var nodeIDs = message.nodes;
    var attributeName = message.attribute;

    for(var i=0; i < nodeIDs.length; i++){
       var id = nodeIDs[i];
       var newValue = message.values[i];
       var node = self.cyj.getElementById(id);
       node.data({[attributeName]:  newValue});
       };
})
//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("setEdgeAttributes", function(message){

    log("setEdgeAttributes requested")

    var attributeName = message.attributeName;
    var sourceNodes = message.sourceNodes;
    var targetNodes = message.targetNodes;
    var interactions = message.interactions;
    var values = message.values

   for(var i=0; i < sourceNodes.length; i++){
      //var selectorString = "edge[source='" + sourceNodes[i] + "'][target='" + targetNodes[i] +
      //                     "'][interaction='" + interactions[i] + "']";
      //log(selectorString);
      var id = sourceNodes[i] + "-(" + interactions[i] + ")-" + targetNodes[i];
      log("edge id: " + id)
      var edge = self.cyj.getElementById(id)
      log(edge)
      if(edge != undefined){
         log("setting edge " + attributeName + " to " + values[i])
         edge.data({[attributeName]: values[i]})
         }
      //var dataObj = self.cyj.edges().filter(selectorString).data();
      // log("--- edge object after filtering:")
      // log(dataObj)
      //if(dataObj != undefined){
      //   Object.defineProperty(dataObj, attributeName, {value: values[i]});
      //   }
      } // for i

}) // setEdgeAttributes
//----------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("selectNodes", function(message){

   log("selectNodes requested: " + message);

   var nodeIDs = message;

   if(typeof(nodeIDs) == "string")
      nodeIDs = [nodeIDs];

   var filterStrings = [];

   for(var i=0; i < nodeIDs.length; i++){
     var s = '[id="' + nodeIDs[i] + '"]';
     filterStrings.push(s);
     } // for i

   log("filtersStrings, joined: " + filterStrings);

   var nodesToSelect = window.cyj.nodes(filterStrings.join());
   nodesToSelect.select()

});
//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("clearSelection", function(message){

    log("clearSelection requested: " + message);
    self.cyj.filter("node:selected").unselect();

})
//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("getSelectedNodes", function(message){

    log("getSelectedNodes requested: " + message);
    var value = self.cyj.filter("node:selected")
        .map(function(node) {
            return(node.data().id)})
             //return {id: node.data().id, label: node.data().label}})

    log(self.cyj.filter("node:selected"));
    log(value)
    Shiny.setInputValue("selectedNodes", value, {priority: "event"});

});
//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("sfn", function(message){

    log("sfn requested: " + message);
    self.cyj.nodes(':selected').neighborhood().nodes().select();

})
//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("fitSelected", function(message){

    log("fitSelected requested");
    var padding = message.padding;

    var selectedNodes = self.cyj.filter("node:selected");

    if(selectedNodes.length == 0){
	log("no nodes currently selected")
     }
   else{
       log("fitSelected, with padding " + padding);
       self.cyj.fit(selectedNodes, padding)
   }
})
//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("fit", function(message){
    log("fit requested: ");
    var padding = message.padding;
    log("   padding: " + padding)
    self.cyj.fit(padding);
    });
//------------------------------------------------------------------------------------------------------------------------
if(HTMLWidgets.shinyMode) Shiny.addCustomMessageHandler("loadStyle", function(message) {

    log("loading style");
    var styleSheet = message.json;
    window.cyj.style(styleSheet);
    });

//------------------------------------------------------------------------------------------------------------------------
