[ {"selector": "node", "css": {
      "shape": "ellipse",
      "text-valign":"right",
      "text-halign":"center",
      "label": "data(tooltip)",
      "background-color": "#FFFFFF",
      "border-color": "black","border-width":"1px",
      "width":  "20px",
      "height": "20px",
      "font-size":"12px"}},

   {"selector":"node:selected", "css": {
       "border-color": "black",
       "overlay-opacity": 0.2,
       "background-color": "green"
       }},

   {"selector": "edge", "css": {
       "opacity": 0.5,
       "curve-style": "bezier",
	"source-arrow-shape": "triangle",
	"target-arrow-shape": "triangle"
       }},

   {"selector":"edge:selected", "css": {
       "overlay-opacity": 0.2,
       "overlay-color": "red"
       }}
]
