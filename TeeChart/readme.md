## Custom TeeChart Legend

Create an Eventhandler in the definition of your from containing the TeeChart.

It is not possible to connect the EventHandler to the TeeChart in de events list, so in the `FormCreate` connect the handler to the legend.

Implement the code needed to custimize the legend, in this case it creates rectangles before the legend text.