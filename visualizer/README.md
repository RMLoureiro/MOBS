# GUI

The GUI provides three main functionalities: ```parameters```, ```visualizer``` and ```statistics```.

The GUI is not tied to our specific simulator, as any simulator can be extended to receive and produce ```json``` files with the expected format.


## Parameters

The simulator is parametrized through ```json``` files. The GUI offers a way to change the parameterizations without the need of manually changing those files. It also allows the user to specify intervals of possible values for specific parameters. By the clicking on ```Run Simulation```, the program produces one ```json``` file for each combination of parameter values, within the specified intervals, and executes the simulation with each of those parameterizations (which produces ```out.json``` files).

The fields displayed in this page are dynamic, and correspond to those defined in the [default-parameters.json](../simulator/default-parameters.json) file.

The inputs are generated in the [input directory](../input_files), and the outputs are generated in the [output directory](../output_files).


## Visualizer

The visualizer is an extension of the [SimBlock Visualizer](https://github.com/dsg-titech/simblock-visualizer), where an user can upload an ```out.json``` file of a simulation, and see the evolution of the state of the nodes.

An addition is that an user can also select a **node** and/or a **link** between nodes, and see details about the current state of a node, and the messages currently in transit in a certain link.



## Statistics

A simulation produces statistics. Through the statistics page, a user can observe the effects caused by the variation of different parameters. It is also possible to see the maximum and minimum values for each statistic, and the combination of parameters that originated those values.

In this page, the statistics displayed are also dynamic. These are defined by the user when implementing the ```Statistics``` module of a protocol.

