# GUI

The GUI provides three main functionalities: ```parameters```, ```visualizer``` and ```statistics```.

The GUI is not tied to our specific simulator, as any simulator can be extended to receive and produce ```json``` files with the expected format.

---

## Parameters

The simulator is parametrized through ```json``` files. The GUI offers a way to change the parameterizations without the need of manually changing those files. It also allows the user to specify intervals of possible values for specific parameters. By the clicking on ```Run Simulation```, the program produces one ```json``` file for each combination of parameter values as well as batches, within the specified intervals, and executes the simulation with each of those parameterizations (which produces ```outputX-Y.json``` files, where X is the id assigned to the combination of parameters, and Y is the batch number for that combination).

The fields displayed in this page are dynamic, and correspond to those defined in the [default-parameters.json](../simulator/default-parameters.json) file.

The inputs are generated in the [input directory](../input_files), and the outputs are generated in the [output directory](../output_files).

---

## Topology

Since through parameterization, users can only specify general parameters that are then used to pseudo-randomly generate the topology, we decided to add the ability to fine-tune the topologies to be used in the simulation through a ```topology.json``` file. This file became complex to create by hand, so we added a graphical way to create and customize them.

Essentially, this page allows users to:
- create/delete nodes
- create/delete links between nodes
- individually parametrize nodes (region; hashing power; stake; whether or not the node is malicious, and when it starts acting maliciously; period of time when node is offline)
- save the created topologies, and load existing ones

---


## Visualizer

The visualizer is an extension of the [SimBlock Visualizer](https://github.com/dsg-titech/simblock-visualizer), where a user can upload an ```output.json``` file of a simulation, and see the evolution of the state of the nodes.

The main additions are:
- a user can select a **node** and/or a **link** between nodes, and see details about the current state of a node, and the messages currently in transit in a certain link
- a user can change the playback speed

---

## Statistics

A simulation produces statistics. Through the statistics page, a user can observe the effects caused by the variation of different parameters. It is also possible to see the maximum and minimum values for each statistic, and the combination of parameters that originated those values.

The values shown and plotted are obtained by averaging the results of all the batches for each combination of parameters.

It is also possible to view per-node-statistics, such as the number of events processed by each node.

The following metrics are extracted by default:
- average time to reach consensus
- number of messages exchanged
- megabytes exchanged
- events processed per node

The statistics displayed are also dynamic. Additional statistics will be displayed, as they are defined by the user when implementing the ```Statistics``` module for a protocol.