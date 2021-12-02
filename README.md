# MOdular Blockchain Simulator

MOBS is a simulator and GUI developed with the goal of aiding in the development and study of consensus protocols.

For now, since changes are regularly being made, you need to compile the simulator and GUI yourself. At a later stage, everything will be packaged into a standalone executable.

---

## Requirements

The simulator has the following requirements:
- an installation of OCaml
- an installation of opam, along with the following packages
	- ```opam install dune```
	- ```opam install sha.1.14```
	- ```opam install yojson.1.7.0```

The GUI has the following requirements:
- an installation of NodeJS (version >= 14)
- an installation of npm, along with yarn, electron and vue
	- ```npm install --global yarn```
---

## Usage

To build the simulator, run ```dune build``` in the ```/simulator``` directory. This will produce a ```_build``` directory.

To build the GUI, run ```yarn install``` in the ```/visualizer``` directory.

Running a simulation can be done via the GUI, or by running ```./simulator/_build/default/bin/main.exe``` (note that dune uses the ```.exe``` extension regardless of the operating system).

Running the GUI can be done with ```yarn electron:serve``` in the ```/visualizer``` directory.

[How to use the simulator](simulator/README.md)

[How to use the GUI](visualizer/README.md)













