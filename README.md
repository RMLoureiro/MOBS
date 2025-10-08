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
- an installation of NodeJS (version == 16)
- an installation of npm, along with yarn, electron and vue
	- ```npm install --global yarn```
---

## Usage

Testes and executions of the protocol should be done in the master branch, small tweaks and patches have been done to the added protocols since their pull requests were merged.

### Protocols implemented
- Membership protocols:
 	- Chord: https://github.com/RMLoureiro/MOBS/pull/1 - Chord has no validation script unlike the other protocols.

- Consensus protocols:
	- Paxos: https://github.com/RMLoureiro/MOBS/pull/2
	- Chandra-toueg: https://github.com/RMLoureiro/MOBS/pull/4
	- pBFT: https://github.com/RMLoureiro/MOBS/pull/6
	- Ethereum: https://github.com/RMLoureiro/MOBS/pull/7


---

## Build

To build the simulator, run ```dune build``` in the ```/simulator``` directory. This will produce a ```_build``` directory.

To build the GUI, run ```yarn install``` in the ```/visualizer``` directory.

Running a simulation can be done via the GUI, or by running ```./simulator/_build/default/bin/main.exe``` (note that dune uses the ```.exe``` extension regardless of the operating system).

Running the GUI can be done with ```yarn electron:serve``` in the ```/visualizer``` directory.

[How to use the simulator](simulator/README.md)

[Documentation of the simulator](https://mce-alves.github.io/MOBS)

[How to use the GUI](visualizer/README.md)

---

## Validation scripts

Before running the validations scripts the user need to:
 - open the ```output_files/out0-1.json``` file, delete the last comma and add a ```]``` to end of the file to close the array.
 - replace the ```<your_file_path.json>``` in the validation script with a string representing the absolute file path to the ```output_files/out0-1.json``` file












