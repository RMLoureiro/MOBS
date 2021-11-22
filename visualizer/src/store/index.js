import { createStore } from "vuex";
const fs = require('fs');
const child = require('child_process').spawn;


let input_dir          = '../input_files/';
let output_dir         = '../output_files/';
let sim_path           = '../simulator/_build/default/bin/main.exe';
let default_params_dir = '../simulator/default-parameters.json';

/*
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
*/

function mySort(first, second) {
    if(first.length > second.length) {
        return 1;
    }
    if(first.length < second.length) {
        return -1;
    }
    return first.localeCompare(second);
}

function toJson(arr) {
    let res = "{";

    arr.forEach(elem => {
        res += '"';
        res += elem.label;
        res += '"';
        res += ":";
        res += JSON.stringify(JSON.parse(elem.value));
        res += ",";
    });

    res = res.slice(0,-1);
    res += "}";
    return res;
}


const store = createStore({
    state: {
        generalParameters: [],
        networkParameters: [],
        protocolParameters: [],
        loaded: false,
        numSimulations: 0,
        parameters:[],
        outputs:[],
        per_node_outputs:[]
    },
    getters: {},
    mutations: {
        getParameters: state => {
            if(!state.loaded) {
                let rawData = fs.readFileSync(default_params_dir)
                let json = JSON.parse(rawData);
                
                let generalParamsJson  = json.general;
                Object.keys(generalParamsJson).forEach(function(key) {
                    state.generalParameters.push({label:key, value:JSON.stringify(generalParamsJson[key])});
                });

                let networkParamsJson  = json.network;
                Object.keys(networkParamsJson).forEach(function(key) {
                    state.networkParameters.push({label:key, value:JSON.stringify(networkParamsJson[key])});
                });

                let protocolParamsJson = json.protocol;
                Object.keys(protocolParamsJson).forEach(function(key) {
                    state.protocolParameters.push({label:key, value:JSON.stringify(protocolParamsJson[key])});
                });

                state.loaded = true;
            }
        },
        clearInputFiles(state) {
            // delete old input files (JSON)
            let input_files = fs.readdirSync(input_dir);
            input_files.forEach(file => {
                if(file.endsWith(".json")) {
                    fs.unlinkSync(input_dir+file, function(err) {
                        if(err) console.log(err);
                    });
                }
            });

            // delete old output files (JSON)
            let output_files = fs.readdirSync(output_dir);
            output_files.forEach(file => {
                if(file.endsWith(".json")) {
                    fs.unlinkSync(output_dir+file, function(err) {
                        if(err) console.log(err);
                    });
                }
            });

            state.numSimulations = 0;
        },
        computeGraphInOut(state) {
            let out_per_batch = new Map();
            let out_per_node_per_batch = new Map();
            let prefixes = [];
            state.parameters = [];
            state.outputs = [];
            fs.readdirSync(input_dir).sort(mySort).forEach(file => {
                if(file.endsWith(".json")) {
                    let raw = fs.readFileSync(input_dir+file);
                    let sim_input = JSON.parse(raw);
                    state.parameters.push(sim_input.protocol);
                }
            });
            fs.readdirSync(output_dir).sort(mySort).forEach(file => {
                if(file.endsWith(".json")) {
                    let prefix = file.split("-")[0];
                    let raw = fs.readFileSync(output_dir+file);
                    let sim_output = JSON.parse(raw);
                    let out_obj = {filename:file, stats:sim_output[sim_output.length - 3].content};
                    let per_node_out_obj = {filename:file, stats:sim_output[sim_output.length - 2].content};
                    if(out_per_batch.get(prefix)) {
                        out_per_batch.get(prefix).push(out_obj);
                        out_per_node_per_batch.get(prefix).push(per_node_out_obj);
                    }
                    else {
                        prefixes.push(prefix);
                        out_per_batch.set(prefix, [out_obj]);
                        out_per_node_per_batch.set(prefix, [per_node_out_obj]);
                    }
                }
                
            });
            prefixes.forEach(prefix => {
                let batch_outputs = [];
                out_per_batch.get(prefix).forEach(out => batch_outputs.push(out['stats']));
                // General stats
                let avg_stat_json = {};
                Object.keys(batch_outputs[0]).forEach(key => {
                    let avg = 0;
                    batch_outputs.forEach(output => {

                        avg += output[key];
                    });
                    avg = avg / batch_outputs.length;
                    avg_stat_json[key] = avg;
                });
                state.outputs.push({filename:(prefix+".json"), stats:avg_stat_json});
                // Per node stats
                let batch_node_outputs = [];
                let avg_per_node_stat_json = {};
                out_per_node_per_batch.get(prefix).forEach(out => batch_node_outputs.push(out['stats']));
                Object.keys(batch_node_outputs[0]).forEach(key => {
                    let avg_arr = [];
                    batch_node_outputs.forEach(output => {
                        let arr = output[key];
                        if(avg_arr.length == 0) {
                            arr.forEach(elem => avg_arr.push(elem));
                        }
                        else {
                            for(let i = 0; i < arr.length; i++) {
                                avg_arr[i] += arr[i];
                            }
                        }
                    });
                    for(let i = 0; i < avg_arr.length; i++) {
                        avg_arr[i] = avg_arr[i] / batch_node_outputs.length;
                    }
                    avg_per_node_stat_json[key] = avg_arr;
                });
                state.per_node_outputs.push({filename:(prefix+".json"), stats:avg_per_node_stat_json});
            });
            console.log(state.per_node_outputs);
        },
        storeParamsDefault(state,params) {
            let g = params[0];
            let n = params[1];
            let p = params[2];

            let gString = toJson(g);
            let nString = toJson(n);
            let pString = toJson(p);

            let outputJson = {general:JSON.parse(gString), network:JSON.parse(nString), protocol:JSON.parse(pString)};

            fs.writeFileSync(default_params_dir, JSON.stringify(outputJson));
        }
    },
    actions: {
        async produce(state,params) {
            state.loaded = false;
            let g = params[0];
            let n = params[1];
            let p = params[2];
            let execution = params[3];
            state.generalParameters  = g;
            state.networkParameters  = n;
            state.protocolParameters = p;

            // produce file JSON parameters
            let gString = toJson(g);
            let nString = toJson(n);
            let pString = toJson(p);

            let outputJson = {general:JSON.parse(gString), network:JSON.parse(nString), protocol:JSON.parse(pString)};

            state.numSimulations += 1;
            fs.writeFileSync(input_dir+'parameters'+execution+'.json', JSON.stringify(outputJson));
            
            // call the execution of the simulator, using the produced parameter file
            return new Promise(function(resolve) {
                let sim = child(sim_path, [input_dir+'parameters'+execution+'.json', output_dir+'out'+execution+'.json']);
                sim.on('close', (code) => {console.log(code); resolve("ok");});
                sim.stdout.on('data', (data) => {console.log(`${data}`)});
            });
        }
    },
});

export default store;