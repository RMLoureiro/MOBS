import { createStore } from "vuex";
const fs = require('fs');


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
        numSimulations: 0
    },
    getters: {},
    mutations: {
        getParameters: state => {
            if(!state.loaded) {
                let rawData = fs.readFileSync('../simulator/default-parameters.json')
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
        setParameters (state,params) {
            state.loaded = false;
            let g = params[0];
            let n = params[1];
            let p = params[2];
            state.generalParameters  = g;
            state.networkParameters  = n;
            state.protocolParameters = p;

            let gString = toJson(g);
            let nString = toJson(n);
            let pString = toJson(p);

            let outputJson = {general:JSON.parse(gString), network:JSON.parse(nString), protocol:JSON.parse(pString)};

            state.numSimulations += 1;
            fs.writeFileSync('../input_files/parameters'+state.numSimulations+'.json', JSON.stringify(outputJson));
        }
    },
    actions: {

    },
});

export default store;