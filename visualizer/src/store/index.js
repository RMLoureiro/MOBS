import { createStore } from "vuex";
const fs = require('fs');


const store = createStore({
    state: {
        generalParameters: [],
        networkParameters: [],
        protocolParameters: [],
        loaded: false
    },
    getters: {},
    mutations: {
        getParameters: state => {
            if(!state.loaded) {
                let rawData = fs.readFileSync('../simulator/parameters.json')
                let json = JSON.parse(rawData);
                
                let generalParamsJson  = json.general;
                Object.keys(generalParamsJson).forEach(function(key) {
                    state.generalParameters.push({label:key, value:generalParamsJson[key]});
                });

                let networkParamsJson  = json.network;
                Object.keys(networkParamsJson).forEach(function(key) {
                    state.networkParameters.push({label:key, value:networkParamsJson[key]});
                });

                let protocolParamsJson = json.protocol;
                Object.keys(protocolParamsJson).forEach(function(key) {
                    state.protocolParameters.push({label:key, value:protocolParamsJson[key]});
                });

                state.loaded = true;
            }
        }
    },
    actions: {

    },
});

export default store;