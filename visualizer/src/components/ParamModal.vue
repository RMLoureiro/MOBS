<template>
      <transition name="modal">
        <div class="modal-mask">
          <div class="modal-wrapper">
            <div class="modal-container">

              <div class="modal-header">
                <slot name="header">
                  <h3>Parameters</h3>
                  <h4>Output File: {{getFilename()}}</h4>
                </slot>
              </div>

              <div class="modal-body">
                <slot name="body">
                  <p v-for="params in getParams()" :key="params">
                    {{params[0]}} : {{params[1]}}
                  </p>
                </slot>
              </div>

              <div class="modal-footer">
                <slot name="footer">
                  <button class="modal-default-button" @click="$emit('close')">
                    Close
                  </button>
                </slot>
              </div>
            </div>
          </div>
        </div>
      </transition>
</template>


<script>

export default {
    props: {
      data:Object
    },
    methods: {
      getParams: function() {
        let params = [];
        if(this.data != null) {
          Object.keys(this.data.parameters).forEach(key => {
            let p = [];
            p.push(key);
            p.push(this.data.parameters[key]);
            params.push(p);
          });
        }
        return params;
      },
      getFilename: function() {
        if(this.data != null) {
          return this.data.filename;
        }
        return "";
      }
    }
}
</script>

<style>
    /* Style from https://vuejs.org/v2/examples/modal.html */

    .modal-mask {
        position: fixed;
        z-index: 9998;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0, 0, 0, 0.5);
        display: table;
        transition: opacity 0.3s ease;
    }

    .modal-wrapper {
        display: table-cell;
        vertical-align: middle;
    }

    .modal-container {
        width: 300px;
        margin: 0px auto;
        padding: 20px 30px;
        background-color: #fff;
        border-radius: 2px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.33);
        transition: all
         0.3s ease;
        font-family: Helvetica, Arial, sans-serif;
    }

    .modal-header h3 {
        margin-top: 0;
        color: #42b983;
    }

    .modal-body {
        margin: 20px 0;
        text-align: center;
    }

    .modal-default-button {
        float: right;
    }



    .modal-enter {
        opacity: 0;
    }

    .modal-leave-active {
        opacity: 0;
    }

    .modal-enter .modal-container,
    .modal-leave-active .modal-container {
        -webkit-transform: scale(1.1);
        transform: scale(1.1);
    }

</style>