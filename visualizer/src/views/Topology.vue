<template>
    <section class="topology">
        <p>Usage: Left click on the canvas to add a new node. Right click two nodes to add a link between them. Press D to delete a selected node or link.</p>
        <canvas id="topologyCanvas"></canvas>
        <topology-form :selected="selected"></topology-form>
    </section>
</template>

<script>
    import { mapActions, mapMutations, mapState } from "vuex";
    import TopologyForm from '../components/TopologyForm.vue';


    import {TNode, TLink} from "../js/Topology.js";

    let canvas;

    export default {
        data() {
            return {
                nodes: [],
                links: [],
                numNodes: 0,
                numLinks: 0,
                nextNodeId: 1,
                nextLinkId: 1,
                selected: {type:"node", target:null},
                creatingLink: {status:0,start:null,end:null}, // (status: 0=idle, 1=first_selected, 2=done)
                intervalDraw: "",
            };
        },
        components: {
            TopologyForm
        },
        created() {

        },
        beforeUnmount() {
            clearInterval(this.intervalDraw);
            this.clearData();
            window.removeEventListener("keydown", this.delHandler);
            window.removeEventListener("mouseup", this.clickHandler);
            canvas = null;
        },
        mounted() {
            this.clearData();
            canvas = document.getElementById("topologyCanvas");
            this.draw();
            this.intervalDraw = setInterval(this.draw, 1000);
            window.addEventListener("keydown", this.delHandler);
            window.addEventListener("mouseup", this.clickHandler);
        },
        computed: {

        },
        methods: {
            draw : function() {
                if(canvas) {
                    // Scale canvas
                    canvas.style.width ='100%';
                    canvas.style.height='75%';
                    canvas.width  = canvas.offsetWidth;
                    canvas.height = canvas.offsetHeight;
                    
                    // Clear background
                    let ctx = canvas.getContext("2d");
                    ctx.beginPath();
                    ctx.clearRect(0,0,canvas.width,canvas.height);
                    ctx.fillStyle = 'rgba(75, 75, 75, 0.4)';
                    ctx.fillRect(0,0,canvas.width,canvas.height);
                    ctx.closePath();

                    // Draw Links and Nodes
                    for(let i = 0; i < this.links.length; i++) {
                        this.links[i].show(ctx);
                    }
                    for(let i = 0; i < this.nodes.length; i++) {
                        this.nodes[i].show(ctx);
                    }
                }
            },
            delHandler : function(event) {
                if(event.code == "KeyD") { // Delete selected node or link
                    if(this.selected.type == "node" && this.selected.target !== null) {
                        this.deleteNode(this.selected.target);
                    }
                    else if(this.selected.type == "link" && this.selected.target !== null) {
                        this.deleteLink(this.selected.target);
                    }
                    this.creatingLink = {status:0,start:null,end:null};
                    this.selected = {type:"node", target:null};
                    this.draw();
                }
            },
            clickHandler : function(event) {
                let ctx = canvas.getContext("2d");
                const offsetX = ctx.canvas.getBoundingClientRect().left;
                const offsetY = ctx.canvas.getBoundingClientRect().top;
                const x = event.clientX - offsetX;
                const y = event.clientY - offsetY;
                if(x < 0 || y > canvas.height || x > canvas.width || y < 0) { return; }
                const node = this.getCollidedNode(x, y);
                const link = this.getCollidedLink(x, y);
                if(event.button == 0 || event.button == 2) {
                    if (node !== null) {
                        if(this.selected.target != null) {
                            this.selected.target.unselect();
                        }
                        node.select();
                        this.selected.type = "node";
                        this.selected.target = node;
                    }
                    else if(link !== null) {
                        if(this.selected.target != null) {
                            this.selected.target.unselect();
                        }
                        link.select();
                        this.selected.type = "link";
                        this.selected.target = link;
                    }
                    else {
                        if(event.button == 0) {
                            this.createNode(x,y);
                        }
                        this.creatingLink = {status:0,start:null,end:null};
                        if(this.selected.target != null) {
                                this.selected.target.unselect();
                            }
                        this.selected = {type:"node", target:null};
                    }
                }
                if(event.button == 2) {
                    if(node !== null) {
                        if(this.creatingLink.status == 0) {
                            this.creatingLink.start = node;
                            this.creatingLink.status = 1;
                        }
                        else if(this.creatingLink.status == 1) {
                            this.creatingLink.end = node;
                            this.createLink(this.creatingLink.start, this.creatingLink.end);
                            this.creatingLink = {status:0,start:null,end:null};
                            if(this.selected.target != null) {
                                this.selected.target.unselect();
                            }
                        }
                    }
                }
                this.draw();
            },
            clearData : function() {
                this.nodes = [];
                this.links = [];
                this.numNodes = 0;
                this.numLinks = 0;
                this.nextNodeId = 1;
                this.nextLinkId = 1;
                this.selected = {type:"node", target:null};
                this.creatingLink = {status:0,start:null,end:null};
                this.intervalDraw = "";
            },
            createNode : function(x,y) {
                this.nodes.push(new TNode(this.nextNodeId++,x,y,[],0,0,0));
                this.draw();
            },
            createLink : function(from,to) {
                if(!this.linkExists(from, to) && (from.id != to.id)) {
                    this.links.push(new TLink(this.nextLinkId++, from, to));
                    from.addLink(to.id);
                    to.addLink(from.id);
                }
                this.draw();
            },
            deleteNode : function(n) {
                let nodeId = n.id;
                this.nextNodeId = this.nextNodeId - 1;
                let i = 0;
                while(i < this.links.length) {
                    if(this.links[i].from.id == n.id || this.links[i].to.id == n.id) {
                        this.deleteLink(this.links[i]);
                    }
                    else {
                        i++;
                    }
                }
                let toSplice = 0;
                for(let j = 0; j < this.nodes.length; j++) {
                    this.nodes[j].decrementIds(nodeId);
                    if(this.nodes[j].id == n.id) {
                        toSplice = j;
                    }
                    else {
                        if(this.nodes[j].id > nodeId) {this.nodes[j].id = this.nodes[j].id-1;}
                    }
                }
                this.nodes.splice(toSplice,1);
                this.draw();
            },
            deleteLink : function(l) {
                l.from.removeLink(l.to.id);
                l.to.removeLink(l.from.id);
                for(let i = 0; i < this.links.length; i++) {
                    if(this.links[i].from.id == l.from.id && this.links[i].to.id == l.to.id) {
                        this.links.splice(i,1);
                        break;
                    }
                }
                this.draw();
            },
            linkExists : function(n1,n2) {
                for(let i = 0; i < this.links.length; i++) {
                    if(this.links[i].from.id == n1.id && this.links[i].to.id == n2.id) {
                        return true;
                    }
                }
                return false;
            },
            getCollidedNode : function(mouseX, mouseY) {
                for(let i = 0; i < this.nodes.length; i++) {
                    if(this.nodes[i].collide(mouseX,mouseY)) {
                        return this.nodes[i];
                    }
                }
                return null;
            },
            getCollidedLink : function(mouseX, mouseY) {
                for(let i = 0; i < this.links.length; i++) {
                    if(this.links[i].collide(mouseX,mouseY)) {
                        return this.links[i];
                    }
                }
                return null;
            },
            updateNode : function(newNodeState) {
                if(this.selected.target.id == newNodeState.id) {
                    this.selected.target.region = newNodeState.region;
                    this.selected.target.hPower = newNodeState.hPower;
                    this.selected.target.stake = newNodeState.stake;
                }
            },
            toJSON : function() {
                
            },
            fromJSON : function() {

            }
        }
    }

</script>



<style scoped>

    .topology {
        position: absolute;
        height: 98%;
        width: 98%;
    }

    canvas {
        width: 100%;
        height: 75%;
    }

</style>