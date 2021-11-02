<template>
    <section class="topology">
        <keep-alive>
            <canvas id="topologyCanvas"></canvas>
        </keep-alive>
    </section>
</template>

<script>
    import { mapActions, mapMutations, mapState } from "vuex";

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
                creatingLink: {status:0,start:null,end:null} // (status: 0=idle, 1=first_selected, 2=done)
            };
        },
        components: {
            
        },
        created() {
            setInterval(this.draw, 1000);
        },
        mounted() {
            canvas = document.getElementById("topologyCanvas");
            this.draw();
            setInterval(this.draw, 1000);
            // REMOVE LATER
            this.nodes.push(new TNode(this.nextNodeId++,50,50,[],0,0));
            this.nodes.push(new TNode(this.nextNodeId++,150,150,[],0,0));
            this.links.push(new TLink(this.nextLinkId++,this.nodes[0],this.nodes[1]));
            // END REMOVE LATER
            window.addEventListener("keydown", event => {
                if(event.code == "Backspace") { // Delete selected node or link
                    if(this.selected.type == "node" && this.selected.target !== null) {
                        this.deleteNode(this.selected.target);
                    }
                    else if(this.selected.type == "link" && this.selected.target !== null) {
                        this.deleteLink(this.selected.target);
                    }
                    this.selected = {type:"node", target:null};
                }
            });
            window.addEventListener("mouseup", event => {
                let ctx = canvas.getContext("2d");
                const offsetX = ctx.canvas.getBoundingClientRect().left;
                const offsetY = ctx.canvas.getBoundingClientRect().top;
                const x = event.clientX - offsetX;
                const y = event.clientY - offsetY;
                const node = this.getCollidedNode(x, y);
                const link = this.getCollidedLink(x, y);
                if(event.button == 0) {
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
                        this.selected.target = null;
                        this.createNode(x,y);
                    }
                }
                else if(event.button == 2) {
                    if(node !== null) {
                        if(this.creatingLink.status == 0) {
                            this.creatingLink.start = node;
                            this.creatingLink.status = 1;
                        }
                        else if(this.creatingLink.status == 1) {
                            this.creatingLink.end = node;
                            this.createLink(this.creatingLink.start, this.creatingLink.end);
                            this.creatingLink = {status:0,start:null,end:null};
                        }
                    }
                }
            });
        },
        computed: {

        },
        methods: {
            draw : function() {
                if(canvas) {
                    // Scale canvas
                    canvas.style.width ='100%';
                    canvas.style.height='80%';
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
            createNode : function(x,y) {
                this.nodes.push(new TNode(this.nextNodeId++,x,y,[],0,0));
            },
            createLink : function(from,to) {
                if(!this.linkExists(from, to)) {
                    this.links.push(new TLink(this.nextLinkId++, from, to));
                    from.addLink(to.id);
                    to.addLink(from.id);
                }
            },
            deleteNode : function(n) {
                let i = 0;
                while(i < this.links.length) {
                    if(this.links[i].from.id == n.id || this.links[i].to.id == n.id) {
                        this.links.splice(i,1);
                    }
                    else {
                        i++;
                    }
                }
                for(let j = 0; j < this.nodes.length; j++) {
                    if(this.nodes[j].id == n.id) {
                        this.nodes.splice(j,1);
                        break;
                    }
                }
            },
            deleteLink : function(l) {
                l.from.removeLink(l.to);
                l.to.removeLink(l.from);
                for(let i = 0; i < this.links.length; i++) {
                    if(this.links[i].from.id == l.from.id && this.links[i].to.id == l.to.id) {
                        this.links.splice(i,1);
                        break;
                    }
                }
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
            }
        }
    }

</script>



<style scoped>

    h1 {
        color: green;
    }

    .topology {
        position: absolute;
        height: 98%;
        width: 98%;
    }

    canvas {
        width: 100%;
        height: 80%;
    }

</style>