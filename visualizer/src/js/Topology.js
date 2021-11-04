import u from "@/js/utils";

function color(id) {
    let c = u.colorForId(id);
    return "rgba("+c.r+","+c.g+","+c.b+",0.7)";
}

const radius = 25;
const selectedRadius = 30;

export class TLink {

    constructor(id, from, to) {
        this.id   = id;
        this.from = from;
        this.to   = to;
        this.selected = false;
    }

    show(ctx) {
        ctx.beginPath();
        ctx.fillStyle = 'rgb(0,0,0)';
        ctx.lineWidth = this.selected ? 2 : 1;
        ctx.moveTo(this.from.x,this.from.y);
        ctx.lineTo(this.to.x,this.to.y);
        ctx.stroke();
        ctx.closePath();
    }

    linkedToNode(nodeId) {
        return this.from.id == nodeId || this.to.id == nodeId;
    }

    select() {
        this.selected = true;
    }

    unselect() {
        this.selected = false;
    }

    collide(mouseX, mouseY) {
        const threshold = 5;
        const P = {x:mouseX, y:mouseY};
        const A = {x:this.from.x, y:this.from.y};
        const B = {x:this.to.x, y:this.to.y};
        return this.distToSegment(P, A, B) < threshold;
    }

    sqr(x) { return x*x; }
    dist2(v, w) { return this.sqr(v.x - w.x) + this.sqr(v.y - w.y); }
    distToSegmentSquared(p, v, w) {
        let l2 = this.dist2(v, w);
        if (l2 == 0) { return this.dist2(p,v); }
        let t = ((p.x - v.x) * (w.x - v.x) + (p.y - v.y) * (w.y - v.y)) / l2;
        t = Math.max(0, Math.min(1, t));
        return this.dist2(p, {x:v.x+t*(w.x-v.x), y:v.y+t*(w.y-v.y)});
    }
    distToSegment(p, v, w) { return Math.sqrt(this.distToSegmentSquared(p, v, w)); }

}

export class TNode {

    constructor(id, x, y, links, region, hPower, stake, offline, malicious) {
        this.id        = id;
        this.x         = x;
        this.y         = y;
        this.links     = links;
        this.region    = region;
        this.hPower    = hPower;
        this.stake     = stake;
        this.selected  = false;
        this.offline   = offline;
        this.malicious = malicious;
    }

    show(ctx) {
        ctx.beginPath();
        ctx.fillStyle = 'rgb(0,0,0)';
        let r = this.selected ? selectedRadius : radius;
        ctx.arc(this.x, this.y, r+2, 0, 2 * Math.PI);
        ctx.fill();
        ctx.closePath();
        ctx.beginPath();
        ctx.fillStyle = color(this.region);
        ctx.arc(this.x, this.y, r, 0, 2 * Math.PI);
        ctx.fill();
        ctx.font = '24px serif';
        ctx.fillStyle = 'rgb(0,0,0)';
        ctx.fillText(""+this.id,this.x-5, this.y+6);
        ctx.closePath();
    }

    select() {
        this.selected = true;
    }

    unselect() {
        this.selected = false;
    }

    addLink(link) {
        this.links.push(link);
    }

    removeLink(target) {
        for(let i = 0; i < this.links.length; i++) {
            if(this.links[i] == target) {
                this.links.splice(i,1);
                break;
            }
        }
    }

    decrementIds(removedId) {
        for(let i = 0; i < this.links.length; i++) {
            if(this.links[i] > removedId) {
                this.links[i] = this.links[i] - 1;
            }
        }
    }

    collide(mouseX, mouseY) {
        const dSq =
            (this.x - mouseX) * (this.x - mouseX) + (this.y - mouseY) * (this.y - mouseY);
        return dSq < (radius * radius);
    }

    toJSON() {
        let nodedata = {id:this.id, region:this.region, stake:this.stake, hPower:this.hPower, links:this.links, offline:this.offline, malicious:this.malicious};
        let guidata  = {id:this.id, pos:{x:this.x, y:this.y}};
        return {node:nodedata, gui:guidata};
    }

}