import u from "@/js/utils";

export default class Link {
  constructor(worldMap, timestamp, beginNode, endNode) {
    this.worldMap = worldMap;
    this.timestamp = timestamp;
    this.beginNode = beginNode;
    this.endNode = endNode;
    this.selected = false;
    this.messages = []; // content format = {start, end, blockId, type, owner, msg-data}
  }

  draw(ctx, timestamp) {
    this._drawMessageExchange(ctx, timestamp);
    //this._drawLink(ctx);
    //this._drawFlow(ctx, timestamp);
  }

  _drawMessageExchange(ctx, timestamp) {
    for(let m of this.messages) {
      if(timestamp >= m.start && timestamp <= m.end) {
        // if there is a message being exchanged within this period, draw the link
        const beginPos = this.worldMap.latLngToPixel(
          this.beginNode.latitude,
          this.beginNode.longitude
        );
        const endPos = this.worldMap.latLngToPixel(
          this.endNode.latitude,
          this.endNode.longitude
        );
        ctx.beginPath();
        ctx.moveTo(beginPos.x, beginPos.y);
        ctx.lineTo(endPos.x, endPos.y);
        ctx.lineWidth = this.selected ? 1 : 0.25;
        const blockId = m.blockId;
        const c = u.colorForId(blockId);
        ctx.strokeStyle = `rgba(${c.r}, ${c.g}, ${c.b}, ${0.8})`;
        ctx.stroke();
        ctx.closePath();
        break;
      }
      else if(timestamp < m.start) {
        return;
      }
    }
  }

  _drawLink(ctx) {
    const beginPos = this.worldMap.latLngToPixel(
      this.beginNode.latitude,
      this.beginNode.longitude
    );
    const endPos = this.worldMap.latLngToPixel(
      this.endNode.latitude,
      this.endNode.longitude
    );
    ctx.beginPath();
    ctx.moveTo(beginPos.x, beginPos.y);
    ctx.lineTo(endPos.x, endPos.y);
    ctx.lineWidth = 0.5;
    ctx.strokeStyle = "rgba(0.8, 0.8, 0.8, 0.02)";
    ctx.stroke();
    ctx.closePath();
  }

  _drawFlow(ctx, timestamp) {
    const beginBlock = this.beginNode.getBlock(timestamp);
    const endBlock = this.endNode.getNextBlock(timestamp);
    if (beginBlock === null || endBlock === null) {
      return;
    }
    // beginNode -> endNode
    let isFlowing =
      timestamp >= endBlock.sendingTimestamp &&
      timestamp <= endBlock.receivingTimestamp &&
      this.beginNode.id === endBlock.fromNode.id;
    let isJustRecepted =
      timestamp === endBlock.receivingTimestamp &&
      this.beginNode.id === endBlock.fromNode.id;
    if (!isFlowing) {
      return;
    }
    const beginPos = this.worldMap.latLngToPixel(
      this.beginNode.latitude,
      this.beginNode.longitude
    );
    const endPos = this.worldMap.latLngToPixel(
      this.endNode.latitude,
      this.endNode.longitude
    );
    const strokeColor = this.endNode.getStrokeColor(
      endBlock.receivingTimestamp
    );
    ctx.beginPath();
    ctx.moveTo(beginPos.x, beginPos.y);
    ctx.lineTo(endPos.x, endPos.y);
    ctx.lineWidth = isJustRecepted ? 3 : 1;
    ctx.strokeStyle = `rgba(${strokeColor.r}, ${strokeColor.g}, ${
      strokeColor.b
    }, ${strokeColor.a * 0.8})`;
    ctx.stroke();
    ctx.closePath();
  }

  select() {
    this.selected = true;
  }

  unselect() {
    this.selected = false;
  }

  collide(mouseX, mouseY, timestamp) {
    const threshold = 3;
    const A = this.worldMap.latLngToPixel(this.beginNode.latitude, this.beginNode.longitude);
    const B = this.worldMap.latLngToPixel(this.endNode.latitude, this.endNode.longitude);
    const P = {x:mouseX, y:mouseY};
    if(this.distToSegment(P, A, B) < threshold) {
      // Only return true if the line is visible (this is checked last because it is more resource intensive)
      for(let m of this.messages) {
        if(timestamp >= m.start && timestamp <= m.end) {
          return true;
        }
      }
    }
  }

  log(timestamp) {
    let currentMessages = [];
    let msgCount = 0;
    let latencySum = 0;
    for(let m of this.messages) {
      if(timestamp >= m.start && timestamp <= m.end) {
        msgCount++;
        latencySum = latencySum + (m.end - m.start);
        currentMessages.push({blockId:m.blockId, msgData:m.content});
      }
      if(timestamp < m.start){
        break;
      }
    }
    console.log({
      type: "LINK",
      from: this.beginNode,
      to: this.endNode,
      content: currentMessages,
      avgLatency: (latencySum / msgCount)
    });
  }





  // below code is from https://stackoverflow.com/a/1501725
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
  /////////
}
