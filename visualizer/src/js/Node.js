import u from "@/js/utils";

export default class Node {
  constructor(worldMap, timestamp, id, region) {
    this.worldMap = worldMap;
    this.timestamp = timestamp;
    this.id = id;
    this.region = region;
    this.blockList = [];
    this.inCommittee = []; // list of rounds where this node was in the committee
    this.proposing = []; // list of rounds where this node proposes the block
    this.selected = false;
    this.initPosition();
  }

  select() {
    this.selected = true;
  }

  unselect() {
    this.selected = false;
  }

  initPosition() {
    const pos = this.region.getRandomPosition();
    this.latitude = pos.latitude;
    this.longitude = pos.longitude;
  }

  draw(ctx, timestamp) {
    const block = this.getBlock(timestamp);
    const height = this.getBlockHeight(timestamp);
    const blockId = (block === null ? -1 : block.id);
    const pos = this.worldMap.latLngToPixel(this.latitude, this.longitude);
    const c = u.colorForId(blockId);
    ctx.beginPath();
    ctx.arc(pos.x, pos.y, this.getRadius(timestamp), 0, Math.PI * 2, false);
    ctx.fillStyle = `rgba(${c.r}, ${c.g}, ${c.b}, ${0.5})`;
    ctx.fill();
    ctx.lineWidth = 0.5;
    ctx.strokeStyle = `rgba(${c.r}, ${c.g}, ${c.b}, ${1})`;
    ctx.stroke();
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    ctx.beginPath();
    ctx.fillStyle = "black";
    ctx.strokeStyle = "black";
    let letter = "";
    if(window.PROTOCOL == 'POW') {
      if(block != null) {
        if(block.ownerNode.id == this.id) {
          letter = "M";
        }
      }
    }
    else if(window.PROTOCOL == 'POS') {
      if(this.proposing.includes(height)) {
        letter = "P";
      }
      else if(this.inCommittee.includes(height)) {
        letter = "C";
      }
    }
    ctx.fillText(letter, pos.x, pos.y);
    ctx.strokeText(letter, pos.x, pos.y);
    ctx.fill();
    ctx.stroke();
    ctx.closePath();
  }

  addCommitteeMembership(round) {
    if(!this.inCommittee.includes(round)) {
      this.inCommittee.push(round);
    }
  }

  addProposerRound(round) {
    if(!this.proposing.includes(round)) {
      this.proposing.push(round);
    }
  }

  collide(mouseX, mouseY, timestamp) {
    const pos = this.worldMap.latLngToPixel(this.latitude, this.longitude);
    const dSq =
      (pos.x - mouseX) * (pos.x - mouseX) + (pos.y - mouseY) * (pos.y - mouseY);
    const r = this.getRadius(timestamp);
    return dSq < r * r;
  }

  getRadius(timestamp) {
    const block = this.getBlock(timestamp);
    return (
      5 * (this.selected ? 1.5 : 1.2)
    );
  }

  log(timestamp) {
    let p = false;
    if(window.PROTOCOL == "POS") {
      p = this.isProposer(timestamp);
    }
    else {
      const b = this.getBlock(timestamp);
      if(b != null) {
        p = (b.ownerNode.id == this.id);
      }
    }
    console.log({
      id: this.id,
      region: this.region.name,
      currentStatistics: {
        chainHead: this.getBlock(timestamp),
        inCommittee: this.committeeMember(timestamp),
        proposer: p
      },
      globalStatistics:{
        committeeParticipations: this.inCommittee.length,
        proposalsCreated: this.proposing.length,
        totalNumberOfBlocks: this.blockList.length
      }
    });
  }

  committeeMember(timestamp) {
    const b = this.getBlock(timestamp);
    if(b == null) {
      return this.inCommittee.includes(1);
    }
    let round = 0;
    for(let block of this.blockList) {
      round++;
      if(block.id == b.id) {
        return this.inCommittee.includes(round);
      }
    }
    return false;
  }

  isProposer(timestamp) {
    const b = this.getBlock(timestamp);
    if(b == null) {
      return this.proposing.includes(1);
    }
    let round = 0;
    for(let block of this.blockList) {
      round++;
      if(block.id == b.id) {
        return this.proposing.includes(round);
      }
    }
    return false;
  }

  getFillColor(timestamp) {
    const block = this.getBlock(timestamp);
    const blockId = (block === null ? -1 : block.id) + 1;
    const color = u.getColor((blockId + (this.selected ? 0.1 : 0.0)) * 0.23);
    const alpha = this.selected ? 0.9 : 0.5;
    return { r: color.r, g: color.g, b: color.b, a: alpha };
  }

  getStrokeColor(timestamp) {
    const block = this.getBlock(timestamp);
    const blockId = (block === null ? -1 : block.id) + 1;
    const color = u.getColor((blockId + (this.selected ? 0.1 : 0.0)) * 0.23);
    const alpha = this.selected ? 1.0 : 0.8;
    return { r: color.r, g: color.g, b: color.b, a: alpha };
  }

  getBlock(timestamp) {
    let result = null;
    for (const block of this.blockList) {
      if (block.receivingTimestamp > timestamp) {
        continue;
      }
      if (
        result === null ||
        block.receivingTimestamp > result.receivingTimestamp
      ) {
        result = block;
      }
    }
    return result;
  }

  getBlockHeight(timestamp) {
    let result = 0;
    for (const block of this.blockList) {
      if(block.receivingTimestamp > timestamp) {
        continue;
      }
      else {
        result++;
      }
    }
    return result;
  }

  getNextBlock(timestamp) {
    for (const block of this.blockList) {
      if (block.receivingTimestamp >= timestamp) {
        return block;
      }
    }
    return null;
  }

  isMiner(block) {
    return block !== null && block.ownerNode.id === this.id;
  }
}
