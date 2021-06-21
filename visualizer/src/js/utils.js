export default {
    // t ∈ [0, 1]
    getColor: t => {
      const f = theta => Math.sin(theta + t * Math.PI * 2) * 0.5 + 0.5;
      return {
        r: Math.floor(f((-1 * Math.PI * 2) / 3) * 256),
        g: Math.floor(f((0 * Math.PI * 2) / 3) * 256),
        b: Math.floor(f((1 * Math.PI * 2) / 3) * 256),
        a: 1
      };
    },
  
    uniq: xs => {
      const ys = [];
      for (const x of xs) {
        if (ys.length > 0 && ys[ys.length - 1] === x) {
          continue;
        }
        ys.push(x);
      }
      return ys;
    },
  
    colorForId: (id) => {
      if(id == -1) {
        return {r:100, g:100, b:100};
      }
      // This code snippet was published by Adam Cole on 2011-Sept-14
      // in StackOverflow: https://stackoverflow.com/a/7419630
      var r, g, b;
      var h = id / 10;
      var i = ~~(h * 6);
      var f = h * 6 - i;
      var q = 1 - f;
      switch(i % 6){
          case 0: r = 1; g = f; b = 0; break;
          case 1: r = q; g = 1; b = 0; break;
          case 2: r = 0; g = 1; b = f; break;
          case 3: r = 0; g = q; b = 1; break;
          case 4: r = f; g = 0; b = 1; break;
          case 5: r = 1; g = 0; b = q; break;
      }
      var c = {r:r*255, g:g*255, b:b*255}
      return (c);
    }
  };
  