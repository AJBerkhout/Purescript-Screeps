"use Strict";

exports.describeExits = 
  function(roomName){
      var response = Game.map.describeExits(roomName);
      var a = response["1"];
      if (a == undefined){
        a = "";
      }
      var b = response["3"];
      if (b == undefined){
        b = "";
      }
      var c = response["5"];
      if (c == undefined){
        c = "";
      }
      var d = response["7"];
      if (d == undefined){
        d = "";
      }
      var x ={top:a, right: b, bottom: c, left: d};
      return x;
}