"use Strict";

exports.describeExits2 = 
  function(roomName){
    return function(toMaybe){
      var response = Game.map.describeExits(roomName);
      var a = toMaybe(response["1"]);
      var b = toMaybe(response["3"]);
      var c = toMaybe(response["5"]);
      var d = toMaybe(response["7"]);
      return {top:a, left: b, right: c, bottom: d}
    }
}