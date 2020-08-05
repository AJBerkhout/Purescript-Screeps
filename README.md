# Purescript-Screeps
This is an update of the existing port of the purescript screeps api. In addition, it includes a basic implementation of some code for some worker functionality

#Running The Code On Screeps

To run the code, clone the repo. Run spago bundle-app in the root directory. This will generate an index.js file.

Replace the last line of the index.js with module.exports.loop = PS["Main"].loop

Paste the contents of index.js into the main.js file of the screeps simulation found https://screeps.com/a/#!/sim
