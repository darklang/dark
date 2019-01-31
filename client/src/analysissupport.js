var sha2 = require('sha2');

self.onmessage = function(e) {
  // encode the actual bs value
  var result = {};
  try {
    //send it
    result.analysis = darkAnalysis.performAnalysis(e.data.params);
  }
  catch (error) {
    result.error = error;
  }
  // decode resutt
  postMessage(result);
}


module.exports = {
  sha2: sha2,
};
