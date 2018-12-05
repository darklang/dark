var sha2 = require('sha2');

self.onmessage = function(e) {
  var result = {};
  try {
    result.analysis = darkAnalysis.performAnalysis(e.data.params);
  }
  catch (error) {
    result.error = error;
  }
  postMessage(result);
}


module.exports = {
  sha2: sha2,
};
