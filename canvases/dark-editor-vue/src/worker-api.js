import Worker from 'worker-loader!./worker.js';
const worker = new Worker;

export function sendMessage(msg){
  worker.postMessage(msg);
}