// r = await fetch("https://cors-anywhere.herokuapp.com/http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz");

async function truc() {

  r = await fetch("https://cors-anywhere.herokuapp.com/http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz");
  reader = r.body.getReader();
  contentLength = +r.headers.get('Content-Length');
  console.log(contentLength);

  receivedLength = 0;
  chunks = [];
  while(true) {
    const {done, value} = await reader.read();
    if (done) {
      break;
    }
    chunks.push(value);
    receivedLength += value.length;
    console.log(`Received ${receivedLength} of ${contentLength}`);
  }

  chunksAll = new Uint8Array(receivedLength); // (4.1);
  position = 0;
  for(let chunk of chunks) {
    chunksAll.set(chunk, position); // (4.2);
    position += chunk.length;
  }

  blob = new Blob([chunksAll]);

  console.log(blob);
  ds = new DecompressionStream('gzip');
  console.log(ds);
  ds = blob.stream().pipeThrough(ds);
  console.log(ds);
  blob = await new Response(ds).blob();
  console.log(blob);
  b = new Uint8Array(await blob.arrayBuffer())
  console.log(b);
}
