const nodeVersion = process["version"];
if (Number(nodeVersion["split"](/[v.]/)[1]) < 6) {
  process.exit(3);
}

const externalRequire = require;

const gunzipSync = externalRequire("zlib")["gunzipSync"];
const vm = externalRequire("vm");
const fs = externalRequire("fs");

function readFully(fd, size) {
  const buf = Buffer["alloc"](size);
  let offset = 0;
  while (offset < size) {
    const numRead = fs["readSync"](fd, buf, offset, size - offset, null);
    if (numRead <= 0) {
      throw new Error("EOF");
    }
    offset += numRead;
  }
  return buf;
}

const script = new vm["Script"](
  gunzipSync(
    Buffer.from(
      readFully(0, Number(process["argv"][1])).toString("ascii"),
      "base64"
    )
  ).toString("utf-8")
);

script["runInThisContext"]()(script);
