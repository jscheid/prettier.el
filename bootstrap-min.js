6>Number(process.version.split(/[v.]/)[1])&&process.exit(3);const a=require,b=a("zlib").gunzipSync,c=a("vm"),d=a("fs");var e=c.Script,g=Buffer,h=g.from,k=Number(process.argv[1]);const l=Buffer.alloc(k);let m=0;for(;m<k;){const f=d.readSync(0,l,m,k-m,null);if(0>=f)throw Error("EOF");m+=f}const n=new e(b(h.call(g,l.toString("ascii"),"base64")).toString("utf-8"));n.runInThisContext()(n);
