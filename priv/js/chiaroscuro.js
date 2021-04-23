const get_memory = () => chiaroscuro.memory;

var chiaroscuro = {
    imports: {
        env: {
            // exported functions
            abort(msg, file, line, column) {
                console.error("abort(" + msg + ")@" + file + " " + line + ":" + column);
            },
            jsConsoleLogInt(int) {
                console.log("int:" + int)
            },
            jsConsoleLogStr(ptr, len) {
                var bytes = new Int8Array(get_memory().buffer, ptr, len);
                console.log(new TextDecoder().decode(bytes));
            }
        },
    },
    exports: undefined,
    memory: undefined,
    modules: {}
}

// CHIAROSCURO LOADING PIPELINE
//

const load_chiaroscuro = (response) => {
    if (response.ok) return response.arrayBuffer();
    console.error("failed to load chiaroscuro: " +
      response.status + " " +
      response.statusText);
    throw "error";
}

const instantiate_chiaroscuro = (bytes) =>
    WebAssembly.instantiate(bytes, chiaroscuro.imports);

const rebind_exports = (wasm_instance) => {
    chiaroscuro.exports =
        wasm_instance.instance.exports;
    chiaroscuro.memory = wasm_instance.instance.exports.memory;
}

// MODULE LOADING PIPELINE
//

const load_module = (module) => {
    fetch("modules/" + beam_filename(module))
        .then(load_beam_module)
        .then(instantiate_module)
    chiaroscuro.modules[module] = "foobar"
    window[module] = chiaroscuro.modules[module]
}

const load_beam_module = (response) => {
    if (response.ok) return response.arrayBuffer();
    basename = response.url.replace(/\\/g,'/').replace( /.*\//, '' );
    console.error("failed to load module " +
      basename + ": " +
      response.status + " " +
      response.statusText);
    throw "error";
}

const instantiate_module = (bytes) => {
    length = bytes.byteLength;
    addr = chiaroscuro.exports.allocate(length);
    buffer = chiaroscuro.memory.buffer;
    // transfer contents from the module bytes to the new thing
    new Uint8Array(buffer, length, addr).set(new Uint8Array(bytes), 0)
    // triggers processing the module into memory.
    chiaroscuro.exports.instantiate(addr, length);
}

const beam_filename = (name) => {
    if (/[A-Z]/.test(name[0])) {
        return "Elixir." + name + ".beam"
    } else {
        return name + ".beam"
    }
}

chiaroscuro.load = load_module;

// run this part on load.
fetch('wasm/chiaroscuro.wasm')
    .then(load_chiaroscuro)
    .then(instantiate_chiaroscuro)
    .then(rebind_exports)