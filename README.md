# idris-canvas
Idris bindings to and abstractions over the JavaScript Canvas API

## Compiling the Demo
This is a bit involved for now (.ipkg coming soon!), but here are the instructions:
  1. Make sure you have the dependencies -- currently the only one that doesn't come with Idris is
  [IdrisScript](http://github.com/idris-hackers/IdrisScript). To install that package, clone the
  repository and run `idris --install /path/to/IdrisScript/idrisscript.ipkg`.
  2. Compile the demo. This command is a bit of a mouthful:
  
    ```
   idris Demo.idr --codegen javascript -p effects -p idrisscript -o demo/main.js
    ```
  3. Visit `/path/to/idris-canvas/demo/main.html` in your browser!
