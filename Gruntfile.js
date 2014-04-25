module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs"
    ],
    
    clean: {
      tmp: ["tmp"],
      lib: ["output"]
    },
  
    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
  
    psc: {
      prelude: {
        options: {
          module: "Main",
          main: true
        },
        src: ["tests/Tests.purs", "<%=libFiles%>"],
        dest: "tmp/Tests.js"
      }
    },
    
    execute: {
      prelude: "tmp/Tests.js"
    }      
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");
  
  grunt.registerTask("make", ["pscMake", "dotPsci"]);
  grunt.registerTask("tests", ["clean:tmp", "psc", "execute"]);
  grunt.registerTask("default", ["make", "tests"]);
};
