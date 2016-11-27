package edu.cmu.cs.nodesec

import edu.cmu.cs.nodesec.analysis._


/**
  * Created by ckaestne on 11/24/16.
  */
class FileAnalysisTest extends AbstractAnalysisTest {

  import MethodCompositionAnalysis._

  test("test.js") {
    passFile("src/test/resources/test.js", noAlwaysUnresolvedFunctionCalls +
      noForbiddenGlobalObjects +
      noPrototype +
      noWriteToClosure +
      noCallToRequire)
  }
  test("parse leftpad") {
    //TODO handle toString call
    passFile("src/test/resources/leftpad.js", noCallToRequire + noWriteToClosure + noPrototype + noForbiddenGlobalObjects)
  }
  //  test("parse wordwrap") {
  //    passFile("src/test/resources/wordwrap.js",allPolicies)
  //  }
//  ignore("parse gulp") {
//    passFile("src/test/resources/gulp.js", allPolicies)
//  }
  //
  test("parse demo") {
    rejectFile("src/test/resources/demo.js", allPolicies)
  }

  test("parse albabroc") {
    passFile("src/test/resources/albabroc.js", noCallToRequire + noWriteToClosure + noPrototype + noForbiddenGlobalObjects)
  }
}
