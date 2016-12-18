package edu.cmu.cs.nodesec


/**
  * Created by ckaestne on 11/24/16.
  */
class FileAnalysisTest extends AbstractAnalysisTest {

  import edu.cmu.cs.nodesec.analysis.Policies._

  test("test.js") {
    passFile("src/test/resources/test.js", allPolicies)
  }
  test("parse leftpad") {
    //TODO handle toString call
    passFile("src/test/resources/leftpad.js", /*allPolicies) */ noCallToRequire + noWriteToClosure + noPrototype + noReadFromClosure)
  }
  test("parse wordwrap") {
    passFile("src/test/resources/wordwrap.js", noCallToRequire + noWriteToClosure + noPrototype + noReadFromClosure)
  }
  test("parse gulp") {
    rejectFile("src/test/resources/gulp.js", noCallToRequire)
  }

  test("parse demo") {
    rejectFile("src/test/resources/demo.js", allPolicies)
  }

  test("parse albabroc") {
    passFile("src/test/resources/albabroc.js", allPolicies)
  }
}
