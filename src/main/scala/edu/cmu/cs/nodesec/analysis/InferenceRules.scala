package edu.cmu.cs.nodesec.analysis

import edu.cmu.cs.nodesec.datalog.Datalog

/**
  * Created by ckaestne on 12/18/16.
  */
object InferenceRules {

  def loadRules(datalog: Datalog): Unit = {
    //assignments, load, store
    var rules =
    "%% global inference rules\n" +
      "stack(V,O):-stack(V2,O), assign(V, V2). \n" +
      "heap(O1,F,O2):-store(V1,F,V2), stack(V1,O1), stack(V2,O2).\n" +
      "stack(V2,O2):-load(V2,V1,F),heap(O1,F,O2),stack(V1,O1).\n"

    //method composition (merge closures)
    rules +=
    "stack(V1,O):-stack(V2,O),closure2local(V1,V2).\n" +
      "stack(V1,O):-stack(V2,O),closure2local(V2,V1).\n"

    //method calls
    rules +=
    //helper: from origin OFUNID (with object representing the target TARGETOBJ and the resulting value RETVAL) to the target function TFUNID
    "call(OFUNID,TARGETVAR,RETVAR,TFUNID) :- invoke(OFUNID,TARGETVAR,RETVAR), stack(TARGETVAR, TARGETOBJ), functiondecl(TARGETOBJ, TFUNID).\n" +
    //link returned value of function call to return-value of the target function
      "assign(R, O) :- call(U1, U2, R, F), return(F, O).\n" +
    //link formal parameter to actual parameter
      "assign(A, B) :- actual(T, Z, B), call(U1, T, X, F), formal(F, Z, A).\n"


    //    rules +=
    //      """
    //        |% transitive pointsTo relation
    //        |pt(FROM, TO) :- pt(FROM, B), pt(B, TO).
    //        |
    //        |% resolved call graph edges
    //        |% from origin OFUNID (with object representing the target TARGETOBJ and the resulting value RETVAL) to the target function TFUNID
    //        |call(OFUNID,TARGETOBJ,RETVAL,TFUNID) :- invoke(OFUNID,TARGETOBJ,RETVAL), functiondecl(U, TARGETOBJ, TFUNID).
    //        |call(OFUNID,TARGETOBJ,RETVAL,TFUNID) :- invoke(OFUNID,TARGETOBJ,RETVAL), pt(TARGETOBJ, O), functiondecl(U, O, TFUNID).
    //        |
    //        |% link returned value of function call to return-value of the target function
    //        |pt(R, O) :- call(U1, U2, R, F), return(F, O).
    //        |% link formal parameter to actual parameter
    //        |pt(A, B) :- actual(T, Z, B), call(U1, T, X, F), formal(F, Z, A).
    //        |
    //        |% merge members pointing to the same obj:
    //        |pt(A, B) :- member(X, F, A), member(X, F, B).
    //        |pt(A, B) :- member(X, F, A), pt(X, Y), member(Y, F, B).
    //        |
    //        |% link scope to scope of closure
    //        |parentscope(OUTER, INNER) :- functiondecl(OUTER, U, INNER).
    //        |pt(A, B):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "closure", B).
    //        |pt(A, B):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "local", B).
    //        |
    //        |%    TODO: the following would allow writes to be propagated back, but makes everything absolutely
    //        |%    conservative by merging local and global scopes of all function that contain any other function decl.
    //        |%    for now we rather have a policy against writing to outer environments
    //        |% scope needs to be shared both directions, as inner functions can update values in outer scopes
    //        |pt(B, A):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "closure", B).
    //        |pt(B, A):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "local", B).
    //        |      """.stripMargin
    print(rules)
    datalog.loadRules(rules)
  }

}
