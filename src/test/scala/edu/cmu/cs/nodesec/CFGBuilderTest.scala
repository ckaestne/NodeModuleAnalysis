package edu.cmu.cs.nodesec

import java.io.ByteArrayInputStream

import edu.cmu.cs.nodesec.analysis.CFGBuilder
import edu.cmu.cs.nodesec.parser.{CompoundStmt, JSASTParser}
import org.scalatest.FunSuite

/**
  * Created by ckaestne on 12/3/16.
  */
class CFGBuilderTest extends FunSuite {

  test("basics") {
    p("a();")
    p("{a();b();}")
    p("var x; x=y(); z();")
    p("x(); if(y()) z();")
    p("x(); if(y()) {a();b();c();}")
    p("x(); if(a()) {y();{z1();z2();}} else bar();")
    p("x(); if(a()) y(); else if (b()) c(); else bar(); z();")
    p("x(); while (a()) y(); z();")
    p("x(); do y(); while (a()); z();")
    p("x(); for (var a=1; a<5; a++) foo();")
    //    p("function f(){x(); return y; z();}")
    p("x(); throw y; z();")
    p("begin(); while(x()) { y(); continue; z();} end();")
    p("begin(); while(x()) { y(); break; z();} end();")
    p("begin(); while(x()) { y(); if (a()) continue; z();} end();")
    p("begin(); while(x()) { y(); if (a()) break; z();} end();")
    p("begin(); while(a()) while(x()) { y(); if (a()) break; z();} end();")
  }
  test("exceptions") {
    p("x(); if (a) throw y; z();")
    p("try { x(); if (a) throw y; z(); } catch (e) {} foo();")
    p("try { x(); if (a) throw y; z(); } catch (e) { bar(); throw e; } foo();")
    p("try { x(); if (a) throw y; z(); } finally { bar(); } foo();")
    p("try { " +
      " try { x(); if (a) throw y; } finally { bar(); } " +
      "} catch (e){ foo();} xx(); ")
    p("try { " +
      " try { x(); if (a) throw y; } finally { bar(); } " +
      "} catch (e){ foo(); throw e; } xx(); ")
    p("try { x(); if (a) throw y; z(); } catch (e) { bar(); throw e; } finally { foo();} end();")
  }

  def p(s: String) = {
    import scala.sys.process._

    val json =
      Process("node esprint/parse.js").#<(new ByteArrayInputStream(s.getBytes("UTF-8"))).lineStream_!.head
    val r= new JSASTParser().parse(json, s)

//    val p = new JSParser()
//    val r = p.parseAll(p.Program, s)

    println(CFGBuilder.buildCFG(CompoundStmt(r.inner)).toDot())
  }

}
