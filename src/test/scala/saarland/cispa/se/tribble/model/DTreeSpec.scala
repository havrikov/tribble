package saarland.cispa.se.tribble
package model

class DTreeSpec extends TestSpecification {

  "The leaves method of DTree" should "return the leaves in the right order" in {
/*
0            root
            /    \
1      child1    child2
       /    \    /     \
2   Hello    , World    !
*/
    val root = DNode(null, None)
    val child1 = DNode(null, Some(root))
    val child2 = DNode(null, Some(root))
    root.children(0) = child1
    root.children(1) = child2
    val leaf1 = DLeaf(null, Some(child1), "Hello")
    val leaf2 = DLeaf(null, Some(child1), ", ")
    child1.children(0) = leaf1
    child1.children(1) = leaf2
    val leaf3 = DLeaf(null, Some(child2), "World")
    val leaf4 = DLeaf(null, Some(child2), "!")
    child2.children(0) = leaf3
    child2.children(1) = leaf4

    root.depth() shouldEqual 2
    root.leaves.map(_.value).mkString should === ("Hello, World!")
  }

}
