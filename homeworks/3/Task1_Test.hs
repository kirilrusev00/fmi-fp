import Task1
import Test.HUnit

emptyTree :: Tree Int
emptyTree = EmptyTree

exampleIntTree1 :: Tree Int
exampleIntTree1 = Node {value=1, left=EmptyTree, right=EmptyTree}

exampleIntTree2 :: Tree Int
exampleIntTree2 = Node {value=1, left=Node {value=2, left=Node {value=4, left=EmptyTree, right=EmptyTree},
                                                     right=Node {value=5, left=Node {value=6, left=EmptyTree, right=EmptyTree},  
                                                                          right=EmptyTree}}, 
                                 right=Node {value=3, left=Node {value=9, left=EmptyTree, right=EmptyTree}, 
                                                      right=Node {value=7, left=Node {value=8, left=EmptyTree, right=EmptyTree}, 
                                                                           right=EmptyTree}}}

exampleIntTree3 :: Tree Int
exampleIntTree3 = Node {value = 10, left = Node {value = 15, left = Node {value = 2, left = EmptyTree, right = EmptyTree}, 
                                                             right = Node {value = 7, left = EmptyTree, right = EmptyTree}},
                                    right = Node {value = 5, left = Node {value = 22, left = Node {value = 2, left = EmptyTree, right = EmptyTree},
                                                                                      right = Node {value = 6, left = EmptyTree, right = EmptyTree}},
                                                             right = Node {value = 1, left = EmptyTree,
                                                                                      right = Node {value = 3, left = Node {value = 111, left = EmptyTree, right = EmptyTree},
                                                                                                               right = EmptyTree}}}}

exampleCharTree :: Tree Char
exampleCharTree = Node {value = 'a', left = Node {value = 'b', left = EmptyTree, right = EmptyTree},
                                     right = Node {value = 'c', left = Node {value = '!', left = EmptyTree, right = EmptyTree},
                                                                right = EmptyTree}}

exampleListDoubleTree :: Tree [Double]
exampleListDoubleTree = Node {value = [2.45,5.6], left = Node {value = [254,34], left = EmptyTree, right = EmptyTree},
                                            right = Node {value = [-74.56], left = EmptyTree, 
                                                                            right = Node {value = [], left = EmptyTree, right = EmptyTree}}}

testListEmptyTree = TestList [

    "Empty tree is converted to empty list" ~: 
        (values Inorder emptyTree) ~?= []

    ]

testListIntTree1 = TestList [

    "Tree with one value returns list with the value when inorder" ~: 
        (values Inorder exampleIntTree1) ~?= [1],

    "Tree with one value returns list with the value when postorder" ~: 
        (values Postorder exampleIntTree1) ~?= [1],

    "Tree with one value returns list with the value when preorder" ~: 
        (values Preorder exampleIntTree1) ~?= [1]
    ]

testListIntTree2 = TestList [

    "Example tree 2 is converted correctly to inorder list of the values in nodes" ~: 
        (values Inorder exampleIntTree2) ~?= [4,2,6,5,1,9,3,8,7],

    "Example tree 2 is converted correctly to postorder list of the values in nodes" ~: 
        (values Postorder exampleIntTree2) ~?= [4,6,5,2,9,8,7,3,1],

    "Example tree 2 is converted correctly to preorder list of the values in nodes" ~: 
        (values Preorder exampleIntTree2) ~?= [1,2,4,5,6,3,9,7,8]
    ]

testListIntTree3 = TestList [

    "Example tree 3 is converted correctly to inorder list of the values in nodes" ~: 
        (values Inorder exampleIntTree3) ~?= [2,15,7,10,2,22,6,5,1,111,3],

    "Example tree 3 is converted correctly to postorder list of the values in nodes" ~: 
        (values Postorder exampleIntTree3) ~?= [2,7,15,2,6,22,111,3,1,5,10],

    "Example tree 3 is converted correctly to preorder list of the values in nodes" ~: 
        (values Preorder exampleIntTree3) ~?= [10,15,2,7,5,22,2,6,1,3,111]
    ]

testListCharTree = TestList [

    "Example tree with chars is converted correctly to inorder list of the values in nodes" ~: 
        (values Inorder exampleCharTree) ~?= ['b','a','!','c'],

    "Example tree with chars is converted correctly to postorder list of the values in nodes" ~: 
        (values Postorder exampleCharTree) ~?= ['b','!','c','a'],

    "Example tree with chars is converted correctly to preorder list of the values in nodes" ~: 
        (values Preorder exampleCharTree) ~?= ['a','b','c','!']

    ]

testListListDoubleTree = TestList [

    "Example tree with list of doubles is converted correctly to inorder list of the values in nodes" ~: 
        (values Inorder exampleListDoubleTree) ~?= [[254,34],[2.45,5.6],[-74.56],[]],

    "Example tree with list of doubles is converted correctly to postorder list of the values in nodes" ~: 
        (values Postorder exampleListDoubleTree) ~?= [[254,34],[],[-74.56],[2.45,5.6]],

    "Example tree with list of doubles is converted correctly to preorder list of the values in nodes" ~: 
        (values Preorder exampleListDoubleTree) ~?= [[2.45,5.6],[254,34],[-74.56],[]]

    ]

main = do
    runTestTT testListEmptyTree
    runTestTT testListIntTree1
    runTestTT testListIntTree2
    runTestTT testListIntTree3
    runTestTT testListCharTree
    runTestTT testListListDoubleTree
