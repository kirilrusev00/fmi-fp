module K3_62309_Test where

import Test.HUnit
import K3_62309

sampleTree1 :: Tree Char
sampleTree1 = Node 'a' (Node 'b' (Node 'd' EmptyTree
                                          (Node 'g' EmptyTree EmptyTree))
                                (Node 'e' EmptyTree EmptyTree))
                      (Node 'c' EmptyTree
                                (Node 'f' EmptyTree EmptyTree))

sampleTree2 :: Tree Char
sampleTree2 = Node 'a' EmptyTree EmptyTree

sampleTree3 :: Tree Char
sampleTree3 = Node 'a' (Node 'b' (Node 'd' EmptyTree EmptyTree) EmptyTree) EmptyTree

testTask1EmptyTree = "treeWords returns empty list for empty tree" ~: treeWords EmptyTree ~=? []

testTask1SampleTree1 = "treeWords returns list of words correctly for sample tree 1" ~: 
                          treeWords sampleTree1 ~?= ["abdg","abe","acf"]

testTask1SampleTree2 = "treeWords returns list of words correctly for sample tree 2" ~:
                          treeWords sampleTree2 ~?= ["a"]

testTask1SampleTree3 = "treeWords returns list of words correctly for sample tree 3" ~:
                          treeWords sampleTree3 ~?= ["abd"]

tl1 = TestList [testTask1EmptyTree, testTask1SampleTree1, testTask1SampleTree2, testTask1SampleTree3]

fIdentity :: Integral t => t -> t
fIdentity x = x

fSquare :: Integral t => t -> t
fSquare x = x * x

fThirdPower :: Integral t => t -> t
fThirdPower x = x * x * x - x * x + 5 * x + 2 + 1

testListTask2Identity = TestList [

  "mapsTo returns the ends of interval for identity function" ~:
    mapsTo fIdentity (-4) 5 ~?= (-4,5),

  "mapsTo returns interval containing only one number when a = b" ~:
    mapsTo fIdentity 3 3 ~?= (3,3)

  ]

testListTask2Square = TestList [

  "mapsTo returns interval beginning from 0 when a < 0" ~:
    mapsTo fSquare (-4) 5 ~?= (0,25),

  "mapsTo returns interval containing only one number when a = b" ~:
    mapsTo fSquare 3 3 ~?= (9,9)

  ]

testListTask2ThirdPower = TestList [

  "mapsTo returns interval beginning from 0 when a < 0" ~:
    mapsTo fThirdPower (-1) 0 ~?= (-4,3),

  "mapsTo returns interval containing only one number when a = b" ~:
    mapsTo fThirdPower 3 3 ~?= (36,36)

  ]

main = do
  runTestTT tl1
  runTestTT testListTask2Identity
  runTestTT testListTask2Square
  runTestTT testListTask2ThirdPower
