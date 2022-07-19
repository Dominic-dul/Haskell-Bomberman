# bomberman-server
The server is run on Haskell using Yesod as its web framework.
## Main API Functions
- POST newGame
- POST game/{gameId}

# JSON Implementation
The server and the client both use the same JsonLike implementation from Lib2 and Lib3 libraries.

# bomberman-client
bomberman-client is a client for the game called bomberman, which receives information from the server about the state of the game and renders the game to terminal.
## Main Functions 
The program consists of a main file in /app3/Main.hs, libraries in /src and a test file for the libraries in /test/Spec.hs
## /app3/Main.hs
The main interacts with the bomberman server and processes the responses from the server.
## /src/
Functions helping main to process the commands: parsing json string, converts commands to json objects and vice versa, updating state, drawing state
## /spec/Test.hs
Test file with a couple of test cases for /src/ library functions.
##### Server Message Example - CreateGame
```json
{
   "height":15,
   "uuid":"85bee0fb-a879-45eb-a1a6-9110a4a86613",
   "width":60
}
```
#### Server Message Example - Command
```json
{
   "bomb":null,
   "bomb_surrounding":null,
   "surrounding":{
      "bombermans":{
         "head":[
            1,
            1
         ],
         "tail":{
            "head":null,
            "tail":null
         }
      },
      "bricks":{
         "head":[
            8,
            7
         ],
         "tail":{
            "head":[
               8,
               3
            ],
            "tail":{
               "head":[
                  8,
                  1
               ],
               "tail":{
                  "head":[
                     6,
                     7
                  ],
                  "tail":{
                     "head":[
                        6,
                        5
                     ],
                     "tail":{
                        "head":[
                           5,
                           8
                        ],
                        "tail":{
                           "head":[
                              5,
                              4
                           ],
                           "tail":{
                              "head":[
                                 3,
                                 6
                              ],
                              "tail":{
                                 "head":[
                                    3,
                                    4
                                 ],
                                 "tail":{
                                    "head":[
                                       2,
                                       3
                                    ],
                                    "tail":{
                                       "head":[
                                          2,
                                          1
                                       ],
                                       "tail":{
                                          "head":[
                                             1,
                                             8
                                          ],
                                          "tail":{
                                             "head":[
                                                1,
                                                7
                                             ],
                                             "tail":{
                                                "head":[
                                                   1,
                                                   6
                                                ],
                                                "tail":{
                                                   "head":null,
                                                   "tail":null
                                                }
                                             }
                                          }
                                       }
                                    }
                                 }
                              }
                           }
                        }
                     }
                  }
               }
            }
         }
      },
      "gates":{
         "head":null,
         "tail":null
      },
      "ghosts":{
         "head":null,
         "tail":null
      },
      "wall":{
         "head":[
            8,
            8
         ],
         "tail":{
            "head":[
               8,
               6
            ],
            "tail":{
               "head":[
                  8,
                  4
               ],
               "tail":{
                  "head":[
                     8,
                     2
                  ],
                  "tail":{
                     "head":[
                        8,
                        0
                     ],
                     "tail":{
                        "head":[
                           7,
                           0
                        ],
                        "tail":{
                           "head":[
                              6,
                              8
                           ],
                           "tail":{
                              "head":[
                                 6,
                                 6
                              ],
                              "tail":{
                                 "head":[
                                    6,
                                    4
                                 ],
                                 "tail":{
                                    "head":[
                                       6,
                                       2
                                    ],
                                    "tail":{
                                       "head":[
                                          6,
                                          0
                                       ],
                                       "tail":{
                                          "head":[
                                             5,
                                             0
                                          ],
                                          "tail":{
                                             "head":[
                                                4,
                                                8
                                             ],
                                             "tail":{
                                                "head":[
                                                   4,
                                                   6
                                                ],
                                                "tail":{
                                                   "head":[
                                                      4,
                                                      4
                                                   ],
                                                   "tail":{
                                                      "head":[
                                                         4,
                                                         2
                                                      ],
                                                      "tail":{
                                                         "head":[
                                                            4,
                                                            0
                                                         ],
                                                         "tail":{
                                                            "head":[
                                                               3,
                                                               0
                                                            ],
                                                            "tail":{
                                                               "head":[
                                                                  2,
                                                                  8
                                                               ],
                                                               "tail":{
                                                                  "head":[
                                                                     2,
                                                                     6
                                                                  ],
                                                                  "tail":{
                                                                     "head":[
                                                                        2,
                                                                        4
                                                                     ],
                                                                     "tail":{
                                                                        "head":[
                                                                           2,
                                                                           2
                                                                        ],
                                                                        "tail":{
                                                                           "head":[
                                                                              2,
                                                                              0
                                                                           ],
                                                                           "tail":{
                                                                              "head":[
                                                                                 1,
                                                                                 0
                                                                              ],
                                                                              "tail":{
                                                                                 "head":[
                                                                                    0,
                                                                                    8
                                                                                 ],
                                                                                 "tail":{
                                                                                    "head":[
                                                                                       0,
                                                                                       7
                                                                                    ],
                                                                                    "tail":{
                                                                                       "head":[
                                                                                          0,
                                                                                          6
                                                                                       ],
                                                                                       "tail":{
                                                                                          "head":[
                                                                                             0,
                                                                                             5
                                                                                          ],
                                                                                          "tail":{
                                                                                             "head":[
                                                                                                0,
                                                                                                4
                                                                                             ],
                                                                                             "tail":{
                                                                                                "head":[
                                                                                                   0,
                                                                                                   3
                                                                                                ],
                                                                                                "tail":{
                                                                                                   "head":[
                                                                                                      0,
                                                                                                      2
                                                                                                   ],
                                                                                                   "tail":{
                                                                                                      "head":[
                                                                                                         0,
                                                                                                         1
                                                                                                      ],
                                                                                                      "tail":{
                                                                                                         "head":[
                                                                                                            0,
                                                                                                            0
                                                                                                         ],
                                                                                                         "tail":{
                                                                                                            "head":null,
                                                                                                            "tail":null
                                                                                                         }
                                                                                                      }
                                                                                                   }
                                                                                                }
                                                                                             }
                                                                                          }
                                                                                       }
                                                                                    }
                                                                                 }
                                                                              }
                                                                           }
                                                                        }
                                                                     }
                                                                  }
                                                               }
                                                            }
                                                         }
                                                      }
                                                   }
                                                }
                                             }
                                          }
                                       }
                                    }
                                 }
                              }
                           }
                        }
                     }
                  }
               }
            }
         }
      }
   }
}
```
##### State
```haskell
data Dimensions = Dimensions {
  gameWidth :: Int,
  gameHeight :: Int
  }

data Surrounding = Surrounding {
  bombermans :: [[Int]],
  bricks :: [[Int]],
  gates :: [[Int]],
  ghosts :: [[Int]],
  wall :: [[Int]]
  }

data BombSurrounding = BombSurrounding {
  bomb_bricks:: [[Int]],
  bomb_wall :: [[Int]]
  }

data CommandsResponse = CommandsResponse {
  bomb :: [[Int]],
  bomb_surrounding :: BombSurrounding,
  surrounding :: Surrounding
  }

data State = State{
  initialResponse :: CommandsResponse,
  initialData :: Dimensions
  }
  ```
## Game Render Example
![example_game_render](./img/bomberman.png)
