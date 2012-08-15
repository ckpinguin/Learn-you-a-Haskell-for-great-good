import Data.List (break)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)


myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaa"
        , File "popel_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_shuriken.jpg" "zzzissshhh"
            , File "text1.txt" "bla bla blubber blah!"
            , File "skull_man.bmp" "Yikes!"
            ]
        , File "dijon_senape.doc" "nice mustard"
        , Folder "progs"
            [ File "pirates of altenburg.exe" "10 goto ship"
            , File "this_is_not_a_virus.exe" "Really, trust me"
            , Folder "source"
                [ File "best_hs_stuff.hs" "main = do print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

x -: f = f x

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
    (Folder folderName (item:items), bs)

