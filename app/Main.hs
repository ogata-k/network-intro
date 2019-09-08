module Main where

import Data.Matrix

main :: IO ()
main = do
    let vSize = 6
    
    let adjacencyFunc (r, c)= case (r, c) of
            (1, 2) -> True
            (2, 4) -> True
            (3, 1) -> True
            (3, 2) -> True
            (3, 5) -> True
            (4, 3) -> True
            (4, 5) -> True
            (5, 6) -> True
            (6, 4) -> True
            otherwise -> False
        
    let lowerFlowFunc (r, c) = case (r, c) of
            (1, 2) -> 2
            (2, 4) -> 1
            (3, 1) -> 1
            (3, 2) -> 0
            (3, 5) -> 2
            (4, 3) -> 3
            (4, 5) -> 5
            (5, 6) -> 4
            (6, 4) -> 0
            otherwise -> 0
        
    let upperFlowFunc (r, c)= case (r, c) of
            (1, 2) -> 5
            (2, 4) -> 4
            (3, 1) -> 4
            (3, 2) -> 3
            (3, 5) -> 4
            (4, 3) -> 3
            (4, 5) -> 8
            (5, 6) -> 7
            (6, 4) -> 3
            otherwise -> 0
    
    let costFunc (r, c) = case (r, c) of
            (1, 2) -> 6
            (2, 4) -> 3
            (3, 1) -> 1
            (3, 2) -> 1
            (3, 5) -> 1
            (4, 3) -> 1
            (4, 5) -> 4
            (5, 6) -> 8
            (6, 4) -> 2
            otherwise -> 0
    
    let network = NetWork{
        vertexSize = vSize,
        adjacency = adjacencyFunc,
        lower = lowerFlowFunc,
        upper = upperFlowFunc,
        cost = costFunc
    }
    
    let flowFunc (r, c)
         = case (r, c) of
            (1, 2) -> 4
            (2, 4) -> 3
            (3, 1) -> 1
            (3, 2) -> 0
            (3, 5) -> 2
            (4, 3) -> 3
            (4, 5) -> 6
            (5, 6) -> 5
            (6, 4) -> 3
            otherwise -> 0
    
    putStrLn $ "NetWork is :\n" ++ (show network)
    putStrLn $ "target flow is :\n" ++ (show $ flowMatrix vSize adjacencyFunc flowFunc)
    putStrLn $ "is feasible ?: " ++ (show $ isFeasible network flowFunc)
    putStrLn $ "cost: " ++ (show $ calcCost network flowFunc )
    putStrLn $ "balance vector: \n" ++ (show $ getFlowRates network flowFunc)
    
{-- ------------------------------------------------------------ --}

{-- 頂点数 --}
type VertexSize = Int

{-- 行列の内部型 --}
{-- フローに使う場合は非負整数として強制的に変換する --}
type Value = Int

{-- 正方行列 --}
type SquareMatrix = Matrix Value

{-- 隣接関係 --}
type Index = Int
type Adjacency = (Index, Index) -> Bool

{-- その他補助 --}
boolAsValue :: Bool -> Value
boolAsValue True = 1
boolAsValue False = 0

{-- フロー(補助) --}
asEdgeFlow :: Adjacency -> ((Index, Index) -> Value) -> (Index, Index) -> Value
asEdgeFlow adj flowFunc indices
     = let v = (flowFunc indices) * (boolAsValue $ adj indices) in if v < 0 then 0 else v

flowMatrix :: VertexSize -> Adjacency -> ((Index, Index) -> Value) -> SquareMatrix
flowMatrix vSize adj flowFunc = matrix vSize vSize (asEdgeFlow adj flowFunc)

{-- ネットワーク --}
{-- 対象とする有向グラフは自己ループは許すが多重辺は許さない --}
data NetWork = NetWork {
    vertexSize :: VertexSize,
    adjacency :: Adjacency,
    lower :: (Index, Index) -> Value,
    upper :: (Index, Index) -> Value,
    cost :: (Index, Index) -> Value
}

instance Show NetWork where
    show nw = 
        let 
            vSize = vertexSize nw
            adjMat = adjacencyMatrix nw
            lowerMat = lowerFlowMatrix nw
            upperMat = upperFlowMatrix nw
            costMat = costMatrix nw
        in
            "NetWork{"
            ++ "\nvertex size:" ++ (show vSize)
            ++ "\nadjacency matrix:\n" ++ (show adjMat)
            ++ "\nlower flow matrix:\n" ++ (show lowerMat)
            ++ "\nupper flow matrix:\n" ++ (show upperMat)
            ++ "\ncost on adjacency matrix:\n" ++ (show costMat)
            ++ "\n}"

{-- 隣接行列 --}
adjacencyMatrix :: NetWork -> SquareMatrix
adjacencyMatrix nw = 
    let 
        vSize = vertexSize nw
        adj = boolAsValue.adjacency nw
    in
        matrix vSize vSize adj

{-- フロー（下限） --}
lowerFlowMatrix :: NetWork -> SquareMatrix
lowerFlowMatrix nw =
    let 
        vSize = vertexSize nw
        adj = adjacency nw
        flowFunc = lower nw
    in
        flowMatrix vSize adj (asEdgeFlow adj flowFunc)

{--　フロー（上限） --}
upperFlowMatrix :: NetWork -> SquareMatrix
upperFlowMatrix nw =
    let 
        vSize = vertexSize nw
        adj = adjacency nw
        flowFunc = upper nw
    in
        flowMatrix vSize adj (asEdgeFlow adj flowFunc)

{-- 許容フロー --}
isFeasible :: NetWork -> ((Index, Index) -> Value) -> Bool
isFeasible nw flow =
    let 
        flowMat = flowMatrix (vertexSize nw) (adjacency nw) flow
        lowerFlowMat = lowerFlowMatrix nw
        upperFlowMat = upperFlowMatrix nw
        lowerDiff = flowMat - lowerFlowMat
        upperDiff = upperFlowMat - flowMat
    in
        all ((<=) 0) (toList lowerDiff) && all ((<=) 0) (toList upperDiff) 

{-- コスト行列 --}
costMatrix :: NetWork -> SquareMatrix
costMatrix nw = 
    let
        costFunc :: (Index, Index) -> Value -> Value
        costFunc indices =  (*) ((cost nw) indices)
    in
        mapPos costFunc (adjacencyMatrix nw)

{-- フローのコストを計算 --}
calcCost :: NetWork -> ((Index, Index) -> Value) -> Value
calcCost nw flow =
    let
        costMat = costMatrix nw
        vSize = vertexSize nw
        adj = adjacency nw
        flowMat = flowMatrix vSize adj flow
    in 
       sum $ zipWith (*) (toList costMat) (toList flowMat)

{-- フローのバランスベクトル(つまり指定頂点の流量) --}
flowRate :: NetWork -> ((Index, Index) -> Value) -> (Index -> Value)
flowRate nw flow index
    | index <= 0 || index > vertexSize nw = 0
    | otherwise = 
        let
            vSize = vertexSize nw
            adj = adjacency nw
            flowMat = flowMatrix vSize adj flow
        in
            (sum (getRow index flowMat)) - (sum (getCol index flowMat))

{-- バランスベクトル取得 --}
getFlowRates :: NetWork -> ((Index, Index) -> Value) -> Matrix Value
getFlowRates nw flow =
    let 
        vSize = vertexSize nw
    in 
        fromList 1 vSize (map (flowRate nw flow) [1 .. vSize])
