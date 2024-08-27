module Calculator where
import Text.Read (readEither)

data Expr = Lit Int -- Literal numbers
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

prettyPrint :: Expr -> String
prettyPrint (Lit x) = show x 
prettyPrint expr = 
  case safeEval expr of
    Left err -> err
    Right val -> prettyPrint' expr <> " = " <> show val
  where
    paren :: String -> String
    paren str = "( " <> str <> " )"

    nest :: Expr -> String
    nest (Lit x) = show x
    nest expr = paren $ prettyPrint' expr

    prettyPrint' :: Expr -> String
    prettyPrint' expr = 
      case expr of
        Add x y -> nest x <> " + " <> nest y 
        Sub x y -> nest x <> " - " <> nest y
        Mul x y -> nest x <> " * " <> nest y
        Div x y -> nest x <> " / " <> nest y 

eval :: Expr -> Int
eval expr =
  case expr of
    Lit num -> num
    Add arg1 arg2 -> eval' (+) arg1 arg2
    Sub arg1 arg2 -> eval' (-) arg1 arg2
    Mul arg1 arg2 -> eval' (*) arg1 arg2
    Div arg1 arg2 -> eval' div arg1 arg2
  where
    eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
    eval' operator arg1 arg2 =
      operator (eval arg1) (eval arg2)

safeEval :: Expr -> Either String Int
safeEval expr =
  case expr of
    Lit num -> Right num
    Add arg1 arg2 -> safeEval' (+) arg1 arg2
    Sub arg1 arg2 -> safeEval' (-) arg1 arg2
    Mul arg1 arg2 -> safeEval' (*) arg1 arg2
    Div arg1 arg2 -> safeEval' div arg1 arg2
  where
    safeEval' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
    safeEval' operator arg1 arg2 = 
      case (safeEval arg1, safeEval arg2, operator) of
        (Right val1, Right 0, div) -> Left "division by zero"
        (Right val1, Right val2, _) -> Right (operator val1 val2)
        (Left err, _, _) -> Left err
        (_, Left err, _) -> Left err


parse :: String -> Either String Expr
parse str =
  let
    parse' :: [String] -> Either String (Expr, [String])
    parse' [] = Left "unexpected end of expression"
    parse' (token:rest) =
      case token of
        "+" -> parseBinary Add rest
        "*" -> parseBinary Mul rest
        "-" -> parseBinary Sub rest
        "/" -> parseBinary Div rest
        lit ->
          case readEither lit of
            Left err -> Left err
            Right lit' -> Right (Lit lit', rest)

    parseBinary :: (Expr -> Expr -> Expr)
                   -> [String]
                   -> Either String (Expr, [String])
    parseBinary exprConstructor args =
      case parse' args of
        Left err -> Left err
        Right (firstArg, rest') ->
          case parse' rest' of
            Left err -> Left err
            Right (secondArg, rest'') ->
              Right (exprConstructor firstArg secondArg, rest'')

  in case parse' (words str) of
    Left err -> Left err
    Right (e,[]) -> Right e
    Right (_, rest) -> Left $ "Found extra tokens: " <> unwords rest

run :: String -> String
run expr =
  case parse expr of
    Left err -> "Error: " <> err
    Right expr' ->
      let answer = show $ eval expr'
      in "The answer is: " <> answer

safeRun :: String -> String
safeRun expr =
  case parse expr of
    Left err -> "Error: " <> err
    Right expr' ->
      case safeEval expr' of
        Left err -> "Error: " <> err
        Right val -> "The answer is: " <> show val
