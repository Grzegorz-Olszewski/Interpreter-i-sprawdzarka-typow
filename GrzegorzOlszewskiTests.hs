-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Tests gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module GrzegorzOlszewskiTests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import DataTypes

-- Lista testów do zadania
-- Należy uzupełnić jej definicję swoimi testami
tests :: [Test]
tests =
  [  Test "cantReturnBool2" (SrcString "10 <> 11 and not true") TypeError
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "minus_bool" (SrcString "input a in if - true then 1 else 0") TypeError
  ,Test "cantReturnBool1" (SrcString "5 <= 0 and 3") TypeError
  , Test "type_TandF"           (SrcString "input u in true")                TypeError
  , Test "type_mod"             (SrcString "true mod (not false)")                   TypeError
  , Test "type_badReturn"       (SrcString "input x in false")                     TypeError
  , Test "type_undefVar"        (SrcString "x")                                    TypeError
  , Test "type_let"             (SrcString "let x = 6 in true")                    TypeError
  , Test "type_let2"            (SrcString "let x = not 6 in 5")                   TypeError
  , Test "type_if"              (SrcString "if 2 + 2 then 3 else 6")               TypeError
  , Test "type_if2"             (SrcString "if true or 2 + 2 then 3 else 6")       TypeError
  , Test "type_ifNeg"           (SrcString "input x in if not x then 1 else 2")    TypeError
  , Test "type_add"             (SrcString "input x in x + true")                  TypeError
  , Test "nestedExpError" (SrcString "let x = true in let y = 5 + 2 in if not x then y else x") TypeError
    , Test "arithmetic_with_bool_unary" (SrcString "not 100*2 + 3") TypeError

  , Test "arithmetic_with_bool_binary" (SrcString "(200*3 or 3) and 5") TypeError

  , Test "variables_with_undefined" (SrcString "input x y in x*y + z") TypeError

  , Test "is_number_true" (SrcString "if 5 then 5 else 5") TypeError

  , Test "number_as_bool" (SrcString "input a in if a or true then a else a + 1") TypeError

  , Test "equality_operators_with_bool" (SrcString "if true <> false then 1 else 0") TypeError,
  Test "numberAsBool" (SrcString "if 14 then 1997 else 88") TypeError,

  Test "numberAsBool2" (SrcString "if 8 and true then 3 else 4") TypeError,

  Test "numberAsBool3" (SrcString "if 7 or false then 3 else 4") TypeError,

  Test "boolAsNumber" (SrcString "true") TypeError

  , Test "if_Er"     (SrcString "input x in if 13 then true else x * 150") TypeError

  , Test "let_Er"      (SrcString "input x x1 in let x = x1 * 3 in x2") TypeError

  , Test "bool_Er"      (SrcString "input x in if true then true else x") TypeError

  , Test "bool"      (SrcString "input x in if true then x else false") TypeError

  , Test "errorWithBoolInIf"     (SrcString "input x in if 11 then x * 22 else x * 33") TypeError
  , Test "undefinedVariables"      (SrcString "input x y in x*y + z") TypeError

  , Test "notValidReturnThen"      (SrcString "input x in if true then true else x * 2") TypeError

  , Test "orAppliedToNumberAndBool" (SrcString "input a in if a or true then a else a") TypeError 

  , Test "errorWithBoolInIf" (SrcString "if 1 then 2 else 3") TypeError

  , Test "usingUndefinedVariable" (SrcString "input x y in x + y + z") TypeError

  , Test "mixedBoolAndIntegerOperator" (SrcString "input a in if - true then 1 else 0") TypeError

  , Test "undefVar" (SrcString "x")                TypeError

  , Test "smplTypeTest8" (SrcString "2 + true")        TypeError

  , Test "smplTypeTest9" (SrcString "input x in if 5 then false else x")

                                                   TypeError

  , Test "smplTypeTest10" (SrcString "input x in x + y")

                                                   TypeError

  , Test "smplTypeLet11" (SrcString "input z in let x = z + y in x")

                                                   TypeError

  , Test "smplTypeIf12" (SrcString "input x y in if x = y then x else false")

                                                   TypeError

  , Test "smplTypeTest22" (SrcString "let x = 2 < 1 in x")

                                                   TypeError

  , Test "simple_inc_1" (SrcString "true") TypeError,   Test "simple_inc_2" (SrcString "false") TypeError

  , Test "simple_inc_3" (SrcString "true or false") TypeError

  , Test "simple_inc_4" (SrcString "2 + true") TypeError,   Test "simple_inc_5" (SrcString "not 2") TypeError

  , Test "cmp_inc_1" (SrcString "true and not false or false or true or false and not true or false") TypeError

  , Test "cmp_inc_2" (SrcString "true + true or true") TypeError

  , Test "cmp_inc_3" (SrcString "2 or 1 and 5") TypeError

  , Test "cmp_inc_4" (SrcString "true and false + 2") TypeError

  , Test "cmp_inc_5" (SrcString "true div true") TypeError

  , Test "undefVar" (SrcString "x")                TypeError


  , Test "TypeError1" (SrcString "let x = true in x") TypeError
  , Test "TypeError2" (SrcString "x ") TypeError
  , Test "TypeError3" (SrcString "if false or 0 then 1 else 0") TypeError
  , Test "TypeError4" (SrcString "1 + true") TypeError
  , Test "TypeError5" (SrcString "4 or 1") TypeError
  , Test "TypeError6" (SrcString "2 and 5") TypeError
  , Test "TypeError7" (SrcString "if 1 = 1 then false else 0") TypeError
  , Test "TypeError8" (SrcString "if 1 = 1 then false else true") TypeError
  , Test "TypeError9" (SrcString "if 1 then 1 else 0") TypeError
  , Test "TypeError10" (SrcString "if 1 = false then 1 else 0") TypeError
  , Test "TypeError11" (SrcString "if if 1 = 1 then 1 else 2 then 1 else 0") TypeError
  , Test "TypeError12" (SrcString "if true = true then 1 else 0") TypeError
  , Test "num" (SrcString "-4") (Eval [] (Value (-4)))
  , Test "mathExp1" (SrcString "input x in 2 + 3 * 4 div x") (Eval [4] (Value 5))
  , Test "mathExp2" (SrcString "input x in (-x) mod 2 ") (Eval [6] (Value 0)) 
  , Test "mathExp3" (SrcString "input x in 4-(-x)") (Eval [4] (Value 8)) 
  , Test "letExp" (SrcString "input y in let x = 3 in x + y ") (Eval [2] (Value 5))
  , Test "ifExp1" (SrcString "input x y in if x <> y then y else x") (Eval [4,60] (Value 60))
  , Test "ifExp2" (SrcString "input x y in if not 2<1 then x*y else x div y") (Eval [100,10] (Value 1000) ) 
  , Test "ifExp3" (SrcString "input x y in if x*x = x + x and x > y then x+y else y+2") (Eval [2,1] (Value 3)) 
  , Test "nestedExp" (SrcString "let x = 5 in let y = x + 2 in if x = 5 and y > x then x + y else y") (Eval [] (Value 12))
  , Test "trickyExp" (SrcString "let x = 10 in if true then x else x div 0") (Eval [] (Value 10))
  , Test "notTrickyError" (SrcString "let x = 10 in if false then x else x mod 0")  (Eval [] RuntimeError)
  ,Test "double_If" (SrcString "input x in if if x = 42 then true else false then 42 else x") (Eval [42] (Value 42))
  , Test "triple_If" (SrcString "input x in if if x = 42 then true else false then if true then 42 else 42 + x else x") (Eval [42] (Value 42))
  , Test "nestedIfElse" (SrcString "input x in if if x >= 1 then true else false then if true then x * 3 else 42 + x else x") (Eval [1] (Value 3))
  , Test "cokolwiek" (SrcString "if not (if false then false else true) then 1 else 2") (Eval [] (Value 2))
  , Test "warunkizagniezdzone3" (SrcString "if (if 1 = 1 then 1 else 0) = (if false then 1 else 0) then 1 else 0") (Eval [] (Value 0))
  , Test "warunkizagniezdzone1" (SrcString "if (if 1 = 1 then false else true) then 1 else 0") (Eval [] (Value 0))
  , Test "warunkizagniezdzone2" (SrcString "if (if 1 = 1 then false else true) and (if 1 = 1 then false else true) then 1 else 0") (Eval [] (Value 0))
  ,Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "lazyIf_with_vars"   (SrcString "input x in if true then x + 5 else 5 div 0") (Eval [5] (Value 10))
  , Test "lazyIf"   (SrcString "if true then 1 else 1 div 0") (Eval [] (Value 1))
  , Test "lazyIf_with_false" (SrcString "if false then 5 div 0 else 1") (Eval [] (Value 1))
  , Test "let_test" (SrcString "input x in let t = 5 in t*t + x") (Eval [17] (Value 42))
  , Test "tricky_arithmetic" (SrcString "10 + -7") (Eval [] (Value 3))
  , Test "lots_of_minuses" (SrcString "10 - - - - 7") (Eval [] (Value 17))
  , Test "proper_or" (SrcString "input a in if a > 0 or a < 0 then 14*3 else 42 div 7") (Eval [-10] (Value 42))
  , Test "proper_and" (SrcString "input a b in if a < 0 and b >= 0 then 2*2 else 5 + 5") (Eval [1, 1] (Value 10))
  , Test "plus_associativity" (SrcString "input a b in 5 + b + a + 2 + 1") (Eval [3,4] (Value 15))
  , Test "or_associativity" (SrcString "if false or false or false or true then 1 else 0") (Eval [] (Value 1))
  , Test "variable_used_more_than_once" (SrcString "input a b in if a > 0 then a*b + a*5 + a else b*a + b*5 + b") (Eval [1,1000] (Value 1006))
  , Test "unused_variable" (SrcString "input a b in a*2") (Eval [5,2031] (Value 10)) 
  , Test "basic_arithmetic" (SrcString "100*2 + 10*5 + 6") (Eval [] (Value 256))
  , Test "more_variables" (SrcString "input x y z in x + y + z") (Eval [1,2,3] (Value 6))
  , Test "proper_order_of_let" (SrcString "let x = 5 in let x = x + 10 in x") (Eval [] (Value 15))
  , Test "normal_if" (SrcString "input var in if var > 1 then 10 else 5 + var") (Eval [0] (Value 5))
  , Test "trivial_associativity" (SrcString "input x in x + x * x") (Eval [2] (Value 6))
  , Test "negation" (SrcString "input x in if not false then 5 + x else 5 - x") (Eval [1] (Value 6))
  , Test "double_negation" (SrcString "input x in if not not false then 5 + x else 5 - x") (Eval [1] (Value 4))
  , Test "simple_arithmetic_with_parentheses" (SrcString "(2 + 3 + 4)*5 + 1") (Eval [] (Value 46))
  , Test "other_let" (SrcString "input x in let x = 2*x in 6 + x") (Eval [4] (Value 14))
  , Test "nested_let" (SrcString "let x = 2 in let x = 2*x in let x = x + 5 in let x = 3*x in x + 1") (Eval [] (Value 28))
  , Test "prac1" (SrcString "let x = 3 div 0 in 3") (Eval [] RuntimeError)
  , Test "lazyIf_dependent_from_var" (SrcString "input x in if x >= 0 then x else x div 0") (Eval [42] (Value 42))
  , Test "bool_variable" (SrcString "let y = false in if y then 2 else 3") (Eval [] (Value 3))
  , Test "changing_type" (SrcString "let x = true in let x = 3 in x") (Eval [] (Value 3))
  , Test "changing_type2" (SrcString "let x = 3 in let x = false in let x = true in if x then 1 else 1 div 0") (Eval [] (Value 1))
  ,
  Test "singleVar" (SrcString "input x in x") (Eval [69] (Value 69)),
  Test "not" (SrcString "if not true then 2 else 1") (Eval [] (Value 1)),
  Test "not2" (SrcString "if not false then 2 else 1") (Eval [] (Value 2)),
  Test "neg" (SrcString "-3") (Eval [] (Value (-3))),
  Test "and" (SrcString "if true and true then 3 else 7") (Eval [] (Value 3)),
  Test "and2" (SrcString "if true and false then 3 else 7") (Eval [] (Value 7)),
  Test "and3" (SrcString "if false and true then 3 else 7") (Eval [] (Value 7)),
  Test "and4" (SrcString "if false and false then 3 else 7") (Eval [] (Value 7)),
  Test "or" (SrcString "if true or true then 3 else 7") (Eval [] (Value 3)),
  Test "or2" (SrcString "if true or false then 3 else 7") (Eval [] (Value 3)),
  Test "or3" (SrcString "if false or true then 3 else 7") (Eval [] (Value 3)),
  Test "or4" (SrcString "if false or false then 3 else 7") (Eval [] (Value 7)),
  Test "eq" (SrcString "if 1 = 1 then 1 else 4") (Eval [] (Value 1)),
  Test "eq2" (SrcString "if 1 = 4 then 1 else 4") (Eval [] (Value 4)),
  Test "nEq" (SrcString "if 1 <> 1 then 1 else 4") (Eval [] (Value 4)),
  Test "nEq2" (SrcString "if 1 <> 4 then 1 else 4") (Eval [] (Value 1)),
  Test "LT" (SrcString "if 1 < 4 then 1 else 4") (Eval [] (Value 1)),
  Test "LT2" (SrcString "if 1 < 1 then 1 else 4") (Eval [] (Value 4)),
  Test "GT" (SrcString "if 1 > 4 then 1 else 4") (Eval [] (Value 4)),
  Test "GT2" (SrcString "if 1 > 1 then 1 else 4") (Eval [] (Value 4)),
  Test "LE" (SrcString "if 1 <= 4 then 1 else 4") (Eval [] (Value 1)),
  Test "LE2" (SrcString "if 1 <= 1 then 1 else 4") (Eval [] (Value 1)),
  Test "LE3" (SrcString "if 1 <= -1 then 1 else 4") (Eval [] (Value 4)),
  Test "GE" (SrcString "if 1 >= 4 then 1 else 4") (Eval [] (Value 4)),
  Test "GE2" (SrcString "if 1 >= 1 then 1 else 4") (Eval [] (Value 1)),
  Test "GE3" (SrcString "if 1 >= -1 then 1 else 4") (Eval [] (Value 1)),
  Test "add" (SrcString "8 + 8") (Eval [] (Value 16)),
  Test "sub" (SrcString "8 - 8") (Eval [] (Value 0)),
  Test "mul" (SrcString "8 * 8") (Eval [] (Value 64)),
  Test "div" (SrcString "8 div 8") (Eval [] (Value 1)),
  Test "mod" (SrcString "37 mod 21") (Eval [] (Value 16)),
  Test "let" (SrcString "let x = 21 in x + 14") (Eval [] (Value 35)),
  
  Test "muchInput" (SrcString "input q w e r t y u i o p a s d f g h j k l z x c v b n m in h + a + s + k + e + l + l") (Eval [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7] (Value 38)),
  
  Test "tricky" (SrcString "if true then 21 else 37 div 0") (Eval [] (Value 21)),
  Test "tricky2" (SrcString "69 + -69") (Eval [] (Value 0)),
  Test "tricky3" (SrcString "- -1337") (Eval [] (Value 1337)),
  Test "tricky4" (SrcString "- - -1337") (Eval [] (Value (-1337))),
  Test "bool" (SrcString "if 13 = 37 and true then 13 else 37") (Eval [] (Value 37)),
  Test "bool2" (SrcString "if 13 = 37 or true then 13 else 37") (Eval [] (Value 13)),
  
  Test "wrongLet" (SrcString "let x = 89 in 23") (Eval [] (Value 23))
  
  , Test "op"       (SrcString "input x x1 in x * 5 - x1 div 2") (Eval [9,16] (Value 37))
  , Test "op_Er"     (SrcString "input x in x div 0") (Eval [42] RuntimeError)
  , Test "if"       (SrcString "input x x1 in if x = 3 then 4 else x1") (Eval [3,5] (Value 4))
 
  , Test "if_Div"    (SrcString "input x x1 in if x + 88 = 42 then 42 div 0 else x1") (Eval [12, 162] (Value 162))
  , Test "let"      (SrcString "input x x1 x2 in let  x = x1 * 3 in x2") (Eval [3,1,5] (Value 5))
  , Test "div"      (SrcString "input x in if x div 2 = 4 then x else 0") (Eval [8] (Value 8))
 
  , Test "mult_Add"      (SrcString "input x x1 x2 x3 x4 x5 in x + x1 + x2 + x3 + x4 + x5") (Eval [1,2,3,4,5,6] (Value 21))
  , Test "mult_Sub"      (SrcString "input x x1 x2 x3 x4 x5 in x - x1 - x2 - x3 - x4 - x5") (Eval [42,1,5,4,10,2] (Value 20))
  , Test "Add_Sub_Er"      (SrcString "input x x1 x2 x3 x4 x5 in x - x1 + x2 + x3 - x4 - x5") (Eval [0,0,0,0,0,0] (Value 0))
 
  , Test "mod_Er"      (SrcString "input x in x mod 0") (Eval [1] RuntimeError)
  , Test "no_eval"      (SrcString "42 + 0") (Eval [] (Value 42))

  , Test "simplifyOperators" (SrcString " input x in x + 10 - - -  1") (Eval [1] (Value 10))
  , Test "ifStatement"       (SrcString "input x y in if x >= 1 then 144 else y * 5") (Eval [2, 3] (Value 144))
  , Test "negationSimplifiedToTrue" (SrcString "input x in if not not true then x * 1 else x div 1") (Eval [1] (Value 1))
  , Test "simpleInputIn"       (SrcString "input x y in x * 1 - y div 4") (Eval [4,12] (Value 1))
  , Test "evaluateBoolExpression" (SrcString "if true or false or false or true then 1 else 0") (Eval [] (Value 1))
  , Test "checkBoolInIf"      (SrcString "input x in if x div 2 >= 1 then x div 3 else x * 3") (Eval [24] (Value 8))
  , Test "properIfLazy" (SrcString "input x in if x >= 0 then x else x div 0") (Eval [1] (Value 1))
  , Test "quiteHardIfExpr" (SrcString "input a b c in if a < 0 and b < 0 then c else a + b") (Eval [-1, -1, 1] (Value 1))
  , Test "multipleSums"      (SrcString "input x y z a b in x + y + z + a + b") (Eval [1,11,11,11,11] (Value 45))
  , Test "thereIsUnusedVar" (SrcString "input x y in x*2") (Eval [1,2] (Value 2))
  , Test "scriptInterpretLanguage" (SrcString "1 + 10 + 100") (Eval [] (Value 111))
  , Test "multipleSubstractions"      (SrcString "input a b c d in a - b - c - d") (Eval [3,1,1,1] (Value 0))
  , Test "priorityOperators"      (SrcString "input a b c in a * b - c") (Eval [2,2,3] (Value 1))
  , Test "binaryWithUnaryOperator" (SrcString "10 + -7") (Eval [] (Value 3))       
  , Test "alwaysReturnInput"      (SrcString "input x in if true then x else x mod 2") (Eval [1] (Value 1))
  , Test "meine" (SrcString "let x = true in if not x then 2 else 3") (Eval [] (Value 3))
  , Test "letStatementNested" (SrcString "let x = 1 in let x = 2*x in let x = x + 5 in let x = 3*x in x + -1") (Eval [] (Value 20))
  , Test "negationIf" (SrcString "input x in if not false then x else -x") (Eval [1] (Value 1))

  , Test "val_inc"              (SrcString "input y in y")                                                  (Eval [5] (Value 5))
  , Test "val_inc1"             (SrcString "input x in x + 1")                                              (Eval [42] (Value 43))
  , Test "val_unarny"           (SrcString "input x in - x")                                                (Eval [5] (Value (-5)))
  , Test "val_letOP5priority"   (SrcString "let x = 5 in x + 6 - x + x")                                    (Eval [] (Value 11))
  , Test "val_letOP6priority"   (SrcString "let x = 5 in let y = 6 in x + y * x * x div y")                 (Eval [] (Value 30))
  , Test "val_if"               (SrcString "input x in if x>5 then 3 else 6")                               (Eval [10] (Value 3))
  , Test "val_if2"              (SrcString "input x y in if x>y or x<y then 1 else 0")                      (Eval [5,5] (Value 0))
  , Test "val_if3"              (SrcString "input x y z in if x>y or x<y and z = 5 then 1 else 0")          (Eval [4,6,5] (Value 1))
  , Test "val_ifNeg"            (SrcString "input a b in if not a = b then 1 else 0")                       (Eval [4,6] (Value 1))
  , Test "val_let"              (SrcString "input x in let x = x + 5 in let x = x * x in let x = x+1 in x") (Eval [5] (Value 101))
  , Test "val_contr"            (SrcString "let x = - - 5 in x")                                            (Eval [] (Value 5))
  , Test "val_combined"         (SrcString "input x in let x = 5 in if x = 6 and x <=6  then 10 else 0")    (Eval [10] (Value 0))
 
  , Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "smplTest1" (SrcString "2")               (Eval [] (Value 2))
  , Test "smplTest2" (SrcString "-2 + 2")          (Eval [] (Value 0))
  , Test "smplTest3" (SrcString "-2 div 0")          (Eval [] RuntimeError)
  , Test "smplTest4" (SrcString "1 - 2 + 3")       (Eval [] (Value 2))
  , Test "smplTest5" (SrcString "1 div 2")         (Eval [] (Value 0))
  , Test "smplTest6" (SrcString "14 mod 10")       (Eval [] (Value 4))
  , Test "smplTest7" (SrcString "11 mod 10 * 10")  (Eval [] (Value 10))

  , Test "smplLet11" (SrcString "input z y in let x = z + y in x")
                                                   (Eval [2, 3] (Value 5))



  , Test "smplIf13" (SrcString "input x z y in if x = z + y then x else z")
                                                   (Eval [5, 3, 2] (Value 5))

  , Test "smplIf14" (SrcString "input x z y in if x = z + y then x else z")
                                                   (Eval [6, 3, 2] (Value 3))

  , Test "smplIf15" (SrcString "if true then 42 else 1 div 0")
                                                   (Eval [] (Value 42))

  , Test "smplTest16" (SrcString "42 mod 0")       (Eval [] RuntimeError)

  , Test "override"
             (SrcString "input x in let x = x + 1 in let x = x + 2 in x")
                                                   (Eval [39] (Value 42))

  , Test "smplTest18" (SrcString
                "if 4 > 2 then if true or false then 2 else 0 else 1")
                                                   (Eval [] (Value 2))

  , Test "smplTest19" (SrcString "if true and false then 0 else 1")
                                                   (Eval [] (Value 1))

  , Test "smplTest20"
                (SrcString "if 2 <> 1 and 3>=3 and 2=2 then 1 else 0")
                                                   (Eval [] (Value 1))

  , Test "prioBool21"
                 (SrcString "if true or false and true then 1 else 0")
                                                   (Eval [] (Value 1))

  , Test "simple_eva_1" (SrcString "2") (Eval [] (Value 2))
  , Test "simple_eva_2" (SrcString "-277") (Eval [] (Value (-277)))
  , Test "simple_eva_3" (SrcString "1 + 2") (Eval [] (Value 3))
  , Test "simple_eva_4" (SrcString "let x = 2 in x + 2")(Eval [] (Value 4))
  , Test "simple_eva_5" (SrcString "if 2 < 3 then 5 else 3") (Eval [] (Value 5))
  , Test "simple_ari_mod" (SrcString "3 mod 0") (Eval [] RuntimeError)
  , Test "simple_ari_div" (SrcString "0 div 0") (Eval [] RuntimeError)
  , Test "cmp_eva_1" (SrcString "if not true or false then -5 else 2 + 1") (Eval [] (Value 3))
  , Test "cmp_eva_2" (SrcString "input x y in x - y - 2") (Eval [1, 1] (Value (-2)))
  , Test "cmp_eva_3" (SrcString "2 * 4 mod 3 + 3") (Eval [] (Value 5))
  , Test "cmp_eva_4" (SrcString "if true and not false or false then 1 else 0" ) (Eval [] (Value 1))
  , Test "cmp_eva_5" (SrcString "let x = 2 * 8 in x mod 9") (Eval [] (Value 7))
  , Test "cmp_ari_mod" (SrcString "if 3 mod 0 < 0 then -8 else 4 - 2") (Eval [] RuntimeError)
  , Test "cmp_ari_div" (SrcString "if 0 div 0 = 7 - 2 then -2 + 2 else 2 * 5") (Eval [] RuntimeError)
  , Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "dzialania1" (SrcString "1 + 2") (Eval [] (Value 3))
  , Test "dzialania2" (SrcString "4 - 1") (Eval [] (Value 3))
  , Test "dzialania3" (SrcString "2 * 5") (Eval [] (Value 10))
  , Test "dzialania4" (SrcString "3 div 2") (Eval [] (Value 1))
  , Test "dzialania5" (SrcString "2 mod 1") (Eval [] (Value 0))
  , Test "dzialania6" (SrcString "-2") (Eval [] (Value (-2)))

  , Test "zmienne1" (SrcString "input x y in x + y + 0") (Eval [1, 2] (Value 3))
  , Test "zmienne2" (SrcString "input x y in 2 + x + y") (Eval [1, 2] (Value 5))
  , Test "zmienne3" (SrcString "input x y in x - y") (Eval [2, 1] (Value 1))
  , Test "zmienne4" (SrcString "input x y in x * y") (Eval [1, 2] (Value 2))
  , Test "zmienne5" (SrcString "input x y in x div y") (Eval [1, 2] (Value 0))
  , Test "zmienne6" (SrcString "input x y in x mod y") (Eval [1, 2] (Value 1))
  , Test "zmienne7" (SrcString "input x y z in x + y * z") (Eval [1, 2, 3] (Value 7))
  , Test "zmienne8" (SrcString "input x y z in x + y div z") (Eval [1, 2, 3] (Value 1))
  , Test "zmienne9" (SrcString "input x y z in x + y mod z") (Eval [1, 2, 3] (Value 3))
  , Test "zmienne10" (SrcString "input x y z in x div y mod z") (Eval [1, 2, 3] (Value 0))
  , Test "zmienne11" (SrcString "input x y z in x div y mod z") (Eval [3, 2, 2] (Value 1))
  , Test "zmienne12" (SrcString "input x y z in x div y * z") (Eval [2, 2, 3] (Value 3))
  , Test "zmienne13" (SrcString "input x y z in x + y - z") (Eval [1, 2, 3] (Value 0))
  , Test "zmienne14" (SrcString "input x y z in x + y - z") (Eval [3, 2, 1] (Value 4))
  , Test "zmienne15" (SrcString "input x in x") (Eval [42] (Value 42))
  , Test "zmienne16" (SrcString "input x y z in x * y * z") (Eval [1, 2, 3] (Value 6))
  , Test "zmienne17" (SrcString "input x in let y = x in 2 * y") (Eval [21] (Value 42))

  , Test "let1" (SrcString "let x = 42 in x") (Eval [] (Value 42))
  , Test "let2" (SrcString "input x in let x = x + 2 in x + x") (Eval [2] (Value 8))
  , Test "let3" (SrcString "input x in let x = x + 1 in let x = x + 1 in x + x") (Eval [2] (Value 8))

  , Test "ify1" (SrcString "input x y in if true then x else y") (Eval [1, 2] (Value 1))
  , Test "ify2" (SrcString "input x y in if not true then x else y") (Eval [1, 2] (Value 2))
  , Test "ify3" (SrcString "input x y in if false then x else y") (Eval [1, 2] (Value 2))
  , Test "ify4" (SrcString "input x y in if not false then x else y") (Eval [1, 2] (Value 1))

  , Test "prostewarunki1" (SrcString "if 1 > 0 then 1 else 0") (Eval [] (Value 1))
  , Test "prostewarunki2" (SrcString "if 1 < 0 then 1 else 0") (Eval [] (Value 0))
  , Test "prostewarunki3" (SrcString "if 1 = 1 then 1 else 0") (Eval [] (Value 1))
  , Test "prostewarunki4" (SrcString "if 1 = 0 then 1 else 0") (Eval [] (Value 0))
  , Test "prostewarunki5" (SrcString "if 1 <> -1 then 1 else 0") (Eval [] (Value 1))
  , Test "prostewarunki6" (SrcString "if 1 <> 1 then 1 else 0") (Eval [] (Value 0))
  , Test "prostewarunki7" (SrcString "if 1 >= 1 then 1 else 0") (Eval [] (Value 1))
  , Test "prostewarunki8" (SrcString "if 0 >= 1 then 1 else 0") (Eval [] (Value 0))
  , Test "prostewarunki9" (SrcString "if 1 <= 1 then 1 else 0") (Eval [] (Value 1))
  , Test "prostewarunki10" (SrcString "if 1 <= 0 then 1 else 0") (Eval [] (Value 0))

  , Test "dzialaniawarunki1" (SrcString "if 1 = 0 + 1 then 1 else 0") (Eval [] (Value 1))
  , Test "dzialaniawarunki2" (SrcString "if 2 = 1 * 2 then 1 else 0") (Eval [] (Value 1))
  , Test "dzialaniawarunki3" (SrcString "if 4 div 2 = 1 then 1 else 0") (Eval [] (Value 0))
  , Test "dzialaniawarunki4" (SrcString "if 2 * 3 mod 4 = 6 mod 4 then 1 else 0") (Eval [] (Value 1))
  , Test "dzialaniawarunki5" (SrcString "if 2 > 1 or 3 < 2 then 1 else 0") (Eval [] (Value 1))
  , Test "dzialaniawarunki6" (SrcString "if 1 > 2 or 0 < -2 then 1 else 0") (Eval [] (Value 0))
  , Test "dzialaniawarunki7" (SrcString "if 1 = 1 and 3 >= 3 then 1 else 0") (Eval [] (Value 1))
  , Test "dzialaniawarunki8" (SrcString "if 0 <> 2 and 0 <> 0 then 1 else 0") (Eval [] (Value 0))
  , Test "dzialaniawarunki9" (SrcString "if 1 = 1 and not 2 = 5 then 1 else 0") (Eval [] (Value 1))

  , Test "boolwarunki1" (SrcString "if true or false then 1 else 0") (Eval [] (Value 1))
  , Test "boolwarunki2" (SrcString "if false or false then 1 else 0") (Eval [] (Value 0))
  , Test "boolwarunki3" (SrcString "if true and true then 1 else 0") (Eval [] (Value 1))
  , Test "boolwarunki4" (SrcString "if true and false then 1 else 0") (Eval [] (Value 0))
  , Test "boolwarunki5" (SrcString "if true and not false then 1 else 0") (Eval [] (Value 1))
  , Test "boolwarunki6" (SrcString "if not true or false then 1 else 0") (Eval [] (Value 0))
  , Test "boolwarunki7" (SrcString "if not true or not false then 1 else 0") (Eval [] (Value 1))

  , Test "prostezagniezdzone1" (SrcString "let x = if true then 1 else 0 in x") (Eval [] (Value 1))
  , Test "prostezagniezdzone2" (SrcString "if 1 = 0 + 1 then 1 else 0") (Eval [] (Value 1))
  , Test "prostezagniezdzone3" (SrcString "input x in let y = true in if y then x else 0") (Eval [1] (Value 1))

  , Test "warunkizagniezdzone3" (SrcString "if (if 1 = 1 then 1 else 0) = (if false then 1 else 0) then 1 else 0") (Eval [] (Value 0))

  , Test "nietypowy1" (SrcString "input x in if true then x + 42 else x div 0") (Eval [1] (Value 43))
  , Test "RuntimeError1" (SrcString "input x in if false then x + 42 else x div 0")  (Eval [1] RuntimeError)
  , Test "RuntimeError2" (SrcString "1 mod 0")  (Eval [] RuntimeError)

  , Test "NextTest" (SrcString "let x = 10 div 0 in 1") (Eval [] RuntimeError)
  ]
