import Poly
import RootFinding
import SimpleLang
import Test.Hspec
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Hspec (TreatPendingAs(..), testSpec)
import qualified Properties
import Control.Exception (evaluate, anyErrorCall)

main :: IO ()
main = do
    examples <- testSpec "Hspec" spec
    defaultMain $ testGroup "Lab2"
        [ localOption TreatPendingAsSuccess examples
        , Properties.tests
        ]

spec :: Spec
spec = do
    describe "poly" $ do
        it "applyPoly" $ do
            applyPoly (P [1, 2, 3] :: Poly Integer) 0 `shouldBe` 1
            applyPoly (P [1, 2, 3] :: Poly Integer) 1 `shouldBe` 6
            applyPoly (P [1, 2, 3] :: Poly Integer) 2 `shouldBe` 17
            applyPoly (P [] :: Poly Integer) 5 `shouldBe` 0
            applyPoly (P [5] :: Poly Integer) 100 `shouldBe` 5
            applyPoly x (7 :: Integer) `shouldBe` 7
        it "degree" $ do
            degree (P [] :: Poly Integer) `shouldBe` Nothing
            degree (P [0, 0] :: Poly Integer) `shouldBe` Nothing
            degree (P [5] :: Poly Integer) `shouldBe` Just 0
            degree (P [1, 0, 3, 0, 0] :: Poly Integer) `shouldBe` Just 2
            degree (P [0, 0, 0, 1] :: Poly Integer) `shouldBe` Just 3
            degree (P [1, 2, 3] :: Poly Integer) `shouldBe` Just 2
        it "composePoly" $ do
            composePoly (P [1, 0, 1] :: Poly Integer) (P [1, 1]) `shouldBe` P [2, 2, 1]
            composePoly (P [0, 1] :: Poly Integer) (P [1, 1]) `shouldBe` P [1, 1]
            composePoly (P [5] :: Poly Integer) (P [1, 1]) `shouldBe` P [5]
            composePoly (P [0, 0, 1] :: Poly Integer) (P [0, 1]) `shouldBe` P [0, 0, 1]
            let p = P [1, 0, 1] :: Poly Integer
                q = P [1, 1] :: Poly Integer
                t = 3 :: Integer
            applyPoly (composePoly p q) t `shouldBe` applyPoly p (applyPoly q t)
        describe "Eq" $ do
            it "(==)" $ do
                (P [1, 2, 3] :: Poly Integer) `shouldBe` P [1, 2, 3]
                (P [1, 2, 3] :: Poly Integer) `shouldNotBe` P [1, 2, 4]
            it "пустой список и список нулей задают нулевой многочлен" $ do
                (P [] :: Poly Integer) `shouldBe` P [0, 0, 0]
                (P [0, 0, 0] :: Poly Integer) `shouldBe` P []
            it "нули в конце списка не меняют многочлен" $ do
                (P [1, 2, 0, 0] :: Poly Integer) `shouldBe` P [1, 2]
                (P [1, 2] :: Poly Integer) `shouldBe` P [1, 2, 0, 0, 0]
        describe "Num" $ do
            it "(+)" $ do
                (P [1, 2] :: Poly Integer) + P [3, 4] `shouldBe` P [4, 6]
                (P [1] :: Poly Integer) + P [0, 1] `shouldBe` P [1, 1]
            it "(*)" $ do
                (P [1, 1] :: Poly Integer) * P [1, 1] `shouldBe` P [1, 2, 1]
                (P [2] :: Poly Integer) * P [3] `shouldBe` P [6]
                (P [1, 2] :: Poly Integer) * P [3, 4] `shouldBe` P [3, 10, 8]
            it "negate" $ do
                negate (P [1, -2, 3] :: Poly Integer) `shouldBe` P [-1, 2, -3]
            it "fromInteger" $ do
                (fromInteger 5 :: Poly Integer) `shouldBe` P [5]
                (fromInteger 0 :: Poly Integer) `shouldBe` P [0]
            it "складывает многочлены с разным числом коэффициентов" $ do
                (P [1] :: Poly Integer) + P [0, 0, 0, 5] `shouldBe` P [1, 0, 0, 5]
            it "умножение на нулевой многочлен даёт нулевой многочлен" $ do
                (P [1, 2, 3] :: Poly Integer) * P [0] `shouldBe` P [0]
                (P [1, 2, 3] :: Poly Integer) * P [] `shouldBe` P []
        describe "Differentiable" $ do
            it "apply совпадает с applyPoly" $ do
                apply (P [1, 2, 3] :: Poly Integer) 4 `shouldBe` applyPoly (P [1, 2, 3]) 4
            it "deriv" $ do
                deriv (P [1, 2, 3] :: Poly Integer) `shouldBe` P [2, 6]
                deriv (P [5] :: Poly Integer) `shouldBe` P []
                deriv (P [] :: Poly Integer) `shouldBe` P []
            it "nderiv" $ do
                nderiv 0 (P [1, 2, 3] :: Poly Integer) `shouldBe` P [1, 2, 3]
                nderiv 1 (P [1, 2, 3] :: Poly Integer) `shouldBe` P [2, 6]
                nderiv 2 (P [1, 2, 3] :: Poly Integer) `shouldBe` P [6]
                nderiv 3 (P [1, 2, 3] :: Poly Integer) `shouldBe` P []
        describe "show (готовая реализация)" $ do
            let render coefficients = show (P coefficients :: Poly Integer)
            it "выводит нулевой многочлен как 0" $ do
                render [] `shouldBe` "0"
                render [0, 0, 0] `shouldBe` "0"
            it "выводит константы" $ do
                render [5] `shouldBe` "5"
                render [-5] `shouldBe` "-5"
            it "опускает нулевые члены и лишние нули в конце" $ do
                render [1, 0, 3, 0, 0] `shouldBe` "3 * x^2 + 1"
            it "опускает коэффициенты 1 и -1 при степенях x" $ do
                render [0, 1] `shouldBe` "x"
                render [0, -1] `shouldBe` "-x"
                render [0, 0, 1] `shouldBe` "x^2"
                render [0, 0, -1] `shouldBe` "-x^2"
            it "выводит члены по убыванию степени с обычными знаками" $ do
                render [1, -1, 3] `shouldBe` "3 * x^2 - x + 1"
                render [-1, 2, -3] `shouldBe` "-3 * x^2 + 2 * x - 1"
            it "поддерживает дробные коэффициенты" $ do
                show (P [0.5, -1.5] :: Poly Double) `shouldBe` "-1.5 * x + 0.5"
                show (P [1 / 2, -3 / 2] :: Poly Rational)
                    `shouldBe` "-(3 % 2) * x + 1 % 2"
            it "не переполняется при выводе минимального Int" $ do
                let smallest = minBound :: Int
                    magnitude = show (negate (toInteger smallest))
                show (P [smallest] :: Poly Int) `shouldBe` show smallest
                show (P [1, smallest] :: Poly Int)
                    `shouldBe` "-" ++ magnitude ++ " * x + 1"
            it "учитывает приоритет при вложенном выводе" $ do
                show (Just (P [1, 1] :: Poly Integer)) `shouldBe` "Just (x + 1)"
                show (Just (P [0, 1] :: Poly Integer)) `shouldBe` "Just x"
                show (Just (P [0, 0, 1] :: Poly Integer)) `shouldBe` "Just (x^2)"
    describe "rootFinding" $ do
        describe "newtonStep" $ do
            it "делает один шаг для x^2 - 2" $ do
                newtonStep (P [-2, 0, 1] :: Poly Rational) 1 `shouldBe` Just (3 / 2)
            it "возвращает Nothing при нулевой производной" $ do
                newtonStep (P [-2, 0, 1] :: Poly Rational) 0 `shouldBe` Nothing
        describe "newton" $ do
            it "находит приближение к sqrt 2 с заданной невязкой" $ do
                case newton 1e-10 20 (P [-2, 0, 1] :: Poly Double) 1 of
                    Just t  -> abs (t - sqrt 2) `shouldSatisfy` (< 1e-6)
                    Nothing -> expectationFailure "Ожидалось Just"
            it "принимает начальный корень даже при лимите 0" $ do
                newton 1e-10 0 (P [-2, 1] :: Poly Double) 2 `shouldBe` Just 2.0
            it "проверяет последнюю точку после разрешённого числа шагов" $ do
                newton 1e-10 0 (P [-2, 0, 1] :: Poly Double) 1 `shouldBe` Nothing
            it "возвращает Nothing при исчерпании лимита" $ do
                newton 1e-20 1 (P [-2, 0, 1] :: Poly Double) 1 `shouldBe` Nothing
            it "возвращает Nothing при нулевой производной вне корня" $ do
                newton 1e-10 10 (P [1, 0, 1] :: Poly Double) 0 `shouldBe` Nothing
            it "отклоняет неположительную точность и отрицательный лимит" $ do
                newton 0 10 (P [-2, 0, 1] :: Poly Double) 1 `shouldBe` Nothing
                newton (-1e-10) 10 (P [-2, 0, 1] :: Poly Double) 1 `shouldBe` Nothing
                newton 1e-10 (-1) (P [-2, 0, 1] :: Poly Double) 1 `shouldBe` Nothing
        describe "halley (необязательно)" $ do
            it "halleyStep делает один шаг для x^2 - 2" $ do
                halleyStep (P [-2, 0, 1] :: Poly Rational) 1 `shouldBe` Just (7 / 5)
            it "возвращает Nothing при нулевом знаменателе вне корня" $ do
                halleyStep (P [0, 3, -3, 1] :: Poly Rational) 1 `shouldBe` Nothing
            it "находит приближение к sqrt 2" $ do
                case halley 1e-10 20 (P [-2, 0, 1] :: Poly Double) 1 of
                    Just t  -> abs (t - sqrt 2) `shouldSatisfy` (< 1e-6)
                    Nothing -> expectationFailure "Ожидалось Just"
            it "соблюдает те же правила точности и лимита, что и newton" $ do
                halley 1e-10 0 (P [-2, 1] :: Poly Double) 2 `shouldBe` Just 2.0
                halley 0 10 (P [-2, 0, 1] :: Poly Double) 1 `shouldBe` Nothing
                halley 1e-10 (-1) (P [-2, 0, 1] :: Poly Double) 1 `shouldBe` Nothing
    describe "simpleLang" $ do
        describe "empty и extend" $ do
            it "empty возвращает 0 для любой переменной" $ do
                empty "x" `shouldBe` 0
                empty "" `shouldBe` 0
            it "extend меняет выбранную переменную, сохраняя остальные" $ do
                let st = extend empty "x" 5
                st "x" `shouldBe` 5
                st "y" `shouldBe` 0
            it "повторный extend заменяет предыдущее значение" $ do
                let st = extend (extend empty "x" 5) "x" 10
                st "x" `shouldBe` 10
        describe "eval" $ do
            it "вычисляет арифметику и сравнения (1 или 0)" $ do
                eval empty (Op (Val 2) Plus (Val 3)) `shouldBe` 5
                eval empty (Op (Val 2) Minus (Val 3)) `shouldBe` -1
                eval empty (Op (Val 2) Times (Val 3)) `shouldBe` 6
                eval empty (Op (Val 2) Gt (Val 3)) `shouldBe` 0
                eval empty (Op (Val 3) Gt (Val 2)) `shouldBe` 1
                eval empty (Op (Val 2) Ge (Val 2)) `shouldBe` 1
                eval empty (Op (Val 2) Lt (Val 3)) `shouldBe` 1
                eval empty (Op (Val 2) Le (Val 1)) `shouldBe` 0
                eval empty (Op (Val 2) Eql (Val 2)) `shouldBe` 1
                eval empty (Op (Val 2) Eql (Val 3)) `shouldBe` 0
            it "деление отрицательных чисел округляет вниз" $ do
                eval empty (Op (Val (-3)) Divide (Val 2)) `shouldBe` -2
            it "деление на ноль вызывает ошибку" $ do
                evaluate (eval empty (Op (Val 1) Divide (Val 0))) `shouldThrow` anyErrorCall
        describe "desugar" $ do
            it "преобразует Incr в присваивание" $ do
                desugar (Incr "x") `shouldBe` DAssign "x" (Op (Var "x") Plus (Val 1))
            it "преобразует For с порядком initialization, condition, body, step" $ do
                let stmt = For (Assign "i" (Val 0)) (Op (Var "i") Lt (Val 5)) (Incr "i") (Assign "x" (Var "i"))
                desugar stmt `shouldBe` DSequence
                    (DAssign "i" (Val 0))
                    (DWhile (Op (Var "i") Lt (Val 5))
                            (DSequence (DAssign "x" (Var "i")) (DAssign "i" (Op (Var "i") Plus (Val 1)))))
            it "преобразует Block [] в DSkip" $ do
                desugar (Block []) `shouldBe` DSkip
        describe "runSimpler и run" $ do
            it "пустой блок сохраняет значения переменных" $ do
                run (extend empty "x" 5) (Block []) "x" `shouldBe` 5
            it "цикл с ложным начальным условием не выполняет тело" $ do
                let st = run empty (While (Val 0) (Assign "x" (Val 1)))
                st "x" `shouldBe` 0
            it "For выполняет инициализацию даже при ложном начальном условии" $ do
                let st = run empty (For (Assign "x" (Val 42)) (Val 0) (Incr "x") (Assign "x" (Val 0)))
                st "x" `shouldBe` 42
            it "factorial записывает результат в Out" $ do
                let st = run (extend empty "In" 5) factorial
                st "Out" `shouldBe` 120
            it "factorial для 0 возвращает 1" $ do
                let st = run (extend empty "In" 0) factorial
                st "Out" `shouldBe` 1
            it "squareRoot обрабатывает 0, точные квадраты и числа между ними" $ do
                run (extend empty "A" 0) squareRoot "B" `shouldBe` 0
                run (extend empty "A" 1) squareRoot "B" `shouldBe` 1
                run (extend empty "A" 4) squareRoot "B" `shouldBe` 2
                run (extend empty "A" 9) squareRoot "B" `shouldBe` 3
                run (extend empty "A" 10) squareRoot "B" `shouldBe` 3
                run (extend empty "A" 15) squareRoot "B" `shouldBe` 3
                run (extend empty "A" 16) squareRoot "B" `shouldBe` 4
                run (extend empty "A" 17) squareRoot "B" `shouldBe` 4
            it "fibonacci возвращает 1 для In = 0 и In = 1" $ do
                run (extend empty "In" 0) fibonacci "Out" `shouldBe` 1
                run (extend empty "In" 1) fibonacci "Out" `shouldBe` 1
                run (extend empty "In" 2) fibonacci "Out" `shouldBe` 2
                run (extend empty "In" 3) fibonacci "Out" `shouldBe` 3
                run (extend empty "In" 5) fibonacci "Out" `shouldBe` 8