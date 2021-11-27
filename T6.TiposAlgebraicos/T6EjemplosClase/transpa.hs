--  TIPOS ALGEBRAICOS

-- Definir como enumeracion
--data Dia = Lu | Ma | Mi | Ju | Vi | Sa | Do
--data Bool = False | True

--Definir de forma recursiva
--data Nat = Cero | Suc Nat

-- Definir con newType
-- newtype Fecha = F (Int,Int,Int)
-- newtype Natural a = Nat a
-- data Racional = Int :/ Int
-- newtype RacionalN = C (Int,Int)

-- Definir dia como enumerado, 2 formas
-- 1.
-- Declarar como instancia de enum
-- instance Enum Dia where
--  fromEnum Lu = 0
--  fromEnum Ma = 1
--  fromEnum Mi = 2
--  fromEnum Ju = 3
--  fromEnum Vi = 4
--  fromEnum Sa = 5
--  fromEnum Do = 6
--  toEnum 0 = Lu
--  toEnum 1 = Ma
--  toEnum 2 = Mi
--  toEnum 3 = Ju
--  toEnum 4 = Vi
--  toEnum 5 = Sa
--  toEnum 6 = Do


--2. derivar directamente de deriving
data Dia = Lu | Ma | Mi | Ju | Vi | Sa | Do deriving (Eq, Ord, Enum, Show)

-- Otro ejemplo deriving
-- Usando operaciones heredadas
festivo :: Dia -> Bool
laborable :: Dia -> Bool

festivo d = d == Do
laborable d = (d >= Lu) && (d <= Vi)

siguiente :: Dia -> Dia
siguiente d = if d == Do then Lu else succ d

anterior :: Dia -> Dia
anterior d = if d == Lu then Do else pred d

-- Tipos recursivos -- NO ENTIENDO NADA 
data Nat = Cero | Suc Nat deriving (Eq, Ord, Show)

suma :: Nat -> Nat -> Nat
suma Cero y = y
suma (Suc x) y = Suc(suma x y)

-- Intancia de la clase Eq
data MiRacional = Integer :/ Integer
instance Eq MiRacional where
    (x:/y) == (x':/y') = x*y' == y*x'

instance Show MiRacional where
    show (x:/y) = show (div x z) ++ "/"++ show (div y z)
                      where z = gcd x y

-- Tipos polimorficos y recursivos
data Lista a = Vac | Cons a (Lista a) deriving (Eq, Ord, Show)

-- Funciones polimorficas sobre una Lista a
longitud :: Lista a -> Int 
longitud Vac = 0
longitud (Cons x s) = 1 + longitud s

-- Tipo polimorfico predefinido maybe
data Maybee a = Nothinge | Juste a deriving (Eq, Ord, Read, Show)

maybee :: b -> (a -> b) -> Maybee a -> b
maybee n f Nothinge = n
maybee n f (Juste x) = f x