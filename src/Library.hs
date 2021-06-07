module Library where
import PdePreludat

--Kalinin Alexander


--Modelado de ratones

data Raton=UnRaton{
    nombre:: String
,   edad :: Number
,   peso :: Number
,   enfermedades :: [Enfermedad]
}deriving (Show,Eq)

type Enfermedad = String


--Punto 1
cerebro=UnRaton{
    nombre = "Cerebro"
,   edad = 9   
,   peso = 0.2
,   enfermedades = ["brucelosis", "sarampion", "tuberculosis"] 
}

bicenterrata=UnRaton{
    nombre = "Bicenterrata"
,   edad = 256   
,   peso = 0.2
,   enfermedades = [] 
}

huesudo=UnRaton{
    nombre = "Huesudo"
,   edad = 4   
,   peso = 10
,   enfermedades = ["alta obesidad", "sinusitis" ] 
}


--Punto 2

type Hierba = Raton->Raton


hierbaBuena :: Hierba
hierbaBuena  = modificarEdad

modificarEdad :: Raton->Raton
modificarEdad aRaton = aRaton {edad= sqrt (edad aRaton)} 

------
hierbaVerde :: String->Hierba
hierbaVerde tipoEnf  = curarEnfermedad tipoEnf

curarEnfermedad :: String->Raton->Raton
curarEnfermedad tipoEnf aRaton = aRaton{enfermedades=  eliminarEnfer tipoEnf (enfermedades aRaton)   }

eliminarEnfer  :: String->[Enfermedad]->[Enfermedad]
eliminarEnfer tipoEnf  = filter (condicionTipoEnf tipoEnf) 

condicionTipoEnf :: String->Enfermedad->Bool
condicionTipoEnf tipoEnf =  (reverse tipoEnf /= ) . darVuelta tipoEnf

darVuelta :: String->Enfermedad->String
darVuelta tipoEnf enferm = take  (length tipoEnf)  (reverse enferm)
------

alcachofa :: Hierba
alcachofa aRaton | peso aRaton > 2  =  modificarPesoPorcentaje 10  aRaton
                 |  otherwise = modificarPesoPorcentaje 5 aRaton

modificarPesoPorcentaje :: Number->Raton->Raton
modificarPesoPorcentaje porcentaje aRaton = aRaton {peso= peso aRaton - (peso aRaton * porcentaje/100)}

------

hierbaZort :: Hierba
hierbaZort aRaton = transformacionPinky aRaton

transformacionPinky :: Raton->Raton
transformacionPinky aRaton = aRaton {edad= 0, enfermedades=[]}
--------

hierbaDelDiablo :: Hierba
hierbaDelDiablo  =  eliminarEnferDiablo . modificarPesoNumero 

modificarPesoNumero :: Raton->Raton
modificarPesoNumero aRaton = aRaton {peso= max 0 (peso aRaton - 0.1 )}

eliminarEnferDiablo :: Raton->Raton
eliminarEnferDiablo aRaton = aRaton {enfermedades= quitarEnferm (enfermedades aRaton)}

quitarEnferm :: [Enfermedad]->[Enfermedad]
quitarEnferm listaEnfermedades = filter (condicionDiablo) listaEnfermedades

condicionDiablo :: Enfermedad->Bool
condicionDiablo enferm = length enferm > 10
----------


--Punto 3)

type Medicamento = [Hierba]

comerHierba :: Hierba->Raton->Raton
comerHierba hierba  = hierba  

tomarMedicamento :: Medicamento->Raton->Raton
tomarMedicamento medicamento aRaton = foldl (flip comerHierba ) aRaton medicamento

--------
--a)

pondsAntiAge :: Medicamento
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]

---------
--b)

type Potencia = Number

listaVacia = []

reduceFatFast ::Potencia-> Medicamento
reduceFatFast potencia = [hierbaVerde "obesidad"] ++ listaAlcachofas potencia listaVacia

listaAlcachofas :: Potencia->[Hierba]->Medicamento
listaAlcachofas potencia lista  | length lista /= potencia =  listaAlcachofas potencia ( lista ++ [alcachofa]  )
                                | otherwise = lista
---------


--c)

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina :: Medicamento
pdepCilina = map hierbaVerde sufijosInfecciosas


-- Punto 4)

--a)
listaNum :: [Number]
listaNum = [1..]

cantidadIdeal :: (Number->Bool)->Number
cantidadIdeal condicion =  head (filter condicion listaNum)


--b)

lograEstabilizar :: Medicamento->[Raton]->Bool
lograEstabilizar medicamento = all (condicionEstabilizar medicamento ) 

condicionEstabilizar :: Medicamento->Raton->Bool
condicionEstabilizar medicamento raton = tieneSobrepeso medicamento raton && tenerMenosEnfer 3 medicamento raton
 
tenerMenosEnfer :: Number->Medicamento->Raton->Bool
tenerMenosEnfer n medicamento  = (<n). length . enfermedades . tomarMedicamento medicamento 

tieneSobrepeso :: Medicamento->Raton->Bool
tieneSobrepeso medicamento  =  (<1). peso . tomarMedicamento medicamento    


--c)


-----------------FORMA DE AUGUSTO---------------------------------------------
encontrarPotenciaIdealFatFast :: [Raton] -> Number
encontrarPotenciaIdealFatFast ratones = cuantaPotenciaNecesaria 1 ratones

cuantaPotenciaNecesaria :: Number -> [Raton] -> Number
cuantaPotenciaNecesaria potencia ratones 
    | lograEstabilizar (reduceFatFast potencia) ratones = potencia
    | otherwise = cuantaPotenciaNecesaria (potencia+1) ratones 




-------------------------- FORMA DE LEAN------------------------------------------------------------
-- Diseñar el siguiente experimento: dado una comunidad de ratones, encontrar la potencia ideal del 
-- reduceFatFast necesaria para estabilizar la comunidad.
potenciaIdeal :: [Raton]->Potencia
potenciaIdeal  = primeroDe.potenciasValidas

primeroDe :: [Potencia]->Potencia
primeroDe [] = -1                   -- si devuelve -1 es porque no hay niguna potencia que lograEstabilizar
primeroDe lista = head lista

potenciasValidas :: [Raton]->[Potencia]
potenciasValidas comunidad = filter (cond comunidad .  reduceFatFast)  listaNum  --Basicamente filtra los numero Naturales que cumplen la condicion, 
                                                                                         --es decir a partir por ejem del 30 para arriba se cumple entonces se arma una 
                                                                                          -- lista inf de esos numeros, en donde desp solo se devuelve el primero (con head)

cond :: [Raton] -> Medicamento -> Bool
cond comunidad = flip (lograEstabilizar) comunidad  

--potenciaIdeal [huesudo ]
--[huesudo] con reduceFatFast 29 llega a lograEstabilizar

-----------Forma de Rusito--------------------------------------

--listaNum :: [Number]
--listaNum = [1..]

--cantidadIdeal :: (Number->Bool)->Number
--cantidadIdeal condicion =  head (filter condicion listaNum)

potenciaValida' :: [Raton]->Number
potenciaValida' comunidad = cantidadIdeal (cond' comunidad .  reduceFatFast)

cond' :: [Raton] -> Medicamento -> Bool
cond' comunidad = flip (lograEstabilizar) comunidad  
