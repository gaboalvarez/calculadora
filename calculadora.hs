import Text.Show.Functions
data Calculadora = Calculadora{memoria::[Int],acumA::Int,acumB::Int,pc::Int,errorCalc::String}deriving(Show)
ejemplo = Calculadora{memoria=[1..10],acumA=10,acumB=20,pc=0,errorCalc=""}
xt8088 = Calculadora{memoria=(replicate 10 0),acumA=0,acumB=0,pc=0,errorCalc=""}

nop calc = calc{pc=(pc calc)+1}
--modificarAcum v1 v2 op = v1 op v2
add calc = calc{acumA = (acumA calc) + (acumB calc),acumB=0}
dividir calc = calc{acumA = div (acumA calc) (acumB calc),acumB=0}
swap calc = calc{acumB=(acumA calc),acumA=(acumB calc)}
lod addr calc =calc{acumA=((memoria calc) !! (addr-1))}
str addr val calc =calc{memoria=(take (addr-1) (memoria calc))++[val]++(drop addr (memoria calc))}
lodv val calc =calc{acumA=val}

prueba1= (nop.nop.nop)
prueba2= (nop.nop.nop.nop.add.(lodv 22).swap.(lodv 10))
