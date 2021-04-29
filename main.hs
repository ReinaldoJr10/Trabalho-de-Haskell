import Data.Time.Calendar

type Nome=String
type Endereco=String
type Telefone=Int

type Cliente = (Nome,Endereco,Telefone)

getNome::Cliente->[Char]
getNome(a,b,c) = a

setNome::Cliente->[Char]->Cliente
setNome(a,b,c) x = (x,b,c)

getEndereco::Cliente->[Char]
getEndereco(a,b,c) = b

setEndereco::Cliente->[Char]->Cliente
setEndereco(a,b,c) x = (a,x,c)

getTelefone::Cliente->Int
getTelefone(a,b,c) = c

setTelefone::Cliente->Int->Cliente
setTelefone(a,b,c) x = (a,b,x)

type Numero=Int
type Preco=Float
type Expresso=Bool
type RealizacaoData=Day
type EntregaData=Day

type Pedido = (Numero,Preco,Expresso,Cliente,RealizacaoData,EntregaData)

getNumero::Pedido->[Int]
getNumero(a,b,c,d,e,f) = [a]

setNumero::Pedido->Int->Pedido
setNumero(a,b,c,d,e,f) x = (x,b,c,d,e,f)

getPreco::Pedido->Float
getPreco(a,b,c,d,e,f) = b

setPreco::Pedido->Float->Pedido
setPreco(a,b,c,d,e,f) x = (a,x,c,d,e,f)

getExpresso::Pedido->Bool
getExpresso(a,b,c,d,e,f) = c

setExpresso::Pedido->Bool->Pedido
setExpresso(a,b,c,d,e,f) x |(c==True) = (a,b,c,d,e,f)
                           |otherwise = (a,b*1.2,x,d,e,f)

getCliente::Pedido->Cliente
getCliente(a,b,c,d,e,f) = d

setCliente::Pedido->Cliente->Pedido
setCliente(a,b,c,d,e,f) x = (a,b,c,x,e,f)

getRealizacao::Pedido->Day
getRealizacao(a,b,c,d,e,f) = e

setRealizacao::Pedido->Day->Pedido
setRealizacao(a,b,c,d,e,f) x = (a,b,c,d,x,f)

getEntrega::Pedido->Day
getEntrega(a,b,c,d,e,f) = f

setEntrega::Pedido->Day->Pedido
setEntrega(a,b,c,d,e,f) x = (a,b,c,d,e,x)

toString::Pedido->[Char]
toString(a,b,c,d,e,f) |((verificaPrazo e f)==True) = "Nome: "++getNome (d)++" Preco: "++show (b)++" entregue no prazo"
                      |otherwise ="Nome: "++getNome (d)++" Preco: "++show (b)++" entrega atrasada"

copia::Pedido->Pedido
copia(a,b,c,d,e,f) = (a,b,c,d,e,f)

verificaPrazo::Day->Day->Bool
verificaPrazo a b |((diffDays a b)==0) = True
                  |otherwise = False

main::IO()
main = do
    let dat1=fromGregorian 2021 04 04
    let dat2=fromGregorian 2021 04 04
    let cliente1=("Fulano","Rua A",9999) 
    let cliente2=("Ciclano","Rua B",8888)
    let pedido1=(1,500.0,False,cliente1,dat1,dat2)
    let pedido2=(2,500.0,False,cliente2,dat1,dat2)
    let aux=setExpresso pedido2 True    
    let pedido2=copia aux
    putStrLn(show (toString pedido1)) 
    putStrLn(show (toString pedido2))   