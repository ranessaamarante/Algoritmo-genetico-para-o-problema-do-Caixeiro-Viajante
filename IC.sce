//Problema do Caixeiro Viajante
//Problema de Otimização Combinatorial
//Custo mínimo escontrado = 14 
//Possível caminho com custo 14: [1,12,13,14,11,8,6,4,3,10,9,7,5,2]
//Outro possível caminho com custo 14: [14,13,12,1,2,5,7,9,10,3,4,6,8,11]
//No último exemplo, da cidade 11, retorna-se à cidade de início (14)
//Para obter o resultado, digite no console o seguinte comando: [path,c]=time_passing(150)  
//Minha sugestão, como mostrada, é de 150 rounds
//path indicará o caminho
//c indicará o custo de path
//O resultado frequentemente fornece o custo 14, mas em alguns casos há uma variação pequena
//Sugiro rodar algumas vezes para uma análise melhor




function nseq=permutar(u)   //permuta os elementos de u
    
    nseq=[]
    while(length(u))>0
     r=grand(1, "uin",1,length(u))
     nseq=[nseq u(r)]
     u(r)=[]  
    end
        
endfunction




function u=generate()   //gera uma sequência aleatória de 14 bits
    u=[]
    u = prbs_a(14,7)
        for j=1:14
            if u(j)==-1 then
               u(j)=0
            end
         end
endfunction




//distances(a,b) diz o custo de ir de a até b

distances = [0,1,2,4,6,2,2,3,3,5,6,1,4,5;1,0,3,2,1,3,6,3,4,4,2,4,4,4;2,3,0,1,3,3,2,3,4,1,3,5,5,6;4,2,1,0,5,1,4,2,3,4,4,8,2,2;6,1,3,5,0,2,1,6,5,2,3,4,2,2;2,3,3,1,2,0,3,1,2,3,5,7,3,4;2,6,2,4,1,3,0,2,1,2,5,2,4,3;3,3,3,2,6,1,2,0,5,5,1,5,3,6;3,4,4,3,5,2,1,5,0,1,4,4,5,3;5,4,1,4,2,3,2,5,1,0,5,4,4,2;6,2,3,4,3,5,5,1,4,5,0,4,2,1;1,4,5,8,4,7,2,5,4,4,4,0,1,3;4,4,5,2,2,3,4,3,5,4,2,1,0,1;5,4,6,2,2,4,3,6,3,2,1,3,1,0]




function tot=total_d(L) //calcula o custo do caminho L
    len = length(L)
    tot=0
    for i=2:len
        tot = tot+distances(L(i),L(i-1))
    end
    tot=tot+distances(L(14),L(1))   
endfunction




function c=cost(path)   //calcula o custo de cada caminho em path
    
    len=size(path)(1)
    c=ones(len,1)
    for k=1:len
        c(k)=total_d(path(k,:))
    end
    
endfunction




function pop=in_pop()   //gera a população inicial
    u=[1,2,3,4,5,6,7,8,9,10,11,12,13,14]
    pop=[]
    for i=1:100
      ch=[]
      for w=1:3
      ch = [ch ;permutar(u)]  
      end
     [a,i]=min(cost(ch))
      pop=[pop;ch(i,:)]  
      
    end
endfunction




function fit=fitness(c) //função de avaliação
    len=length(c)
    fit=ones(len,1)
    [m,i]=min(c)
    for k=1:len
        fit(k) = (100/(c(k)-13))^2
    end
endfunction




function p=percentages(fit) //calcula a porcentagem de cada nota em relação a soma de todas
    len=length(fit)
    p=ones(len,1)
    for i=1:len
        p(i)=fit(i)*100/sum(fit)
    end
endfunction




function r=acc_pb(p)   //porcentagens acumuladas
    r(1)=p(1)
    for i=2:length(p)
        r(i)=r(i-1)+p(i)
    end
endfunction




function prep=roleta(r)  //roleta: r é um vetor com os intervalos de porcentagem configurados

prep=[]

while length(prep) <length(r)


    y = grand(1, "uin", 0, 100) //valores aleatórios y serão gerados
    for k=1:length(r)  //verifica-se a qual intervalo o y pertece e, assim, o índice desse intervalo vai para prep
        if y<=r(k) then   
            break
        end
    end
    
prep=[prep k]    
end
    
endfunction




function cross_seq=selection(pop,fit,pop_size)  //função de seleção
    cross_seq=[]
    p=percentages(fit)
    r=acc_pb(p)
    indices=roleta(r) //utilização da roleta
    
  
    for i=1:pop_size
        cross_seq(i,:)=pop(indices(i),:)
        
    end
endfunction

 
    
     
 function f=half_c(p1,p2) //gera um dos filhos de p1 e p2, utilizando p1 para a cópia quando o bit de g for 1
    
    g=generate() //geração de uma sequência de 14 bits
    f=ones(1,14)
    aux=[]
    falta=[]
    m=1
    for i=1:14
        if g(i)==1 then  //caso o bit g(i) seja 1->fazer cópia de p1(i) para f1(i) 
            f(i)=p1(i)
            
        else
            falta = [falta i]  // índices faltantes em f
            aux=[aux p1(i)]   //cidades faltantes em f
        end
    end
    
    while(length(aux)>0)
    for k=1:14    //as cidades faltantes em f serão configuradas na mesma ordem que estão em p2
        ind=find(aux==p2(k))
        if(ind<>[]) then
            f(falta(m))=aux(ind)
            m=m+1
            aux(ind)=[]
            
        end
    end
    end
endfunction




//crossover baseado em ordem
//pais(i) cruza com pais(i+1)
function filhos=crossover(pais)  
    filhos=[]
    i=1
    while i<size(pais)(1)
    filhos = [filhos; half_c(pais(i,:),pais(i+1,:))] //utilizando pais(i) para cópia
    filhos = [filhos; half_c(pais(i+1,:),pais(i,:))] //utilizando pais(i+1) para a cópia
    i=i+2
    end
endfunction




function mutantes=mutation(pop,taxa) //mutação baseada em ordem 
//2 pontos de corte
    mutantes=pop
    [v,j]= max(fitness(cost(pop)))
    qtd=(size(pop)(1))*taxa
    for i=1:round(qtd)
            sel=grand(1, "uin",1 , size(pop)(1))
            while sel==j //não modificar o indivíduo com melhor avaliação
                sel=grand(1, "uin",1 , size(pop)(1))
                continue
            end
    
    pc1=grand(1, "uin",1,7)  //primeiro ponto de corte
    pc2=grand(1, "uin",8,14)  //segundo ponto de corte
    cro=pop(sel,:)
    aux=cro(pc1:pc2)
    cro(pc1:pc2)=permutar(aux)  //o segmento de pc1 até pc2 é permutado aleatoriamente
    mutantes(sel,:)=cro
  
    end
endfunction




function mutantes=mutation2(pop,taxa) //mutação baseada em ordem 
//1 ponto de corte
    mutantes=pop
    [v,j]= max(fitness(cost(pop)))
    qtd=(size(pop)(1))*taxa
    for i=1:round(qtd)
            sel=grand(1, "uin",1 , size(pop)(1))
            while sel==j //não modificar o indivíduo com melhor avaliação
                sel=grand(1, "uin",1 , size(pop)(1))
                continue
            end
    
    pc=grand(1, "uin",2,13)  // ponto de corte
    s=rand()
    cro=pop(sel,:)
    if (s>0.5) then     //o segmento de pc até 14 é permutado aleatoriamente
        aux=cro(pc:14)    
        cro(pc:14)=permutar(aux)
    else    
        aux=cro(1:pc)    //o segmento de 1 até pc é permutado aleatoriamente
        cro(1:pc)=permutar(aux)
        end
    
    mutantes(sel,:)=cro
  
    end
endfunction




function new_g=disputa(pop1,pop2,taxa) //disputa entre pop1 e pop2
    
    fit_pop1=fitness(cost(pop1))
    fit_pop2=fitness(cost(pop2))
    new_g=[]    //nova geração
    t=length(fit_pop1)
    m=1
    
    while m<round(t*taxa/2) 
    [v,i]=max(fit_pop1)
    new_g(m,:)= pop1(i,:) //nova geração recebe o indivíduo com maior nota em pop1
    fit_pop1(i)=[] //retirar nota do indivíduo
    pop1(i,:)=[]  //retirar indivíduo 
    [h,j]=max(fit_pop2)
   
    new_g(m+1,:)=pop2(j,:)  //nova geração recebe o indivíduo com maior nota em pop2
    fit_pop2(j)=[]   //retirar nota do indivíduo
    pop2(j,:)=[]    //retirar indivíduo 
    
    m=m+2
    end

    while size(new_g)(1)<t   // usar indivíduos aleatórios entre os de pop1 e pop2 para popular o que falta da nova geração
        r1=grand(1, "uin",1,length(fit_pop2))
        r2=grand(1, "uin",1,length(fit_pop1))
        new_g=[new_g; pop2(r1,:)]
        new_g=[new_g; pop1(r2,:)]
        fit_pop2(r1)=[]
        pop2(r1,:)=[]
        fit_pop1(r2)=[]
        pop1(r2,:)=[]
        end
endfunction




function [path,c]=time_passing(num_g) //num_g é a quantidade de rounds
    
    pop=in_pop()
    sigma=[]
    sigma(1)=1

    for u=1:num_g
        fit=fitness(cost(pop))
        sigma(u+1)= stdev(fit)
        [a,b] = max(fitness(cost(pop)))

     if (sigma(u+1)<sigma(u))  then  
          pop=mutation(pop,0.3265306) 
     end
          filhos1=crossover(selection(pop,fit,length(fit))) 
          pop=mutation2(pop,0.7142857) 
          filhos2=crossover(selection(pop,fit,length(fit))) 
         
          filhos=disputa(filhos1,filhos2, 0.9795918)
          pop=disputa(pop,filhos,0.9387755) 
         
     end
    
    [row,i]=max(fitness(cost(pop)))
    path=pop(i,:)
    c=total_d(path)
    
endfunction
    





