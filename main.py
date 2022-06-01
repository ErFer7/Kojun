import random

def abrir_arquivo(nome):
    'extrair dados do tabuleiro'
    dados = {}
    with open(nome,'r') as f:
        t = int(f.readline())
        dados['tamanho'] = t
        dados['regioes'] = []
        dados['iniciais'] = []

        'regioes definidas por indice da celula'
        for l in range(t):
            linha = f.readline()
            dados['regioes'] += list(map(int,linha.split()))

        'valores iniciais, imutaveis do tabuleiro'
        for l in range(t):
            linha = f.readline()
            dados['iniciais'] += list(map(int,linha.split()))
    return dados

def criar_tabuleiro(dados):
    tabuleiro = {}
    t = dados['tamanho']

    tabuleiro['tamanho'] = t

    'coloca [valor_inicial, regiao] na matriz'
    tabuleiro['celulas']=[
        dados['iniciais'][i] for i in range(t**2) # valor inicial    
    ]

    'coloca coordenadas da celula na regiao correspondente'
    tabuleiro['regioes'] = {}
    for i in range(t**2):
        if dados['regioes'][i] in tabuleiro['regioes']:
            tabuleiro['regioes'][dados['regioes'][i]].append(i)
        else:
            tabuleiro['regioes'][dados['regioes'][i]] = [i]

    'calcula valores possiveis para cada celula no comeco'
    valores_possiveis(tabuleiro)
    return tabuleiro

def montar_tabuleiro(nome_arquivo):
    tab = abrir_arquivo(nome_arquivo)
    tab = criar_tabuleiro(tab)
    return tab

def print_tabuleiro(tabuleiro):
    if not tabuleiro:
        print("Solucao nao encontrada")
        return
    tamanho = tabuleiro['tamanho']
    print("Celulas:")
    for i in range(tamanho**2):
        print(tabuleiro['celulas'][i],end=' ' if (i+1)%tamanho else '\n')
    print("\nRegioes:\n")
    for i in tabuleiro['regioes']:
        print('Regiao',i,'=',tabuleiro['regioes'][i])
    print("\nValores Possiveis para celulas vazias\n")
    for i in tabuleiro['valores']:
        print('Regiao',i,'=',tabuleiro['valores'][i])

def valores_possiveis(tabuleiro):
    valores = {i:[] for i in tabuleiro['regioes']}

    for indice_regiao in tabuleiro['regioes']:
        valores[indice_regiao] = [i+1 for i in range(len(tabuleiro['regioes'][indice_regiao]))]
        for coord in tabuleiro['regioes'][indice_regiao]:
            valor = tabuleiro['celulas'][coord]
            if valor != 0:
                valores[indice_regiao].remove(valor)
    
    tabuleiro['valores'] = valores





def valores_possiveis_v1(tabuleiro):
    # METODO DIFERENTE UTILIZADO PARA SIMPLIFICAR ALGORITMO DE TENTATIVA
    t = tabuleiro['tamanho']
    valores = [[] for i in range(tabuleiro['tamanho']**2)]

    #cada indice recebe lista de valores 1..N para celula em regiao tamanho n
    for i in tabuleiro['regioes']:
        tamanho = len(tabuleiro['regioes'][i])
        presentes = []
        for j in tabuleiro['regioes'][i]:
            valor = tabuleiro['celulas'][j][1]
            if valor == 0:
                valores[j] = [k for k in range(1,tamanho+1)]
            else:
                presentes.append(valor)

        print('presentes=',presentes)
        #depois, valores ja definidos em uma regiao sao eliminados
        for j in tabuleiro['regioes'][i]:
            if valores[j]:
                for elem in presentes:
                    valores[j].remove(
                        elem
                    )
    
    #entao, valores adjacentes sao eliminados
    for i in range(tabuleiro['tamanho']**2):
        if i // t != 0 and tabuleiro['celulas'][i-t][1] in valores[i]: #acima
            valores[i].remove(tabuleiro['celulas'][i-t][1])
        if i // t != t-1 and tabuleiro['celulas'][i+t][1] in valores[i]: #abaixo
            valores[i].remove(tabuleiro['celulas'][i+t][1])
        if i % t != 0 and tabuleiro['celulas'][i-1][1] in valores[i]: #esquerda
            valores[i].remove(tabuleiro['celulas'][i-1][1])
        if i % t != t-1 and tabuleiro['celulas'][i+1][1] in valores[i]: #direita
            valores[i].remove(tabuleiro['celulas'][i+1][1])

    tabuleiro['valores'] = valores





'''
algoritmo resolucao


1) verificar celulas com apenas um valor possivel e preenche-la

2) verificar regioes com apenas uma celula compativel com um certo valor
e preencher esta

3) verificar regioes com apenas uma coluna com valores mutaveis
e preencher com estes em ordem ascendente

importante: ao inserir valor em celula, notificar as adjacentes e as de sua
regiao para atualizar seus valores possiveis

Repetir passos 1 - 3 ate nao houver mudanca

4) preencher as celulas restantes com backtracking

---
backtracking, from wikipedia, the free encyclopedia

# root(P): return the partial candidate at the root of the search tree.
# reject(P,c): return true only if the partial candidate c is not worth completing.
# accept(P,c): return true if c is a solution of P, and false otherwise.
# first(P,c): generate the first extension of candidate c.
# next(P,s): generate the next alternative extension of a candidate, after the extension s.
# output(P,c): use the solution c of P, as appropriate to the application.

def backtrack(c):
    if reject(P, c): return
    if accept(P, c): output(P, c)
    s = first(P, c)
    while s != None:
        backtrack(s)
        s = next(P, s)

'''
def checar_insercao_vizinhos(pos,val,tabuleiro):
    t = tabuleiro['tamanho']
    return not (
        (pos // t != 0 and tabuleiro['celulas'][pos-t] == val) or
        (pos // t != t-1 and tabuleiro['celulas'][pos+t] == val) or
        (pos % t != 0 and tabuleiro['celulas'][pos-1] == val) or
        (pos % t != t-1 and tabuleiro['celulas'][pos+1] == val)
    )

def backtracking(tabuleiro):
    pass

def aleatorio(tabuleiro:dict,tolerancia:int=99,tentativas=10):
    original = tabuleiro.copy()
    resolvido = False
    for tries in range(tentativas):
        tabuleiro = original.copy()
        procurando = True
        for i in tabuleiro['regioes']:
            for iteracao in range(tolerancia):
                aleas = random.sample(tabuleiro['regioes'][i],len(tabuleiro['regioes'][i]))
                for j in range(len(aleas)):
                    if not checar_insercao_vizinhos(
                        aleas[j],
                        tabuleiro['valores'][i][j], 
                        tabuleiro):
                        break
                else:
                    for item in range(len(tabuleiro['regioes'][i])):
                        tabuleiro['celulas'][tabuleiro['regioes'][i][item]] = aleas[item]
                    procurando = False
                    break
            if procurando == False:
                break
        else:
            resolvido = True
        if resolvido: 
            break
    if resolvido:
        return tabuleiro
    else:
        return False
        
'''
teste = montar_tabuleiro('Puzzles/Kojun_12.txt')
teste = aleatorio(teste)
print_tabuleiro(teste)
'''
print('\n\nesquece, vamo direto pra Haskell')
