
'extrair dados do tabuleiro'
def abrir_arquivo(nome):
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

    'coloca [valor_inicial, regiao, imutavel] na matriz'
    tabuleiro['celulas']=[
            [
                dados['iniciais'][i],       # valor inicial
                dados['regioes'][i],        # regiao a qual pertence
                (dados['iniciais'][i] != 0) # se for imutavel
            ] for i in range(t**2)
        ]

    'coloca coordenadas da celula na regiao correspondente'
    tabuleiro['regioes'] = {}
    for i in range(t**2):
            if tabuleiro['celulas'][i][1] in tabuleiro['regioes']:
                tabuleiro['regioes'][tabuleiro['celulas'][i][1]].append(i)
            else:
                tabuleiro['regioes'][tabuleiro['celulas'][i][1]] = [i]
    
    'marca valores possiveis [1..N] para regiao de tamanho N'
    for i in tabuleiro['regioes']:
        tamanho = len(tabuleiro['regioes'][i])
        for j in tabuleiro['regioes'][i]:
            if tabuleiro['celulas'][j][2] == 0: 
                tabuleiro['celulas'][j][0] = [i+1 for i in range(tamanho)]
    
    return tabuleiro

def montar_tabuleiro(nome_arquivo):
    tab = abrir_arquivo(nome_arquivo)
    tab = criar_tabuleiro(tab)
    return tab
    
def print_tabuleiro(tabuleiro):
    tamanho = tabuleiro['tamanho']
    print("Celulas:")
    for i in range(tamanho):
        print("\nLinha %d:\n" % (i))
        for j in range(tamanho):
            print(tabuleiro['celulas'][i*tamanho+j])
    print("\nRegioes:\n")
    for i in tabuleiro['regioes']:
        print(tabuleiro['regioes'][i])

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

def backtracking(tabuleiro):
    pass

teste = montar_tabuleiro('Puzzles/Kojun_12.txt')
print_tabuleiro(teste)
