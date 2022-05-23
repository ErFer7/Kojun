
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
                tabuleiro['regioes'][tabuleiro['celulas'][i][1]].append(
                    (i//t,i%t))
            else:
                tabuleiro['regioes'][tabuleiro['celulas'][i][1]] = [(i//t,i%t)]
    return tabuleiro

def montar_tabuleiro(nome_arquivo):
    tab = abrir_arquivo(nome_arquivo)
    tab = criar_tabuleiro(tab)
    return tab

'''
algoritmo resolucao

1) verificar regioes com apenas uma coluna com valores mutaveis
e preencher com estes em ordem ascendente

2) verificar celulas com apenas um valor possivel e preenche-la

3) verificar regioes com apenas uma celula compativel com um certo valor
e preencher esta

importante: ao inserir valor em celula, notificar as adjacentes e as de sua
regiao para atualizar seus valores possiveis

Repetir passos 1 - 3 ate nao houver mudanca

4) preencher as celulas restantes com backtracking

'''

teste = montar_tabuleiro('Puzzles/Kojun_12.txt')
print(teste)
