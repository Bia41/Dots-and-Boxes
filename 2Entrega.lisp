;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;GRUPO AL045;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Ana Rita Pereira 69899;;;;;;
;;;;Beatriz Bernardo 69979;;;;;;
;;;;;;Afonso Silva 69451;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Tipos Abstractos de Informacao;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;Tipo Posicao;;;;;;;;;;;

; Estrutura posicao tem identificadores dos fios e a moeda de uma posicao
(defstruct posStruct
  fios
  moeda)

;cria-posicao: inteiro x inteiro -> posicao 
;Este construtor recebe uma linha e uma coluna, e retorna uma posicao com a linha e coluna recebidas. 
(defun cria-posicao(l c)
  (cons l c))

;posicao-linha: posicao -> inteiro 
;Este selector, dada uma posicao p, retorna a linha correspondente a posicao. 
(defun posicao-linha(p)
  (car p))

;posicao-coluna: posicao -> inteiro 
;Este selector, dada uma posicao p, retorna a coluna correspondente a posicao.
(defun posicao-coluna(p)
  (cdr p))

;posicao-p: universal -> logico 
;Este reconhecedor recebe um objecto x qualquer e retorna T se o objecto recebido for uma 
;posicao, e NIL caso contrario.
(defun posicao-p(x)
  (consp x))

;posicoes-iguais-p: posicao x posicao -> logico 
;Este teste recebe duas posicoes p1 e p2, e retorna T se p1 e p2 corresponderem a mesma posicao 
;e NIL caso contrario. 
(defun posicoes-iguais-p(p1 p2)
  (equal p1 p2))
  

;;;;;;;;;;;Tipo Fio;;;;;;;;;;;;;

;cria-fio: inteiro x posicao x posicao -> fio 
;Este construtor recebe o id do fio no tabuleiro, a posicao da moeda de origem no tabuleiro, e outra 
;posicao que corresponde a posicao da moeda destino. Retorna o fio que liga as duas posicoes. 
(defun cria-fio(id p1 p2)
  (list id p1 p2))

;fio-id: fio -> inteiro 
;Este selector recebe um fio e retorna o identificador desse fio no tabuleiro.
(defun fio-id(f)
  (first f))

;fio-origem: fio -> posicao
;Este selector recebe um fio e retorna a posicao correspondente a origem do fio.
(defun fio-origem(f)
  (nth 1 f))

;fio-destino: fio -> posicao 
;Este selector recebe um fio e retorna a posicao correspondente ao destino do fio. 
(defun fio-destino(f)
  (nth 2 f))

;fio-p: universal -> logico 
;Este reconhecedor recebe um objecto qualquer x, e retorna T se x for um fio, e NIL caso contrario. 
(defun fio-p(x)
  (and (consp x) (= (length x) 3)))


;;;;;;;;;Tipo Tabuleiro;;;;;;;;;

;Estrutura que tem o ultimo id a ser utilizado em fios, a hashtable de fios e a matriz de posicoes.
(defstruct tabuleiro 
  idActual 
  fiosHash 
  posicoes)

;cria-tabuleiro: inteiro x inteiro -> tabuleiro 
;Este construtor recebe numero de linhas e o numero de colunas. Retorna um tabuleiro vazio. 
(defun cria-tabuleiro(nl nc)
  (let ((tab (make-array (list nl nc))))
    (dotimes (l (array-dimension tab 0))
      (dotimes (c (array-dimension tab 1))
        (setf (aref tab l c) (make-posStruct :fios nil :moeda nil))))
    (make-tabuleiro :idActual 0 :fiosHash (make-hash-table) :posicoes tab)))

;copia-tabuleiro: tabuleiro -> tabuleiro 
;Este construtor recebe um tabuleiro, e cria um novo tabuleiro cujo conteudo e o mesmo do 
;tabuleiro recebido.
(defun copia-tabuleiro(tabuleiro)
  (let* ((l (tabuleiro-linhas tabuleiro))
          (c (tabuleiro-colunas tabuleiro))
          (tab (cria-tabuleiro l c)))
          (setf (tabuleiro-idActual tab) (tabuleiro-idActual tabuleiro)) 
          (setf (tabuleiro-fiosHash tab) (make-hash-table :initial-contents (hash-key-value (tabuleiro-fiosHash tabuleiro))))
          (setf (tabuleiro-posicoes tab) (copia-estrutura (tabuleiro-posicoes tabuleiro) l c)) 
        tab))

;hash-key-value: hash-table -> lista
;Este selector e uma funcao auxiliar que vai a todos os elementos da hash e retira apenas os values,
;colocando-os numa lista.
(defun hash-key-value(hash-table)
  (let ((keys nil))
  (loop for key being the hash-keys of hash-table do 
    (setf keys (append (list (cons key (gethash key hash-table))) keys)))
  keys))

;copia-estrutura: tabuleiro x inteiro x inteiro -> tabuleiro
;Esta funcao auxiliar que cria um novo array e copia todas as posicoes do array dado,
;retorna esse novo array.
(defun copia-estrutura (tabuleiro nl nc)
  (let ((tab (make-array (list nl nc))))
  (dotimes (l nl)
    (dotimes (c nc)
      (setf (aref tab l c) (posStruct-copy (aref tabuleiro l c)))))
  tab))

;posStruct-copy: estrutura -> estrutura
;Esta funcao auxiliar copia a estrutura recebida, colocando na nova estrutura todas as informacoes. 
(defun posStruct-copy (estrutura)
  (make-posStruct :fios (copy-list (posStruct-fios estrutura)) :moeda (posStruct-moeda estrutura)))

;tabuleiro-linhas: tabuleiro -> inteiro 
;Este selector recebe um tabuleiro e devolve o numero de linhas do tabuleiro. 
(defun tabuleiro-linhas(tabuleiro)
  (array-dimension (tabuleiro-posicoes tabuleiro) 0))

;tabuleiro-colunas: tabuleiro -> inteiro 
;Este selector recebe um tabuleiro e devolve o numero de colunas do tabuleiro.
(defun tabuleiro-colunas(tabuleiro)
  (array-dimension (tabuleiro-posicoes tabuleiro) 1))

;tabuleiro-fios: tabuleiro -> lista de fios 
;Este selector recebe um tabuleiro e devolve uma lista com todos os fios existentes no tabuleiro. 
(defun tabuleiro-fios(tabuleiro)
  (let((lista nil))
    (loop for value being the hash-values of (tabuleiro-fiosHash tabuleiro) do
        (setf lista (append (list value) lista)))
    lista))

;tabuleiro-fio-com-id: tabuleiro x inteiro -> fio 
;Esse selector recebe um tabuleiro e o identificador de um fio, e retorna o fio do tabuleiro com esse
;identificador. Caso nao exista e retornado NIL. 
(defun tabuleiro-fio-com-id(tabuleiro id)
  (if (not (gethash id (tabuleiro-fiosHash tabuleiro)))
    nil
  (let ((fio (gethash id (tabuleiro-fiosHash tabuleiro))))
  fio))) 
  
;tabuleiro-fios-posicao: tabuleiro x posicao -> lista de fios 
;Este selector recebe um tabuleiro e uma posicao do tabuleiro, e retorna uma lista com todos os 
;fios que estao ligados a posicao recebida. 
(defun tabuleiro-fios-posicao(tabuleiro posicao)
  (let ((listaOut nil)
        (aux nil)
        (fios (posStruct-fios(aref (tabuleiro-posicoes tabuleiro) (posicao-linha posicao) (posicao-coluna posicao)))))
  (setf aux (percorre-fios tabuleiro fios listaOut))
  aux))

;percorre-fios: tabuleiro x lista x lista -> lista
;Esta funcao auxiliar recebe o tabuleiro, uma lista que corresponde a todos fios de uma posicao, 
;e uma lista vazia. Percorre a lista de fios e guarda apenas os ids na segunda lista. Retorna a nova lista
(defun percorre-fios (tabuleiro listaIn listaOut)
  (if (eq listaIn nil)
    listaOut
    (progn
      (setf listaOut (append (list(tabuleiro-fio-com-id tabuleiro (car listaIn))) listaOut))
      (percorre-fios tabuleiro (cdr listaIn) listaOut))))
 
;tabuleiro-moeda-posicao: tabuleiro x posicao -> inteiro 
;Este selector recebe um tabuleiro e uma posicao, e retorna o valor da moeda que esta nessa posicao. 
;Este valor e um inteiro entre 1 e 9. Caso nao exista moeda na posicao recebida e retornado NIL.
(defun tabuleiro-moeda-posicao(tabuleiro posicao)
  (posStruct-moeda (aref (tabuleiro-posicoes tabuleiro) (posicao-linha posicao) (posicao-coluna posicao))))

;tabuleiro-total-moedas: tabuleiro -> inteiro
;Este selector recebe um tabuleiro e devolve a soma do valor de todas as moedas que estao no 
;tabuleiro. Se nao existir nenhuma moeda no tabuleiro, devera retornar 0.
(defun tabuleiro-total-moedas(tabuleiro)
  (let ((soma 0))
    (dotimes (l (array-dimension (tabuleiro-posicoes tabuleiro) 0))
      (dotimes (c (array-dimension (tabuleiro-posicoes tabuleiro) 1))
        (if(not (eq (posStruct-moeda (aref (tabuleiro-posicoes tabuleiro) l c)) nil))
          (setf soma (+ soma (posStruct-moeda (aref (tabuleiro-posicoes tabuleiro) l c)))))))
    soma))

;tabuleiro-moedas: tabuleiro -> lista de moedas
;Este selector e uma funcao auxiliar das heuristicas, recebe um tabuleiro e devolve a lista de todas as moedas 
;que se encontram nesse tabuleiro. Se nao existir nenhuma moeda no tabuleiro, retorna uma lista vazia.
(defun tabuleiro-moedas(tabuleiro)
  (let ((lista nil))
    (dotimes (l (tabuleiro-linhas tabuleiro))
      (dotimes (c (tabuleiro-colunas tabuleiro))
        (if(not (eq (tabuleiro-moeda-posicao tabuleiro (cria-posicao l c)) nil))
          (setf lista (append (list (list (tabuleiro-moeda-posicao tabuleiro (cria-posicao l c)) (cria-posicao l c))) lista)))))
    lista))

;tabuleiro-adiciona-fio!: tabuleiro x posicao x posicao -> {} 
;Este modificador recebe um tabuleiro, e duas posicoes adjacentes, e cria um fio que liga as duas 
;posicoes recebidas, adicionando-o ao tabuleiro. Esta funcao altera o tabuleiro recebido para que este
;contenha o novo fio criado. O identificador do fio criado corresponde a ordem de insercao do mesmo. 
(defun tabuleiro-adiciona-fio!(tabuleiro posicao1 posicao2)
  (let ((novoId (incf (tabuleiro-idActual tabuleiro)))
    (pos1 (aref (tabuleiro-posicoes tabuleiro) (posicao-linha posicao1) (posicao-coluna posicao1)))
    (pos2 (aref (tabuleiro-posicoes tabuleiro) (posicao-linha posicao2) (posicao-coluna posicao2))))
    (setf (posStruct-fios pos1) 
          (append (list novoId) (posStruct-fios pos1)))
    (setf (posStruct-fios pos2) 
          (append (list novoId) (posStruct-fios pos2)))
    (setf (gethash novoId (tabuleiro-fiosHash tabuleiro)) (cria-fio novoId posicao1 posicao2))
    nil))

;tabuleiro-adiciona-moeda-posicao!: tabuleiro x posicao x inteiro -> {} 
;Este modificador recebe um tabuleiro, uma posicao do tabuleiro, e o valor de uma moeda e adiciona
;a moeda a posicao recebida do tabuleiro. Se ja existia uma moeda nessa posicao, e substituida.
;Esta funcao nao retorna nada, mas altera o tabuleiro recebido de modo a que este contenha a moeda. 
(defun tabuleiro-adiciona-moeda-posicao!(tabuleiro posicao moeda)
  (setf (posStruct-moeda (aref (tabuleiro-posicoes tabuleiro) (posicao-linha posicao) (posicao-coluna posicao))) moeda)
  nil)

;tabuleiro-remove-fio-com-id!: tabuleiro x inteiro -> {} 
;Este modificador recebe um tabuleiro e o identificador de um fio do tabuleiro a remover. Esta funcao
;nao retorna nada, mas altera o tabuleiro recebido removendo o fio em questao.
(defun tabuleiro-remove-fio-com-id!(tabuleiro id)
  (if (not (gethash id (tabuleiro-fiosHash tabuleiro)))
    nil
  (progn
    (let* ((fio (tabuleiro-fio-com-id tabuleiro id))
          (posOrigem (fio-origem fio))
          (posDestino (fio-destino fio))
          (fiosOrigem (posStruct-fios (aref (tabuleiro-posicoes tabuleiro) (posicao-linha posOrigem) (posicao-coluna posOrigem))))
          (fiosDestino (posStruct-fios (aref (tabuleiro-posicoes tabuleiro) (posicao-linha posDestino) (posicao-coluna posDestino)))))
    (setf (posStruct-fios (aref (tabuleiro-posicoes tabuleiro) (posicao-linha posOrigem) (posicao-coluna posOrigem))) (delete id fiosOrigem))
    (setf (posStruct-fios (aref (tabuleiro-posicoes tabuleiro) (posicao-linha posDestino) (posicao-coluna posDestino))) (delete id fiosDestino))
    (remhash id (tabuleiro-fiosHash tabuleiro))
  nil))))

;tabuleiro-remove-moeda-posicao!: tabuleiro x posicao -> {} 
;Este modificador recebe um tabuleiro e uma posicao, e remove a moeda que se encontra no tabuleiro na 
;posicao recebida. Esta funcao nao retorna nada, mas altera o tabuleiro recebido removendo a moeda 
;em questao. Caso nao exista nenhuma moeda na posicao recebida, esta funcao nao faz nada. 
(defun tabuleiro-remove-moeda-posicao! (tabuleiro posicao)
  (setf (posStruct-moeda (aref (tabuleiro-posicoes tabuleiro) (posicao-linha posicao) (posicao-coluna posicao))) nil))
      
  
;;;;;;;;;;;Tipo Jogo;;;;;;;;;;;;

;Esta estrutura Jogo contem os pontos do jogador 1, os pontos do jogador 2, o proximo jogador, a lista 
;de fios removidos e o tabuleiro desse jogo.
(defstruct jogo 
  ptJogador1 
  ptJogador2 
  proximoJogador 
  fiosRemovidos 
  tab)

;cria-jogo: tabuleiro -> jogo 
;Este construtor recebe um tabuleiro e retorna o jogo que corresponde ao estado inicial do jogo 
;para o tabuleiro recebido. Ou seja, ambos os jogadores comecam com 0 pontos, o proximo 
;jogador a jogar e o jogador 1, e ainda nenhuma jogada foi efectuada. 
(defun cria-jogo (tabuleiro)
  (make-jogo :ptJogador1 0 :ptJogador2 0 :proximoJogador 1 :fiosRemovidos nil :tab tabuleiro))

;copia-jogo: jogo -> jogo
;Este construtor recebe um jogo, e devolve um novo jogo cujo conteudo e o mesmo do jogo recebido. O 
;jogo retornado tem que ser uma nova copia, se o jogo original for alterado, o jogo retornado nao pode mudar. 
(defun copia-jogo (jogo)
  (let ((jog (cria-jogo (copia-tabuleiro (jogo-tabuleiro jogo)))))

    (setf (jogo-ptJogador1 jog) (jogo-ptJogador1 jogo))
    (setf (jogo-ptJogador2 jog) (jogo-ptJogador2 jogo))
    (setf (jogo-proximoJogador jog) (jogo-proximoJogador jogo))
    (setf (jogo-fiosRemovidos jog) (jogo-fiosRemovidos jogo))
  jog))

;jogo-tabuleiro: jogo -> tabuleiro 
;Este selector recebe um jogo e retorna o tabuleiro com o estado do jogo. 
(defun jogo-tabuleiro (game)
  (jogo-tab game))

;jogo-jogador: jogo -> inteiro 
;Este selector recebe um jogo e retorna um inteiro (1 ou 2) que indica qual o jogador a jogar. 
(defun jogo-jogador (game)
  (jogo-proximoJogador game))

;jogo-pontos-jogador1: jogo -> inteiro 
;Este selector recebe um jogo e retorna um inteiro que indica o numero de pontos acumulados pelo jogador1. 
(defun jogo-pontos-jogador1 (game)
  (jogo-ptJogador1 game))

;jogo-pontos-jogador2: jogo -> inteiro 
;Este selector recebe um jogo e retorna um inteiro que indica o numero de pontos acumulados pelo jogador2. 
(defun jogo-pontos-jogador2 (game)
  (jogo-ptJogador2 game))

;jogo-historico-jogadas: jogo -> lista de inteiros 
;Este selector recebe um jogo e retorna uma lista com todas as jogadas efectuadas desde o inicio do 
;jogo. Como as jogadas correspondem a remover fios, este procedimento retorna uma lista com os identificadores
;dos fios removidos. Os elementos da lista estao ordenados pela ordem da jogada. 
(defun jogo-historico-jogadas (game)
  (jogo-fiosRemovidos game))

;soma-pontos-jogador: jogo x posicao -> {}
;Esta funcao auxiliar recebe um jogo e uma posicao, ve qual e o jogador que esta a jogar, e soma os
;pontos da moeda dessa posicao a esse jogador. Esta funcao nao retorna nada, mas altera o tabuleiro,
;retirando a moeda dessa posicao. 
(defun soma-pontos-jogador (game posicao)
  (if (= (jogo-proximoJogador game) 1)
    (setf (jogo-ptJogador1 game) (+ (jogo-pontos-jogador1 game) (tabuleiro-moeda-posicao (jogo-tab game) posicao)))
    (setf (jogo-ptJogador2 game) (+ (jogo-pontos-jogador2 game) (tabuleiro-moeda-posicao (jogo-tab game) posicao))))
  (tabuleiro-remove-moeda-posicao! (jogo-tab game) posicao))

;jogo-aplica-jogada!: jogo x inteiro -> {} 
;Este modificador e responsavel por efectuar uma jogada. Como as jogadas correspondem a remover fios,
;este procedimento recebe um jogo e o identificador do fio a remover. Este procedimento nao retorna 
;nada, mas altera o jogo recebido de modo a aplicar a jogada feita. Primeiro remove-se o fio. Se o fio
;removido corresponder ao ultimo fio de uma ou mais moedas, essas moedas sao tambem removidas do tabuleiro,
;e os seus pontos atribuidos ao jogador que fez a jogada. Nesta situacao, o jogador joga novamente. Caso 
;nao seja o ultimo fio de nenhuma moeda, apenas se remove o fio, e passa-se o turno ao proximo jogador. 
(defun jogo-aplica-jogada! (game id)
  (let* ((tabu (jogo-tab game))
        (fio (tabuleiro-fio-com-id tabu id))
        (posOrigem (fio-origem fio))
        (posDestino (fio-destino fio))
        (idJogador (jogo-proximoJogador game)))

  (if (not (eq fio nil))
    (progn
      (tabuleiro-remove-fio-com-id! (jogo-tab game) id)
      (setf (jogo-fiosRemovidos game) (append (jogo-historico-jogadas game) (list id)))
      (if (and (not (eq (tabuleiro-fios-posicao (jogo-tab game) posOrigem) nil)) (not (eq (tabuleiro-fios-posicao (jogo-tab game) posDestino) nil)))
        (progn
          (if (= idJogador 1) 
            (setf (jogo-proximoJogador game) 2)
            (setf (jogo-proximoJogador game) 1)))
        (progn
          (if (eq (tabuleiro-fios-posicao (jogo-tab game) posOrigem) nil)
            (soma-pontos-jogador game posOrigem))
          (if (eq (tabuleiro-fios-posicao (jogo-tab game) posDestino) nil)
            (soma-pontos-jogador game posDestino))))
        nil)
    nil)))

;jogo-terminado-p: jogo -> logico 
;Este reconhecedor recebe um jogo e verifica se o jogo recebido corresponde a um jogo que 
;terminou. Um jogo termina quando nao existem mais fios para serem removidos. 
(defun jogo-terminado-p(game)
  (zerop (hash-table-count (tabuleiro-fiosHash (jogo-tab game)))))


;;;;;;;;;Tipo Problema;;;;;;;;;;

;Esta estrutura Problema contem o estado-inicial que corresponde ao jogo, o jogador que corresponde
;a chamar a funcao jogo-jogador, accoes que corresponde a chamar a funcao accoes, o resultado que
;corresponde a chamar a funcao resultado, o teste-corte-p que corresponde a funcao teste-terminal-p,
;a funcao-avaliacao que corresponde a funcao utilidade, o historico-accoes que corresponde a funcao 
;que recebe um estado do jogo e que retorna uma lista com todas as accoes executadas desde o estado 
;inicial ate o estado recebido. As accoes estao ordenadas pela ordem de execucao, a chave-equivalencia
;funcao que recebe um estado e retorna uma chave do estado.
(defstruct problema
  estado-inicial
  jogador
  accoes
  resultado
  teste-corte-p
  funcao-avaliacao
  historico-accoes
  chave-equivalencia)


;;;;;;Funcoes do Problema;;;;;;;

;ids-fios: hashtable -> lista de inteiros
;Esta funcao auxiliar recebe uma hashtable e retorna apenas as keys. 
(defun ids-fios(hash)
  (let ((keys nil))
    (loop for key being the hash-keys of hash
      do(setf keys (append keys (list key))))
    keys))
  
;accoes: jogo -> lista de inteiros 
;Esta funcao recebe um estado (um jogo) e devolve uma lista com todas as accoes validas para esse 
;estado. Uma accao corresponde a remover um fio, por isso a accao e representada apenas pelo identificador
;do fio a remover. Retorna uma lista com os ids dos fios que sao possiveis remover no jogo recebido.
(defun accoes(game)
  (ids-fios (tabuleiro-fiosHash (jogo-tab game))))

;resultado: jogo x inteiro -> jogo
;Esta funcao recebe um jogo, e o identificador do fio a remover, e retorna um novo jogo que resulta 
;de fazer a accao. Esta funcao nao altera o jogo recebido. 
(defun resultado(game fioId)
  (let ((jogo (copia-jogo game)))
    (jogo-aplica-jogada! jogo fioId)
    jogo))

;teste-terminal-p: jogo x inteiro x inteiro -> logico 
;Esta funcao recebe um jogo, e a profundidade do estado, no entanto ignora este argumento. Retorna T 
;se e so se o jogo recebido for um jogo terminado. Retorna NIL caso contrario. 
(defun teste-terminal-p(game prof)
  (declare (ignore prof))
  (jogo-terminado-p game))

;utilidade: jogo x inteiro -> inteiro 
;Esta funcao recebe um jogo e um identificador de um jogador (1 ou 2) e retorna a utilidade do jogo. 
(defun utilidade(game jogador)
  (if (= jogador 1)
    (- (jogo-pontos-jogador1 game) (jogo-pontos-jogador2 game))
    (- (jogo-pontos-jogador2 game) (jogo-pontos-jogador1 game))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;Minimax;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;minimax: problema x inteiro -> accao x inteiro x inteiro
;Esta funcao recebe um problema e um inteiro (1 ou 2) que indica qual dos jogadores e o jogador max.
;Retorna 3 coisas: a accao escolhida como a melhor accao a ser executada pelo jogador max, o valor 
;minimax dessa mesma accao, e o numero de nos folha visitados na arvore. Chama a funcao auxiliar minimax-aux 
(defun minimax (prob nrjog)
  (minimax-aux (problema-estado-inicial prob) prob nrjog nrjog 0))

;minimax-aux: jogo x problema x inteiro x inteiro x inteiro -> accao x inteiro x inteiro
;Esta funcao auxiliar recebe um jogo, um problema, o numero do jogador que inicia o minimax, o jogador
;a maximizar, e a profundidade. Esta funcao e adaptada do pseudocodigo disponibilizado no Livro, pois 
;um jogador pode jogar varias vezes seguidas. Este algoritmo minimax e independente do problema, ou seja 
;funciona para este problema do jogo Moedas e Fios, mas tambem funciona para qualquer outro problema.
;Retorna 3 coisas: a accao escolhida como a melhor accao a ser executada pelo jogador max, o valor 
;minimax dessa mesma accao, e o numero de nos folha visitados na arvore.
(defun minimax-aux (jog prob nrjog maxis depth)
  (let ((v 0)
       (jog-aux nil)
       (val nil)
       (nrNos 0)
       (jogada nil)) 

  (cond ((funcall (problema-teste-corte-p prob) jog 3) (list (funcall(problema-funcao-avaliacao prob) jog maxis) 1))
    (t (progn
      (if (= nrjog maxis)
        (progn
          (setf v -100000)
          (loop for x in (funcall (problema-accoes prob) jog) do
            (setf jog-aux (funcall (problema-resultado prob) jog x))
            (setf val (minimax-aux jog-aux prob (funcall (problema-jogador prob) jog-aux) maxis (+ depth 1)))
            (setf nrNos (+ nrNos (nth 1 val)))
            (when (> (car val) v)
              (progn
                (setf v (car val))
                (setf jogada x))))
          (if (> depth 0)
            (list v nrNos)
            (values jogada v nrNos)))
              (progn
                (setf v 100000)
                (loop for x in (funcall (problema-accoes prob) jog) do
                  (setf jog-aux (funcall (problema-resultado prob) jog x))
                  (setf val (minimax-aux jog-aux prob (funcall (problema-jogador prob) jog-aux) maxis (+ depth 1)))
                  (setf nrNos (+ nrNos (nth 1 val)))
                  (when (< (car val) v)
                    (progn
                      (setf v (car val))
                      (setf jogada x))))
                (if (> depth 0)
                  (list v nrNos)
                  (values jogada v nrNos)))))))))

;jogador-minimax-simples: jogo x inteiro x inteiro -> accao 
;Esta funcao recebe um jogo, um inteiro (1 ou 2) que indica qual o jogador a jogar, e um inteiro 
;que indica o tempo limite (em segundos) para o jogador tomar uma decisao (este argumento e ignorado).
;Esta funcao implementa um jogador automatico que decide qual a melhor accao para o jogador usando o algoritmo minimax. 
(defun jogador-minimax-simples (game nrjog temp)
  (declare (ignore temp))
  (let ((p1 (make-problema :estado-inicial game 
                          :jogador #'jogo-jogador 
                          :accoes #'accoes 
                          :resultado #'resultado 
                          :teste-corte-p #'teste-terminal-p 
                          :funcao-avaliacao #'utilidade)))
  (nth-value 0 (minimax p1 nrjog))))

;;;;;;;;;;Minimax com Cortes;;;;;;;;;;;;

;minimax-alfa-beta: problema x inteiro -> accao x inteiro x inteiro
;Esta funcao recebe um problema, e um inteiro (1 ou 2) que indica qual o jogador max. Corresponde a executar o
;algoritmo minimax com cortes alfa-beta. Retorna tres coisas: a accao escolhida como melhor accao a ser executada
;pelo jogador max, o valor minimax dessa mesma accao, e o numero de nos folha visitados na arvore. Chama a funcao
;auxiliar minimax-aux-alfabeta.
(defun minimax-alfa-beta (prob nrjog)
  (minimax-aux-alfabeta (problema-estado-inicial prob) prob nrjog nrjog 0 most-negative-fixnum most-positive-fixnum))

;minimax-aux-alfabeta: jogo x problema x inteiro x inteiro x inteiro x inteiro x inteiro -> lista
;Esta funcao auxiliar foi adaptada da funcao minimax-aux para poder suportar cortes alfa beta. Recebe um jogo,
;o problema, qual o jogador, o jogador a maximizar, a profundidade, o alfa e o beta. Retorna uma lista composta
;por 3 inteiros: a jogada, a utilidade e o numero de nos visitados na arvore.
(defun minimax-aux-alfabeta (jog prob nrjog maxis depth alfa beta) 
  (let ((v 0)
       (jog-aux nil)
       (val nil)
       (nrNos 0)
       (jogada nil))

  (cond ((funcall (problema-teste-corte-p prob) jog 3) (list (funcall(problema-funcao-avaliacao prob) jog maxis) 1))
    (t (progn
      (if (= nrjog maxis)
        (progn
          (setf v most-negative-fixnum)
          (loop for x in (funcall (problema-accoes prob) jog) do
            (setf jog-aux (funcall (problema-resultado prob) jog x))
            (setf val (minimax-aux-alfabeta jog-aux prob (funcall (problema-jogador prob) jog-aux) maxis (+ depth 1) alfa beta))
            (setf nrNos (+ nrNos (nth 1 val)))
            (when (> (car val) v)
              (progn
                (setf v (car val))
                (setf jogada x)))
            (if (>= v beta) (return))
            (setf alfa (max alfa v)))
          (if (> depth 0)
            (list v nrNos)
            (values jogada v nrNos)))
        (progn
          (setf v most-positive-fixnum)
          (loop for x in (funcall (problema-accoes prob) jog) do
            (setf jog-aux (funcall (problema-resultado prob) jog x))
            (setf val (minimax-aux-alfabeta jog-aux prob (funcall (problema-jogador prob) jog-aux) maxis (+ depth 1) alfa beta))
            (setf nrNos (+ nrNos (nth 1 val)))
            (when (< (car val) v)
              (progn
                (setf v (car val))
                (setf jogada x)))
            (if (<= v alfa) (return))
            (setf beta (min beta v)))
          (if (> depth 0)
            (list v nrNos)
            (values jogada v nrNos)))))))))


;;;;;;;;;;;Minimax vBest;;;;;;;;;;;;

;jogador-minimax-vbest: jogo x inteiro x inteiro -> accao
;Esta funcao recebe um problema, um inteiro (1 ou 2) que indica qual o jogador max, e um inteiro que indica
;o numero de segundos que o jogador tem para escolher a jogada. Esta funcao garante que e devolvida uma jogada 
;dentro do tempo limite especificado. No entanto, se nao for necessario gastar o tempo total para tomar uma 
;decisao, retorna a jogada o mais rapido possivel. Utiliza a nossa melhor versao do algoritmo minimax (no nosso
;caso o minimax-vbest) e a melhor funcao de avaliacao (a heuristica-v3).
(defun jogador-minimax-vbest (game jogador temp)
  (let ((p1 (make-problema :estado-inicial game 
                          :jogador #'jogo-jogador 
                          :accoes #'accoes 
                          :resultado #'resultado 
                          :teste-corte-p #'teste-terminal-p
                          :funcao-avaliacao #'heuristica-v3)))
  (minimax-vbest p1 jogador temp)))

;minimax-vbest: problema x inteiro x inteiro -> inteiro
;Esta funcao auxiliar recebe um problema, o jogador e o tempo limite para a jogada. Retorna a jogada. Chama iterativamente
;o minimax-aux-alfabeta-temp ate este retornar null (o tempo de resposta acabou) ou ja nao haver mais niveis para 
;analisar
(defun minimax-vbest (prob jogador temp)
  (let* ((maxDepth 0)
        (jogada_aux nil)
        (jogada 0)
        (initialTime (get-internal-real-time))
        (testTime (floor (* (- temp 0.1) internal-time-units-per-second)))
        (endTime (floor (+ initialTime testTime))))

  (loop do
    (setf jogada_aux (minimax-aux-alfabeta-temp (problema-estado-inicial prob) prob jogador jogador 0 most-negative-fixnum most-positive-fixnum endTime (incf maxDepth)))
    (if (not (null jogada_aux))
      (setf jogada (nth 0 jogada_aux)))
    while (and (not (null jogada_aux))
      (> (list-length (funcall(problema-accoes prob) (problema-estado-inicial prob))) maxDepth)))
  jogada))

;minimax-aux-alfabeta-temp: jogo x problema x inteiro x inteiro x inteiro x inteiro x inteiro x inteiro x inteiro x inteiro -> lista
;Esta funcao auxiliar recebe um jogo, um problema, o jogador, o jogado a maximizar, a profundidade, o alfa,
;o beta, a profundidade e o tempo inicial. Retorna uma lista composta por 3 inteiros: a jogada, a utilidade e o
;numero de nos visitados na arvore. Esta funcao foi adaptada da funcao minimax-aux-alfabeta, para alem do caso
;do caso de paragem de um estado terminal temos tambem o caso de paragem de um nivel completo de uma iteracao
;portanto cada vez que se expande uma folha e verificado se o tempo ja ultrapassou o tempo limite - 0.1. Se tiver
;ultrapassado retorna nil.
(defun minimax-aux-alfabeta-temp (jog prob nrjog maxis depth alfa beta endTime maxDepth) 
  (let ((v 0)
       (jog-aux nil)
       (val 0)
       (nrNos 0)
       (jogada nil))

  (cond ((funcall (problema-teste-corte-p prob) jog 3) (list (funcall(problema-funcao-avaliacao prob) jog maxis) 1))
    ((= depth maxDepth) (list (funcall(problema-funcao-avaliacao prob) jog maxis) 1))
    (t (progn
      (if (= nrjog maxis)
        (progn
          (setf v most-negative-fixnum)
          (loop for x in (funcall (problema-accoes prob) jog) do
            (if (> (get-internal-real-time) endTime) 
              (return-from minimax-aux-alfabeta-temp nil)
              (progn
                (setf jog-aux (funcall (problema-resultado prob) jog x))
                (setf val (minimax-aux-alfabeta-temp jog-aux prob (funcall (problema-jogador prob) jog-aux) maxis (+ depth 1) alfa beta endTime maxDepth))))
            (if (null val)
              (return-from minimax-aux-alfabeta-temp nil))
            (setf nrNos (+ nrNos (nth 1 val)))
            (when (> (car val) v)
              (progn
                (setf v (car val))
                (setf jogada x)))
            (if (>= v beta) (return))
            (setf alfa (max alfa v)))
          (if (> depth 0)
            (list v nrNos)
            (list jogada v nrNos)))
        (progn
          (setf v most-positive-fixnum)
          (loop for x in (funcall (problema-accoes prob) jog) do
            (if (> (get-internal-real-time) endTime) 
              (return-from minimax-aux-alfabeta-temp nil)
              (progn
                (setf jog-aux (funcall (problema-resultado prob) jog x))
                (setf val (minimax-aux-alfabeta-temp jog-aux prob (funcall (problema-jogador prob) jog-aux) maxis (+ depth 1) alfa beta endTime maxDepth))))
            (if (null val)
              (return-from minimax-aux-alfabeta-temp nil))
            (setf nrNos (+ nrNos (nth 1 val)))
            (when (< (car val) v)
              (progn
                (setf v (car val))
                (setf jogada x)))
            (if (<= v alfa) (return))
            (setf beta (min beta v)))
          (if (> depth 0)
            (list v nrNos)
            (list jogada v nrNos)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Heuristicas;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;Heuristica v1;;;;;;;;;;;;

;jogador-minimax-v1: jogo x inteiro x inteiro -> accao 
;Esta funcao recebe um jogo, um inteiro (1 ou 2) que indica qual o jogador a jogar, e um inteiro 
;que indica o tempo limite (em segundos) para o jogador tomar uma decisao (este argumento e ignorado).
;Esta funcao implementa um jogador automatico que utiliza a heuristica-v1 como funcao de avaliacao.
(defun jogador-minimax-v1 (game jogador temp)
  (let ((p1 (make-problema :estado-inicial game 
                          :jogador #'jogo-jogador 
                          :accoes #'accoes 
                          :resultado #'resultado 
                          :teste-corte-p #'teste-terminal-p
                          :funcao-avaliacao #'heuristica-v1)))
  (minimax-vbest p1 jogador temp)))

;heuristica-v1: jogo x inteiro -> inteiro
;Esta heuristica recebe um jogo e um jogador, retorna a estimativa da utilidade da jogada. O valor desta heuristica
;e dado pela soma da utilidade com o valor, positivo ou negativo de acordo com o jogador, das moedas que e 
;possivel obter numa sequencia de cortes ate trocar de jogador.
(defun heuristica-v1 (game jogador)
  (let*((novoJogo (copia-jogo game))
        (tab (jogo-tabuleiro novoJogo))
        (moedas (tabuleiro-moedas tab)))

    (if (= jogador (jogo-jogador game))
      (+ (utilidade game jogador) (heuristica-v1-aux tab moedas))
      (- (utilidade game jogador) (heuristica-v1-aux tab moedas)))))

;heuristica-v1-aux: tabuleiro x lista -> inteiro
;Esta funcao auxiliar recebe um tabuleiro e uma lista de moedas, retorna o modulo da estimativa da utilidade 
;da jogada.
(defun heuristica-v1-aux(tabuleiro moedas)
  (let ((valor 0)
        (removido nil)
        (fios nil)
        (fio nil))

    (loop for moeda in moedas do 
      (setf fios (tabuleiro-fios-posicao tabuleiro (cadr moeda)))
      (if (= (length fios) 1)
        (progn
          (setf fio (car fios))
          (tabuleiro-remove-fio-com-id! tabuleiro (fio-id fio))
          (if (sem-fios tabuleiro (fio-origem fio))
            (incf valor (retira-moeda! tabuleiro (fio-origem fio) moedas)))
          (if (sem-fios tabuleiro (fio-destino fio))
            (incf valor (retira-moeda! tabuleiro (fio-destino fio) moedas)))
          (setf removido T))))

    (if (null removido)
      valor
      (+ valor (heuristica-v1-aux tabuleiro moedas)))))


;;;;;;;;;;Heuristica v2;;;;;;;;;;;;

;jogador-minimax-v2: jogo x inteiro x inteiro -> accao 
;Esta funcao recebe um jogo, um inteiro (1 ou 2) que indica qual o jogador a jogar, e um inteiro 
;que indica o tempo limite (em segundos) para o jogador tomar uma decisao (este argumento e ignorado).
;Esta funcao implementa um jogador automatico que utiliza a heuristica-v2 como funcao de avaliacao.
(defun jogador-minimax-v2 (game jogador temp)
  (let ((p1 (make-problema :estado-inicial game 
                          :jogador #'jogo-jogador 
                          :accoes #'accoes 
                          :resultado #'resultado 
                          :teste-corte-p #'teste-terminal-p
                          :funcao-avaliacao #'heuristica-v2)))
  (minimax-vbest p1 jogador temp)))

;heuristica-v2: jogo x inteiro -> inteiro
;Esta heuristica recebe um jogo e um jogador, retorna a estimativa da utilidade da jogada. O valor desta heuristica
;e dado pela soma da utilidade com o valor, positivo ou negativo de acordo com o jogador, das moedas que estao
;ligadas apenas por um fio.
(defun heuristica-v2 (game jogador)
  (let ((valor 0)
        (tab (jogo-tabuleiro game)))

    (loop for moeda in (tabuleiro-moedas tab) do
      (if (= (length (tabuleiro-fios-posicao tab (cadr moeda))) 1)
        (incf valor (car moeda))))

    (if (= jogador (jogo-jogador game))
      (+ (utilidade game jogador) valor)
      (- (utilidade game jogador) valor))))


;;;;;;;;;;Heuristica v3;;;;;;;;;;;;

;jogador-minimax-v3: jogo x inteiro x inteiro -> accao 
;Esta funcao recebe um jogo, um inteiro (1 ou 2) que indica qual o jogador a jogar, e um inteiro 
;que indica o tempo limite (em segundos) para o jogador tomar uma decisao (este argumento e ignorado).
;Esta funcao implementa um jogador automatico que utiliza a heuristica-v3 como funcao de avaliacao.
(defun jogador-minimax-v3 (game jogador temp)
  (let ((p1 (make-problema :estado-inicial game 
                          :jogador #'jogo-jogador 
                          :accoes #'accoes 
                          :resultado #'resultado 
                          :teste-corte-p #'teste-terminal-p
                          :funcao-avaliacao #'heuristica-v3)))
  (minimax-vbest p1 jogador temp)))

;heuristica-v3: jogo x inteiro -> inteiro
;Esta heuristica recebe um jogo e um jogador, retorna a estimativa da utilidade da jogada. O valor desta heuristica
;e dado pela soma da utilidade com o valor, positivo ou negativo de acordo com o jogador, das moedas que estao
;ligadas apenas por um fio e das que estao na outra extremidade desses fios e estao ligadas por menos de 3 fios.
(defun heuristica-v3 (game jogador)
  (let ((valor 0)
        (tab (jogo-tabuleiro game)))

    (loop for moeda in (tabuleiro-moedas tab) do
      (let ((fios (tabuleiro-fios-posicao tab (cadr moeda))))
        (if (= (length fios) 1)
          (incf valor (calcula-valor-livres (car fios) tab)))))

    (if (= jogador (jogo-jogador game))
      (+ (utilidade game jogador) valor)
      (- (utilidade game jogador) valor))))

;;;;;;;;;;Heuristica v4;;;;;;;;;;;;

;jogador-minimax-v4: jogo x inteiro x inteiro -> accao 
;Esta funcao recebe um jogo, um inteiro (1 ou 2) que indica qual o jogador a jogar, e um inteiro 
;que indica o tempo limite (em segundos) para o jogador tomar uma decisao (este argumento e ignorado).
;Esta funcao implementa um jogador automatico que utiliza a heuristica-v4 como funcao de avaliacao.
(defun jogador-minimax-v4 (game jogador temp)
  (let ((p1 (make-problema :estado-inicial game 
                          :jogador #'jogo-jogador 
                          :accoes #'accoes 
                          :resultado #'resultado 
                          :teste-corte-p #'teste-terminal-p
                          :funcao-avaliacao #'heuristica-v4)))
  (minimax-vbest p1 jogador temp)))

;heuristica-v4: jogo x inteiro -> inteiro
;Esta heuristica recebe um jogo e um jogador, retorna a estimativa da utilidade da jogada. O valor desta heuristica
;e dado pela diferenca entre o valor das moedas ligadas por um numero impar de fios e o valor das moedas ligadas
;por um numero par de fios. Este valor e somado ou subtraido a utilidade de acordo com o jogador.
(defun heuristica-v4 (game jogador)
  (let ((valor 0)
        (tab (jogo-tabuleiro game)))

    (loop for moeda in (tabuleiro-moedas tab) do
      (let ((fios (tabuleiro-fios-posicao tab (cadr moeda))))
        (if (oddp (length fios))
          (incf valor (car moeda))
          (decf valor (car moeda)))))

    (if (= jogador (jogo-jogador game))
      (+ (utilidade game jogador) valor)
      (- (utilidade game jogador) valor))))


;;;;;;;;;;Heuristica v5;;;;;;;;;;;;

;jogador-minimax-v5: jogo x inteiro x inteiro -> accao 
;Esta funcao recebe um jogo, um inteiro (1 ou 2) que indica qual o jogador a jogar, e um inteiro 
;que indica o tempo limite (em segundos) para o jogador tomar uma decisao (este argumento e ignorado).
;Esta funcao implementa um jogador automatico que utiliza a heuristica-v5 como funcao de avaliacao.
(defun jogador-minimax-v5 (game jogador temp)
  (let ((p1 (make-problema :estado-inicial game 
                          :jogador #'jogo-jogador 
                          :accoes #'accoes 
                          :resultado #'resultado 
                          :teste-corte-p #'teste-terminal-p
                          :funcao-avaliacao #'heuristica-v5)))
  (minimax-vbest p1 jogador temp)))

;heuristica-v5: jogo x inteiro -> inteiro
;Esta heuristica recebe um jogo e um jogador, retorna a estimativa da utilidade da jogada. E a juncao da
;heuristica-v4 com a heuristica-v3
(defun heuristica-v5 (game jogador)
  (let ((valor 0)
        (tab (jogo-tabuleiro game)))

    (loop for moeda in (tabuleiro-moedas tab) do
      (let ((fios (tabuleiro-fios-posicao tab (cadr moeda))))
        (if (= (length fios) 1)
          (incf valor (calcula-valor-livres (car fios) tab)))
        (if (oddp (length fios))
          (incf valor (car moeda))
          (decf valor (car moeda)))))

    (if (= jogador (jogo-jogador game))
      (+ (utilidade game jogador) valor)
      (- (utilidade game jogador) valor))))


;;;;;;;;;;Heuristica v6;;;;;;;;;;;;

;jogador-minimax-v6: jogo x inteiro x inteiro -> accao 
;Esta funcao recebe um jogo, um inteiro (1 ou 2) que indica qual o jogador a jogar, e um inteiro 
;que indica o tempo limite (em segundos) para o jogador tomar uma decisao (este argumento e ignorado).
;Esta funcao implementa um jogador automatico que utiliza a heuristica-v6 como funcao de avaliacao.
(defun jogador-minimax-v6 (game jogador temp)
  (let ((p1 (make-problema :estado-inicial game 
                          :jogador #'jogo-jogador 
                          :accoes #'accoes 
                          :resultado #'resultado 
                          :teste-corte-p #'teste-terminal-p
                          :funcao-avaliacao #'heuristica-v6)))
  (minimax-vbest p1 jogador temp)))

;heuristica-v6: jogo x inteiro -> inteiro
;Esta heuristica recebe um jogo e um jogador, retorna a estimativa da utilidade da jogada. O valor desta heuristica
;e dado pela soma do valor das moedas ligadas por um numero impar. Este valor e somado ou subtraido a utilidade 
;de acordo com o jogador.
(defun heuristica-v6 (game jogador)
  (let ((valor 0)
        (tab (jogo-tabuleiro game)))

    (loop for moeda in (tabuleiro-moedas tab) do
      (let ((fios (tabuleiro-fios-posicao tab (cadr moeda))))
        (if (oddp (length fios))
          (incf valor (car moeda)))))

    (if (= jogador (jogo-jogador game))
      (+ (utilidade game jogador) valor)
      (- (utilidade game jogador) valor))))


;;;;;;;;;;Auxiliares;;;;;;;;;;;;

;calcula-valor-livres: fio x tabuleiro -> inteiro
;Esta funcao auxiliar recebe um fio e um tabuleiro retornando o valor da soma das moedas ligadas a esse fio e por
;menos de 3 fios no total. 
(defun calcula-valor-livres (fio tabuleiro)
  (let ((valor 0))

    (if(<= (length (tabuleiro-fios-posicao tabuleiro (fio-origem fio))) 2)
      (incf valor (tabuleiro-moeda-posicao tabuleiro (fio-origem fio))))
    (if(<= (length (tabuleiro-fios-posicao tabuleiro (fio-destino fio))) 2)
      (incf valor (tabuleiro-moeda-posicao tabuleiro (fio-destino fio))))
    valor))

;sem-fios: tabuleiro x posicao -> logico
;Esta funcao recebe um tabuleiro e uma posicao, retornando T caso essa posicao nao tenha fios, caso contrario
;retorna null.
(defun sem-fios(tabuleiro posicao)
  (null (tabuleiro-fios-posicao tabuleiro posicao)))

;retira-moeda!: tabuleiro x posicao x lista -> inteiro
;Esta funcao recebe um tabuleiro, uma posicao e a lista de moedas, e remove a moeda do tabuleiro e da lista, e
;devolve o seu valor.
(defun retira-moeda!(tabuleiro posicao moedas)
  (let ((moeda (tabuleiro-moeda-posicao tabuleiro posicao)))

    (tabuleiro-remove-moeda-posicao! tabuleiro posicao)
    (delete (list moeda posicao) moedas)
    moeda))




;;;;;;;;;;;Minimax vBest-tab;;;;;;;;;;;;

;jogador-minimax-vbest-tab: jogo x inteiro x inteiro -> accao
;Esta funcao recebe um problema, um inteiro (1 ou 2) que indica qual o jogador max, e um inteiro que indica
;o numero de segundos que o jogador tem para escolher a jogada. Esta funcao garante que e devolvida uma jogada 
;dentro do tempo limite especificado. No entanto, se nao for necessario gastar o tempo total para tomar uma 
;decisao, retorna a jogada o mais rapido possivel. Utiliza uma versao do algoritmo minimax (minimax-best-tab),
;melhor funcao de avaliacao (a heuristica-v3), a chave de equivalencia que corresponde a uma entrada na tabela 
;de transposicao. 
(defun jogador-minimax-vbest-tab (game jogador temp)
  (let ((p1 (make-problema :estado-inicial game 
                          :jogador #'jogo-jogador 
                          :accoes #'accoes 
                          :resultado #'resultado 
                          :teste-corte-p #'teste-terminal-p
                          :funcao-avaliacao #'heuristica-v3
                          :chave-equivalencia #'chave-eq)))
  (minimax-vbest-tab p1 jogador temp)))

;minimax-vbest-tab: problema x inteiro x inteiro -> inteiro
;Esta funcao auxiliar recebe um problema, o jogador e o tempo limite para a jogada. Retorna a jogada. Chama iterativamente
;o minimax-aux-alfabeta-temp-tab, com uma hash table que representa uma tabela de transposicao, e espera ate esta funcao retornar
; ate este retornar null (o tempo de resposta acabou) ou ja nao haver mais niveis para analisar.
(defun minimax-vbest-tab (prob jogador temp)
  (let* ((maxDepth 0)
        (jogada_aux nil)
        (jogada 0)
        (initialTime (get-internal-real-time))
        (testTime (floor (* (- temp 0.1) internal-time-units-per-second)))
        (endTime (floor (+ initialTime testTime))))

  (loop do
    (setf jogada_aux (minimax-aux-alfabeta-temp-tab (problema-estado-inicial prob) prob jogador jogador 0 
      most-negative-fixnum most-positive-fixnum endTime (incf maxDepth) (make-hash-table)))
    (if (not (null jogada_aux))
        (setf jogada (nth 0 jogada_aux)))
    while (and (not (null jogada_aux))
      (> (list-length (funcall(problema-accoes prob) (problema-estado-inicial prob))) maxDepth)))
  jogada))

;minimax-aux-alfabeta-temp-tab: jogo x problema x inteiro x inteiro x inteiro x inteiro x inteiro x inteiro x inteiro x inteiro x hash -> lista
;Esta funcao auxiliar recebe um jogo, um problema, o jogador, o jogado a maximizar, a profundidade, o alfa,
;o beta, a profundidade, o tempo inicial e uma hashTable. Retorna uma lista composta por 3 inteiros: a jogada e a utilidade.
;Esta funcao foi adaptada da funcao minimax-aux-alfabeta-temp, so que aqui e verificada se a key (proximo-jogador, utilidade,
;numero de fios que faltam remover do tabuleiro) pertence à tabela de transposicao, se pertencer o minimax já nao desce mais 
;niveis, iterativamente, apenas compara os valores de utilidade anteriores. Caso nao pertenca a tabela entao e inserida a nova
;chave com o valor de utilidade correspondente.
(defun minimax-aux-alfabeta-temp-tab (jog prob nrjog maxis depth alfa beta endTime maxDepth hash) 
  (let ((v 0)
       (jog-aux nil)
       (val 0)
       (jogada nil)
       (key nil))

  (cond ((funcall (problema-teste-corte-p prob) jog 3) (funcall(problema-funcao-avaliacao prob) jog maxis))
    ((= depth maxDepth) (funcall(problema-funcao-avaliacao prob) jog maxis))
    (t (progn
      (if (= nrjog maxis)
        (progn
          (setf v most-negative-fixnum)
          (loop for x in (funcall (problema-accoes prob) jog) do
            (if (> (get-internal-real-time) endTime) 
              (return-from minimax-aux-alfabeta-temp-tab nil)
              (progn
                (setf jog-aux (funcall (problema-resultado prob) jog x))
                (setf key (funcall (problema-chave-equivalencia prob) jog-aux))
                (setf val (gethash key hash))
                (if (null val)
                  (progn 
                    (setf val (minimax-aux-alfabeta-temp-tab jog-aux prob (funcall (problema-jogador prob) jog-aux) maxis (+ depth 1) alfa beta endTime maxDepth hash))
                    (setf (gethash key hash) val)))))
            (if (null val)
              (return-from minimax-aux-alfabeta-temp-tab nil))
            (when (> val v)
              (progn
                (setf v val)
                (setf jogada x)))
            (if (>= v beta) (return))
            (setf alfa (max alfa v)))
          (if (> depth 0)
            v
            (list jogada v)))
        (progn
          (setf v most-positive-fixnum)
          (loop for x in (funcall (problema-accoes prob) jog) do
            (if (> (get-internal-real-time) endTime) 
              (return-from minimax-aux-alfabeta-temp-tab nil)
              (progn
                (setf jog-aux (funcall (problema-resultado prob) jog x))
                (setf key (funcall (problema-chave-equivalencia prob) jog-aux))
                (setf val (gethash key hash))
                (if (null val)
                  (progn 
                    (setf val (minimax-aux-alfabeta-temp-tab jog-aux prob (funcall (problema-jogador prob) jog-aux) maxis (+ depth 1) alfa beta endTime maxDepth hash))
                    (setf (gethash key hash) val)))))
            (if (null val)
              (return-from minimax-aux-alfabeta-temp-tab nil))
            (when (< val v)
              (progn
                (setf v val)
                (setf jogada x)))
            (if (<= v alfa) (return))
            (setf beta (min beta v)))
          (if (> depth 0)
            v
            (list jogada v)))))))))

;chave-eq: jogo -> lista
;Esta funcao devolve uma lista que representa uma key numa tabela de transposicao. E composta pelo id do proximo jogador,
;pela utilidade dos pontos dos jogadores e pelos fios do tabuleiro que ainda falta remover.
(defun chave-eq (jogo)
  (list (jogo-jogador jogo) (- (jogo-pontos-jogador2 jogo) (jogo-pontos-jogador1 jogo)) (tabuleiro-fios (jogo-tabuleiro jogo))))



(load "exemplos.fas")
(load "interface-moedas.fas")
