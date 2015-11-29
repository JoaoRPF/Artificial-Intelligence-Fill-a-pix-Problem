;GRUPO 37 T
;JOAO FERREIRA N76390
;MARIANA VICENTE N76527
;RAQUEL CRISTOVAO N76513

;(load "exemplos.fas")
 (defun ignore-value (x)
 	(declare (ignore x))
 	'ignore)
	
	(time (procura (cria-problema (meu-estado-inicial (make-array (list 4 4) :initial-contents 
 	'((NIL 1 NIL NIL)
 	  (NIL NIL NIL 1)
 	  (1 NIL NIL NIL)
	  (NIL NIL 1 NIL)))) (list #'meu-operador) :objectivo? #'meu-objectivo? :estado= #'equal) "profundidade" :espaco-em-arvore? T))
	  
	  (defun cv-operador (estado)
  (labels ((pos-valida? (x y)
	     (and (>= x 0) (< x 6)
		  (>= y 0) (< y 6)))
	   (livre (x y array)
	     (not (aref array x y))))
  (let ((pos-x (car (cv-estado-posicao estado)))
	(pos-y (cdr (cv-estado-posicao estado)))
	estados)
    (dolist (el '((1 . -2) (2 . -1) (2 . 1) (1 . 2) (-1 . 2) (-2 . 1) (-2 . -1) (-1 . -2)))
      (let ((new-pos-x (+ pos-x (car el)))
	    (new-pos-y (+ pos-y (cdr el))))
	(when (and (pos-valida? new-pos-x new-pos-y)
		   (livre new-pos-x new-pos-y (cv-estado-tabuleiro estado)))
	  (let ((novo-tabluleiro (copy-array (cv-estado-tabuleiro estado))))
	    (setf (aref novo-tabluleiro new-pos-x new-pos-y) (- 36 (cv-estado-jogadas estado)))
	    (push (make-cv-estado :jogadas (1- (cv-estado-jogadas estado))
				  :posicao (cons new-pos-x new-pos-y)
				  :tabuleiro novo-tabluleiro)
		  
		  estados)))))
    (values estados))))
	  


 (defun conjuntos-iguais-p (l1 l2 &key (test #'equal))
 	(and (null (set-difference l1 l2 :test test))
 		 (null (set-difference l2 l1 :test test))))

 (defparameter e0 (make-array (list 3 3) :initial-contents 
 	'((NIL NIL NIL)
 	  (NIL 9 NIL)
 	  (NIL NIL NIL))))

 (defparameter e0_1 (make-array (list 3 3) :initial-contents 
	'((1 NIL 0)
	  (NIL NIL NIL)
	  (0 NIL 0))))

 (defparameter e1_1 (make-array (list 5 5) :initial-contents
	'((NIL NIL 1 NIL NIL)
	  (NIL 1 NIL NIL 5)
	  (1 NIL NIL NIL 6)
	  (NIL NIL NIL 9 NIL)
	  (NIL 5 6 NIL NIL))))

 (defparameter e1_2(make-array (list 5 5) :initial-contents
	'((0 NIL 2 NIL 0)
	  (NIL NIL NIL NIL NIL)
	  (2 NIL 5 NIL 2)
	  (NIL NIL NIL NIL NIL)
	  (0 NIL 2 NIL 0))))

;--------------------------------------------------------------------------------------------------;
;				    VARIAVEIS GLOBAIS															   ;
;--------------------------------------------------------------------------------------------------;

;valorRestricao - Indica o valor da restricao (0-9 ou NIL) presente em cada variavel

(defparameter valorRestricao (list (cons nil nil)))
(defparameter restricaoAtual nil)

;---------------------------------------------------------------------------------------------------;
;				    FUNCOES AUXILIARES																;
;---------------------------------------------------------------------------------------------------;

;ATRIBUICAO-PERCORRE
;FUNCAO AUXILIAR
;recebe: lista
;retorna: lista de atribuicoes existentes 

(defun atribuicao-percorre (lista)
	(cond ((null lista) nil)
		  (t(cond ((eq (cdr(first lista)) nil) (atribuicao-percorre (rest lista)))
			  	  (t (cons(cons (car(first lista)) 
			  					(cdr(first lista)))
			  		 (atribuicao-percorre(rest lista))))))))


;ATRIBUICAO-INICIA
;FUNCAO AUXILIAR
;recebe: lista de atribuicoes
;retorna: lista de atribuicoes com todas as variaveis a terem um valor NIL atribuido

(defun atribuicao-inicia (lista) ;FUNCAO AUXILIAR
	(cond ((null lista) nil)
		  (t (cons (cons (first lista) 
		  			      nil)
		  	       (atribuicao-inicia(rest lista))))))


;-----------------------------------------------------------------------------------------;
;				      TIPO RESTRICAO 													  ;
;-----------------------------------------------------------------------------------------;

;Estrutura restricao
(defstruct restricao  lista-variaveis
					  funcao-verifica)


;CRIA-RESTRICAO
;Construtor
;recebe: uma lista com as variaveis da restricao e uma funcao que verifica a restricao
;retorna: restricao r

(defun cria-restricao(lv pred)
	(make-restricao	:lista-variaveis lv
				   	:funcao-verifica pred))


;RESTRICAO-VARIAVEIS
;Selector
;recebe: restricao r 
;retorna: lista com todas as variaveis da restricao

(defun restricao-variaveis(r) (
	restricao-lista-variaveis r))


;RESTRICAO-FUNCAO-VALIDACAO
;Selector
;recebe: restricao r 
;retorna: funcao de verificacao da restricao(recebe um PSR e retorna T sse o PSR verificar a restricao)

(defun restricao-funcao-validacao (r) (
	restricao-funcao-verifica r))


;-----------------------------------------------------------------------------------------;
;				      TIPO PSR 															  ;
;-----------------------------------------------------------------------------------------;

;Estrutura utilizada nas procuras
 (defstruct psr 	variaveis
 					dominios 
 					restricoes
 					atribuicoes-var)


;CRIA-PSR
;Construtor
;recebe: lista com variaveis, lista com dominios de cada variavel, lista de restricoes
;retorna: psr

(defun cria-psr(listaV listaD listaR )
	(make-psr	:variaveis 	listaV
		 		:dominios 	listaD
		 		:restricoes listaR
				:atribuicoes-var (atribuicao-inicia listaV)))

;PSR-ATRIBUICOES
;Selector
;recebe: psr
;retorna: lista de atribuicoes (valores das variaveis)

(defun psr-atribuicoes (p)
	(atribuicao-percorre(psr-atribuicoes-var p)))


;PSR-VARIAVEIS-TODAS
;SELECTOR
;recebe: psr
;retorna: lista de variaveis do psr

(defun psr-variaveis-todas(p)
	(psr-variaveis p))


;PSR-VARIAVEIS-NAO-ATRIBUIDAS
;SELECTOR
;recebe: psr
;retorna: lista de variaveis nao atribuidas (sem valor) do psr

(defun psr-variaveis-nao-atribuidas (p)
	(let ((lista))
		(dolist (var (psr-atribuicoes-var p))
			(cond ((eq (cdr var) nil) (setq lista (concatenate 'list lista (cons (car var) nil))))
				  (t nil)))
		lista))


;PSR-VARIAVEL-VALOR
;SELECTOR
;recebe: psr e variavel
;retorna: valor da variavel no psr

(defun psr-variavel-valor (p var)
	(dolist (posicao (psr-atribuicoes-var p))
		(cond ((equal var (car posicao)) (return (cdr posicao)))
			  (t nil))))


;PSR-VARIAVEL-DOMINIO
;SELECTOR
;recebe: psr e variavel
;retorna: valores possiveis do dominio da variavel no psr

(defun psr-variavel-dominio (p var)
	(let ((contador 0))
		(dolist (posicao (psr-variaveis p))
			(cond ((equal posicao var) (return (nth contador (psr-dominios p))))
				  (t nil))
		(setq contador (+ 1 contador)))))


;PSR-VARIAVEL-RESTRICOES
;SELECTOR
;recebe: psr e variavel
;retorna: restricoes em que a variavel esta envolvida

(defun psr-variavel-restricoes (p var)
	(let ((lista))
		(dolist (posicao (psr-restricoes p))
			(dolist (variavel (restricao-lista-variaveis posicao))
				(cond ((equal variavel var) (setq lista (concatenate 'list lista (list posicao))))
					  (t nil))))
		lista))


;PSR-ADICIONA-ATRIBUICAO!
;MODIFICADOR
;recebe: psr, variavel e valor
;retorna: nada diretamente (mas coloca faz a atribuicao do valor a variavel)

(defun psr-adiciona-atribuicao! (p var valor)
	(dolist (posicao (psr-atribuicoes-var p))
		(cond ((equal var (car posicao)) (setf(cdr posicao) valor))
			  (t nil))))


;PSR-REMOVE-ATRIBUICAO!
;MODIFICADOR
;recebe: psr e variavel
;retorna: nada diretamente (mas retira a atribuicao do valor presente na variavel)

(defun psr-remove-atribuicao! (p var)
	(dolist (posicao (psr-atribuicoes-var p))
		(cond ((equal var (car posicao)) (setf(cdr posicao) nil))
			(t nil))))


;PSR-ALTERA-DOMINIO!
;MODIFICADOR
;recebe: psr, variavel e dominio
;retorna: nada diretamente (mas altera o dominio da variavel)

(defun psr-altera-dominio! (p var dom)
	(let ((contador 0))
		(dolist (posicao (psr-variaveis p))
			(cond ((equal posicao var) (setf (nth contador (psr-dominios p)) dom))
				  (t nil))
		(setq contador (+ 1 contador)))))


;PSR-COMPLETO-P
;RECONHECEDOR
;recebe: psr
;retorna: T se todas as variaveis tiverem valor atribuido, NIL caso contrario

(defun psr-completo-p (p)
	(cond ((eq (list-length (psr-variaveis p)) (list-length (psr-atribuicoes p))) t)))


;PSR-CONSISTENTE-P
;RECONHECEDOR
;recebe: psr
;retorna: 2 valores - T se o psr verificar todas as restricoes, NIL caso contrario
;					- Numero de testes necessarios para verificar consistencia

(defun psr-consistente-p (p)
	(cond ((null (psr-restricoes p)) (values t 0))
		  (t
			(let ((numTestes 0))
				(dolist (rst (psr-restricoes p))
					(setf restricaoAtual rst)
					(let((resultado (funcall(restricao-funcao-verifica rst) p )))
						(setf numTestes (+ 1 numTestes))
						(cond ((eq resultado nil) (return-from psr-consistente-p(values nil numTestes)))
							  (t (cond ((equal numTestes (list-length (psr-restricoes p))) (return-from psr-consistente-p(values t numTestes))))))))))))


;PSR-VARIAVEL-CONSISTENTE-P
;RECONHECEDOR
;recebe: psr e variavel
;retorna: 2 valores - T se variavel for consistente com as atribuicoes do psr, NIL caso contrario
;					- Numero de testes necessarios para determinar consistencia

(defun psr-variavel-consistente-p (p var)
	(let ((numTestes 0))
		(dolist (rst (psr-restricoes p))
			(dolist (variavel (restricao-lista-variaveis rst))
				(cond ((equal variavel var)
					(setf restricaoAtual rst)
					(let((resultado (funcall(restricao-funcao-verifica rst) p )))
						(setf numTestes (+ 1 numTestes))
						(cond ((eq resultado nil) (return-from psr-variavel-consistente-p(values nil numTestes)))))))))
		(return-from psr-variavel-consistente-p(values t numTestes))))


;PSR-ATRIBUICAO-CONSISTENTE-P
;RECONHECEDOR
;recebe: psr, variavel e valor
;retorna: 2 valores - T se a atribuicao for consistente com as restricoes do psr, NIL caso contrario
; 					- Numero de testes necessarios para determinar consistencia

(defun psr-atribuicao-consistente-p (p var valor)
	(let ((numTestes 0)
		(valorAntigo (psr-variavel-valor p var)))
		  	(dolist (rst (psr-restricoes p)) 
				(dolist (variavel (restricao-lista-variaveis rst))
					(cond ((equal variavel var) 
						(psr-adiciona-atribuicao! p var valor)
						(setf restricaoAtual rst)
						(let ((resultado (funcall(restricao-funcao-verifica rst) p )))
							(setf numTestes (+ 1 numTestes))
							(cond ((eq resultado nil)
									(psr-adiciona-atribuicao! p var valorAntigo)
									(return-from psr-atribuicao-consistente-p(values nil numTestes)))))))))
			(psr-adiciona-atribuicao! p var valorAntigo)
			(return-from psr-atribuicao-consistente-p(values t numTestes))))


;PSR-ATRIBUICOES-CONSISTENTES-ARCO-P
;RECONHECEDOR
;recebe: psr, variavel1, valor1, variavel2 e valor2
;retorna: 2 valores - T caso as atribuicoes sejam consistentes entre si, NIL caso contrario
;					- Numero de testes necessarios para determinar consistencia

(defun psr-atribuicoes-consistentes-arco-p (p var1 valor1 var2 valor2)
	(let ((numTestes 0)
		  (valorAntigo1 (psr-variavel-valor p var1))
		  (valorAntigo2 (psr-variavel-valor p var2)))
				(dolist (rst (psr-restricoes p))
					(dolist (variavel1 (restricao-lista-variaveis rst))
						(dolist (variavel2 (restricao-lista-variaveis rst))
							(cond ((equal variavel1 var1)
								(cond ((equal variavel2 var2)
									(psr-adiciona-atribuicao! p var1 valor1)
									(psr-adiciona-atribuicao! p var2 valor2)
									(setf restricaoAtual rst)
									(let ((resultado (funcall(restricao-funcao-verifica rst) p )))
										(setf numTestes (+ 1 numTestes))
										(cond ((eq resultado nil)
											(psr-adiciona-atribuicao! p var1 valorAntigo1)
											(psr-adiciona-atribuicao! p var2 valorAntigo2)
											(return-from psr-atribuicoes-consistentes-arco-p(values nil numTestes))))))))))))
		(psr-adiciona-atribuicao! p var1 valorAntigo1)
		(psr-adiciona-atribuicao! p var2 valorAntigo2)
		(return-from psr-atribuicoes-consistentes-arco-p(values t numTestes))))


;FILL-A-PIX->PSR
;TRANSFORMADOR
;recebe: array com as restricoes de cada casa
;retorna: psr que representa o problema no array

(defun fill-a-pix->psr (a)
	(let ((linhas (array-dimension a 0))
		  (colunas (array-dimension a 1))
		  (listaVariaveis))
				(loop for i from 0 to (- linhas 1)
					do (loop for j from 0 to (- colunas 1)
						do (setq listaVariaveis (concatenate 'list listaVariaveis (list (concatenate 'string (concatenate 'string (write-to-string i) " ") (write-to-string j)))))))

				(let ((listaDominios))
					(loop for i from 0 to (list-length listaVariaveis)
						do (setq listaDominios (concatenate 'list listaDominios (list(list 0 1)))))

				(let ((listaVariaveisRestricoes)
					  (funcaoRestricoes)
					  (contaVar -1))
					  (loop for i from 0 to (- linhas 1)
						do (loop for j from 0 to (- colunas 1)
							do (setq listaVariaveisRestricoes (concatenate 'list listaVariaveisRestricoes (list(list (concatenate 'string (concatenate 'string (write-to-string i) " ") (write-to-string j))))))
							   (setq contaVar (+ 1 contaVar))

							   ;CIMA ESQUERDA
							   (cond ((>(- i 1) -1)
							   		(cond ((>(- j 1) -1)
							   			(setf (nth contaVar listaVariaveisRestricoes) (concatenate 'list (nth contaVar listaVariaveisRestricoes) (list(concatenate 'string (concatenate 'string (write-to-string (- i 1)) " ") (write-to-string (- j 1))))))))))

							   ;CIMA
							   (cond ((>(- i 1) -1)
							   			(setf (nth contaVar listaVariaveisRestricoes) (concatenate 'list (nth contaVar listaVariaveisRestricoes) (list(concatenate 'string (concatenate 'string (write-to-string (- i 1)) " ") (write-to-string j)))))))

							   ;CIMA DIREITA
							   (cond ((>(- i 1) -1)
							   		(cond ((<(+ j 1) colunas)
							   			(setf (nth contaVar listaVariaveisRestricoes) (concatenate 'list (nth contaVar listaVariaveisRestricoes) (list(concatenate 'string (concatenate 'string (write-to-string (- i 1)) " ") (write-to-string (+ j 1))))))))))

							   ;ESQUERDA
							   (cond ((>(- j 1) -1)
							   			(setf (nth contaVar listaVariaveisRestricoes) (concatenate 'list (nth contaVar listaVariaveisRestricoes) (list(concatenate 'string (concatenate 'string (write-to-string i) " ") (write-to-string (- j 1))))))))

							   ;DIREITA
							   (cond ((<(+ j 1) colunas)
							   			(setf (nth contaVar listaVariaveisRestricoes) (concatenate 'list (nth contaVar listaVariaveisRestricoes) (list(concatenate 'string (concatenate 'string (write-to-string i) " ") (write-to-string (+ j 1))))))))

							   ;BAIXO ESQUERDA
							   	(cond ((<(+ i 1) linhas)
							   		(cond ((>(- j 1) -1)
							   			(setf (nth contaVar listaVariaveisRestricoes) (concatenate 'list (nth contaVar listaVariaveisRestricoes) (list(concatenate 'string (concatenate 'string (write-to-string (+ i 1)) " ") (write-to-string (- j 1))))))))))

							   ;BAIXO
							   (cond ((<(+ i 1) linhas)
							   			(setf (nth contaVar listaVariaveisRestricoes) (concatenate 'list (nth contaVar listaVariaveisRestricoes) (list(concatenate 'string (concatenate 'string (write-to-string (+ i 1)) " ") (write-to-string j)))))))


							   ;BAIXO DIREITA
							   	(cond ((<(+ i 1) linhas)
							   		(cond ((<(+ j 1) colunas)
							   			(setf (nth contaVar listaVariaveisRestricoes) (concatenate 'list (nth contaVar listaVariaveisRestricoes) (list(concatenate 'string (concatenate 'string (write-to-string (+ i 1)) " ") (write-to-string (+ j 1))))))))))

							   (setq funcaoRestricoes (concatenate 'list funcaoRestricoes (list (aref a i j))))))

				(let((restricoes)
					 (contador 0))
						(dolist (rst listaVariaveisRestricoes)
							(cond ((eq (nth contador funcaoRestricoes) nil))
								  (t
								  	(setq valorRestricao (concatenate 'list valorRestricao (cons(cons (first rst)(nth contador funcaoRestricoes))nil)))
									(setq restricoes (concatenate 'list restricoes (list(cria-restricao rst																										
																										#'(lambda (psr)
																											(let ((retorno))
																												(dolist (var1 valorRestricao)
																													;(dolist (var2 (psr-restricoes psr))
																													
																														(cond ((string= (car var1) (first(restricao-lista-variaveis restricaoAtual)))
																															
																															(let ((valor (cdr var1)))
																																(let ((numZeros 0)
																																  	  (numUns 0)
																																 	  (atribuicao)
																																  	  (limiteZeros (-(list-length (restricao-lista-variaveis restricaoAtual)) valor))
																																  	  (numRestricao valor))
																																	    (dolist (var3 (restricao-lista-variaveis restricaoAtual))
																																			(setf atribuicao (psr-variavel-valor psr var3))

																																			(cond ((eq atribuicao nil))
																																				  ((equal atribuicao 0)
																				  																  		(setf numZeros (+ 1 numZeros)))
																																				  ((equal atribuicao 1)
																																						(setf numUns (+ 1 numUns))))

																																			(cond ((> numZeros limiteZeros) (setf retorno 0))
																																				  ((> numUns numRestricao) 	(setf retorno 0))
																																				  (t (setf retorno 1)))))))))
																											(if (equal retorno 0) nil) 
																											(if (equal retorno 1) T)))))))))
																											
																												  
																													


																																	   

									(setf contador (+ 1 contador)))

				(let ((psrCriado))
					(setf psrCriado (cria-psr listaVariaveis listaDominios restricoes))

				psrCriado))))))


;PSR->FILL-A-PIX 
;TRANSFORMADOR
;recebe: psr (resolvido), linhas, colunas
;retorna: array de dimensao linhas,colunas com o valor correto para cada variavel

(defun psr->fill-a-pix (p linhas colunas)
	(let((resultado (make-array (list linhas colunas)))
		 (i)
		 (j))
			(dolist (var (psr-variaveis p))
				(setf i (parse-integer (string (char var 0))))
				(setf j (parse-integer (string (char var 2))))
				(setf (aref resultado i j) (psr-variavel-valor p var)))
	resultado))



;-----------------------------------------------------------------------------------------;
;				  FUNCOES DE PROCURA E RESOLUCAO DE PROBLEMAS							  ;
;-----------------------------------------------------------------------------------------;

;PROCURA-RETROCESSO-SIMPLES
;FUNCAO DE PROCURA
;recebe: psr
;retorna: 2 valores - psr resolvido ou NIL caso nao seja possivel resolver
;					- Numero de testes de consistencia que foram necessarios para o resolver

(defun procura-retrocesso-simples (p)
	(let ((atribuicao)
		  (numTestes)
		  (resultado)
		  (numTestesGeral 0))

		(cond ((eq (psr-completo-p p) T)
			(return-from procura-retrocesso-simples (values p numTestesGeral))))

		(let ((variavel (first(psr-variaveis-nao-atribuidas p))))
 			 (dolist (dominio (psr-variavel-dominio p variavel))
 	  			(multiple-value-bind (atribuicaoAux numTestesAux) (psr-atribuicao-consistente-p p variavel dominio) (setf atribuicao atribuicaoAux)
																													(setf numTestes numTestesAux)
 																													(setf numTestesGeral (+ numTestes numTestesGeral)))
 	  			(cond ((eq atribuicao T)
 		  			(psr-adiciona-atribuicao! p variavel dominio)
 		  			(multiple-value-bind (resultadoAux testesAux) (procura-retrocesso-simples p) (setf resultado resultadoAux)
 		  																						 (setf numTestesGeral (+ testesAux numTestesGeral)))
 		  			(cond ((eq resultado nil))
 		  				  (t 
 		  					(return-from procura-retrocesso-simples (values resultado numTestesGeral)))))))
					(psr-remove-atribuicao! p variavel)

		(return-from procura-retrocesso-simples (values nil numTestesGeral)))))


;PROCURA-RETROCESSO-GRAU
;FUNCAO DE PROCURA
;recebe: psr
;retorna: 2 valores - psr resolvido ou NIL caso nao seja possivel resolver
;					- Numero de testes de consistencia que foram necessarios para o resolver

(defun procura-retrocesso-grau (p)
	(let ((atribuicao)
		  (numTestes)
		  (resultado)
		  (numTestesGeral 0)) 

		(cond ((eq (psr-completo-p p) T)
			(return-from procura-retrocesso-grau (values p numTestesGeral))))

		(let ((variavel (heuristica-grau p)))
 			 (dolist (dominio (psr-variavel-dominio p variavel))
 	  			(multiple-value-bind (atribuicaoAux numTestesAux) (psr-atribuicao-consistente-p p variavel dominio) (setf atribuicao atribuicaoAux)
																													(setf numTestes numTestesAux)
 																													(setf numTestesGeral (+ numTestes numTestesGeral)))
 	  			(cond ((eq atribuicao T)
 		  			(psr-adiciona-atribuicao! p variavel dominio)
 		  			(multiple-value-bind (resultadoAux testesAux) (procura-retrocesso-grau p) 	 (setf resultado resultadoAux)
 		  																						 (setf numTestesGeral (+ testesAux numTestesGeral)))
 		  			(cond ((eq resultado nil))
 		  				  (t 
 		  					(return-from procura-retrocesso-grau (values resultado numTestesGeral)))))))
			(psr-remove-atribuicao! p variavel)

		(return-from procura-retrocesso-grau (values nil numTestesGeral)))))
















(defun procura-retrocesso-fc-mrv (p)
	(let ((inferencias (list))
		  (resultado)
		  (testesTotais 0))

		(cond ((eq (psr-completo-p p) T)
			(return-from procura-retrocesso-fc-mrv (values p testesTotais))))
			
		(let ((variavel (heuristica-mrv p)))
			(dolist (dominio (psr-variavel-dominio p variavel))
				(multiple-value-bind (atribuicaoAux numTestesAux) (psr-atribuicao-consistente-p p variavel dominio) (setf atribuicao atribuicaoAux)
 																													(setf testesTotais (+ numTestesAux testesTotais)))
 	  			(cond ((eq atribuicao T) 
 	  				(psr-adiciona-atribuicao! p variavel dominio)
 	  				(multiple-value-bind (inferenciaAux testesAux) (forward-checking p variavel) (setf inferencias inferenciaAux)
 		  																						 (setf testesTotais (+ testesAux testesTotais)))
 	  				(cond ((eq inferencias nil))
 	  					(t 
 	  						(dolist (inf inferencias)
 	  							(psr-altera-dominio! p (car inf) (cdr inf))
 	  							(multiple-value-bind (resultadoAux testesAux) (procura-retrocesso-fc-mrv p) (setf resultado resultadoAux)
 		  																						 			(setf testesTotais (+ testesAux testesTotais)))
 	  							(cond ((eq resultado nil) 
 	  								;(dolist (inf inferencias)
 	  									;(psr-altera-dominio! p (car inf) (cdr inf))))
 	  								(setf inferencias nil))
 	  								(t
 	  									(return-from procura-retrocesso-fc-mrv (values resultado testesTotais)))))))

 	  				
 	  				(psr-remove-atribuicao! p variavel)))))
			
			(return-from procura-retrocesso-fc-mrv (values nil testesTotais))))







(defun forward-checking (p var)
  	(let ((testesTotais 0)
 		  (inferencias (list))
 		  (revised)
  		  (lista-arcos (arcos-vizinhos-nao-atribuidos p var))) 

  		(dolist (ligacao lista-arcos)
  			(let ((v1 (car ligacao))
  				  (v2 (cdr ligacao)))


  			(multiple-value-bind (revisedAux numTestesAux infAux) (revise p v2 v1 inferencias) (setf revised revisedAux) 
 																							   (setf testesTotais (+ numTestesAux testesTotais))
  																							   (setf inferencias infAux))
  			
  			(cond ((eq revised T)
  				(dolist (inf inferencias)
  					(cond ((eq (car inf) v2)
  						(cond ((equal (list-length(cdr inf)) 0)
  							(return-from forward-checking (values nil testesTotais)))))))))))

  		(return-from forward-checking (values inferencias testesTotais))))



(defun revise (p x y inferencias)
	(let ((testesTotais 0)
		  (revised NIL)
		  (marcadorX 0)
		  (marcadorY 0)
		  (encontrouX 0)
		  (dominio-x)
		  (dominio-y)
		  (valorY)
		  (novo-dominio-x)
		  (valorConsistenteEncontrado)
		  (atribuicao))

		(dolist (inf inferencias)
			(cond ((equal x (car inf))
				(setf marcadorX 1)
				(setf dominio-x (cdr inf)))))

		(cond ((equal marcadorX 0)
			(setf dominio-x (psr-variavel-dominio p x)))) 

		(setf novo-dominio-x dominio-x)
		(setf valorY (psr-variavel-valor p y))

		(cond ((eq valorY nil)
			(dolist (inf inferencias)
				(cond ((equal y (car inf))
					(setf marcadorY 1)
					(setf dominio-y (cdr inf)))))

			(cond ((equal marcadorY 0)
				(setf dominio-y (psr-variavel-dominio p y))))) 
			  
		(t 
			(setf dominio-y (list valorY)))) 

		(dolist (vx dominio-x)
			(setf valorConsistenteEncontrado NIL)
			(dolist (vy dominio-y)
				(multiple-value-bind (atribuicaoAux numTestesAux) (psr-atribuicoes-consistentes-arco-p p x vx y vy)  (setf atribuicao atribuicaoAux)
 																													 (setf testesTotais (+ numTestesAux testesTotais)))
				(cond ((eq atribuicao T)
					(setf valorConsistenteEncontrado T)
					(return))))

			(cond ((eq valorConsistenteEncontrado nil)
				(setf revised T)
				(remove vx novo-dominio-x))))
		
		(cond ((eq revised T)
			(dolist (inf inferencias)
				(cond ((equal x (car inf))
					(setf encontrouX 1)
					(setf (cdr inf) novo-dominio-x))))

			(cond ((equal encontrouX 0)
				(setq inferencias (concatenate 'list inferencias (cons(cons x novo-dominio-x)nil)))))))


		(return-from revise (values revised testesTotais inferencias))))




(defun arcos-vizinhos-nao-atribuidos (p var)
	(let ((lista-arcos (list))
		  (encontrouVar 0)
		  (encontrouVarNaoAtrib 0))

		(dolist (varNaoAtrib (psr-variaveis-nao-atribuidas p))
			(cond ((equal var varNaoAtrib))
				  (t
				  	(dolist (rst (psr-restricoes p)) 
				  	 	(setf encontrouVar 0)
				  	 	(setf encontrouVarNaoAtrib 0)	
				  		
				  		(dolist (var2 (restricao-lista-variaveis rst))			  			
				  			(cond ((equal var var2)
				  				(setf encontrouVar 1)))
				  			
				  			(cond ((equal varNaoAtrib var2)
				  				(setf encontrouVarNaoAtrib 1)))

				  			(cond ((equal encontrouVar 1)
				  				(cond ((equal encontrouVarNaoAtrib 1)
				  					(setf encontrouVar 0)
				  	 				(setf encontrouVarNaoAtrib 0)
				  					(setq lista-arcos (concatenate 'list lista-arcos (cons(cons varNaoAtrib var)nil))))))))))))

		(return-from arcos-vizinhos-nao-atribuidos (remove-duplicates lista-arcos :test #'equal))))

				  	
				  	







;RESOLVE-SIMPLES
;FUNCAO PARA RESOLVER PROBLEMAS SIMPLES
;recebe: array de restricoes
;retorna: array fill-a-pix resolvido

(defun resolve-simples (a)
	(let ((jogo)
	      (p))
			(setf jogo (fill-a-pix->psr a))
			(multiple-value-bind (psr) (procura-retrocesso-simples jogo) (setf p psr))
			(return-from resolve-simples(psr->fill-a-pix p (array-dimension a 0) (array-dimension a 1)))))



;-----------------------------------------------------------------------------------------;
;				  					FUNCOES HEURISTICAS							  		  ;
;-----------------------------------------------------------------------------------------;

;HEURISTICA-GRAU
;FUNCAO HEURISTICA
;recebe: psr
;retorna: variavel envolvida no maior numero de restricoes com outras variaveis nao atribuidas, 
;		  ou primeira a aparecer na lista de variaveis nao atribuidas, em caso de empate

(defun heuristica-grau (p)
	(let ((contador 0)
		  (maior -1)
		  (numNils 0)
		  (marcador 0)
		  (varMaisRestricoes))

		(dolist (var (psr-variaveis-nao-atribuidas p))
			(setf contador 0)
			(dolist (rst (psr-restricoes p))
				(setf numNils 0)
				(setf marcador 0)
				(dolist (var2 (restricao-lista-variaveis rst))
					(cond ((equal var var2)
						(setf marcador 1)))
					(let ((valor (psr-variavel-valor p var2)))
						(cond ((eq valor nil)
							(setf numNils (+ numNils 1))))))

				(cond ((equal marcador 1)
					(cond ((> numNils 1)
						(setf contador (+ contador 1)))))))

			(cond ((> contador maior)
				(setf maior contador)
				(setf varMaisRestricoes var))))

		(return-from heuristica-grau varMaisRestricoes)))


;HEURISTICA-MRV
;FUNCAO HEURISTICA
;recebe: psr
;retorna: variavel com menor dominio, ou primeira a aparecer na lista de variaveis nao atribuidas, em caso de empate

(defun heuristica-mrv (p)
	(let ((contador 0)
		  (menor 9999)
		  (varMenorDominio))
	
		(dolist (var (psr-variaveis-nao-atribuidas p))
			(setf contador 0)
			(dolist (dom (psr-variavel-dominio p var))
				(setf contador (+ contador 1)))

			(cond ((< contador menor)
				(setf menor contador)
				(setf varMenorDominio var)
				(cond ((equal menor 0)
					(return-from heuristica-mrv varMenorDominio))))))
		(return-from heuristica-mrv varMenorDominio)))
