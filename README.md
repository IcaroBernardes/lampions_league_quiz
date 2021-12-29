# Quiz da Lampions League

Teste seus conhecimentos sobre as principais ligas do Nordeste!

Esse Quiz é inspirado em um similar [Desafio do GE](https://interativos.globoesporte.globo.com/futebol/futebol-internacional/chuta-ai/chuta-ai-acerte-todos-os-98-participantes-das-top-5-ligas-da-europa) e é feito em Shiny (R).

## Funcionamento do app (resumo)
Os nomes dos clubes, de suas ligas e as diferentes formas de identificá-los são inseridos no app em tibbles. As componentes da interface são postas em linhas e cada linha é dividida em colunas. Isso é feito usando as funções fixedRow() e column(). Assim o app tem alguma responsaividade para telas pequenas, pois o conteúdo em uma dada linha é quebrado automáticamente se não houver espaço.

A tabela contendo as respostas foi gerada com tags html comuns como table, tr e td. O interessante é que com a ajuda do purrr foi possível evitar repetições para compor as tabelas. Em cada linha, foi posto o nome de cada clube incialmente invisível. A cada chute do usuário, é verificado se o chute (despido de acentuação e em minúsculas) corresponde a alguma das alternativas de resposta. Caso sim, a resposta é revelada em azul e o nome é removido para que não seja contado novamente. A caixa de entrada de texto é limpa em caso de acerto para que o usuário possa imediatamente continuar jogando.

O tempo só começa a contar quando o usuário digita o 1o caractere. O jogo termina quando o tempo acaba ou o usuário acerta todos clubes. Ao fim do quiz, as respostas não-adivinhadas são reveladas e o usuário recebe feeddback em destaque e novos links de compartilhamento que incluem o desemepenho que tiveram no quiz.
