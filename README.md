Дано:

  - Дерево произвольной глубины
  - Узлы дерева имеют листья ( 0 - N )
  - Каждый лист имеет ассоциированный с ним вес - целое ненулевое число
  - Листья для каждого узла представлены в виде однонаправленного связанного списка

Нужно:
  - Для каждого узла - отсортировать листья по весу без использования библиотечных функций сортировки,
    при этом сумма весов листьев для каждого конкретного узла дерева - не должна превышать заданную константу W,
    лишние листья переносятся к следующему узлу дерева ( в соответствии с алгоритмом обхода узлов дерева ), лишние листья самого последнего узла дерева отбрасываются.

Пример:
   
    Узел дерева имеет 3 дочерних узла ( a1, a2, a3 ) а также 4 листа b1, b2, b3, b4 с соотв. весами: 1 для b1, 2 для b2, 3 для b3 и 4 для b4
    Константа W = 3
    
    Начальное состояние списка листьев для данного узла: b2, b4, b3, b1
    Конечное состояние списка листьев для данного узла: b1, b2, при этом листья b3, b4 переносятся к дочернему узлу a1