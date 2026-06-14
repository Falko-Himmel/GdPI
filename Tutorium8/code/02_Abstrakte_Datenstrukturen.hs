-- Abstrakte Datenstrukturen: Stacks und Queues (LIFO/FIFO)
-- =======================================================

-- Stacks (Keller) und Queues (Warteschlangen) sind grundlegende abstrakte Datenstrukturen, die sich vor allem durch ihre Entnahme-Reihenfolge unterscheiden:

-- - Stack (LIFO): Last In, First Out. Das zuletzt eingelegte Element wird als erstes wieder entfernt.
-- 	Typische Operationen: `push` (Element oben auf den Stack legen), `pop` (oberstes Element entfernen/zurückgeben), `empty` (ist der Stack leer?).
-- 	Anwendungsfälle: Funktionsaufruf-Stacks, Undo-Historien, Tiefensuche.

-- - Queue (FIFO): First In, First Out. Das zuerst eingelegte Element wird als erstes wieder entfernt.
-- 	Typische Operationen: `enqueue` (Element hinten anstellen), `dequeue` (vorderstes Element entfernen/zurückgeben), `empty` (ist die Queue leer?).
-- 	Anwendungsfälle: Job-/Task-Warteschlangen, Breadth-First-Suche, Event-Verarbeitung.

-- Haskell-Typklassen und Listen-Instanzen
---------------------------------------

-- Über Typklassen lassen sich die Schnittstellen von Stack und Queue generisch beschreiben. Die folgenden Typklassen definieren die Kernoperationen; danach folgen einfache Instanzen für Listen (`[]`) als naive Referenzimplementationen.

-- Stack: LIFO-Semantik
class Stack s where
	push  :: a -> s a -> s a 
    -- das s a bedeutet, dass der Typkonstruktor s auf den Typ a auf das s angewendet wird
	pop   :: s a -> (a, s a)
	sEmpty :: s a -> Bool

instance Stack [] where
	push = (:)
	pop []     = error "Stack is empty!"
	pop (x:xs) = (x, xs)
	sEmpty = null

-- Queue: FIFO-Semantik
class Queue q where
	enqueue :: a -> q a -> q a
	dequeue :: q a -> (a, q a)
	qEmpty   :: q a -> Bool

instance Queue [] where
	-- Wir fügen vorne ein und entfernen hinten (FIFO),
	-- dadurch ist enqueue O(1), dequeue O(n).
	enqueue = (:)
	dequeue [] = error "Queue is empty!"
	dequeue xs = (last xs, init xs)
	qEmpty = null


-- Mini-Beispiele (mit Listeninstanzen)
------------------------------------

-- Stack (LIFO):
	-- push 3 [1,2] == [3,1,2]
	-- pop [3,1,2] == (3,[1,2])

-- Queue (FIFO):
	-- enqueue 1 []   == [1]
	-- enqueue 2 [1]  == [2,1]
	-- dequeue [2,1]  == (1,[2])    -- zuerst eingelegtes Element (1) kommt zuerst heraus
