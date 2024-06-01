Star Wars: Haskell Espacial


Las fuerzas rebeldes analizan diferentes estrategias para enfrentarse a las naves espaciales imperiales de Darth Vader. Debido a los bajos números de la resistencia decidieron que por cada flota enemiga van a enviar a una única nave para llevar adelante misiones sorpresa. Para ello, decidieron hacer un programa en Haskell Espacial que les permita planificar adecuadamente la lucha contra el lado oscuro simulando estos combates.
De todas las naves conociendo su durabilidad, su ataque, su escudo y su poder especial. A modo de ejemplo se presentan algunas: 

Nombre: TIE Fighter
Durabilidad: 200
Escudo: 100
Ataque: 50
Poder:  Hace un movimiento Turbo, el cual incrementa su ataque en 25

Nombre: X Wing
Durabilidad: 300
Escudo: 150
Ataque: 100
Poder: Hace una reparación de emergencia, lo cual aumenta su durabilidad en 50 pero reduce su ataque en 30.

Nombre: Nave de Darth Vader
Durabilidad: 500
Escudo: 300
Ataque: 200
Poder: Hace un movimiento Super Turbo, lo cual significa hacer 3 veces el movimiento Turbo y reducir la durabilidad en 45.

Nombre: Millennium Falcon
Durabilidad: 1000
Escudo: 500
Ataque: 50
Poder: Hace una reparación de emergencia y además se incrementan sus escudos en 100.
Modelar las naves espaciales mencionadas y agregar una nueva nave, con un poder especial sutilmente diferente a alguna de las anteriores, en el que se aproveche las otras implementaciones.

Calcular la durabilidad total de una flota, formada por un conjunto de naves, que es la suma de la durabilidad de todas las naves que la integran.

Saber cómo queda una nave luego de ser atacada por otra. Cuando ocurre un ataque ambas naves primero activan su poder especial y luego la nave atacada reduce su durabilidad según el daño recibido, que es la diferencia entre el ataque de la atacante y el escudo de la atacada. (si el escudo es superior al ataque, la nave atacada no es afectada). La durabilidad, el escudo y el ataque nunca pueden ser negativos, a lo sumo 0.

Averiguar si una nave está fuera de combate, lo que se obtiene cuando su durabilidad llegó a 0. 

Averiguar cómo queda una flota enemiga luego de realizar una misión sorpresa con una nave siguiendo una estrategia. Una estrategia es una condición por la cual la nave atacante decide atacar o no una cierta nave de la flota. Por lo tanto la misión sorpresa de una nave hacia una flota significa atacar todas aquellas naves de la flota que la estrategia determine que conviene atacar. Algunas estrategias que existen, y que deben estar reflejadas en la solución, son:

1. Naves débiles: Son aquellas naves que tienen menos de 200 de escudo.
2. Naves con cierta peligrosidad: Son aquellas naves que tienen un ataque mayor a un valor dado. Por ejemplo, en alguna misión se podría utilizar una estrategia de peligrosidad mayor a 300, y en otra una estrategia de peligrosidad mayor a 100.
3. Naves que quedarían fuera de combate: Son aquellas naves de la flota que luego del ataque de la nave atacante quedan fuera de combate. 
4. Inventar una nueva estrategia

Considerando una nave y una flota enemiga en particular, dadas dos estrategias, determinar cuál de ellas es la que minimiza la durabilidad total de la flota atacada y llevar adelante una misión con ella.

Construir una flota infinita de naves. ¿Es posible determinar su durabilidad total? ¿Qué se obtiene como respuesta cuando se lleva adelante una misión sobre ella? Justificar conceptualmente.
