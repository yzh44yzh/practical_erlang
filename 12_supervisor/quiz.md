## 1. Дерево супервизоров

Создается на старте ноды, и потом его нельзя изменить

К нему можно динамически добавлять и удалять узлы и ветки


## 2. Супервизор

Стартует рабочие потоки

Останавливает рабочие потоки

Берет на себя функцию рабочего потока, если тот падает

Может иметь родительский поток-супервизор

Может иметь родительский рабочий поток

Может иметь дочерний поток-супервизор

Может иметь дочерний рабочий поток


## 3. Для запуска супервизора нужно вызвать

supervisor:start_link/1

supervisor:start_link/2

supervisor:start_link/3

supervisor:start_link/4

supervisor:init/1

supervisor:init/2


## 4. Существуют следующие рестарт стратегии

one_for_one

one_for_all

rest_for_one

simple_one_for_one

all_for_all

nevermind

whatever


## 5. Shutdown может принимать значения

число миллисекунд

infinity

brutal_kill

число секунд

fatality

killing_me_softly


## 6. Количество рестартов ограничивается настройками

intensity

period

max_restarts

restart_limit

limit_restart
