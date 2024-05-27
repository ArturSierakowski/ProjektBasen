# <center>Projekt BASEN</center>
System do rezerwacji basenów. 
Stworzony na potrzeby zaliczenia przedmiotu <b>_Programowanie deklaratywne_</b> w roku akademickim 2023/2024.

## Autorzy

- [Tarik Alaiwi](https://github.com/Tarik-Alaiwi)
- [Artur Sierakowski](https://github.com/ArturSierakowski)


## GUI
Wlaczamy komendą `start.`

![image](https://github.com/ArturSierakowski/ProjektBasen/assets/115699855/f755b2a1-eba5-4e79-ad5d-5f8b8f478e0c)



## Predykaty

| Nazwa                                      | Sposób użycia                                                                                                                                                                                                                          |
|----------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `dodaj_rezerwacje/4`                           | `dodaj_rezerwacje('Artur', 7, '2024-05-26', 14).`                                                                             |
| `dostepnosc/3`                       | `dostepnosc(7, '2024-05-26', 14).`                                                |
| `anuluj_rezerwacje/4`                       | `anuluj_rezerwacje('Artur', 7, '2024-05-26', 14).`     |
| `wyswietl_rezerwacje/0`                       | `wyswietl_rezerwacje.` (wszystkie w systemie)    |
| `wyszukaj_rezerwacje_uzytkownika/1`                       | `wyszukaj_rezerwacje_uzytkownika('Artur').`   |
| `wyszukaj_rezerwacje_daty/1`                       | `wyszukaj_rezerwacje_daty('2024-05-26').` |
| `wyszukaj_rezerwacje_toru/1`                       | `wyszukaj_rezerwacje_tor(7).`    |
| `edytuj_rezerwacje/5`                       | `edytuj_rezerwacje('Artur', 1, 7, '2024-05-26', 20).` (drugi parametr jest to numer ID rezerwacji zapisanej w systemie wskazanego użytkownika)     |
| `dodaj_rezerwacje_cykliczne/5`                       | `dodaj_rezerwacje_cykliczne('Artur', 7, '2024-05-26', 20, 5).` (co tydzień od wskazanego dnia; ostatni parametr określa przez ile tygodni trwa rezerwacja) |     
| `raport_liczba_rezerwacji/0`                       | `raport_liczba_rezerwacji` (Wyświetla liczbę wszystkich rezerwacji dla każdego toru oddzielnie)                            |
| `najczesciej_rezerwowane_godziny/0`                       | `najczesciej_rezerwowane_godziny` (od największego obłożenia do najmniejszego)                            |



## Technologie
![Prolog](https://www.swi-prolog.org/download/logo/swipl-128.png)


## Licencja

MIT
