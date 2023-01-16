from functools import cache
# =============================================================================
# Najdaljše naraščajoče podzaporedje
# =============================================================================

# -----------------------------------------------------------------------------
# Napišite funkcijo `najdaljse_narascajoce_podazporedje`, ki sprejme seznam in
# poišče najdaljše (ne strogo) naraščajoce podzaporedje števil v seznamu.
#
# Primer: v seznamu `[2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]` kot rezultat vrne
# podzaporedje `[2, 3, 4, 4, 6, 7, 8, 9]`.
# -----------------------------------------------------------------------------
def najdaljse_narascaroce_podzaporedje (sez):
    def najdaljse (min, sez):
        if len(sez) == 0:
            return [], 0
        elif sez[0] >= min:
            brez, n = najdaljse(min, sez[1:])
            z, m = najdaljse(sez[0], sez[1:])
            z = [sez[0]]+z
            m += 1
            if n<m:
                return z, m
            else:
                return brez, n
        else:
            brez, n = najdaljse(min, sez[1:])
            return brez, n
    return(najdaljse(0, sez)[0])
print(najdaljse_narascaroce_podzaporedje([2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]))
# -----------------------------------------------------------------------------
# Rešitev sedaj popravite tako, da funkcija `vsa_najdaljsa` vrne seznam vseh
# najdaljših naraščajočih podzaporedij.
# -----------------------------------------------------------------------------
def vsa_najdaljša (sez):
    def najdaljse (min, i):
        if i >= len(sez):
            return (0, [])
        elif sez[i] < min:
            return (min, i+1)
        else:
            d_z, zap_z = najdaljse (sez[i], i+1)
            d_brez, zap_brez = najdaljse (min, i+1)
            if d_z+1 > d_brez:
                return (d_z+1, [[sez[i]]+zap for zap in zap_z])
            elif d_z+1 < d_brez:
                return (d_brez, zap_brez)
            else:
                return (d_brez, [[sez[i]]+zap for zap in zap_z] + zap_brez)
    return najdaljse(0, 0)[1]
#print(vsa_najdaljša([2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]))
# =============================================================================
# Žabica
# =============================================================================
# Žabica se je izgubila v močvari in želi kar se da hitro odskakljati ven. Na
# srečo močvara vsebuje veliko muh, s katerimi si lahko povrne energijo, kajti
# utrujena žabica ne skoči daleč.
# 
# S funkcijo `zabica(mocvara)` želimo ugotoviti, kako hitro lahko žabica
# odskaklja iz močvare. Močvaro predstavimo s tabelo, kjer žabica prične na
# ničtem polju. Če je močvara dolžine `k`, je cilj žabice priskakljati vsaj na
# `k`-to polje ali dlje (torej prvo polje, ki ni več vsebovano v tabeli).
# 
# Energičnost žabice predstavimo z dolžino najdaljšega možnega skoka. Torej
# lahko žabica z količino energije `e` skoči naprej za katerokoli razdaljo med
# `1` in `e`, in če skoči naprej za `k` mest ima sedaj zgolj `e - k` energije.
# Na vsakem polju močvare prav tako označimo, koliko energije si žabica povrne,
# ko pristane na polju. Tako se včasih žabici splača skočiti manj daleč, da
# pristane na polju z več muhami. Predpostavimo, da ima vsako polje vrednost
# vsaj `1`, da lahko žabica v vsakem primeru skoči naprej.
# 
# V primeru `[2, 4, 1, 2, 1, 3, 1, 1, 5]` lahko žabica odskaklja iz močvare v
# treh skokih, v močvari `[4, 1, 8, 2, 11, 1, 1, 1, 1, 1]` pa potrebuje zgolj
# dva.
# =============================================================================
def zabica(mocvara):
    @cache
    def skoki(i, e):
        if i >= len(mocvara):
            return 0
        # ce ji zmanjka energije, se more spustiti
        elif e == 0:
            return 1+skoki(i+1, e + mocvara[i])
        else:
            # lahko se vseeno takticno spusti
            n_spust = 1+skoki(i+1, e + mocvara[i])
            # ali pa leti naprej
            n_let = skoki(i+1, e-1)
            return min(n_spust, n_let)
    return(skoki(0, 0))
print(zabica([4, 1, 8, 2, 11, 1, 1, 1, 1, 1]))

# =============================================================================
# Nageljni
# =============================================================================
# Mama Franca želijo na balkon širine `n` postaviti `m` korit z nageljni, širine
# `l` (korit, ne nageljnov). Zaradi lažjega zalivanja mora biti med dvema
# koritoma vsaj za 1 enoto prostora. Mama Franca želijo postaviti vsa korita,
# jih pa zaradi slabega vida med seboj ne razlikujejo. 
# 
# Vnuk je že spisal program, ki poišče število možnih postavitev, ne zna pa
# vrniti rešitev. Napišite funkcijo `nageljni(n, m, l)`, ki vrne seznam vseh
# možnih postavitev, da se bodo mama Franca lažje odločili.
# 
# Primer vseh štirih možnih postavitev pri balkonu širine 9 s tremi koriti
# širine 2 (kjer z 1 označimo nagelj in z 0 prazen prostor):
# 
#     [1, 1, 0, 1, 1, 0, 1, 1, 0]
#     [1, 1, 0, 1, 1, 0, 0, 1, 1]
#     [1, 1, 0, 0, 1, 1, 0, 1, 1]
#     [0, 1, 1, 0, 1, 1, 0, 1, 1]
# =============================================================================
@cache
def nageljni(n, m, l):
    if m <= 0:
        return [[0 for _ in range(n)]]
    elif n < l:
        return []
    elif n == l and m == 1:
        # zapolnimo do potankosti
        # dodan kot robni primer, da lahko v naslednji opciji vedno dodamo 0
        # na desno stran korita
        return [[1 for _ in range(n)]]
    else:
        ne_postavimo = [[0] + postavitev for postavitev in nageljni(n-1, m, l)]
        postavimo = \
            [[1 for _ in range(l)] + [0] + postavitev
             for postavitev in nageljni(n-l-1, m-1, l)]
        return postavimo + ne_postavimo

print(nageljni(9, 3, 2))

# =============================================================================
# Pobeg iz Finske
# =============================================================================
# Vaš sošolec Mortimer se je med potovanjem po Finski spravil v krepko godljo.
# Po divjem poskušanju lokalne vodke se je namreč stepel s kravo, zaradi česar
# ga sedaj lovi finska govedorejska mafija. Na srečo so za njegovo hrabro bitko
# slišale vse rokovske in metalske skupine, ki so mu pripravljene ponuditi
# prevoz.
# 
# Ker je Mortimer pridno poslušal predavanja iz finančne matematike, med potjo
# uspe prislužiti nekaj denarja, s katerim bo lahko plačal prevoz. Finci,
# navdušeni nad Mortimerjevim pogumom, mu dovolijo, da se med potjo zadolži,
# dokler na koncu pobega vse stroške povrne.
# 
# Mesta na poti predstavimo kot seznam, katerega elementi so seznami vseh
# možnih nadaljnjih poti. Pot je par `(indeks_cilja, denar)`. Kot primer
# 
#     [[(1, 10), (3, -10)],    # 0 
#     [(2, 10), (5, -20)],     # 1
#     [(3, -10)],              # 2 
#     [(4, 15)],               # 3 
#     [(5, 0)]]                # 4 
# 
# pomeni, da lahko v mestu 1 Mortimer izbere med prevozom v mesto 2, kjer
# dodatno zasluži 10 evrov, ali pa prevoz v mesto 5, ki ga stane 20 evrov. Ker
# beži pred mafijo, lahko predpostavite, da bodo možne zgolj poti na mesta z
# višji indeksom (torej ni ciklov).
# 
# Pobeg je uspešen, čim lahko odpotuje v mesto, ki ni več na seznamu (torej
# skok na indeks, ki preseže seznam) in ima po koncu zadnjega skoka 0 ali več
# evrov. Napišite program, ki nam vrne pot z najmanjšim številom skokov,
# predstavljeno kot seznam indeksov mest na poti. Ker pobeg morda ni možen, naj
# v tem primeru funkcija vrne `None`.
# 
# Na primeru je optimalna pot `[0, 3, 4, 5]`, kjer se Mortimer sicer zadolži,
# vendar v skoku iz 3 v 4 zasluži dovolj, da konča z 5 evri. Hitrejša pot bi
# bila `[0, 1, 5]`, vendar v tem primeru Mortimer na koncu dolguje še 10 evrov.
# 
# Mortimer pot vedno začne v mestu z indeksom 0 in ima 0 evrov (saj je vse
# zapil). Funkcija `pobeg` sprejme seznam, ki predstavlja finska mesta in vrne
# seznam indeksov mest, v katerih se Mortimer ustavi.
# =============================================================================
def pobeg(pot):

    @cache
    def pobeg(i, denar):
        if i >= len(pot) and denar >= 0:
            return [i]
        elif i >= len(pot):
            return None
        else:
            moznosti = []
            for (skok, stroski) in pot[i]:
                beg = pobeg(skok, denar + stroski)
                if beg is not None:
                    moznosti.append(beg)
            if len(moznosti) == 0:
                return None
            else:
                return [i] + sorted(moznosti, key=len)[0]

    return pobeg(0, 0)

# =============================================================================
# Pričetek robotske vstaje
# =============================================================================
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
# 
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati
# 
# Robotek se lahko premika le gor, dol, levo in desno ter ima omejeno količino
# goriva. V zbirki programov že ima funkcijo `moznost_pobega(soba, vrsta,
# stolpec, koraki)`, ki pove ali je pobeg možen.
# 
# Napišite funkcijo `pot_pobega(soba, vrsta, stolpec, koraki)`, ki sprejme
# matriko sobe, začetno pozicijo in število korakov ter izračuna pot po kateri
# robotek pobegne (če to ni možno vrne `None`). Pot zakodiramo s seznamom
# ukazov `'gor'`, `'dol'`, `'levo'` in `'desno'`.
# 
# Na primer za laboratorij:
# 
#     [[0, 1, 0, 0, 2],
#      [0, 2, 2, 0, 0],
#      [0, 0, 2, 2, 0],
#      [2, 0, 0, 2, 0],
#      [0, 2, 2, 0, 0],
#      [0, 0, 0, 2, 2]]
# 
# robotek iz vrste 3 in stolpca 1 pri vsaj petih korakih pobegne z ukazi
# 
#      ['gor', 'levo', 'gor', 'gor', 'desno']
# 
# medtem ko iz vrste 5 in stolpca 0 ne more pobegniti.
# =============================================================================
def pot_pobega(soba, vrsta, stolpec, koraki):
    max_vrsta = len(soba)
    max_stolpec = len(soba[0])

    @cache
    def pobegni(vrsta, stolpec, koraki):
        # Padli smo iz sobe
        if not (0 <= vrsta < max_vrsta) or not (0 <= stolpec < max_stolpec):
            return None
        # Pobeg uspešen! All hail our robot overlords!!!
        elif soba[vrsta][stolpec] == 1:
            return []
        # Lahko bežimo naprej
        elif soba[vrsta][stolpec] == 0 and koraki > 0:
            moznosti = \
                [("dol", pobegni(vrsta + 1, stolpec, koraki-1)),
                 ("gor", pobegni(vrsta - 1, stolpec, koraki-1)),
                 ("desno", pobegni(vrsta, stolpec + 1, koraki-1)),
                 ("levo", pobegni(vrsta, stolpec - 1, koraki-1))]
            uspesne = \
                [(smer, pot) for (smer, pot) in moznosti if pot is not None]
            if uspesne:
                return [uspesne[0][0]] + uspesne[0][1]  # [smer] + pot
            else:
                return None
        # Pristali smo na oviri ali pa nam je zmanjkalo korakov
        else:
            return None
    return pobegni(vrsta, stolpec, koraki)