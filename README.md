# Indikatorutvikling for vassdrag og kystvann i 2026

_Vennligst fyll ut denne README.md-malen i henhold til instruksjonene. Gjør prosjektspesifikke tilpasninger der det er nødvendig. Fjern alle instruksjoner og kommentarer når du er ferdig._

- **Norsk tittel**: Indikatorutvikling for vassdrag og kystvann i 2026-2026
- **Prosjekteier**: Miljødirektoratet
- **Oppdragstaker**: NIVA
- **Leveringsdato**: 30.10.2026
- **Nedlastingslenke:** [dataleveranser.miljodirektoratet.no/nedlasting/leveranse-id](https://dataleveranser.miljodirektoratet.no/nedlasting/<leveranse-id>) _Fyll inn URL til nedlastingssiden for leveransen i Dataleveranse-applikasjonen (kun for offentlige leveranser)._

Kort beskrivelse av formål og innhold i repoet.

- **Develop and calculate indicators for rivers and coastal water for national ecosystem condition accounting**:
- **Utvikle og beregne indikatorer for vassdrag og kystvann til bruk i nasjonalt tilstandsregnskap**:

## 1. Bidragsytere / Kontakt

- **Navn**: Elianne Egge, Nicolas Dupont, Roar Brænden, Dag Ø. Hjermann  
- **Organisasjon**: Norsk Institutt for Vannforskning STI (NIVA)
- **Epost**: dag.hjermann@niva,no; Elianne.Egge@niva.no; Nicolas.Dupont@niva.no; roar.branden@niva.no
- **Eventuelle eksterne samarbeidspartnere:** Ingen  

## 2. Formål / Bakgrunn

Norge skal etablere et forvaltningsnyttig nasjonalt naturregnskap i 2026 i tråd med internasjonale standarder. Regnskapet skal gi et helhetlig bilde 
av økosystemenes utbredelse, tilstand og tjenester, og skal brukes til å følge utviklingen over tid. I Pilot for nasjonalt tilstandsregnskap 
(Nybø m.fl. 2025, heretter kalt piloten) er det utviklet en rekke indikatorer for de to klassene Elver, bekker og kanaler og Innsjøer og vannmagasiner. 
Disse indikatorene er i hovedsak basert på data fra Vannmiljø og Naturindeks. Formålet med dette prosjekter var å forbedre indikatorsettene og samtidig 
få mer kunnskap om mulige datakilder for disse klassene. Denne levearansen i github er en stor del av to av delene av prosjektet:
1) Utvikle en indikator på hydrologiske endringer i vassdrag grunnet vannkraft., 2)  Utvikle nye indikatorer for kystvann basert på relevante eksisterende 
indikatorer for vassdragsklassene.  

Metodene som er brukt er basert på åpne data, beregningsverktøy (NEVINA) og rapporter. I noen tilfeller er indikatorene basert på verdier hentet rett fra 
resultattabeller i tilgjengelige rapporter, i andre tilfeller er det trukket ut rådata (fra spesielt Vannmiljø) og beregninger er utført på basis av 
disse. 

Referanse:
Nybø, S. Kolstad. A., Czúcz, B., Framstad, E., Sandvik, H., Töpper, J., Bjørkvoll, B., Clappe, S., Halbritter, A.H., Storaunet, K.O., Dramstad, W., Grainger, M., 
Haande, S., Pedersen, C., Schartau, A.K. & Vandvik, V. (2025). Nasjonal pilot for økologisk tilstandsregnskap. NINA Rapport 2668. Norsk institutt for 
naturforskning. https://www.miljodirektoratet.no/publikasjoner/2025/desember-2025/nasjonal-pilot-forokologisk-tilstandsregnskap/

## 3. Forutsetninger / Dependencies

_Dokumenter alle forutsetninger som kreves for å kjøre analysen og gjenskape resultatene._

- **Hardware**:
  - **OS:** Windows 11  
  - **Memory (RAM):** Ingen spesielle krav
  - **CPU:** Ingen spesielle krav
  - **GPU:** Ikke brukt
  - **Storage:** Ingen spesielle krav
  - **Ytelse:** De fleste indikatorene tar noen få minutter på en moderne laptop  
- **Kjøringsmiljø:**
  - **Container:** Leveransen er ikke containerifisert. R-pakker som er brukt er dokumentert av utskrift av sessionInfo i slutten av mange av scriptene. Vi anser de aller
  fleste av beregningene å være robuste i forhold til oppdtering av pakker.  
  - **Lokal PC:** Alle script er laget og kjørt i Windows 11-miljø. Det er kun benyttet R, og scriptene krever ikke annet enn pakker som kan lastes ned direkte i R.  
  - **Databricks:** Ikke aktuelt
  - **Andre kjøringsmiljø:** Ikke aktuelt
- **KI-verktøy:**
  - Microsoft Copilot (GPT-5) og Claude Code 2.1.278 med Opus 5 og Sonnet 5)   
  - Vi har ikke brukt noen KI-instruksjonsfiler er brukt for vedlagte leveranse  
  - KI er brukt til en viss grad for genrerering av kode, mest i chat-modus  
- **Programmeringsspråk:**
  - R 4.5.3; pakker er oppgitt i de enkelte script    

## 4. Struktur

Dette repoet følger en standard mappestruktur for å gjøre det enkelt å navigere

- _/config_ - Konfigurasjonsfiler, parameterfiler.
- _/notebooks_ - Python eller Rmd Notebooks
- _/src_, _/R_, _/sql_ - Alle kodefiler
- _/data_ - Eksempeldata eller små testfiler (Dersom hensiktsmessig), samt metadata
- _/docs_ - Metodikk, rapporter, dokumentasjon
- _/outputs_ - Resultater (figurer, tabeller, rapporter)

Oppdragstaker kan opprette andre nødvendige filer (som feks. .gitignore etc.) selv.  

De flesr

## 5. Input / Output

- **Input**: Beskriv hvilke filer som trengs, filformat og eksempeldata. Dersom data hentes fra eksterne kilder, skal versjon, uttrekkstidspunkt og framgangsmåte for å hente ut de samme dataene dokumenteres.
- **Output**: Beskriv hva skriptene genererer, f.eks. figurer, tabeller, filer eller rapporter

## 6. Installasjon / Kjøring

- Kort forklaring på hvordan og i hvilke rekkefølge script(ene) kjøres og miljøet settes opp.

## 7. Lisens / Rettigheter

Se [**LICENSE**-filen](LICENSE) for mer informasjon.
​‌
