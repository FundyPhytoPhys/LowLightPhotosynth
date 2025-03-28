---
title: "Photosystem II Sustained Function under Low Light and Low Temperatures; Polar vs. Temperate Phytoplankton"
author:
- Natasha M. Ryan:
    institute: mta 
- Douglas A. Campbell:
    institute: mta     
    email: dcampbel@mta.ca     
    correspondence: TRUE
institute:  
- mta: 'Department of Biology, Mount Allison University, Sackville NB, Canada, E4L1G7'
date: "2025-03-26"
output:
  bookdown::html_document2:
    code_folding: show
    keep_md: yes
    fig_caption: yes
    pandoc_args:
      - '--lua-filter=scholarly-metadata.lua'
      - '--lua-filter=author-info-blocks.lua'
  bookdown::word_document2:
    fig_caption: yes
    reference_docx: KnitTemplate.docx
    pandoc_args:
      - '--lua-filter=scholarly-metadata.lua'
      - '--lua-filter=author-info-blocks.lua'  
keywords: "Photosystem II, Polar Phytoplankton, Low Light Photosynthesis"

abstract: "Polar phytoplankton achieve slow, but ecologically significant productivity, under exceptionally low light. We hypothesized that maintaining photosynthesis under low light involves suppression of energetically wasteful charge recombinations in Photosystem II. We used single turnover variable chlorophyll fluorescence to track persistence of synchronized Photosystem II photochemistry in polar diatoms and green algae, in response to changing temperature and extreme low light, emulated by increasing spacing of photon deliveries. Prolonged synchronous cycling indicates fewer desynchronizations from wasteful recombination reactions and, thus, more efficient photosynthetic energy conversion under low light. We observed that colder temperatures result in more sustained cycling at longer photon spacings, within taxa. Further, polar taxa maintained synchronized cycles for longer than did temperate taxa,  under comparable measurement conditions. Our findings support our hypothesis that diverse lineages of polar phytoplankton suppress energetically wasteful charge recombinations and sustain photosynthesis even under extremely low light. This research shows that temperature interacts with the limits on photosynthesis under light limitation."
bibliography: [LowLightPhotosynthesis.bib, packages.bib]
csl: frontiers.csl
always_allow_html: true
editor_options: 
  markdown: 
    wrap: 72
---

# To Do

## Fix R package citation bibtex

## Fix figure & table citation issue

## Scan recent literature; Gates?

## process & plot tau data to track how long PSII is closed after flash

## do math to estimate probability of double hits contributing to desynchronization and whether that probability varies with flash spacing or temperature

## convert damping index to relative probability of desynchronization

## (re)generate figures; do we need the missing variables?

## remove Fragilariopsis 0C?

## run Thalassiosira pseudonana grown at lower temperature in Halifax or Sackville?

## run temperate diatom?

## process Chaetoceros data or rerun at Takuvik?

## format for Frontiers special issue?

## circulate to co-authors

## more Chlamy measures in Ottawa?















# Introduction {.unnumbered}
Phytoplankton are a functional grouping of photosynthetic microorganisms, with diverse evolutionary histories and ecologies [@pierellakarlusichPhytoplanktonTaraOcean2020].
Photolithotrophic growth, a defining characteristic of phytoplankton, fuels biomass production by harnessing light energy to reduce and assimilate inorganic carbon, nitrogen, phosphorus, sulphur, and other essential micronutrients [@ravenPutOutLight2000]. Photosynthetically Active Radiation (PAR), however, decreases with depth, as the light passing through the water column is scattered and absorbed [@kirkLightPhotosynthesisAquatic2011]. Therefore, with photons as a sole energy input, phytoplankton growth is constrained to the photic zone, the region of water receiving sufficient light for photosynthesis, operationally defined as extending down to a limit receiving 1% of surface irradiance, thus 2-20 µmol photons m^-2^ s^-1^  [@ravenPutOutLight2000].  

Light availability is further constrained in polar seas, presenting unique challenges for phytoplankton growth [@hanckeExtremeLowLight2018, @randelhoffArcticMidwinterPhytoplankton2020, @hoppePhotosyntheticLightRequirement2024]. Seasonally varying solar angle, sea ice, and snow cover, limit the light penetrating the water  [@leuArcticSpringAwakening2015, @hanckeExtremeLowLight2018].

In 1995, the lower limit of the photic zone was reconsidered as benthic microalgae in the Antarctic were reported photosynthetically active at light levels less than 1 µmol photons m^-2^ s^-1^ [@robinsonMicroalgalLightHarvestingExtreme1995]. Some polar psychrophile phytoplankton further demonstrate slow, but ecologically significant, productivity in the winter through photosynthesis at even lower low light levels, below 0.15 µmol photons m^-2^ s^-1^  [@randelhoffArcticMidwinterPhytoplankton2020,@hanckeExtremeLowLight2018, @hoppePhotosyntheticLightRequirement2024], with implications for the responses of phytoplankton communities to climate change [@ardynaPhytoplanktonDynamicsChanging2020].

Slow but significant phytoplankton photosynthesis during polar winters under the ice underscores the ability of psychrophilic phytoplankton to maintain functional photosystems throughout the polar night [@randelhoffArcticMidwinterPhytoplankton2020, @hoppePhotosyntheticLightRequirement2024], and supports a theoretical minimum light level for phytoplankton photosynthesis of 0.01 µmol photons m^-2^ s^-1^ [@ravenPutOutLight2000]. Such low-light phytoplankton photosynthesis and growth may serve to mitigate cell mortality in the extended darkness of winter, maintaining a seeding population for the spring bloom [@randelhoffArcticMidwinterPhytoplankton2020]. These spring blooms, in turn, are a major source of annual net primary production in polar regions [@ardynaPhytoplanktonDynamicsChanging2020]. 

In parallel with seasonally low light, psychrophile phytoplankton contend with cold temperature constraints on water viscosity, solute diffusion rates, membrane fluidity, enzyme kinetics and macromolecule interactions [@lyonPolarMicroalgaeNew2014]. Some psychrophilic phytoplankton exhibit high genetic divergence from related temperate species. The polar diatom *Fragilariopsis cylindrus* shows divergent alleles compared to their mesophilic relatives [@mockEvolutionaryGenomicsColdadapted2017], with changes in protein structure, including amino acid substitutions, H-bonds, and salt bridges [@lyonPolarMicroalgaeNew2014], cold shock proteins[@lyonPolarMicroalgaeNew2014] and anti-freeze proteins [@bayer-giraldiCharacterizationAntifreezeProtein2011]. Polar microbes use cellular-compatible solutes, including sugars, polyols, amino acids, betaine, and DMSP, which reduce intracellular freezing points and maintain enzyme hydration spheres, stabilizing catalytic activity [@lyonPolarMicroalgaeNew2014]. Additionally, they exhibit high levels of polyunsaturated fatty acids (PUFAs) in their lipid membranes [@lyonPolarMicroalgaeNew2014, @cvetkovskaTemperatureStressPsychrophilic2022] contributing to maintainance of membrane fluidity at cold temperatures. 

Oxygenic photosynthesis in eukaryotic phytoplankton occurs in chloroplasts, bounded by a two-to-four-membrane envelope, depending upon taxa [@kirkLightPhotosynthesisAquatic2011]. Thylakoid membranes, containing pigments and electron carriers, traverse the chloroplast stroma. Photosystem II (PSII), a multi-subunit protein complex embedded in the thylakoid membranes [@schubackSingleTurnoverVariableChlorophyll2021], catalyzes the photooxidation of water, releasing O~2~, and passing reductant into the photosynthetic electron transport chain.

In PSII, photons are captured by light-harvesting chlorophyll molecules, [@kirkLightPhotosynthesisAquatic2011], initiating a transition from the ground state to an electrically excited state. Energy absorbed by light harvesting complexes undergoes rounds of inductive resonance transfer among multiple pigments before eventually reaching the photochemical reaction center of PSII,  P~680~, [@kirkLightPhotosynthesisAquatic2011], composed of a Chl a heterodimer [@shenPhotosynthesisPhotosystemII2021, @vassRoleChargeRecombination2011]. Excitation energy is then distributed variably among: i) photochemistry; ii) dissipation as heat; or iii) re-emission as fluorescence (ChlF) [@schubackSingleTurnoverVariableChlorophyll2021].  When P~680~ is raised to its excited state, P~680~*, it shifts reduction potential, allowing transfer of an electron to an initial pheophytin (Phe) acceptor molecule, thereby photoxidizing to P~680~+ [@vassRoleChargeRecombination2011]. The electron from reduced Phe- is transferred to plastoquinone A (Q~A~), followed by transfer to plastoquinone B (Q~B~) [@shenPhotosynthesisPhotosystemII2021]. Q~B~, once fully reduced by receiving two electrons, is released, carrying the electrons into the mobile plastoquinone pool in the lipid phase of the thylakoid membrane [@kirkLightPhotosynthesisAquatic2011]. P~680~+  returns to its ground state P~680~ by taking an electron from a tyrosine residue D1-Tyr-161 (Yz). Yz, in turn, extracts an electron from a manganese cluster on the thylakoid lumenal side of PSII [@shenPhotosynthesisPhotosystemII2021, @mukhopadhyayManganeseClustersRelevance2004].

During photosynthesis, electron transfers thus stabilize separated charges [@vassRoleChargeRecombination2011]. However, these charge separations are reversible through recombination reactions [@vassJanusfacedChargeRecombinations2009, @hanMissesWaterOxidation2012]. Recombinations represent a futile cycle of photochemistry, ans contribute to both photodamage, but also to photoprotection of PSII essential under excess light conditions [@rappaportKineticsPathwaysCharge2002, @rappaportChargeRecombinationThermoluminescence2005], when prior reduction of downstream electron acceptors blocks electron transfer from P~680~*. Under such conditions, the primary radical pair [P~680~+Phe-] will recombine, generating the excited triplet chlorophyll ^3^P~680~ [@laloiGeneticApproachElucidating2006]. Chlorophyll triplets react with ground-state molecular oxygen to produce singlet oxygen (^1^O~2~), a highly damaging, photoinhibitory reactive oxygen species [@rappaportChargeRecombinationThermoluminescence2005].
Conversely, non-radiative charge recombinations act as a mechanism of photoprotection [@vassRoleChargeRecombination2011], enabling direct recombination from the singlet P~680~+Phe- or P~680~+Q~A~- states [@rutherfordBackreactionsShortcircuitsLeaks2012]. Direct recombination helps prevent the accumulation of excess energy and competes with triplet chlorophyll formation in the PSII reaction center, thereby suppressing the formation of harmful ROS [@vassJanusfacedChargeRecombinations2009].  Since there are activation energies for recombinations, the probabilities of recombinations vary with temperature, [@ivanovAcclimationTemperatureIrradiance2006, @hanDirectQuantificationFour2008]. Both plants and cyanobacteria modulate the potentials of electron transfer intermediates, changing the probabilities and temperature dependencies of recombinations [@ivanovAcclimationTemperatureIrradiance2006, @saneChangesRedoxPotential2003, @saneTransientExchangePhotosystem2002, @ivanovLowtemperatureModulationRedox2003]

Beyond their roles in photodamage and photoprotection, charge recombination reactions are wasteful 'miss' processes that lower photosynthetic energy conversion efficiency [@rappaportChargeRecombinationThermoluminescence2005, @hanMissesWaterOxidation2012]. Shifting reduction potentials of downstream electron acceptors, leading to changes in energy gaps for recombinations, may represent evolutionary adaptations aimed at maximizing photoprotection and minimizing losses through back-reactions under light-limited conditions [@vinyardPhotosystemIIReaction2013]. 

The oxygen-evolving complex (OEC) of PSII consists of a manganese-oxo cluster (Mn~4~CaO~5~) where water is oxidized, releasing oxygen and protons [@gatesRealtimeKineticsLight2020]. Four consecutive charge separations at P~680~ induce increasingly oxidized states of the Mn cluster, known as S-States [@zaharievaEnergeticsKineticsSState2019], denoted , from most reduced to most oxidized, as S0, S1,  S2, and S3, followed by a transient S4 state, which rapidly decays to S0. Once the Mn cluster has progessively lost four electrons, the Mn cluster oxidizes two water molecules to one molecule of O~2~. Therefore, a complete water oxidation cycle during oxygenic photosynthesis requires the sequential absorption of four photons by a PSII, with the progressive accumulation of four oxidizing equivalents in the OEC [@dauTimeresolvedXraySpectroscopy2007, @gatesRealtimeKineticsLight2020, @zaharievaEnergeticsKineticsSState2019].  

Under light limitation, excitations of P~680~ are widely spaced. With widely spaced PSII excitations, electron transfer intermediates must persist for longer periods of time,  elevating the probability of energetically wasteful recombination reactions, representing a step backward in the S-State cycle [@kerenMechanismPhotosystemII1997, @dewijnSstateDependenceMiss2002, @hanDirectQuantificationFour2008]. Stable and progressive S-State cycling under low light, in contrast, sustains electron flow for ATP and NADPH production, while minimizing risk of low-light photodamage to PSII [@rappaportChargeRecombinationThermoluminescence2005,@kerenMechanismPhotosystemII1997]. 

We therefore evaluated whether psychrophilic polar diatoms and green algae maintain stable S-State cycling, and thus productive electron transport, under extremely low light and low temperatures. S-State cycling in a phytoplankton sample can be tracked by the applications of sequences of short, very bright, single-turnover light flashes. In darkness the PSII population relaxes primarily to S1 [@hanDirectQuantificationFour2008]. As sequential light flashes are applied, the population of PSII complexes is driven through the S-State cycle [@dauTimeresolvedXraySpectroscopy2007]. In an idealized sample, the four S-States will be reflected by an ongoing periodic oscillation in ChlF, since the S-States have different intrinsic fluorescence yields [@gatesRealtimeKineticsLight2020, @zaharievaEnergeticsKineticsSState2019]. However, recombination reactions contribute to wasteful misses in the S-State cycling of individual PSII [@hanMissesWaterOxidation2012]. As recombination events occur, they contribute to desynchronization of S-State cycling among the PSII population, dampening the observed ChlF oscillation [@dewijnSstateDependenceMiss2002] towards a steady state averaged ChlF for the PSII population. An organism exhibiting S-State cycling sustained over more flash cycles indicates fewer misses, and thus more efficient sustained photosynthetic energy conversion. By comparing the S-State cycling over flash cycles, of psychrophilic and temperate taxa, we can determine if psychrophilic diatoms and green algae have higher ability to maintain PSII function under extreme low light, for stable extraction of electrons from water. 

# Materials and methods {.unnumbered}

## Study Strains and Culturing Conditions
The seven study taxa,  including polar and temperate strains of diatoms and green algae, and their respective culturing conditions are summarized in Table \@ref{tab:taxa_cultures}. 

<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
<caption>(\#tab:taxa_cultures)(\#tab:taxa_cultures)Study taxa and culture growth conditions.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Taxa </th>
   <th style="text-align:left;"> Origin </th>
   <th style="text-align:right;"> Growth °C </th>
   <th style="text-align:right;"> PAR (µE) </th>
   <th style="text-align:right;"> Photoperiod (h) </th>
   <th style="text-align:left;"> Media </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-style: italic;"> Thalassiosira pseudonana </td>
   <td style="text-align:left;"> Temperate </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> F2 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Thalassiosira pseudonana </td>
   <td style="text-align:left;"> Temperate </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> F2 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlorella vulgaris </td>
   <td style="text-align:left;"> Temperate </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> BG11 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlorella vulgaris </td>
   <td style="text-align:left;"> Temperate </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:left;"> BG11 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlamydomonas reinhardtii </td>
   <td style="text-align:left;"> Temperate </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> BBM </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Fragilariopsis cylindrus </td>
   <td style="text-align:left;"> Polar </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> F2 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Fragilariopsis cylindrus </td>
   <td style="text-align:left;"> Polar </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> F2 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlamydomonas priscuii </td>
   <td style="text-align:left;"> Polar </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> BBM </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlamydomonas ICEMDV </td>
   <td style="text-align:left;"> Polar </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> BBM </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlamydomona malina </td>
   <td style="text-align:left;"> Polar </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> BBM </td>
  </tr>
</tbody>
</table>
*Fragilariopsis cylindrus*, a psychrophilic pennate diatom measuring 15-55 µm, thrives in the high salinity and subzero temperatures of Arctic and Antarctic sea-ice systems [@otteDiatomFragilariopsisCylindrus2023, @cefarelliDiversityDiatomGenus2010]. Forming large blooms in the bottom layer of sea ice and across the wider sea ice zone, *Fragilariopsis cylindrus* is as a keystone species for polar ecosystems [@otteDiatomFragilariopsisCylindrus2023,@kangFragilariopsisCylindrusGrunow1992]. Conversely, *Thalassiosira pseudonana* (*Cyclotella nana*) is a small (2.5-15 μm) centric diatom found worldwide in diverse freshwater, coastal, brackish, and marine habitats [@poulsenThalassiosiraPseudonanaCyclotella2023]. *Thalassiosira pseudonana* can tolerate a wide range of salinities (0.5%–37%) and temperatures (4–25°C), contributing to its use as a model diatom species [@poulsenThalassiosiraPseudonanaCyclotella2023].
*Chlamydomonas* ICEMDV and *Chlamydomonas priscuii* are halotolerant algae isolated from the perennially ice-covered hypersaline Lake Bonney, in McMurdo Dry Valleys, Antarctica [@cookAntarcticPsychrophilesChlamydomonas2019,@stahl-rommelCyclicElectronFlow2022]. With 15 to 20 μm biflagellate cells, *Chlamydomonas* ICEMDV dominates the shallow photic zone, where it experiences higher irradiance, extreme nutrient limitation, and lower salinity [@cookAntarcticPsychrophilesChlamydomonas2019, @liUltrastructuralSingleCellLevelCharacterization2016]. The smaller *Chlamydomonas priscuii* dominates the deep photic zone, characterized by permanent low temperatures, low irradiance, and high salinity [@cvetkovskaTemperatureStressPsychrophilic2022,@hunerPhotosyntheticAdaptationMulticellularity2023]. *Chlamydomonas malina* is a marine microalga isolated from the Arctic Ocean’s Beaufort Sea, measuring around 10 μm in length and 5 μm in width, and growing optimally at 4°C [@balzanoDiversityCulturedPhotosynthetic2012,@morales-sanchezTemperatureDependentLipidAccumulation2020]. The temperate *Chlamydomonas reinhardtii* is a model green alga approximately 10 μm in size, found in soil and aquatic environments with an optimal temperature range of 20-32°C [@sassoMolecularManipulationDomesticated2018,@xieChlamydomonasReinhardtiiThermal2013]. *Further, *Chlorella vulgaris*, ranging from 2 μm to 10 μm in size, is primarily found in freshwater environments and grows optimally at 27°C [@wielCharacterizationChlorellaVulgaris2017,@leyvaAccumulationFattyAcids2014].

Cultures of *Thalassiosira pseudonana* and *Chlorella vulgaris* were prepared by Naaman Omar (Mount Allison University); *Chlamydomonas* cultures were prepared by MacKenzie Poirier (Cvetskova Lab, University of Ottawa); and *Fragilariopsis cylindrus* cultures were prepared by Sébastien Guérin (Takuvik International Research Laboratory, Université Laval).

## Single Turnover Variable Chlorophyll Fluorescence  

We used series of single turnover, saturating, flashes to induce variable chlorophyll fluorescence (St-ChlF) [@kolberMeasurementsVariableChlorophyll1998], to evaluate progressive desynchronization of the S-State cycle across a range of phytoplankton species and growth temperatures. A 100 µL sample of each culture was taken for a chlorophyll assay (XXXX citationXXX). Then, a
3 mL sample of culture was loaded into a temperature-controlled water-jacketed quartz cuvette (PolyScience) placed within the measurement chamber of a Soliense fluorometer (Version LIFT-REM 1.0, Soliense Inc). The apparatus was covered to block out incident light and cells were acclimated to the dark for a minimum of 30 seconds. In the dark PSII reaction centres relax to ground state, open for photochemistry upon receipt of an absorbed photon, with a low fluorescence yield [@schubackSingleTurnoverVariableChlorophyll2021]. In parallel the oxygen evolving complex (OEC) relaxes primarily to S1 [@hanDirectQuantificationFour2008], so the S-States of the population of PSII are largely synchronized.
The sample was then exposed to a series of 32 short, high power, evenly spaced flashes at 445 nm.  Each flash comprises a rapid series of 50-70 sub-saturating flashlets of 1.6 µs, delivered every 4.1 µs, over a total 205 - 287 µs per flash [@schubackSingleTurnoverVariableChlorophyll2021]. These flashlets cumulatively deliver light to PSII, which passes an electron downstream to Q~A~-, closing PSII for photochemistry for ~1000 µs, the lifetime for re-opening by downstream electron transport [@dauTimeresolvedXraySpectroscopy2007, @schubackSingleTurnoverVariableChlorophyll2021]. Closing the photochemistry pathway redirects a greater proportion of additional incoming light energy to ChlF, driving ChlF progressively from minimum (F~O~) towards a maximum (F~M~) [@schubackSingleTurnoverVariableChlorophyll2021]. For each flash, F~O~ and F~M~ are extracted using a fitting model (LIFT software version 22.11.11, Solisense Inc) [@kolberMeasurementsVariableChlorophyll1998]. F~O~ and F~M~ can then be used to derive the maximum quantum yield of photochemistry for open PSII [@schubackSingleTurnoverVariableChlorophyll2021], a secondary ChlF parameter calculated as :

\begin{equation}
F_V = \frac{F_M}{F_O}
\end{equation}

For each culture sample we adjusted the number and power of flashlets per flash to achieve closure of ~ 0.95 of PSII complexes, as estimated by the fitting of the chlorophyll fluorescence induction [@kolberMeasurementsVariableChlorophyll1998; @schubackSingleTurnoverVariableChlorophyll2021]. Ideally the saturating flash would achieve complete closure of the PSII population, but concomitant re-opening of PSII by downstream electron transport results in a steady state ratio of mainly closed PSII to a fraction of open PSII, even at F~M~.  The flash power required to drive the PSII population to closure depends largely upon the effective absorption cross-section of photosystem II (σ~PSII~), itself estimated from the ChlF induction curve during the saturation phase [@kolberMeasurementsVariableChlorophyll1998]. σ~PSII~ represents the probability of light capture by the PSII antenna bed associated with the dark-adapted PSII [@schubackSingleTurnoverVariableChlorophyll2021,@xuPhytoplanktonSPSIIExcitation2018]. When σ~PSII~ for a sample is large, weaker or fewer, flashlets are required to saturate the population of PSII,  while limiting excitation stress or double turnovers of PSII associated with excess light [@xuPhytoplanktonSPSIIExcitation2018]. Conversely, a sample with a smaller σ~PSII~ requires brighter, or more numerous flashlets, to drive the proportion of closed reaction centres  progressively towards saturation. If the brightness and number of sub-saturating flashlets are appropriate for the culture, the fluorescence will reach a plateau after approximately 70-80% of the flashlets [@kolberMeasurementsVariableChlorophyll1998].  

We kept the single turnover excitation flash train as short as feasible, to avoid excessive excitation, and to limit the probability of re-openings of PSII, followed by re-closures of PSII complexes by a second round of photochemistry, which contributes to desynchronization of the progression of the PSII complexes through S-States. Thus, as sequential flashes are applied to the culture, each individual PSII is ideally driven through the four S-States (Fig \@ref{fig:rep_osc}) [@dauTimeresolvedXraySpectroscopy2007].  As the oxygen-evolving complex of PSII moves between S-States, it alters the system kinetics and free energy [@vinyardPhotosystemIIReaction2013], and the yield of ChlF varies between S-States [@gatesRealtimeKineticsLight2020], so we can use ChlF to monitor S-State progression. 

The saturating single turnover flash trains drive the pool of PSII to ~ 95% closure through charge separation.  Therefore, desynchronization of the PSII S-State progression accumulates through  'misses' in which a PSII fails to perform a charge separation and close during a flash is approximated by:

D~M~ = 1 * e^(-0.95 * n)


```
##  [1] 1.000000e+00 3.867410e-01 1.495686e-01 5.784432e-02 2.237077e-02
##  [6] 8.651695e-03 3.345965e-03 1.294022e-03 5.004514e-04 1.935451e-04
## [11] 7.485183e-05 2.894827e-05 1.119548e-05 4.329753e-06 1.674493e-06
## [16] 6.475952e-07 2.504516e-07 9.685992e-08 3.745971e-08 1.448720e-08
## [21] 5.602796e-09 2.166831e-09 8.380025e-10 3.240900e-10 1.253389e-10
## [26] 4.847369e-11 1.874676e-11 7.250142e-12 2.803928e-12 1.084394e-12
## [31] 4.193796e-13 1.621913e-13 6.272602e-14
```

Where the D~M~ is the cumulative desynchronization resulting from photochemical misses during each single turnover saturating flash, and n are the number of flashes applied from 0 to 32. 0.95 is the fraction of PSII closed after photochemistry during a flash.

Desynchronization can also result from double hits of photochemistry during the flash, if a PSII is closed by a round of photochemistry, but then re-opens and undergoes a second round of photochemistry.

XXX These estimates seem high; review

need to estimate exponential growth of reopened PSII 
then estimate probability of another hit during a flashlet, and cumulatively




<div class="figure">
<img src="Figures/Rep_osc.png" alt="**Oscillations of the maximum quantum yield of PSII photochemistry over a Series of Single Turnover Saturating Flashes.** F~V~/F~M~ was normalized to the average value over the series for comparison across samples of *Chlamydomonas priscuii*, measured at 4 or 12 °C, with spacing of 1, 4, or 16 s between sequential flashes, excitation rates equivalent to irradiance with 0.708, 0.177 or 0.044 µmol photons m^-2^ s^-1^. Symbol colour indicates inferred majority S-State,  based upon relaxation to S1 during the dark incubation preceding the flash sequence." width="100%" height="100%" />
<p class="caption">(\#fig:rep_osc)**Oscillations of the maximum quantum yield of PSII photochemistry over a Series of Single Turnover Saturating Flashes.** F~V~/F~M~ was normalized to the average value over the series for comparison across samples of *Chlamydomonas priscuii*, measured at 4 or 12 °C, with spacing of 1, 4, or 16 s between sequential flashes, excitation rates equivalent to irradiance with 0.708, 0.177 or 0.044 µmol photons m^-2^ s^-1^. Symbol colour indicates inferred majority S-State,  based upon relaxation to S1 during the dark incubation preceding the flash sequence.</p>
</div>


## Measurements

By evaluating the S-State cycling of polar and temperate taxa of diatoms and green algae under a range of measurement temperatures and effective light levels (\@ref{tab:fluor_meas}), we can determine if polar taxa maintain synchronized PSII function, as an index of their capacity for electron transport under low light, by suppressing wasteful recombinations. While light absorption and excitation energy transfer within PSII are not temperature-sensitive, the redox reactions associated with downstream electron transport are highly temperature-dependent [@hunerPhotosyntheticAdaptationMulticellularity2023]. Furthermore, the probability of charge recombinations decrease below room temperature temperature[@ivanovAcclimationTemperatureIrradiance2006, @hanDirectQuantificationFour2008], as temperature drops below the activation energies for recombination paths. This temperature dependence may interact with acclimatory mechanisms to regulate photosynthetic electron flow, S-State transitions, and energy partitioning [@hunerPhotosyntheticAdaptationMulticellularity2023]. Measurement temperatures ranged from 0 to 28°C, depending on the taxa \@ref{tab:fluor_meas}. 

Increasing the spacing between sequential flashes decreases the repeat rate for excitations, thereby approximating the decrease in excitations of PSII under a decrease in continuous irradiance. As irradiance decreases, fewer photons are delivered to PSII per s, corresponding to longer spacing between sequential saturating flashes in our measurement protocol (Fig \@ref{fig:rep_osc}).  Our instrument control limited our minimum flash spacing to 1 s, so cultures were evaluated at flash spacings of 1, 2, 4, 8, and 16 seconds \@ref{tab:fluor_meas}. The equivalent effective light levels are estimated using the σ~PSII~ determined for each culture as follows: 


\begin{equation}
Light (µmol photons m^-2^ s^-1^) = \frac{1}{flash spacing (s)|}
\end{equation}


$$
Light (\mu mol \photon m^{-2} s^{-1}) = \frac{1}{flash spacing (s)} * {\frac{1}{\sigma_{PSII} (A^{2})} * \frac{1 m^{2}}{1E20A^{2}} * \frac{\mu mol}{6.022E17}
$$
Since the population of PSII starts predominantly from S1 in the dark acclimated state, the population of PSII will initially cycle synchronously through S States, reflected by an oscillation in chlorophyll fluorescence with a period of four(Fig \@ref{fig:rep_osc})[@dewijnSstateDependenceMiss2002]. Recombination reactions, representing the loss of charge separation in a PSII, will cause a missed step in the S-State cycling of an individual PSII. As more recombination events occur, desynchronization of S-State cycling among the population of PSII will scramble the periodic changes in ChlF, dampening the observed oscillation of the population-level fluorescence [@dewijnSstateDependenceMiss2002]. Prolonged synchronous cycling thus reflects a lower probability of desynchronization at each excitation, and thus a lower probability of losses through recombinations.  Photochemical misses, or photochemical double hits, also contribute to desynchronization [@hanMolecularBasisTurnover2022, @hanDirectQuantificationFour2008], but should not vary strongly across changes in flash spacing nor temperature within a sample.

XXXextract plots tau vs. temperature to justify this argumentXXXX

The conversions of flash spacing to equivalent effective light levels gave similar ranges of light levels applied to each strain \@ref{tab:fluor_meas}. For comparison, full sunlight at the sea surface is ~ 2000 µmol photons m^-2^s^-1^, so our measurement light ranges are ~ 5 orders of magnitude lower than full sunlight, and ~ 2-3 orders of magnitude below the ~ 20 µmol photons m^-2^s^-1^ threshold, used to define the conventional bottom of the photic zone supporting photosynthetic productivity in the oceans [@ravenPutOutLight2000], but span light ranges found to support phytoplankton productivity in field studies [@hanckeExtremeLowLight2018, @randelhoffArcticMidwinterPhytoplankton2020, @hoppePhotosyntheticLightRequirement2024].

XXXConsider redoing this as data with conversionsXXX
<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
<caption>(\#tab:fluor_meas)(\#tab:fluor_meas)Study taxa and single turnover saturating flash measurement conditions, with equivalent effective light levels (µmol photons m^-2^s^-1^).</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Taxa </th>
   <th style="text-align:left;"> Flash Spacings (s) </th>
   <th style="text-align:left;"> Equivalent Light (µE) </th>
   <th style="text-align:left;"> Measurement Temperatures (°C) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-style: italic;"> Thalassiosira pseudonana </td>
   <td style="text-align:left;"> 1, 2, 4, 8, 16 </td>
   <td style="text-align:left;"> 0.02428 - 0.53428 </td>
   <td style="text-align:left;"> 10, 14, 18, 20, 22, 24, 28 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlorella vulgaris </td>
   <td style="text-align:left;"> 1, 2, 4, 8, 16 </td>
   <td style="text-align:left;"> 0.05102 - 0.89781 </td>
   <td style="text-align:left;"> 10, 14, 18, 22, 26 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlamydomonas reinhardtii </td>
   <td style="text-align:left;"> 1, 2, 4, 8, 16 </td>
   <td style="text-align:left;"> 0.04670 - 0.84377 </td>
   <td style="text-align:left;"> 12, 16, 20, 24 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Fragilariopsis cylindrus </td>
   <td style="text-align:left;"> 1, 2, 4, 8, 16 </td>
   <td style="text-align:left;"> 0.02981 - 0.65678 </td>
   <td style="text-align:left;"> 0, 2, 6, 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlamydomonas priscuii </td>
   <td style="text-align:left;"> 1, 2, 4, 8, 16 </td>
   <td style="text-align:left;"> 0.04289 - 0.77084 </td>
   <td style="text-align:left;"> 4, 8, 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlamydomonas ICEMDV </td>
   <td style="text-align:left;"> 1, 2, 4, 8, 16 </td>
   <td style="text-align:left;"> 0.04252 - 0.73733 </td>
   <td style="text-align:left;"> 4, 8, 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chlamydomona malina </td>
   <td style="text-align:left;"> 1, 2, 4, 8, 16 </td>
   <td style="text-align:left;"> 0.03705 - 0.65817 </td>
   <td style="text-align:left;"> 4, 8, 12 </td>
  </tr>
</tbody>
</table>

 

## Analytical Methods  

XXXHowtoCitePackagesXXX
Data was processed using R version 4.3.2 [@rcoreteamLanguageEnvironmentStatistical2023] and RStudio version 2023.12.0+369 [@positteamRStudioIntegratedDevelopment2025] on the x86_64-apple-darwin20 (64-bit) platform and running under macOS Sonoma 14.3.1. Fluorescence data files generated by LIFT software were imported, tidied and combined with metadata on each culture using the tidyverse [@R-tidyverse], lubridate [52], and googlesheets4 [53] packages. The tidyverse, doBy [54], and WaveletComp [55] packages were used for wavelet analyses. The mgcv [56] and mgcViz [57] packages were used for generalized additive modelling, while the ggplot2 [58] and metR [59] packages were used for data visualization. XXX

Statistical results tables were done using the packages 'broom' [@R-broom], 'knitr' [@R-knitr], and 'kableExtra' [@R-kableExtra].

## Wavelet Transformations

The fluorescence data yields a time series of F~V~/F~M~ over 32 sequentially applied flashes,  for each flash spacing, and each measurement temperature, for each culture sample. The F~V~/F~M~ time series were analyzed for each combination of strain, growth conditions, measurement temperature, and flash spacing, using wavelet transformations [@theisSpectralTransformations2010], as exemplified in (Fig \@ref{fig:rep_recon}). Unlike traditional methods, wavelet analysis does not assume that the statistical properties of a time series are constant. Instead, wavelet transformations locally decompose the signal across different time scales and estimate spectral characteristics as a function of time [@cazellesWaveletAnalysisEcological2008]. By examining the frequency and wavelet power spectra, we can uncover the dominant patterns in the data [@theisSpectralTransformations2010]. 


<div class="figure">
<img src="Figures/Rep_recon.png" alt="Representative wavelet transformations of Antarctic *Chlamydomonas priscuii* variable chlorophyll fluorescence (F~V~/F~M~) measured at 4, 8 or 12°C, over 32 consecutive single turnover saturating flashes, applied at spacings of 1, 2, 4, 8, 16 s, equivalent to photon delivery rates to PSII achieved under light levels from 0.708 down to 0.045 µmol photons m^-2^ s^-1^." width="100%" height="100%" />
<p class="caption">(\#fig:rep_recon)Representative wavelet transformations of Antarctic *Chlamydomonas priscuii* variable chlorophyll fluorescence (F~V~/F~M~) measured at 4, 8 or 12°C, over 32 consecutive single turnover saturating flashes, applied at spacings of 1, 2, 4, 8, 16 s, equivalent to photon delivery rates to PSII achieved under light levels from 0.708 down to 0.045 µmol photons m^-2^ s^-1^.</p>
</div>


The wavelet transformation involves computing the wavelet power spectrum of the standardized time series using the Morlet wavelet [@theisSpectralTransformations2010]. The statistical significance of the periodic components in the time series was then calculated using a simulation algorithm. Surrogate time series are generated based on a white noise model, consisting of uncorrelated random values with constant mean and variance. The wavelet transform of the data is compared with the white noise model, to estimate p-values for whether the observed periodic components are statistically significant [@theisSpectralTransformations2010].
Statistical significance of the wavelet power at a periodicity of four indicates the culture is exhibiting periodic oscillations in chlorophyll fluorescence, reflecting synchronous S-State cycling across the PSII population. For wavelets exhibiting detectable S-State cycling, we generated a reconstruction limited to areas with a statistically significant signal statistically different from the average value of F~V~/F~M~). We then extracted a Damping Index as the number of flashes applied before desynchronization results in the reconstructed wavelet dropping below a detection threshold significance of p=0.05 above random noise. This Damping Index indicates how many successive photochemical charge separations occur in the PSII population before recombinations, photochemical misses, or photochemical double hits, desynchronize the S-State cycle to a randomized distribution of S-States across the population.

## Generalized Additive Modelling 

The observed patterns in the persistence of S-State cycling across conditions within taxa were then modelled using the nonparametric method of generalized additive modelling (GAM). GAMs fit a model to predict the damping index based on a tensor product smooth of the two predictors, temperature, and light level. The response variable is linked to the independent variables using a smoothing function, where many localized polynomials are joined to form a piecewise function called a spline [@pinillaNonParametricGeneralizedAdditive2021]. For each strain, GAM models were fit to the data using the restricted maximum likelihood method (REML).
 
One GAM model (Table \@ref{tab:gammodel_temp}) examines the response of S-State damping to the measurement temperature (°C) and the equivalent effective light level (µmol photons m^-2^ s^-1^) for the measurement flash spacing. 

#do this as output of model not retyped


A second GAM model (Table \@ref{tab:gammodel_delta}) examines the response of S-State damping to the difference between measurement temperature and growth temperature (Δ°C, Table XXX ), and the equivalent effective light level (µmol photons m^-2^ s^-1^) equivalent for the measurement flash spacing.

#do this as output of model not retyped


Models were validated by verifying the choice of basis dimensions (k) and evaluating the residual plots [@schoenigWorkshopGeneralizedAdditive2023]. Based on the fitted models, the damping index can then be predicted for other combinations of temperature and light [@woodGeneralizedAdditiveModels2017]. These predictions were then visually represented with a contour plot.

# Results {.unnumbered}  

## Single Turnover Variable Chlorophyll Fluorescence

Exposing phytoplankton cultures to a series of 32 successive flashes produced oscillations in the maximum quantum yield of photochemistry in PSII, as estimated through the secondary chlorophyll fluorescence parameter F~V~/F~M~. Initially, the majority of the dark-adapted population of PSII is at S1, with a smaller fraction at S0 [@gatesRealtimeKineticsLight2020, @dewijnSstateDependenceMiss2002]. As shown in Fig \@ref{fig:rep_osc} for the polar alga *Chlamydomonas priscuii*, the time series of F~V~/F~M~ over successive flashes reveals consistent variations in fluorescence yield as the predominant S-States follow each other across the PSII within the population. However, the amplitude of the ChlF oscillations declines progressively over time, and with wider spacing of sequential flashes, equivalent to decreasing effective light levels, and is less persistent at a higher measurement temperature

## Wavelet Analysis  

Wavelet transformations were computed for the fluorescence time series of each unique combination of measurement temperature, flash spacing, growth temperature, and species or strain. Assessing the wavelet power of a 4-step periodicity across conditions, key trends emerge. As exemplified by the Antarctic green alga *Chlamydomonas priscuii* (Figure \@ref{fig:cpriscuii_waveletpower}), the average wavelet power declines with increasing flash spacings, equivalent to decreasing effective light levels, and also with increasing measurement temperatures. 


<div class="figure">
<img src="Figures/Cpriscuii_waveletpower.png" alt="Sample plot of wavelet powers by period of oscillations in the maximum quantum yield of photochemistry Antarctic green algae *Chlamydomonas priscuii*, grown at 4°C, and measured across a range of measurement temperatures and flash spacings, with the equivalent effective light levels. Regions where wavelet power reached statistical significance (p &lt; 0.05) shaded in blue" width="100%" height="100%" />
<p class="caption">(\#fig:cpriscuii_waveletpower)Sample plot of wavelet powers by period of oscillations in the maximum quantum yield of photochemistry Antarctic green algae *Chlamydomonas priscuii*, grown at 4°C, and measured across a range of measurement temperatures and flash spacings, with the equivalent effective light levels. Regions where wavelet power reached statistical significance (p < 0.05) shaded in blue</p>
</div>

In contrast, in the temperate green algae *Chlamydomonas reinhardtii* (Figure \@ref{fig:creinhardtiii_waveletpower}), the wavelet power is consistently lower, showing a weaker 4-step periodicity of ChlF in temperate taxa, which only reaches statistical significance at shorter flash spacings, with higher equivalent effective light levels. 

<div class="figure">
<img src="Figures/Creinhardtii_waveletpower.png" alt="Sample plot of wavelet powers by period of oscillations in the maximum quantum yield of photochemistry in the temperate green algae *Chlamydomonas reinhardtii*, grown at 4°C, and measured across a range of measurement temperatures and flash spacings, with the equivalent effective light levels. Regions where wavelet power reached statistical significance (p &lt; 0.05) shaded in blue" width="100%" height="100%" />
<p class="caption">(\#fig:creinhardtiii_waveletpower)Sample plot of wavelet powers by period of oscillations in the maximum quantum yield of photochemistry in the temperate green algae *Chlamydomonas reinhardtii*, grown at 4°C, and measured across a range of measurement temperatures and flash spacings, with the equivalent effective light levels. Regions where wavelet power reached statistical significance (p < 0.05) shaded in blue</p>
</div>

Polar taxa maintained significant 4-step oscillations in F~V~/F~M~, and thus stronger synchronization of PSII S-State cycling at longer Flash Spacing intervals, and thus at lower equivalent effective light levels, than did their temperate counterparts (Figure \@ref{fig:periodicity_mtrx}), including measures taken at a common temperature of 12°C. 

XXXFix X Axes Scaling in GAM_Figures.RmdXXX
<div class="figure">
<img src="Figures/Periodicity_mtrx.png" alt="Statistical significance of 4-step oscillations in the maximum quantum yield of PSII photochemistry across polar and temperate phytoplankton taxa, as measured through variable chlorophyll fluorescence. Measurement conditions where wavelet power for 4-step periodicity reached statistical significance (p &lt; 0.05) shaded in blue" width="100%" height="100%" />
<p class="caption">(\#fig:periodicity_mtrx)Statistical significance of 4-step oscillations in the maximum quantum yield of PSII photochemistry across polar and temperate phytoplankton taxa, as measured through variable chlorophyll fluorescence. Measurement conditions where wavelet power for 4-step periodicity reached statistical significance (p < 0.05) shaded in blue</p>
</div>

## Generalized Additive Modelling by Difference from Growth Temperature

XXXX


 

XXXX
Predictions from generalized additive modelling were generated for the damping of S-State-induced chlorophyll fluorescence oscillations, as predicted by the tensor product smooth of the difference from growth temperature during measurements (Δ°C) and the equivalent effective light level (µmol photons m^-2^s^-1^; Fig \@ref{fig:fluor_meas}) set by flash spacing, for each strain.  All of the model fits explained over 50% of the variation in the response variable. 

Table Caption: Summary statistics by phytoplankton strain of GAM models using the restricted maximum likelihood method to model the response of the damping of S-State-induced chlorophyll fluorescence oscillations to the predictors of difference from growth temperature (Δ°C) during measurement, and the effective light level (µmol photons m^-2^s^-1^) set by flash spacing. 

The smoothing term was significant for the polar diatom **Fragilariopsis cylindrus** (F = 6.835, p = 2.8e-05), explaining 66.1% (adjusted R2 = 0.584) of the variation in the damping index. Similarly, the model of the temperate diatom, *Thalassiosira pseudonana*, produced a significant smoothing term (F = 9.01, p = 5.38e-06), which accounted for 73.2% of the variation in the damping index (adjusted R2 = 0.674). These models were used to predict the number of consecutive flashes before the damping of ChlF oscillations for each strain at each combination of deviation from growth temperature during measurements (°C) and effective light level (µmol photons m^-2^ s^-1^); (Fig \@ref{fig:diatoms_gamdiff}). 
Both diatom taxa exhibited the longest predicted periodic oscillations in ChlF at higher effective light levels and lower temperatures. Notably, the polar **Fragilariopsis cylindrus** sustained cycling longer than its temperate counterpart, *Thalassiosira pseudonana*, under comparable conditions. This disparity was particularly prevalent at measurements taken above the growth temperature and when longer spacing between flashes produced lower effective light levels (Fig \@ref{fig:diatoms_gamdiff}). Under these conditions, *Thalassiosira pseudonana* cultures did not retain the significant 4-step oscillation in ChlF indicative of synchronized S-State cycling. 

<div class="figure">
<img src="Figures/Diatoms_GAMdiff.png" alt="GAM models for polar and temperate diatoms, of consecutive flashes before damping of S-State-induced chlorophyll fluorescence oscillations. GAM model predicted by the difference from growth temperature (Δ°C) during measurements and the effective light level (µmol photons m^-2^s^-1^, estimated from flash spacings in seconds). White dashed vertical lines represent the growth temperatures." width="100%" height="100%" />
<p class="caption">(\#fig:diatoms_gamdiff)GAM models for polar and temperate diatoms, of consecutive flashes before damping of S-State-induced chlorophyll fluorescence oscillations. GAM model predicted by the difference from growth temperature (Δ°C) during measurements and the effective light level (µmol photons m^-2^s^-1^, estimated from flash spacings in seconds). White dashed vertical lines represent the growth temperatures.</p>
</div>

The GAM outputs varied more among the green algae tested (Table XXX). For *Chlamydomonas priscuii* the three measurement temperatures, limiting the number of basis dimensions used to create the smooth function (k) to 3 [@schoenigWorkshopGeneralizedAdditive2023], proved insufficient to capture the underlying relationship between the predictors and the response variable (Table A2). *Chlamydomonas malina* showed a significant outlier (measurement temperature of 12 °C and a 2-second interval between flashes) in the model training data (see model training dataset at XXX GitHub: S-State Damping), as evaluated by Cook’s distances [@yagerDetectingInfluentialObservations1998]. Removing this outlier point increases the variation explained by the model from 60.2 to 75.2 % and produces a significant smooth term (F = 4.544, p = 0.0255). 
Overall, model predictions for green algal strains exhibited a similar pattern to the diatoms, with the longest predicted oscillations in ChlF at measurement conditions with higher effective light levels (shorter spacing between flashes) and lower measurement temperatures relative to growth temperature (Fig \@ref{fig:greens_gamdiff}). The trends shown by the three strains of polar algae are consistent, differing by only 1-2 flashes before signal damping, under identical conditions (Fig \@ref{fig:greens_gamdiff}). Further, much like the temperate diatoms, the temperate algae *Chlamydomonas reinhardtii* and *Chlorella vulgaris* did not exhibit significant periodic oscillations in ChlF at measurement temperatures near or above their growth temperature under low light conditions produced by longer flash spacings 




## Generalized Additive Modelling By Measurement Temperature  

To facilitate comparisons across strains, generalized additive models were also fit to the data using actual measurement temperatures. For *Thalassiosira pseudonana*, *Chlamydomonas ICEMDV*, *Chlamydomonas priscuii*, *Chlamydomonas malina*, *Chlamydomonas reinhardtii*, and *Chlorella vulgaris*, replicate cultures of the strains were grown at the same temperatures. Therefore, the GAMs generated for these strains encompass different temperature ranges but are represented by the same summary statistics (Table YYY). The polar diatom *Fragilariopsis cylindrus* was cultured at both 0 and 6°C. Thus, separate models were fit for each *Fragilariopsis cylindrus* culture to account for possible physiological differences resulting from the distinct growth conditions. 

Table YYY: Summary statistics by phytoplankton strain of GAM models using the restricted maximum likelihood method to model the response of the damping of S-State-induced chlorophyll fluorescence oscillations to the predictors of measurement temperature (°C) and the effective light level (µmol photons m^-2^ s^-1^). 

(Fig \@ref{fig:diatoms_gamtemp}).


<div class="figure">
<img src="Figures/Diatoms_GAMtemp.png" alt="GAM models for polar and temperate diatoms, of consecutive flashes before damping of S-State-induced chlorophyll fluorescence oscillations. GAM model predicted by the temperature (°C) imposed during measurements and the effective light level (µmol photons m^-2^s^-1^, estimated from flash spacings in seconds). White dashed vertical lines represent the growth temperatures." width="100%" height="100%" />
<p class="caption">(\#fig:diatoms_gamtemp)GAM models for polar and temperate diatoms, of consecutive flashes before damping of S-State-induced chlorophyll fluorescence oscillations. GAM model predicted by the temperature (°C) imposed during measurements and the effective light level (µmol photons m^-2^s^-1^, estimated from flash spacings in seconds). White dashed vertical lines represent the growth temperatures.</p>
</div>

(Fig \@ref{fig:greens_gamtemp}).

<div class="figure">
<img src="Figures/Greens_GAMtemp.png" alt="GAM models for polar and temperate green algae, of consecutive flashes before damping of S-State-induced chlorophyll fluorescence oscillations. GAM model predicted by the difference from growth temperature (Δ°C) during measurements and the effective light level (µmol photons m^-2^s^-1^, estimated from flash spacings in seconds). White dashed vertical lines represent the growth temperatures." width="100%" height="100%" />
<p class="caption">(\#fig:greens_gamtemp)GAM models for polar and temperate green algae, of consecutive flashes before damping of S-State-induced chlorophyll fluorescence oscillations. GAM model predicted by the difference from growth temperature (Δ°C) during measurements and the effective light level (µmol photons m^-2^s^-1^, estimated from flash spacings in seconds). White dashed vertical lines represent the growth temperatures.</p>
</div>



# Discussion {.unnumbered}

Across all study strains ChlF oscillations, provoked by a series of single turnover saturating flashes, showed wavelet power at a period of four, that declined with increasing temperatures and decreasing equivalent light. Thus sustained synchronized S-State cycling of PSII decayed faster under higher temperatures and lower equivalent light (Figure \@ref{fig:cpriscuii_waveletpower}). This decrease in the number of consecutive flashes before the ChlF oscillations damp out suggests that cultures are maintaining synchronicity in their S-State cycling for shorter durations (Figure 14, 16) [@dewijnSstateDependenceMiss2002].  The desynchronization of S-State cycling between the PSII in a population indicates that a sufficient number of charge recombinations have taken place to create a PSII population with a random distribution of S-States. Thus, if this desynchronization occurs after fewer consecutive flashes, it signifies an increased proportion of PSIIs undergoing charge recombinations after each flash. By inference, PSII populations with increased incidence of energetically wasteful charge recombinations, such as those under high temperatures and low light levels, are less efficient in their photosynthetic energy conversion [@rappaportChargeRecombinationThermoluminescence2005]. 
These results are consistent with previous literature evaluating the response of recombination reactions to light conditions. As light levels decline, there are longer intervals between successive PSII excitations, pushing fewer electrons through the electron transport chain [@kerenMechanismPhotosystemII1997]. Consequently, the probability of energetically wasteful charge recombinations is higher, corresponding to weaker maintenance of S-State cycling [@kerenMechanismPhotosystemII1997,@dewijnSstateDependenceMiss2002]. These findings are consistent with recombination reactions decreasing with temperature [@ivanovAcclimationTemperatureIrradiance2006] as ambient temperature falls below the activation temperatures of the recombinations.   In contrast, in PSII-enriched membrane fractions isolated from spinach, the average miss probability of S-State transitions was highest at -10 °C and lowest at 10 °C [@hanMolecularBasisTurnover2022]. Therefore, we would expect to have seen more sustained S-State transitions at moderate temperatures. XXXX

## Comparisons Among Strains  

Polar strains of diatoms and green algae consistently demonstrated a stronger periodicity in ChlF emissions (Figure 9). Further, they exhibited significant 4-step ChlF oscillations under a broader range of measurement conditions (Figure 10) than did their temperate counterparts. These findings illustrate that polar phytoplankton strains have a higher capacity to maintain S-State cycling than do temperate strains. Nevertheless, comparing the number of consecutive flashes before the ChlF oscillations damp out between strains under comparable conditions reveals variable patterns. 
The first comparison was between the polar diatom *Fragilariopsis cylindrus* and the temperate diatom *Thalassiosira pseudonana*. When measured at the same temperature under low light, *Fragilariopsis cylindrus* sustained ChlF oscillations for longer than *Thalassiosira pseudonana*. However, as light levels increased, the duration of cycling in *Thalassiosira pseudonana* increased to near that of *Fragilariopsis cylindrus* (Figure ZZZ). This behaviour is consistent with the trends observed below the growth temperature when comparing these two diatom strains by the difference from growth temperature during measurement (Figure 11). Yet, when temperatures increase the same amount above the growth temperature, *Fragilariopsis cylindrus* maintains cycling for much longer than *Thalassiosira pseudonana*, regardless of the light level. These trends indicate that *Fragilariopsis cylindrus* maintains S-State cycling for longer than *Thalassiosira pseudonana* under low photon delivery and low temperatures. However, in less stressful conditions, *Thalassiosira pseudonana* can match the photosynthetic performance of *Fragilariopsis cylindrus*. Theoretically, the rate of charge recombinations would increase under low light and high temperatures [@kerenMechanismPhotosystemII1997, @ivanovAcclimationTemperatureIrradiance2006]. Thus, the stability of S-State cycling in the PSII of *Fragilariopsis cylindrus* under these conditions reflects the suppression of energetically wasteful charge recombinations by this strain compared to a temperate diatom. 
The second comparison comprises three polar, *Chlamydomonas priscuii*, *Chlamydomonas malina*, and *Chlamydomonas ICEMDV*, and two temperate, *Chlamydomonas reinhardtii*, and *Chlorella vulgaris*, strains of green algae. When evaluated at their growth temperature, the polar strains exhibited longer significant ChlF oscillations than their temperate counterparts across all light conditions (Figure 12). Yet, when compared at the same measurement temperature, these strains showed little variation in the duration of significant ChlF oscillations (Figure 17), suggesting little difference in the incidence of energetically wasteful charge recombinations. This shared measurement temperature represents a departure from the growth temperature of +8 °C for polar strains and -10 to -12 °C for temperate strains. Recombination reactions are expected to increase with temperature [@ivanovAcclimationTemperatureIrradiance2006]. Thus, the capacity for polar strains to exhibit the same durations of S-State cycling when warmed as temperate strains do when cooled suggests some suppression of energetically wasteful charge recombinations in the PSII of polar strains. This finding is underscored by the disparity in recombination reactions observed under comparable conditions. 
Through analyzing ChlF oscillations, we extrapolated the duration of significant S-State cycling and therefore, the incidence of energetically wasteful charge recombinations. Overall, our findings indicate that polar phytoplankton exhibit more stable S-State cycling than do temperate strains under limited light and temperatures surpassing their growth conditions. These findings suggest that polar phytoplankton strains increase their photosynthetic energy conversion efficiency under low light and low temperatures by minimizing energetically wasteful charge recombinations. Stable S-State cycling and minimal energy loss through charge recombination ensure continued electron flow through the ETC, sustaining ATP and NADPH production, and minimizing the risk of photodamage to the photosynthetic machinery [@rappaportChargeRecombinationThermoluminescence2005, @kerenMechanismPhotosystemII1997]. Thus, this ability may be integral for the productivity of polar phytoplankton under the ice during the polar night. 

## Ecological Implications & Future Directions  

Unravelling the mechanisms enabling polar phytoplankton to sustain slow, but significant productivity under the ice in the winter is crucial for predicting the changing dynamics of spring phytoplankton blooms, in the face of rapid warming [@ardynaPhytoplanktonDynamicsChanging2020]. Beyond direct temperature changes, polar aquatic ecosystems are experiencing reductions in sea ice extent and thickness, escalating freshwater inputs, acidification, and increased winds and storms [@ardynaPhytoplanktonDynamicsChanging2020,@cvetkovskaTemperatureStressPsychrophilic2022]. These pressures are causing alterations in the productivity and seasonal peaks of phytoplankton blooms [@ardynaPhytoplanktonDynamicsChanging2020,@croteauContrastingNonphotochemicalQuenching2021]. Understanding the nature of maintaining an intact photosystem under the ice in the winter allows us to better predict under what conditions these blooms will initiate, and which phytoplankton strains will be involved [@hanckeExtremeLowLight2018].
The ability to maintain efficient photosynthetic energy conversion over winter is crucial for the timing and speed of spring bloom initiation [@hanckeExtremeLowLight2018]. Strains with this ability may possess a competitive advantage in quickly initiating spring growth, giving them first access to the nutrients required to form an extensive bloom. Thus, the proportions of these strains may increase in polar regions. Altering the phytoplankton community composition may, in turn, exert bottom-up effects on polar ecosystems [@ardynaPhytoplanktonDynamicsChanging2020]. 
Future directions for this research include more extensive comparative analyses to further our understanding. Incorporating more strains of diatoms and green algae, as well as other phytoplankton groups will enable us to uncover whether certain groups have a higher capacity to suppress energetically wasteful charge recombinations. A difference between these groups may affect their contribution to the phytoplankton community, leading to differences in the ecosystem services offered by spring phytoplankton blooms. Moreover, since there were differences observed between *Fragilariopsis cylindrus* strains grown at 0 and 6 °C, further comparison between ecotypes may yield information on the conditions that lead to the evolution of this ability. 


# Acknowledgements {.unnumbered}

# Supporting information {.unnumbered}

# References {.unnumbered}


