### User Input (unbuffed, no trinket procs)
# http://blog.askmrrobot.com/2012/08/changes-for-patch-5-0-4/

# Primary
stamina_i <- 20237
intellect_i <- 15844
spirit_i <- 14899
agility_i <- 0
strength_i <- 0

# Secondary
mastery <- 3000
criticalstrike <- 3000
haste <- 23523
dodge <- 234
parry <-2323
expertise <- 234234
hit <- 23423

### Base Stats
stamina_b <- 649
intellect_b <- 122
spirit_b <- 121
agility_b <- 107
strength_b <- 176


### Final Stats
stamina_mod <- 146663 + (stamina_i+stamina_b-20)*14 + 20
intellect_mod <- intellect_b + intellect_i
spirit_mod <- spirit_b + spirit_i
agility_mod <- agility_b + agility_i
strength_mod <- strength_b + strength_i

mastery_mod <- mastery/600
spell_criticalstrike_mod <- criticalstrike/600 + intellect_i/2533.66
melee_criticalstrike_mod <- criticalstrike/600 + agility_i/1259.52
ranged_criticalstrike_mod <- criticalstrike/600 + agility_i/1259.52
spell_haste_mod <- haste/425
melee_haste_mod <- haste/425
ranged_haste_mod <- haste/425
dodge_mod <- dodge/885
parry_mod <- parry/885
expertise_mod <- expertise/340
spell_hit_mod <- hit/340
melee_hit_mod <- hit/340
ranged_hit_mod <- hit/340
spellpower_mod <- intellect_i

# 1% Stat Conversions
# Resilience = 310
# PvP Power = 265
# Expertise = 340
# Dodge = 885
# Parry = 885
# Block = 295
# Physical Hit = 340
# Spell Hit = 340
# Physical Crit = 600
# Spell Crit = 600
# Spell Haste = 425
# Melee Haste = 425
# Ranged Haste= 425

### Raid Buffs
# http://raidcomp.mmo-champion.com/

# Attack Power 10%: Trueshot Aura, Horn of Winter, Battle Shout
rb_TA_ap <- 1.1
rb_HoW_ap <- 1.1
rb_BS_ap <- 1.1

# Attack Speed 10%: Cackling Howl, Serpent's Swiftness, Swiftblade's Cunning, Unholy Aura, Unleashed Rage
rb_CH_as <- 1.1
rb_SS_as <- 1.1
rb_SC_as <- 1.1
rb_UA_as <- 1.1
rb_UR_as <- 1.1

# Critical Strike Chance 5% (ranged, melee, and spell): Arcane Brilliance, Fearless Roar, Furious Howl, Still Water, Terrifying Roar, Leader of the Pack, Legacy of the White Tiger
rb_AB_cs <- 1.05
rb_FR_cs <- 1.05
rb_FH_cs <- 1.05
rb_SW_cs <- 1.05
rb_TR_cs <- 1.05
rb_LotP_cs <- 1.05
rb_LofWT_cs <- 1.05

# Mastery (3000 Mastery): Blessing of Might, Grace of Air, Roar of Courage, Spirit Beast Blessing
rb_BoM_m <- 3000/600*PaladinHoly
rb_GoA_m <- 3000/600*PaladinHoly
rb_RoC_m <- 3000/600*PaladinHoly
rb_SBB_m <- 3000/600*PaladinHoly

# Spell Haste (5%): Elemental Oath, Mind Quickening, Moonkin Form, Shadowform
rb_EO_sh <- 1.05
rb_MQ_sh <- 1.05
rb_MF_sh <- 1.05
rb_S_sh <- 1.05

# Spell Power (10%): Arcane Brilliance, Burning Wrath, Dark Intent, Still water
rb_AB_sp <- 1.1
rb_BW_sp <- 1.1
rb_DI_sp <- 1.1
rb_SW_sp <- 1.1

# Stamina (10%): Commanding Shout, Dark Intent, Power Word: Fortitude, Qiraji Fortitude
rb_CS_s <- 1.1
rb_DI_s <- 1.1
rb_PWF_s <- 1.1
rb_QF_s <- 1.1

# Stat Multiplier (): Blessing of Kings, Embrace of the Shale Spider, Legacy of the Emperor, Mark of the Wild
rb_BoK_sai <- 1.05
rb_EofSS_sai <- 1.05
rb_LotE_sai <- 1.05
rb_MotW_sai <- 1.05

### General Weights

# Mastery
DeathKnightBlood2H = 6.25;
DeathKnightFrost2H = 2;
DeathKnightFrostDW = 2;
DeathKnightUnholy2H = 2.5;
DruidMoonkin = 1.875;
DruidFeralCat = 3.13;
DruidFeralBear = 1.25;
DruidRestoration = 1.25;
HunterBeastMastery = 2;
HunterMarksmanship = 2;
HunterSurvival = 1;
MageArcane = 2;
MageFire = 1.5;
MageFrost = 2;
PaladinHoly = 1.5;
PaladinProtection = 1;
PaladinRetribution = 2.1;
PriestDiscipline = 2.5;
PriestHoly = 1.25;
PriestShadow = 1.8;
RogueAssassination = 3.5;
RogueCombat = 2;
RogueSubtlety = 3;
ShamanElemental = 2;
ShamanEnhancement = 2;
ShamanRestoration= 3;
WarlockAffliction = 3.1;
WarlockDemonology = 1;
WarlockDestruction = 3;
WarriorArms = 2.2;
WarriorFury = 1.4;
WarriorFury2H = 1.4;
WarriorProtection = 2.2; // 0.5 for block

### User Input



