/* Copyright 1993 David Fotland.
 * Permission granted to Spec to use and distribute as a computer benchamrk
 * permission is not granted to use or modify this code for any
 * purpose other than benchmarking or computer system performance tuning.
 *
 * This program is available (with a user interface) for:
 * IBM-PC/DOS from Ishi Press Inc San Jose CA (408)944-9900
 * Penpoint from PenGames San Jose Ca (408)985-1236
 */

# include "g2def.h"

int xymap[62] = {
52,	/* 0 (3,4) */
51,	/* 1 (3,3) */
83,	/* 2 (5,3) */
50,	/* 3 (3,2) */
68,	/* 4 (4,4) */
84,	/* 5 (5,4) */
69,	/* 6 (4,5) */
66,	/* 7 (4,2) */
35,	/* 8 (2,3) */
70,	/* 9 (4,6) */
100,	/* 10 (6,4) */
99,	/* 11 (6,3) */
85,	/* 12 (5,5) */
82,	/* 13 (5,2) */
54,	/* 14 (3,6) */
36,	/* 15 (2,4) */
116,	/* 16 (7,4) */
86,	/* 17 (5,6) */
55,	/* 18 (3,7) */
101,	/* 19 (6,5) */
71,	/* 20 (4,7) */
115,	/* 21 (7,3) */
34,	/* 22 (2,2) */
37,	/* 23 (2,5) */
56,	/* 24 (3,8) */
38,	/* 25 (2,6) */
98,	/* 26 (6,2) */
102,	/* 27 (6,6) */
87,	/* 28 (5,7) */
117,	/* 29 (7,5) */
39,	/* 30 (2,7) */
-124,	/* 31 (8,4) */
88,	/* 32 (5,8) */
-125,	/* 33 (8,3) */
103,	/* 34 (6,7) */
40,	/* 35 (2,8) */
57,	/* 36 (3,9) */
72,	/* 37 (4,8) */
53,	/* 38 (3,5) */
-109,	/* 39 (9,3) */
118,	/* 40 (7,6) */
114,	/* 41 (7,2) */
-123,	/* 42 (8,5) */
67,	/* 43 (4,3) */
73,	/* 44 (4,9) */
-93,	/* 45 (10,3) */
58,	/* 46 (3,10) */
119,	/* 47 (7,7) */
104,	/* 48 (6,8) */
-108,	/* 49 (9,4) */
41,	/* 50 (2,9) */
74,	/* 51 (4,10) */
89,	/* 52 (5,9) */
18,	/* 53 (1,2) */
-126,	/* 54 (8,2) */
19,	/* 55 (1,3) */
33,	/* 56 (2,1) */
-92,	/* 57 (10,4) */
-122,	/* 58 (8,6) */
20,	/* 59 (1,4) */
65,	/* 60 (4,1) */
75,	/* 61 (4,11) */
};

int jlib2[NUMJOSBYTES] = {
	0,	/* dummy entry */
	-21,-124,	/* has siblings */
	-55,-126,	/* has siblings */
	62,
	-127,	/* (6) has sibling */
	62,
	-124,	/* (8) has sibling */
	66,	/* (9) last move */
	0,
	4,
	-58,-126,	/* has siblings */
	5,
	38,
	12,
	3,
	8,
	22,
	2,
	7,
	14,
	15,
	23,
	119,	/* (25) last move */
	-61,-126,	/* has siblings */
	8,
	5,
	6,
	13,
	22,
	56,
	23,
	80,	/* (35) last move */
	15,
	-120,	/* (37) has sibling */
	-57,-126,	/* has siblings */
	38,
	5,
	6,
	22,
	23,
	3,
	123,	/* (46) last move */
	22,
	-121,	/* (48) has sibling */
	2,
	3,
	25,
	5,
	11,
	56,
	24,
	29,
	95,	/* (57) last move */
	3,
	7,
	56,
	53,
	55,
	25,
	-58,-120,	/* has siblings */
	-117,	/* (66) has sibling */
	23,
	38,
	14,
	63,21,
	30,
	35,
	63,17,
	59,
	-118,	/* (77) has sibling */
	-123,	/* (78) has sibling */
	12,
	2,
	-107,	/* (81) has sibling */
	41,
	54,
	-112,	/* (84) has sibling */
	19,
	33,
	63,23,
	18,
	24,
	20,
	50,
	37,
	63,24,
	111,	/* (96) last move */
	-38,2,
	16,
	60,
	88,	/* (101) last move */
	63,23,
	18,
	24,
	20,
	50,
	19,
	17,
	37,
	36,
	91,	/* (112) last move */
	-46,2,
	20,
	24,
	21,
	2,
	90,	/* (119) last move */
	-1,23,2,
	18,
	24,
	20,
	50,
	81,	/* (127) last move */
	-4,2,
	63,17,
	-114,	/* (132) has sibling */
	66,	/* (133) last move */
	11,
	87,	/* (135) last move */
	5,
	60,
	2,
	63,49,
	63,17,
	22,
	-51,7,
	53,
	63,81,
	123,	/* (149) last move */
	2,
	7,
	5,
	67,	/* (153) last move */
	-104,	/* (154) has sibling */
	-90,	/* (155) has sibling */
	-128,	/* (156) has sibling */
	15,
	8,
	4,
	1,
	5,
	11,
	125,	/* (163) last move */
	10,
	8,
	45,
	62,
	28,
	17,
	34,
	27,
	111,	/* (172) last move */
	-124,	/* (173) has sibling */
	-62,-126,	/* has siblings */
	1,
	3,
	15,
	22,
	125,	/* (180) last move */
	-123,	/* (181) has sibling */
	-62,-126,	/* has siblings */
	1,
	11,
	10,
	16,
	19,
	-95,	/* (189) has sibling */
	70,	/* (190) last move */
	6,
	85,	/* (192) last move */
	1,
	-115,	/* (194) has sibling */
	0,
	-125,	/* (196) has sibling */
	12,
	10,
	-3,23,	/* last move */
	12,
	7,
	2,
	30,
	109,	/* (205) last move */
	3,
	-62,-125,	/* has siblings */
	7,
	13,
	10,
	21,
	16,
	33,
	-58,-126,	/* has siblings */
	0,
	38,
	23,
	14,
	22,
	25,
	79,	/* (223) last move */
	0,
	6,
	15,
	-36,-112,	/* has siblings */	/* last move */
	95,	/* (229) last move */
	0,
	13,
	12,
	10,
	-3,23,	/* last move */
	1,
	-91,	/* (237) has sibling */
	-123,	/* (238) has sibling */
	18,
	16,
	100,	/* (241) last move */
	44,
	32,
	18,
	2,
	-59,-126,	/* has siblings */
	11,
	6,
	12,
	0,
	10,
	14,
	4,
	102,	/* (255) last move */
	0,
	5,
	14,
	109,	/* (259) last move */
	10,
	2,
	34,
	21,
	-18,-121,	/* has siblings */
	89,	/* (266) last move */
	-20,7,
	110,	/* (269) last move */
	-63,2,
	4,
	0,
	38,
	6,
	78,	/* (276) last move */
	-128,	/* (277) has sibling */
	-59,7,
	2,
	16,
	10,
	19,
	11,
	40,
	-31,23,	/* last move */
	-111,	/* (288) has sibling */
	-36,-126,	/* has siblings */
	-52,-126,	/* has siblings */
	-58,-126,	/* has siblings */
	-108,	/* (295) has sibling */
	5,
	4,
	18,
	37,
	32,
	52,
	63,105,
	-80,	/* (304) has sibling */
	34,
	47,
	63,120,
	27,
	48,
	24,
	40,
	19,
	38,
	0,
	23,
	-49,-126,	/* has siblings */
	35,
	50,
	25,
	63,24,
	63,23,
	30,
	16,
	10,
	11,
	2,
	29,
	5,
	99,	/* (333) last move */
	10,
	35,
	50,
	30,
	36,
	15,
	8,
	59,
	63,-121,
	63,90,
	127,60,	/* (346) last move */
	-54,2,
	44,
	24,
	35,
	36,
	46,
	50,
	127,42,	/* (356) last move */
	-60,2,
	101,	/* (360) last move */
	37,
	6,
	14,
	38,
	111,	/* (365) last move */
	6,
	-90,	/* (367) has sibling */
	14,
	20,
	23,
	-64,-126,	/* has siblings */
	4,
	-39,-126,	/* has siblings */
	18,
	15,
	30,
	63,21,
	36,
	27,
	22,
	8,
	3,
	55,
	85,	/* (387) last move */
	18,
	25,
	30,
	79,	/* (391) last move */
	12,
	0,
	91,	/* (394) last move */
	20,
	-52,-126,	/* has siblings */
	111,	/* (398) last move */
	38,
	27,
	12,
	127,120,	/* (402) last move */
	-124,	/* (404) has sibling */
	5,
	12,
	19,
	6,
	2,
	-91,	/* (410) has sibling */
	0,
	27,
	29,
	92,	/* (414) last move */
	1,
	20,
	18,
	28,
	24,
	3,
	15,
	8,
	0,
	86,	/* (424) last move */
	-116,	/* (425) has sibling */
	6,
	4,
	38,
	19,
	-44,-126,	/* has siblings */
	0,
	14,
	66,	/* (434) last move */
	0,
	28,
	-59,7,
	91,	/* (439) last move */
	20,
	-58,-126,	/* has siblings */
	38,
	0,
	12,
	4,
	-109,	/* (447) has sibling */
	91,	/* (448) last move */
	91,	/* (449) last move */
	12,
	0,
	1,
	92,	/* (453) last move */
	-90,	/* (454) has sibling */
	62,
	-114,	/* (456) has sibling */
	37,
	20,
	28,
	18,
	96,	/* (461) last move */
	62,
	-122,	/* (463) has sibling */
	-111,	/* (464) has sibling */
	94,	/* (465) last move */
	27,
	17,
	28,
	12,
	34,
	-48,-105,	/* has siblings */	/* last move */
	-40,23,	/* last move */
	-122,	/* (475) has sibling */
	-114,	/* (476) has sibling */
	-124,	/* (477) has sibling */
	0,
	2,
	20,
	1,
	7,
	72,	/* (483) last move */
	-46,2,
	4,
	84,	/* (487) last move */
	-124,	/* (488) has sibling */
	14,
	23,
	-107,	/* (491) has sibling */
	89,	/* (492) last move */
	25,
	85,	/* (494) last move */
	-64,2,
	-126,	/* (497) has sibling */
	13,
	4,
	3,
	11,
	26,
	97,	/* (503) last move */
	14,
	-123,	/* (505) has sibling */
	115,	/* (506) last move */
	11,
	-18,-112,	/* has siblings */	/* last move */
	115,	/* (510) last move */
	14,
	6,
	-82,	/* (513) has sibling */
	17,
	23,
	15,
	25,
	76,	/* (518) last move */
	17,
	62,
	-63,-125,	/* has siblings */
	-57,-125,	/* has siblings */
	23,
	15,
	0,
	25,
	68,	/* (529) last move */
	-52,-112,	/* has siblings */	/* last move */
	-64,2,
	7,
	3,
	2,
	8,
	74,	/* (538) last move */
	62,
	-62,-125,	/* has siblings */
	-59,-126,	/* has siblings */
	11,
	10,
	71,	/* (546) last move */
	-54,-112,	/* has siblings */	/* last move */
	-52,-112,	/* has siblings */	/* last move */
	-57,18,	/* last move */
	62,
	-52,7,
	5,
	10,
	11,
	16,
	21,
	31,
	25,
	30,
	23,
	97,	/* (565) last move */
	10,
	-1,59,-126,	/* has siblings */
	36,
	51,
	32,
	37,
	44,
	52,
	20,
	28,
	24,
	48,
	78,	/* (580) last move */
	-63,-126,	/* has siblings */
	-63,-110,	/* has siblings */	/* last move */
	-128,	/* (585) has sibling */
	4,
	6,
	5,
	12,
	2,
	11,
	13,
	26,
	7,
	15,
	8,
	14,
	19,
	17,
	29,
	95,	/* (601) last move */
	-61,2,
	0,
	13,
	110,	/* (606) last move */
	46,
	-104,	/* (608) has sibling */
	-26,-126,	/* has siblings */
	32,
	27,
	63,90,
	1,
	3,
	4,
	2,
	8,
	103,	/* (620) last move */
	-127,	/* (621) has sibling */
	-128,	/* (622) has sibling */
	4,
	6,
	38,
	5,
	15,
	4,
	25,
	8,
	55,
	3,
	92,	/* (633) last move */
	4,
	-64,-126,	/* has siblings */
	102,	/* (637) last move */
	18,
	-91,	/* (639) has sibling */
	0,
	6,
	25,
	17,
	20,
	28,
	35,
	116,	/* (647) last move */
	0,
	101,	/* (649) last move */
	-27,2,
	18,
	20,
	23,
	36,
	64,	/* (656) last move */
	62,
	-7,-121,	/* has siblings */
	62,
	-127,	/* (661) has sibling */
	-125,	/* (662) has sibling */
	4,
	2,
	0,
	86,	/* (666) last move */
	0,
	4,
	-123,	/* (669) has sibling */
	6,
	3,
	8,
	13,
	22,
	56,
	102,	/* (676) last move */
	6,
	5,
	12,
	2,
	11,
	7,
	13,
	43,
	38,
	83,	/* (686) last move */
	64,	/* (687) last move */
	62,
	-127,	/* (689) has sibling */
	0,
	4,
	6,
	5,
	12,
	2,
	11,
	13,
	38,
	26,
	8,
	19,
	3,
	29,
	81,	/* (704) last move */
	45,
	-64,-126,	/* has siblings */
	1,
	4,
	69,	/* (710) last move */
	-63,7,
	-128,	/* (713) has sibling */
	4,
	-56,-126,	/* has siblings */
	-61,-126,	/* has siblings */
	6,
	5,
	12,
	2,
	11,
	77,	/* (724) last move */
	2,
	3,
	5,
	13,
	75,	/* (729) last move */
	6,
	5,
	12,
	2,
	11,
	-57,-126,	/* has siblings */
	13,
	43,
	38,
	-45,-126,	/* has siblings */
	26,
	22,
	81,	/* (744) last move */
	26,
	41,
	63,81,
	83,	/* (749) last move */
	13,
	-120,	/* (751) has sibling */
	7,
	38,
	22,
	104,	/* (755) last move */
	26,
	7,
	15,
	-39,-125,	/* has siblings */
	22,
	8,
	55,
	3,
	38,
	53,
	63,17,
	63,49,
	-24,23,	/* last move */
	8,
	14,
	19,
	17,
	29,
	95,	/* (778) last move */
	-61,2,
	0,
	77,	/* (782) last move */
	-50,-126,	/* has siblings */
	62,
	-127,	/* (786) has sibling */
	0,
	4,
	8,
	3,
	6,
	-49,-126,	/* has siblings */
	5,
	38,
	4,
	22,
	23,
	55,
	20,
	-46,-126,	/* has siblings */
	88,	/* (803) last move */
	25,
	73,	/* (805) last move */
	5,
	38,
	22,
	9,
	16,
	108,	/* (811) last move */
	-84,	/* (812) has sibling */
	-60,-125,	/* has siblings */
	-123,	/* (815) has sibling */
	1,
	-125,	/* (817) has sibling */
	0,
	13,
	12,
	10,
	19,
	80,	/* (823) last move */
	-62,2,
	0,
	103,	/* (827) last move */
	-128,	/* (828) has sibling */
	2,
	6,
	5,
	38,
	1,
	-52,-126,	/* has siblings */
	7,
	93,	/* (837) last move */
	8,
	7,
	73,	/* (840) last move */
	-126,	/* (841) has sibling */
	1,
	3,
	-123,	/* (844) has sibling */
	11,
	22,
	8,
	0,
	56,
	29,
	31,
	127,76,	/* (852) last move */
	-42,-125,	/* has siblings */
	-128,	/* (856) has sibling */
	8,
	70,	/* (858) last move */
	8,
	0,
	56,
	10,
	11,
	21,
	41,
	69,	/* (866) last move */
	0,
	22,
	-14,-112,	/* has siblings */	/* last move */
	19,
	80,	/* (872) last move */
	-63,3,
	-54,-126,	/* has siblings */
	2,
	27,
	85,	/* (879) last move */
	2,
	-51,-126,	/* has siblings */
	75,	/* (883) last move */
	5,
	13,
	-64,-125,	/* has siblings */
	6,
	10,
	85,	/* (890) last move */
	-58,2,
	0,
	38,
	15,
	23,
	8,
	74,	/* (898) last move */
	-127,	/* (899) has sibling */
	0,
	4,
	8,
	3,
	6,
	5,
	38,
	-53,-126,	/* has siblings */
	22,
	7,
	73,	/* (911) last move */
	22,
	-55,-126,	/* has siblings */
	80,	/* (915) last move */
	16,
	19,
	9,
	29,
	-33,-126,	/* has siblings */
	66,	/* (922) last move */
	-87,	/* (923) has sibling */
	13,
	109,	/* (925) last move */
	49,
	85,	/* (927) last move */
	-47,2,
	74,	/* (930) last move */
	-122,	/* (931) has sibling */
	-113,	/* (932) has sibling */
	-27,-125,	/* has siblings */
	9,
	17,
	-52,-126,	/* has siblings */
	28,
	4,
	5,
	38,
	19,
	2,
	6,
	7,
	0,
	1,
	4,
	10,
	12,
	22,
	11,
	13,
	80,	/* (955) last move */
	28,
	20,
	12,
	27,
	4,
	5,
	38,
	19,
	2,
	6,
	7,
	0,
	1,
	4,
	10,
	12,
	22,
	11,
	13,
	16,
	-29,7,
	94,	/* (978) last move */
	9,
	18,
	20,
	1,
	-125,	/* (983) has sibling */
	-40,-112,	/* has siblings */	/* last move */
	-42,2,
	0,
	8,
	88,	/* (990) last move */
	7,
	24,
	109,	/* (993) last move */
	9,
	-52,-126,	/* has siblings */
	15,
	17,
	28,
	34,
	96,	/* (1001) last move */
	-111,	/* (1002) has sibling */
	-49,-126,	/* has siblings */
	28,
	1,
	7,
	24,
	109,	/* (1009) last move */
	-26,-126,	/* has siblings */
	0,
	12,
	4,
	-45,-126,	/* has siblings */
	28,
	36,
	21,
	40,
	112,	/* (1021) last move */
	28,
	27,
	37,
	83,	/* (1025) last move */
	28,
	38,
	23,
	15,
	25,
	12,
	-56,-126,	/* has siblings */
	59,
	1,
	0,
	7,
	13,
	2,
	11,
	4,
	69,	/* (1042) last move */
	98,	/* (1043) last move */
	38,
	81,	/* (1045) last move */
	-104,	/* (1046) has sibling */
	1,
	-61,-126,	/* has siblings */
	0,
	13,
	17,
	5,
	96,	/* (1054) last move */
	0,
	4,
	-113,	/* (1057) has sibling */
	8,
	5,
	6,
	7,
	23,
	22,
	38,
	3,
	59,
	97,	/* (1067) last move */
	-58,-126,	/* has siblings */
	5,
	8,
	38,
	-125,	/* (1073) has sibling */
	15,
	1,
	81,	/* (1076) last move */
	8,
	81,	/* (1078) last move */
	8,
	3,
	6,
	5,
	38,
	22,
	-112,	/* (1085) has sibling */
	-109,	/* (1086) has sibling */
	29,
	27,
	9,
	11,
	21,
	90,	/* (1092) last move */
	12,
	9,
	17,
	28,
	34,
	32,
	111,	/* (1099) last move */
	-109,	/* (1100) has sibling */
	12,
	17,
	-54,-126,	/* has siblings */
	16,
	-101,	/* (1106) has sibling */
	11,
	29,
	2,
	19,
	31,
	9,
	25,
	-113,	/* (1114) has sibling */
	23,
	55,
	92,	/* (1117) last move */
	28,
	87,	/* (1119) last move */
	13,
	91,	/* (1121) last move */
	2,
	-119,	/* (1123) has sibling */
	27,
	29,
	34,
	11,
	13,
	122,	/* (1129) last move */
	27,
	62,
	-53,-121,	/* has siblings */
	77,	/* (1134) last move */
	-43,23,	/* last move */
	9,
	80,	/* (1138) last move */
	0,
	100,	/* (1140) last move */
	62,
	-13,-119,	/* has siblings */
	6,
	38,
	14,
	0,
	27,
	10,
	88,	/* (1150) last move */
	62,
	-58,-106,	/* has siblings */	/* last move */
	62,
	-50,-122,	/* has siblings */
	-19,-121,	/* has siblings */
	33,
	11,
	-112,	/* (1161) has sibling */
	-59,-126,	/* has siblings */
	6,
	12,
	17,
	19,
	122,	/* (1168) last move */
	1,
	-124,	/* (1170) has sibling */
	7,
	13,
	3,
	2,
	23,
	25,
	38,
	6,
	0,
	92,	/* (1180) last move */
	0,
	-60,-125,	/* has siblings */
	-123,	/* (1184) has sibling */
	6,
	38,
	2,
	7,
	-116,	/* (1189) has sibling */
	3,
	10,
	29,
	92,	/* (1193) last move */
	3,
	76,	/* (1195) last move */
	-58,2,
	-121,	/* (1198) has sibling */
	5,
	10,
	19,
	2,
	8,
	4,
	12,
	22,
	43,
	67,	/* (1208) last move */
	5,
	3,
	8,
	13,
	26,
	22,
	12,
	17,
	83,	/* (1217) last move */
	-121,	/* (1218) has sibling */
	-125,	/* (1219) has sibling */
	8,
	-115,	/* (1221) has sibling */
	4,
	2,
	38,
	6,
	15,
	5,
	73,	/* (1228) last move */
	-42,2,
	4,
	2,
	5,
	13,
	26,
	60,
	38,
	15,
	23,
	55,
	74,	/* (1241) last move */
	-60,2,
	22,
	8,
	3,
	15,
	117,	/* (1248) last move */
	3,
	-113,	/* (1250) has sibling */
	7,
	2,
	13,
	10,
	105,	/* (1255) last move */
	7,
	4,
	-106,	/* (1258) has sibling */
	38,
	15,
	6,
	23,
	2,
	8,
	73,	/* (1265) last move */
	5,
	-122,	/* (1267) has sibling */
	38,
	2,
	12,
	9,
	84,	/* (1272) last move */
	-26,18,	/* last move */
	6,
	19,
	42,
	34,
	127,-91,	/* (1279) last move */
	-63,-126,	/* has siblings */
	0,
	7,
	-61,-125,	/* has siblings */
	8,
	22,
	4,
	2,
	15,
	5,
	38,
	70,	/* (1294) last move */
	-126,	/* (1295) has sibling */
	-115,	/* (1296) has sibling */
	8,
	3,
	11,
	26,
	21,
	22,
	15,
	117,	/* (1304) last move */
	8,
	13,
	-49,-126,	/* has siblings */
	23,
	4,
	38,
	63,49,
	53,
	22,
	63,81,
	60,
	120,	/* (1319) last move */
	63,49,
	15,
	86,	/* (1323) last move */
	4,
	22,
	13,
	15,
	23,
	127,49,	/* (1329) last move */
	-26,-126,	/* has siblings */
	-64,-126,	/* has siblings */
	6,
	23,
	25,
	30,
	9,
	-65,22,	/* (1340) has sibling */
	18,
	25,
	88,	/* (1344) last move */
	18,
	2,
	13,
	-53,-126,	/* has siblings */
	-102,	/* (1350) has sibling */
	15,
	63,22,
	1,
	4,
	7,
	5,
	74,	/* (1358) last move */
	4,
	5,
	67,	/* (1361) last move */
	5,
	-38,-126,	/* has siblings */
	15,
	63,22,
	65,	/* (1368) last move */
	63,22,
	26,
	71,	/* (1372) last move */
	6,
	0,
	9,
	-63,-126,	/* has siblings */
	-57,-126,	/* has siblings */
	4,
	5,
	3,
	11,
	25,
	30,
	127,21,	/* (1386) last move */
	4,
	7,
	13,
	3,
	23,
	15,
	25,
	117,	/* (1395) last move */
	4,
	5,
	1,
	-57,-126,	/* has siblings */
	3,
	11,
	25,
	30,
	127,21,	/* (1405) last move */
	13,
	7,
	2,
	25,
	30,
	23,
	3,
	22,
	60,
	120,	/* (1416) last move */
	-64,2,
	-124,	/* (1419) has sibling */
	38,
	-58,-126,	/* has siblings */
	25,
	-46,-126,	/* has siblings */
	30,
	24,
	65,	/* (1428) last move */
	30,
	18,
	9,
	35,
	23,
	63,23,
	79,	/* (1436) last move */
	9,
	-63,-126,	/* has siblings */
	70,	/* (1440) last move */
	6,
	12,
	1,
	-111,	/* (1444) has sibling */
	7,
	13,
	3,
	75,	/* (1448) last move */
	3,
	22,
	-41,-126,	/* has siblings */
	15,
	59,
	8,
	7,
	25,
	30,
	63,21,
	81,	/* (1461) last move */
	7,
	25,
	30,
	23,
	56,
	8,
	-1,22,-126,	/* has siblings */
	18,
	35,
	17,
	20,
	91,	/* (1475) last move */
	17,
	59,
	63,22,
	117,	/* (1480) last move */
	1,
	4,
	5,
	-119,	/* (1484) has sibling */
	-49,-112,	/* has siblings */	/* last move */
	6,
	38,
	12,
	25,
	18,
	30,
	24,
	8,
	22,
	15,
	35,
	63,21,
	75,	/* (1500) last move */
	6,
	20,
	12,
	10,
	19,
	80,	/* (1506) last move */
	-26,-118,	/* has siblings */
	-64,-126,	/* has siblings */
	6,
	-117,	/* (1512) has sibling */
	100,	/* (1513) last move */
	10,
	-20,-112,	/* has siblings */	/* last move */
	100,	/* (1517) last move */
	-55,-120,	/* has siblings */
	-60,-126,	/* has siblings */
	5,
	1,
	13,
	14,
	84,	/* (1526) last move */
	-63,-126,	/* has siblings */
	6,
	7,
	0,
	15,
	4,
	22,
	13,
	14,
	84,	/* (1537) last move */
	6,
	12,
	-111,	/* (1540) has sibling */
	14,
	-39,-125,	/* has siblings */
	28,
	27,
	19,
	-94,	/* (1547) has sibling */
	30,
	15,
	63,22,
	23,
	18,
	32,
	-56,-126,	/* has siblings */
	1,
	3,
	22,
	0,
	4,
	2,
	1,
	55,
	53,
	0,
	5,
	10,
	65,	/* (1569) last move */
	-127,	/* (1570) has sibling */
	36,
	50,
	63,42,
	-18,-120,	/* has siblings */
	-99,	/* (1577) has sibling */
	-48,-126,	/* has siblings */
	10,
	5,
	24,
	35,
	127,43,	/* (1584) last move */
	-124,	/* (1586) has sibling */
	24,
	35,
	63,43,
	8,
	16,
	11,
	20,
	59,
	125,	/* (1596) last move */
	8,
	16,
	11,
	20,
	123,	/* (1601) last move */
	-104,	/* (1602) has sibling */
	35,
	63,43,
	37,
	44,
	52,
	20,
	51,
	37,
	48,
	28,
	111,	/* (1614) last move */
	-20,2,
	63,43,
	35,
	63,26,
	24,
	63,24,
	20,
	127,23,	/* (1626) last move */
	35,
	63,59,
	8,
	29,
	11,
	24,
	4,
	20,
	123,	/* (1637) last move */
	46,
	65,	/* (1639) last move */
	-24,2,
	4,
	15,
	24,
	3,
	7,
	30,
	52,
	22,
	29,
	58,
	113,	/* (1652) last move */
	5,
	19,
	4,
	10,
	2,
	11,
	13,
	92,	/* (1660) last move */
	-59,2,
	4,
	17,
	19,
	20,
	27,
	-36,-126,	/* has siblings */
	23,
	0,
	14,
	-127,	/* (1673) has sibling */
	-125,	/* (1674) has sibling */
	7,
	2,
	22,
	8,
	63,49,
	15,
	3,
	13,
	25,
	18,
	30,
	24,
	35,
	36,
	55,
	112,	/* (1691) last move */
	-57,2,
	18,
	25,
	15,
	3,
	59,
	8,
	22,
	53,
	94,	/* (1702) last move */
	-113,	/* (1703) has sibling */
	25,
	-125,	/* (1705) has sibling */
	7,
	1,
	24,
	10,
	16,
	13,
	21,
	26,
	122,	/* (1714) last move */
	1,
	-125,	/* (1716) has sibling */
	7,
	2,
	22,
	24,
	-54,2,
	-115,	/* (1723) has sibling */
	16,
	60,
	47,
	59,
	8,
	120,	/* (1729) last move */
	-53,18,	/* last move */
	-57,2,
	18,
	3,
	59,
	55,
	86,	/* (1738) last move */
	25,
	18,
	15,
	24,
	2,
	7,
	13,
	8,
	63,21,
	86,	/* (1749) last move */
	14,
	32,
	44,
	85,	/* (1753) last move */
	-122,	/* (1754) has sibling */
	9,
	12,
	0,
	1,
	-111,	/* (1759) has sibling */
	27,
	-45,-125,	/* has siblings */
	11,
	29,
	34,
	88,	/* (1766) last move */
	24,
	75,	/* (1768) last move */
	36,
	109,	/* (1770) last move */
	-84,	/* (1771) has sibling */
	62,
	-128,	/* (1773) has sibling */
	6,
	10,
	20,
	95,	/* (1777) last move */
	-120,	/* (1778) has sibling */
	10,
	18,
	109,	/* (1781) last move */
	-112,	/* (1782) has sibling */
	6,
	-60,-125,	/* has siblings */
	5,
	0,
	-127,	/* (1788) has sibling */
	-46,-125,	/* has siblings */
	12,
	8,
	22,
	28,
	29,
	42,
	40,
	48,
	31,
	49,
	33,
	39,
	41,
	54,
	85,	/* (1805) last move */
	-52,2,
	10,
	-45,-126,	/* has siblings */
	9,
	8,
	90,	/* (1813) last move */
	9,
	19,
	28,
	85,	/* (1817) last move */
	-55,2,
	2,
	11,
	13,
	10,
	71,	/* (1824) last move */
	9,
	12,
	0,
	-111,	/* (1828) has sibling */
	20,
	28,
	-124,	/* (1831) has sibling */
	5,
	2,
	-115,	/* (1834) has sibling */
	11,
	1,
	8,
	22,
	15,
	26,
	41,
	33,
	21,
	53,
	30,
	-14,23,	/* last move */
	-53,2,
	13,
	21,
	7,
	95,	/* (1853) last move */
	1,
	30,
	-40,-126,	/* has siblings */
	36,
	35,
	50,
	63,24,
	37,
	82,	/* (1864) last move */
	25,
	37,
	121,	/* (1867) last move */
	-63,2,
	-32,-112,	/* has siblings */	/* last move */
	-46,2,
	29,
	42,
	104,	/* (1876) last move */
	-54,-125,	/* has siblings */
	-123,	/* (1879) has sibling */
	12,
	11,
	16,
	21,
	31,
	-25,-126,	/* has siblings */
	4,
	2,
	1,
	3,
	22,
	8,
	0,
	-8,-126,	/* has siblings */
	97,	/* (1896) last move */
	71,	/* (1897) last move */
	33,
	49,
	45,
	125,	/* (1901) last move */
	6,
	4,
	5,
	0,
	2,
	12,
	9,
	1,
	11,
	19,
	33,
	30,
	20,
	24,
	101,	/* (1916) last move */
	-109,	/* (1917) has sibling */
	-116,	/* (1918) has sibling */
	17,
	5,
	-110,	/* (1921) has sibling */
	27,
	28,
	29,
	34,
	104,	/* (1926) last move */
	-55,2,
	27,
	-99,	/* (1930) has sibling */
	40,
	42,
	48,
	8,
	85,	/* (1935) last move */
	-27,2,
	29,
	34,
	40,
	100,	/* (1941) last move */
	21,
	-97,	/* (1943) has sibling */
	16,
	29,
	42,
	57,
	58,
	97,	/* (1949) last move */
	125,	/* (1950) last move */
	-59,-125,	/* has siblings */
	-60,-125,	/* has siblings */
	6,
	12,
	10,
	-47,-126,	/* has siblings */
	8,
	90,	/* (1961) last move */
	19,
	16,
	29,
	31,
	0,
	15,
	8,
	23,
	3,
	13,
	7,
	-111,	/* (1973) has sibling */
	42,
	-65,-107,	/* (1975) has sibling */
	63,-121,
	32,
	37,
	20,
	34,
	28,
	49,
	-7,-126,	/* has siblings */
	39,
	45,
	54,
	63,-94,
	41,
	26,
	2,
	11,
	127,113,	/* (1996) last move */
	39,
	57,
	45,
	127,-76,	/* (2001) last move */
	-15,2,
	127,120,	/* (2005) last move */
	-22,-110,	/* has siblings */	/* last move */
	-44,-126,	/* has siblings */
	28,
	37,
	32,
	50,
	36,
	35,
	63,59,
	63,42,
	127,91,	/* (2021) last move */
	-40,2,
	42,
	49,
	37,
	36,
	46,
	63,42,
	127,43,	/* (2032) last move */
	2,
	10,
	21,
	61,
	81,	/* (2038) last move */
	-124,	/* (2039) has sibling */
	-63,-126,	/* has siblings */
	2,
	-59,-110,	/* has siblings */	/* last move */
	13,
	26,
	11,
	5,
	41,
	7,
	63,97,
	67,	/* (2053) last move */
	2,
	1,
	3,
	8,
	80,	/* (2058) last move */
	1,
	3,
	4,
	-120,	/* (2062) has sibling */
	2,
	0,
	7,
	1,
	76,	/* (2067) last move */
	2,
	8,
	80,	/* (2070) last move */
	-118,	/* (2071) has sibling */
	62,
	-128,	/* (2073) has sibling */
	6,
	101,	/* (2075) last move */
	62,
	-92,	/* (2077) has sibling */
	-27,-112,	/* has siblings */	/* last move */
	-46,-112,	/* has siblings */	/* last move */
	1,
	3,
	4,
	2,
	72,	/* (2086) last move */
	-95,	/* (2087) has sibling */
	-1,-76,-112,	/* has siblings */	/* last move */
	100,	/* (2091) last move */
	-92,	/* (2092) has sibling */
	-110,	/* (2093) has sibling */
	-63,-126,	/* has siblings */
	3,
	4,
	-120,	/* (2098) has sibling */
	2,
	0,
	7,
	1,
	5,
	6,
	12,
	73,	/* (2106) last move */
	-62,2,
	94,	/* (2109) last move */
	30,
	35,
	24,
	25,
	20,
	14,
	9,
	6,
	23,
	63,23,
	63,21,
	63,22,
	50,
	0,
	12,
	68,	/* (2128) last move */
	-83,	/* (2129) has sibling */
	1,
	3,
	4,
	66,	/* (2133) last move */
	-65,59,	/* (2134) has sibling */
	1,
	3,
	4,
	2,
	72,	/* (2140) last move */
	-64,18,	/* last move */
	-104,	/* (2143) has sibling */
	-19,-126,	/* has siblings */
	1,
	3,
	4,
	2,
	72,	/* (2150) last move */
	0,
	6,
	-25,-112,	/* has siblings */	/* last move */
	95,	/* (2155) last move */
	1,
	3,
	4,
	-120,	/* (2159) has sibling */
	-126,	/* (2160) has sibling */
	0,
	7,
	-127,	/* (2163) has sibling */
	5,
	6,
	12,
	9,
	24,
	17,
	19,
	78,	/* (2171) last move */
	-58,18,	/* last move */
	-64,2,
	2,
	88,	/* (2177) last move */
	2,
	8,
	109,	/* (2180) last move */
	62,
	-112,	/* (2182) has sibling */
	5,
	20,
	0,
	6,
	15,
	63,59,
	62,
	-24,23,	/* last move */
	62,
	-107,	/* (2194) has sibling */
	-117,	/* (2195) has sibling */
	10,
	5,
	26,
	2,
	16,
	9,
	18,
	64,	/* (2203) last move */
	5,
	45,
	24,
	-55,-125,	/* has siblings */
	19,
	28,
	64,	/* (2211) last move */
	8,
	9,
	14,
	20,
	67,	/* (2216) last move */
	62,
	-117,	/* (2218) has sibling */
	5,
	10,
	17,
	-108,	/* (2222) has sibling */
	0,
	28,
	-119,	/* (2225) has sibling */
	27,
	14,
	82,	/* (2228) last move */
	-37,2,
	6,
	76,	/* (2232) last move */
	19,
	-114,	/* (2234) has sibling */
	-52,-126,	/* has siblings */
	6,
	-37,2,
	28,
	-30,2,
	32,
	-16,2,
	115,	/* (2246) last move */
	25,
	18,
	0,
	12,
	30,
	36,
	24,
	37,
	35,
	-84,	/* (2256) has sibling */
	1,
	7,
	57,
	111,	/* (2260) last move */
	51,
	1,
	71,	/* (2263) last move */
	12,
	-17,-126,	/* has siblings */
	96,	/* (2267) last move */
	24,
	-108,	/* (2269) has sibling */
	18,
	0,
	-19,-126,	/* has siblings */
	25,
	14,
	87,	/* (2276) last move */
	-1,-77,-126,	/* has siblings */
	25,
	14,
	87,	/* (2282) last move */
	-7,-126,	/* has siblings */
	25,
	14,
	87,	/* (2287) last move */
	3,
	23,
	-57,-126,	/* has siblings */
	14,
	127,59,	/* (2293) last move */
	25,
	6,
	14,
	7,
	15,
	8,
	63,21,
	86,	/* (2303) last move */
	23,
	-64,-125,	/* has siblings */
	14,
	-55,-126,	/* has siblings */
	25,
	20,
	1,
	6,
	79,	/* (2314) last move */
	25,
	30,
	9,
	18,
	20,
	-58,-126,	/* has siblings */
	35,
	63,22,
	79,	/* (2325) last move */
	35,
	6,
	36,
	37,
	91,	/* (2330) last move */
	-113,	/* (2331) has sibling */
	14,
	25,
	30,
	63,21,
	18,
	22,
	-91,	/* (2339) has sibling */
	121,	/* (2340) last move */
	-17,23,	/* last move */
	25,
	15,
	63,59,
	121,	/* (2347) last move */
	62,
	-54,-125,	/* has siblings */
	-58,-125,	/* has siblings */
	-60,1,
	-59,1,
	0,
	-126,	/* (2358) has sibling */
	12,
	-119,	/* (2360) has sibling */
	-63,1,
	11,
	19,
	33,
	30,
	37,
	42,
	57,
	63,120,
	115,	/* (2372) last move */
	19,
	9,
	17,
	-100,	/* (2376) has sibling */
	6,
	1,
	12,
	20,
	103,	/* (2381) last move */
	-37,19,	/* last move */
	12,
	2,
	11,
	13,
	16,
	18,
	19,
	71,	/* (2391) last move */
	-123,	/* (2392) has sibling */
	62,
	-122,	/* (2394) has sibling */
	9,
	4,
	19,
	14,
	18,
	25,
	20,
	87,	/* (2402) last move */
	12,
	11,
	16,
	21,
	31,
	33,
	49,
	109,	/* (2410) last move */
	-117,	/* (2411) has sibling */
	5,
	2,
	-18,-126,	/* has siblings */
	16,
	29,
	95,	/* (2418) last move */
	-48,1,
	-31,-112,	/* has siblings */	/* last move */
	46,
	21,
	-49,17,	/* last move */
	-126,	/* (2427) has sibling */
	76,	/* (2428) last move */
	62,
	70,	/* (2430) last move */
	62,
	-1,59,-121,	/* has siblings */
	-44,-125,	/* has siblings */
	12,
	74,	/* (2438) last move */
	69,	/* (2439) last move */
	62,
	-28,-87,9,-125,	/* reconverge */	/* has siblings */
	62,
	-18,-87,9,-125,	/* reconverge */	/* has siblings */
	62,
	-123,	/* (2451) has sibling */
	2,
	-112,	/* (2453) has sibling */
	-60,-126,	/* has siblings */
	6,
	10,
	19,
	12,
	81,	/* (2460) last move */
	10,
	19,
	11,
	29,
	0,
	23,
	97,	/* (2467) last move */
	10,
	-60,-126,	/* has siblings */
	6,
	-117,	/* (2472) has sibling */
	16,
	21,
	95,	/* (2475) last move */
	12,
	75,	/* (2477) last move */
	21,
	62,
	-53,-94,9,110,	/* reconverge */	/* has siblings */
	-82,	/* (2484) has sibling */
	16,
	29,
	42,
	40,
	58,
	111,	/* (2490) last move */
	-117,	/* (2491) has sibling */
	26,
	41,
	54,
	16,
	33,
	13,
	63,113,
	1,
	71,	/* (2501) last move */
	16,
	33,
	31,
	49,
	63,-107,
	63,-91,
	11,
	26,
	13,
	41,
	1,
	-121,	/* (2515) has sibling */
	57,
	39,
	127,-106,	/* (2518) last move */
	63,-106,
	42,
	7,
	63,-90,
	63,-75,
	63,-74,
	127,-58,	/* (2530) last move */
	-123,	/* (2532) has sibling */
	-55,-126,	/* has siblings */
	17,
	28,
	27,
	36,
	98,	/* (2539) last move */
	62,
	-92,	/* (2541) has sibling */
	-65,59,	/* (2542) has sibling */
	9,
	18,
	37,
	1,
	3,
	86,	/* (2549) last move */
	-120,	/* (2550) has sibling */
	9,
	14,
	20,
	35,
	50,
	24,
	37,
	3,
	127,90,	/* (2559) last move */
	-27,-112,	/* has siblings */	/* last move */
	18,
	0,
	28,
	52,
	111,	/* (2567) last move */
	62,
	-104,	/* (2569) has sibling */
	-120,	/* (2570) has sibling */
	9,
	14,
	20,
	67,	/* (2574) last move */
	-52,-126,	/* has siblings */
	19,
	17,
	32,
	27,
	93,	/* (2581) last move */
	9,
	-65,59,	/* (2583) has sibling */
	8,
	39,
	67,	/* (2587) last move */
	0,
	61,
	32,
	27,
	63,120,
	122,	/* (2594) last move */
	-82,	/* (2595) has sibling */
	-104,	/* (2596) has sibling */
	12,
	19,
	17,
	-32,-126,	/* has siblings */
	10,
	16,
	11,
	21,
	77,	/* (2606) last move */
	0,
	-54,-126,	/* has siblings */
	16,
	11,
	21,
	13,
	71,	/* (2614) last move */
	15,
	8,
	32,
	109,	/* (2618) last move */
	9,
	14,
	-44,-126,	/* has siblings */
	18,
	37,
	88,	/* (2625) last move */
	37,
	-58,-126,	/* has siblings */
	12,
	20,
	28,
	17,
	27,
	24,
	9,
	82,	/* (2636) last move */
	18,
	17,
	24,
	109,	/* (2640) last move */
	-104,	/* (2641) has sibling */
	-83,	/* (2642) has sibling */
	-56,-126,	/* has siblings */
	9,
	14,
	96,	/* (2647) last move */
	17,
	0,
	23,
	80,	/* (2651) last move */
	62,
	-120,	/* (2653) has sibling */
	103,	/* (2654) last move */
	0,
	6,
	80,	/* (2657) last move */
	62,
	-119,	/* (2659) has sibling */
	-50,-126,	/* has siblings */
	20,
	-40,-126,	/* has siblings */
	-91,	/* (2665) has sibling */
	36,
	18,
	30,
	35,
	25,
	46,
	50,
	108,	/* (2673) last move */
	18,
	30,
	25,
	-29,-126,	/* has siblings */
	23,
	0,
	15,
	1,
	8,
	22,
	3,
	7,
	63,49,
	60,
	13,
	56,
	3,
	53,
	4,
	63,49,
	6,
	3,
	63,81,
	55,
	123,	/* (2702) last move */
	23,
	35,
	63,22,
	36,
	8,
	101,	/* (2709) last move */
	18,
	37,
	100,	/* (2712) last move */
	18,
	-92,	/* (2714) has sibling */
	-65,59,	/* (2715) has sibling */
	37,
	1,
	3,
	86,	/* (2720) last move */
	20,
	127,60,	/* (2722) last move */
	37,
	-127,	/* (2725) has sibling */
	3,
	15,
	8,
	0,
	-42,-126,	/* has siblings */
	76,	/* (2732) last move */
	20,
	22,
	56,
	117,	/* (2736) last move */
	-44,-126,	/* has siblings */
	28,
	32,
	24,
	-114,	/* (2742) has sibling */
	17,
	35,
	50,
	63,23,
	51,
	72,	/* (2749) last move */
	-34,-126,	/* has siblings */
	52,
	48,
	35,
	-127,	/* (2755) has sibling */
	3,
	0,
	27,
	22,
	7,
	56,
	63,105,
	63,120,
	63,121,
	63,-120,
	122,	/* (2770) last move */
	17,
	34,
	27,
	47,
	6,
	40,
	-118,	/* (2777) has sibling */
	11,
	16,
	21,
	31,
	19,
	12,
	29,
	-95,	/* (2785) has sibling */
	65,	/* (2786) last move */
	8,
	49,
	33,
	39,
	42,
	118,	/* (2792) last move */
	19,
	80,	/* (2794) last move */
	-47,2,
	34,
	6,
	27,
	12,
	30,
	-45,-126,	/* has siblings */
	14,
	10,
	11,
	21,
	41,
	-38,-126,	/* has siblings */
	2,
	54,
	80,	/* (2813) last move */
	33,
	54,
	39,
	63,-110,
	109,	/* (2819) last move */
	14,
	83,	/* (2821) last move */
	35,
	20,
	110,	/* (2824) last move */
	-108,	/* (2825) has sibling */
	-92,	/* (2826) has sibling */
	-120,	/* (2827) has sibling */
	103,	/* (2828) last move */
	63,76,
	52,
	98,	/* (2832) last move */
	45,
	62,
	-128,	/* (2835) has sibling */
	6,
	79,	/* (2837) last move */
	63,59,
	64,	/* (2840) last move */
	36,
	18,
	-98,	/* (2843) has sibling */
	-39,-126,	/* has siblings */
	14,
	35,
	20,
	24,
	37,
	23,
	50,
	63,23,
	52,
	64,	/* (2856) last move */
	-93,	/* (2857) has sibling */
	24,
	25,
	20,
	14,
	9,
	-14,-126,	/* has siblings */
	23,
	63,23,
	76,	/* (2868) last move */
	6,
	23,
	63,23,
	63,21,
	63,22,
	50,
	0,
	12,
	4,
	83,	/* (2881) last move */
	45,
	84,	/* (2883) last move */
	12,
	19,
	17,
	16,
	37,
	0,
	87,	/* (2890) last move */
	-43,-126,	/* has siblings */
	-82,	/* (2893) has sibling */
	24,
	62,
	-128,	/* (2896) has sibling */
	6,
	10,
	28,
	32,
	48,
	52,
	111,	/* (2903) last move */
	-45,-112,	/* has siblings */	/* last move */
	10,
	-58,-126,	/* has siblings */
	9,
	12,
	0,
	14,
	17,
	2,
	18,
	25,
	94,	/* (2917) last move */
	11,
	1,
	3,
	4,
	-120,	/* (2922) has sibling */
	2,
	0,
	7,
	1,
	26,
	6,
	5,
	9,
	103,	/* (2931) last move */
	2,
	8,
	96,	/* (2934) last move */
	-92,	/* (2935) has sibling */
	-64,-110,	/* has siblings */	/* last move */
	18,
	-118,	/* (2939) has sibling */
	-122,	/* (2940) has sibling */
	-55,-126,	/* has siblings */
	12,
	14,
	0,
	-53,-126,	/* has siblings */
	17,
	20,
	26,
	13,
	54,
	2,
	93,	/* (2954) last move */
	20,
	75,	/* (2956) last move */
	11,
	64,	/* (2958) last move */
	11,
	30,
	-29,-126,	/* has siblings */
	24,
	25,
	20,
	14,
	9,
	6,
	4,
	12,
	69,	/* (2971) last move */
	5,
	84,	/* (2973) last move */
	30,
	35,
	24,
	25,
	20,
	14,
	9,
	6,
	23,
	63,23,
	63,21,
	63,22,
	50,
	0,
	12,
	68,	/* (2992) last move */
	10,
	11,
	1,
	3,
	4,
	-56,-126,	/* has siblings */
	2,
	0,
	7,
	1,
	26,
	6,
	5,
	78,	/* (3007) last move */
	2,
	72,	/* (3009) last move */
	-27,-115,	/* has siblings */
	-112,	/* (3012) has sibling */
	-123,	/* (3013) has sibling */
	-55,-126,	/* has siblings */
	19,
	32,
	52,
	28,
	0,
	44,
	51,
	36,
	46,
	35,
	50,
	24,
	127,105,	/* (3028) last move */
	28,
	-35,-126,	/* has siblings */
	42,
	19,
	20,
	31,
	49,
	33,
	39,
	41,
	54,
	21,
	58,
	34,
	96,	/* (3045) last move */
	-96,	/* (3046) has sibling */
	34,
	0,
	112,	/* (3049) last move */
	-55,2,
	20,
	14,
	18,
	25,
	17,
	30,
	24,
	12,
	104,	/* (3060) last move */
	6,
	-55,-126,	/* has siblings */
	12,
	0,
	17,
	-124,	/* (3067) has sibling */
	5,
	2,
	13,
	11,
	1,
	8,
	22,
	15,
	26,
	41,
	33,
	21,
	53,
	35,
	50,
	36,
	24,
	63,42,
	30,
	63,25,
	14,
	-39,2,
	18,
	63,23,
	123,	/* (3096) last move */
	1,
	11,
	21,
	14,
	25,
	18,
	30,
	35,
	36,
	24,
	62,
	41,
	54,
	3,
	22,
	13,
	63,49,
	97,	/* (3115) last move */
	4,
	5,
	0,
	-115,	/* (3119) has sibling */
	-119,	/* (3120) has sibling */
	12,
	20,
	-100,	/* (3123) has sibling */
	-104,	/* (3124) has sibling */
	17,
	44,
	32,
	46,
	-63,7,
	8,
	22,
	79,	/* (3133) last move */
	17,
	27,
	-32,-110,	/* has siblings */	/* last move */
	34,
	32,
	24,
	36,
	30,
	115,	/* (3143) last move */
	32,
	-101,	/* (3145) has sibling */
	17,
	28,
	34,
	24,
	48,
	40,
	1,
	94,	/* (3153) last move */
	34,
	28,
	17,
	27,
	-17,-126,	/* has siblings */
	24,
	-98,	/* (3161) has sibling */
	29,
	35,
	50,
	22,
	95,	/* (3166) last move */
	19,
	78,	/* (3168) last move */
	24,
	36,
	30,
	115,	/* (3172) last move */
	20,
	32,
	-104,	/* (3175) has sibling */
	36,
	18,
	115,	/* (3178) last move */
	-52,2,
	19,
	17,
	74,	/* (3183) last move */
	-52,-126,	/* has siblings */
	-127,	/* (3186) has sibling */
	13,
	67,	/* (3188) last move */
	2,
	-127,	/* (3190) has sibling */
	8,
	22,
	15,
	13,
	11,
	26,
	41,
	33,
	21,
	-75,	/* (3200) has sibling */
	20,
	28,
	18,
	24,
	35,
	50,
	25,
	63,24,
	30,
	32,
	46,
	36,
	27,
	81,	/* (3215) last move */
	-44,18,	/* last move */
	11,
	13,
	21,
	7,
	95,	/* (3222) last move */
	1,
	-120,	/* (3224) has sibling */
	-42,-126,	/* has siblings */
	2,
	12,
	15,
	13,
	11,
	26,
	41,
	33,
	21,
	53,
	20,
	28,
	18,
	24,
	35,
	50,
	25,
	63,24,
	30,
	32,
	46,
	36,
	27,
	81,	/* (3251) last move */
	11,
	22,
	9,
	25,
	118,	/* (3256) last move */
	-52,2,
	10,
	9,
	19,
	-100,	/* (3262) has sibling */
	85,	/* (3263) last move */
	27,
	93,	/* (3265) last move */
	-107,	/* (3266) has sibling */
	-123,	/* (3267) has sibling */
	45,
	-119,	/* (3269) has sibling */
	72,	/* (3270) last move */
	64,	/* (3271) last move */
	6,
	-119,	/* (3273) has sibling */
	12,
	0,
	1,
	17,
	-112,	/* (3278) has sibling */
	-32,-125,	/* has siblings */
	52,
	48,
	44,
	33,
	27,
	20,
	63,122,
	127,-120,	/* (3289) last move */
	-33,-126,	/* has siblings */
	93,	/* (3293) last move */
	34,
	95,	/* (3295) last move */
	-37,2,
	34,
	-88,	/* (3299) has sibling */
	32,
	47,
	63,120,
	63,-120,
	127,121,	/* (3306) last move */
	-36,2,
	-44,-110,	/* has siblings */	/* last move */
	19,
	-88,	/* (3313) has sibling */
	4,
	5,
	2,
	10,
	16,
	29,
	11,
	19,
	14,
	24,
	72,	/* (3324) last move */
	14,
	40,
	20,
	27,
	4,
	18,
	25,
	30,
	15,
	100,	/* (3334) last move */
	-124,	/* (3335) has sibling */
	5,
	0,
	13,
	-110,	/* (3339) has sibling */
	-111,	/* (3340) has sibling */
	24,
	44,
	36,
	51,
	63,-76,
	110,	/* (3347) last move */
	-40,2,
	12,
	-119,	/* (3351) has sibling */
	20,
	17,
	92,	/* (3354) last move */
	10,
	73,	/* (3356) last move */
	-108,	/* (3357) has sibling */
	-96,	/* (3358) has sibling */
	12,
	-45,-125,	/* has siblings */
	17,
	27,
	10,
	2,
	31,
	18,
	14,
	1,
	99,	/* (3370) last move */
	10,
	98,	/* (3372) last move */
	9,
	-110,	/* (3374) has sibling */
	28,
	-93,	/* (3376) has sibling */
	-7,-126,	/* has siblings */
	32,
	34,
	116,	/* (3381) last move */
	32,
	127,-76,	/* (3383) last move */
	24,
	-96,	/* (3386) has sibling */
	36,
	121,	/* (3388) last move */
	44,
	36,
	51,
	127,-76,	/* (3392) last move */
	14,
	28,
	18,
	32,
	114,	/* (3398) last move */
	-55,-126,	/* has siblings */
	12,
	17,
	27,
	28,
	1,
	18,
	93,	/* (3407) last move */
	14,
	-119,	/* (3409) has sibling */
	20,
	28,
	18,
	-40,-125,	/* has siblings */
	32,
	17,
	35,
	50,
	63,23,
	46,
	3,
	57,
	106,	/* (3424) last move */
	32,
	-65,-76,	/* (3426) has sibling */
	24,
	22,
	3,
	56,
	30,
	-39,-126,	/* has siblings */
	72,	/* (3435) last move */
	72,	/* (3436) last move */
	50,
	-7,23,	/* last move */
	-63,-126,	/* has siblings */
	12,
	10,
	81,	/* (3444) last move */
	28,
	3,
	-35,23,	/* last move */
	-19,2,
	64,	/* (3451) last move */
	-117,	/* (3452) has sibling */
	-123,	/* (3453) has sibling */
	10,
	-52,-126,	/* has siblings */
	19,
	17,
	-125,	/* (3459) has sibling */
	7,
	1,
	18,
	25,
	57,
	111,	/* (3465) last move */
	45,
	18,
	-57,23,	/* last move */
	19,
	29,
	12,
	31,
	-110,	/* (3474) has sibling */
	-61,7,
	7,
	8,
	-39,-125,	/* has siblings */
	23,
	0,
	15,
	53,
	14,
	30,
	6,
	4,
	17,
	27,
	92,	/* (3491) last move */
	1,
	-42,-126,	/* has siblings */
	89,	/* (3495) last move */
	53,
	-8,-126,	/* has siblings */
	127,49,	/* (3499) last move */
	63,49,
	120,	/* (3503) last move */
	15,
	-29,23,	/* last move */
	6,
	-119,	/* (3508) has sibling */
	12,
	0,
	2,
	-47,-126,	/* has siblings */
	27,
	28,
	65,	/* (3516) last move */
	1,
	-83,	/* (3518) has sibling */
	17,
	29,
	27,
	80,	/* (3522) last move */
	17,
	57,
	-50,7,
	25,
	18,
	30,
	35,
	36,
	88,	/* (3532) last move */
	0,
	4,
	1,
	-125,	/* (3536) has sibling */
	22,
	7,
	35,
	109,	/* (3540) last move */
	-114,	/* (3541) has sibling */
	25,
	30,
	18,
	9,
	35,
	20,
	63,23,
	109,	/* (3550) last move */
	9,
	-71,	/* (3552) has sibling */
	14,
	7,
	13,
	3,
	66,	/* (3557) last move */
	-50,-112,	/* has siblings */	/* last move */
	30,
	109,	/* (3561) last move */
	-44,-126,	/* has siblings */
	-100,	/* (3564) has sibling */
	9,
	32,
	-123,	/* (3567) has sibling */
	2,
	-54,-126,	/* has siblings */
	85,	/* (3571) last move */
	16,
	10,
	19,
	11,
	21,
	29,
	27,
	42,
	39,
	-79,	/* (3581) has sibling */
	54,
	41,
	33,
	47,
	-56,-126,	/* has siblings */
	0,
	15,
	6,
	17,
	14,
	23,
	18,
	24,
	35,
	36,
	94,	/* (3598) last move */
	24,
	36,
	8,
	22,
	15,
	53,
	30,
	35,
	18,
	26,
	57,
	127,-91,	/* (3610) last move */
	0,
	111,	/* (3613) last move */
	-45,2,
	85,	/* (3616) last move */
	-110,	/* (3617) has sibling */
	-100,	/* (3618) has sibling */
	14,
	6,
	-119,	/* (3621) has sibling */
	17,
	12,
	5,
	19,
	4,
	36,
	47,
	42,
	2,
	63,105,
	40,
	29,
	63,-105,
	127,-91,	/* (3637) last move */
	-117,	/* (3639) has sibling */
	5,
	62,
	-61,-112,	/* has siblings */	/* last move */
	62,
	-95,	/* (3645) has sibling */
	2,
	42,
	72,	/* (3648) last move */
	62,
	-112,	/* (3650) has sibling */
	-95,	/* (3651) has sibling */
	2,
	13,
	7,
	26,
	1,
	127,-77,	/* (3657) last move */
	21,
	2,
	13,
	33,
	54,
	95,	/* (3664) last move */
	62,
	-38,-126,	/* has siblings */
	41,
	7,
	63,97,
	65,	/* (3672) last move */
	-102,	/* (3673) has sibling */
	1,
	109,	/* (3675) last move */
	62,
	-63,-126,	/* has siblings */
	0,
	4,
	23,
	71,	/* (3682) last move */
	62,
	-120,	/* (3684) has sibling */
	15,
	23,
	22,
	88,	/* (3688) last move */
	62,
	-118,	/* (3690) has sibling */
	72,	/* (3691) last move */
	63,59,
	2,
	13,
	7,
	3,
	26,
	60,
	21,
	8,
	74,	/* (3702) last move */
	10,
	19,
	16,
	51,
	32,
	48,
	52,
	1,
	3,
	7,
	-51,-126,	/* has siblings */
	2,
	60,
	4,
	7,
	5,
	11,
	127,76,	/* (3721) last move */
	8,
	-62,-126,	/* has siblings */
	0,
	4,
	1,
	5,
	87,	/* (3730) last move */
	-64,-126,	/* has siblings */
	13,
	22,
	60,
	56,
	53,
	15,
	55,
	59,
	89,	/* (3741) last move */
	23,
	2,
	24,
	36,
	35,
	46,
	30,
	125,	/* (3749) last move */
	9,
	-92,	/* (3751) has sibling */
	-126,	/* (3752) has sibling */
	-60,-126,	/* has siblings */
	5,
	-115,	/* (3756) has sibling */
	26,
	12,
	6,
	7,
	80,	/* (3761) last move */
	6,
	12,
	14,
	7,
	15,
	8,
	0,
	1,
	23,
	109,	/* (3771) last move */
	13,
	4,
	11,
	5,
	1,
	-102,	/* (3777) has sibling */
	41,
	7,
	63,97,
	3,
	8,
	86,	/* (3784) last move */
	7,
	3,
	26,
	60,
	85,	/* (3789) last move */
	-43,-126,	/* has siblings */
	10,
	16,
	19,
	11,
	2,
	98,	/* (3797) last move */
	-117,	/* (3798) has sibling */
	10,
	-62,-126,	/* has siblings */
	5,
	4,
	65,	/* (3804) last move */
	16,
	5,
	-94,	/* (3807) has sibling */
	21,
	33,
	41,
	-97,	/* (3811) has sibling */
	90,	/* (3812) last move */
	54,
	26,
	93,	/* (3815) last move */
	29,
	21,
	33,
	41,
	54,
	26,
	98,	/* (3822) last move */
	-112,	/* (3823) has sibling */
	-123,	/* (3824) has sibling */
	27,
	0,
	23,
	15,
	48,
	105,	/* (3830) last move */
	0,
	15,
	8,
	4,
	1,
	5,
	26,
	33,
	112,	/* (3839) last move */
	10,
	11,
	21,
	16,
	2,
	26,
	19,
	-95,	/* (3847) has sibling */
	13,
	41,
	68,	/* (3850) last move */
	13,
	31,
	8,
	29,
	87,	/* (3855) last move */
	-104,	/* (3856) has sibling */
	16,
	0,
	15,
	8,
	4,
	1,
	5,
	26,
	97,	/* (3865) last move */
	21,
	24,
	36,
	35,
	44,
	-14,-126,	/* has siblings */
	63,42,
	46,
	-65,59,	/* (3876) has sibling */
	30,
	115,	/* (3879) last move */
	63,43,
	30,
	125,	/* (3883) last move */
	94,	/* (3884) last move */
	32,
	28,
	48,
	-30,-126,	/* has siblings */
	-43,-126,	/* has siblings */
	24,
	36,
	63,120,
	63,121,
	127,-119,	/* (3898) last move */
	63,120,
	11,
	0,
	23,
	-54,-125,	/* has siblings */
	5,
	2,
	16,
	19,
	21,
	12,
	6,
	4,
	29,
	40,
	27,
	15,
	47,
	58,
	63,-121,
	63,-106,
	57,
	127,-74,	/* (3926) last move */
	-113,	/* (3928) has sibling */
	5,
	26,
	-115,	/* (3931) has sibling */
	2,
	7,
	10,
	-60,-126,	/* has siblings */
	21,
	1,
	11,
	8,
	111,	/* (3941) last move */
	21,
	12,
	16,
	6,
	24,
	36,
	19,
	3,
	41,
	18,
	14,
	30,
	9,
	86,	/* (3955) last move */
	41,
	13,
	97,	/* (3958) last move */
	-59,2,
	90,	/* (3961) last move */
	11,
	0,
	5,
	23,
	25,
	14,
	6,
	30,
	15,
	63,22,
	1,
	-56,-121,	/* has siblings */
	4,
	-125,	/* (3977) has sibling */
	0,
	59,
	22,
	117,	/* (3981) last move */
	59,
	22,
	0,
	67,	/* (3985) last move */
	62,
	-40,-126,	/* has siblings */
	36,
	35,
	18,
	50,
	46,
	63,42,
	127,43,	/* (3996) last move */
	-40,18,	/* last move */
	-100,	/* (4000) has sibling */
	-44,-126,	/* has siblings */
	9,
	17,
	12,
	27,
	19,
	40,
	11,
	112,	/* (4010) last move */
	-55,-126,	/* has siblings */
	5,
	2,
	10,
	21,
	84,	/* (4017) last move */
	-96,	/* (4018) has sibling */
	34,
	48,
	-53,-125,	/* has siblings */
	5,
	10,
	12,
	20,
	19,
	47,
	95,	/* (4029) last move */
	47,
	63,-119,
	11,
	0,
	5,
	23,
	25,
	14,
	6,
	30,
	15,
	63,22,
	65,	/* (4044) last move */
	-107,	/* (4045) has sibling */
	-118,	/* (4046) has sibling */
	11,
	1,
	3,
	4,
	2,
	51,
	-18,-105,	/* has siblings */	/* last move */
	-32,2,
	112,	/* (4057) last move */
	115,	/* (4058) last move */
	-118,	/* (4059) has sibling */
	-127,	/* (4060) has sibling */
	-60,-125,	/* has siblings */
	0,
	20,
	-58,-126,	/* has siblings */
	12,
	17,
	73,	/* (4069) last move */
	9,
	81,	/* (4071) last move */
	3,
	4,
	-56,-126,	/* has siblings */
	66,	/* (4076) last move */
	2,
	32,
	115,	/* (4079) last move */
	-32,-126,	/* has siblings */
	9,
	20,
	14,
	18,
	89,	/* (4086) last move */
	-13,-125,	/* has siblings */
	-119,	/* (4089) has sibling */
	20,
	14,
	18,
	89,	/* (4093) last move */
	-128,	/* (4094) has sibling */
	6,
	31,
	96,	/* (4097) last move */
	32,
	48,
	116,	/* (4100) last move */
	20,
	-40,-125,	/* has siblings */
	18,
	0,
	51,
	52,
	111,	/* (4108) last move */
	109,	/* (4109) last move */
	6,
	-50,-126,	/* has siblings */
	-116,	/* (4113) has sibling */
	0,
	1,
	96,	/* (4116) last move */
	0,
	12,
	-123,	/* (4119) has sibling */
	19,
	10,
	29,
	16,
	42,
	103,	/* (4125) last move */
	9,
	20,
	18,
	17,
	4,
	24,
	30,
	35,
	23,
	63,23,
	25,
	108,	/* (4138) last move */
	-60,-125,	/* has siblings */
	5,
	0,
	-127,	/* (4143) has sibling */
	18,
	12,
	20,
	85,	/* (4147) last move */
	-55,3,
	-63,-126,	/* has siblings */
	13,
	-106,	/* (4153) has sibling */
	83,	/* (4154) last move */
	7,
	2,
	30,
	84,	/* (4158) last move */
	2,
	-53,-126,	/* has siblings */
	13,
	10,
	26,
	41,
	71,	/* (4166) last move */
	1,
	8,
	22,
	13,
	15,
	-61,-126,	/* has siblings */
	55,
	7,
	8,
	20,
	14,
	10,
	87,	/* (4180) last move */
	23,
	55,
	20,
	14,
	18,
	25,
	10,
	12,
	83,	/* (4189) last move */
	-55,-126,	/* has siblings */
	12,
	0,
	1,
	32,
	51,
	24,
	36,
	-44,-126,	/* has siblings */
	35,
	44,
	52,
	101,	/* (4204) last move */
	99,	/* (4205) last move */
	20,
	-50,-126,	/* has siblings */
	18,
	0,
	115,	/* (4211) last move */
	0,
	-52,-126,	/* has siblings */
	14,
	18,
	87,	/* (4217) last move */
	-40,-126,	/* has siblings */
	14,
	18,
	62,
	-41,7,
	25,
	9,
	-49,-126,	/* has siblings */
	12,
	63,21,
	75,	/* (4232) last move */
	12,
	15,
	8,
	94,	/* (4236) last move */
	-50,-126,	/* has siblings */
	12,
	88,	/* (4240) last move */
	23,
	-49,-126,	/* has siblings */
	12,
	-55,-126,	/* has siblings */
	17,
	14,
	18,
	25,
	-13,-112,	/* has siblings */	/* last move */
	75,	/* (4253) last move */
	5,
	9,
	4,
	93,	/* (4257) last move */
	18,
	-40,-126,	/* has siblings */
	-93,	/* (4261) has sibling */
	36,
	9,
	30,
	14,
	50,
	25,
	127,24,	/* (4268) last move */
	-55,-126,	/* has siblings */
	78,	/* (4272) last move */
	-50,2,
	9,
	35,
	36,
	25,
	63,22,
	30,
	12,
	4,
	50,
	15,
	116,	/* (4286) last move */
	-50,-126,	/* has siblings */
	24,
	9,
	76,	/* (4291) last move */
	9,
	-50,-126,	/* has siblings */
	12,
	4,
	24,
	35,
	36,
	25,
	63,22,
	30,
	50,
	15,
	116,	/* (4306) last move */
	12,
	24,
	-113,	/* (4309) has sibling */
	62,
	-98,	/* (4311) has sibling */
	35,
	44,
	-50,-126,	/* has siblings */
	50,
	25,
	36,
	63,23,
	-1,21,23,	/* last move */
	46,
	63,22,
	14,
	127,21,	/* (4328) last move */
	36,
	-18,7,
	51,
	125,	/* (4334) last move */
	36,
	35,
	44,
	94,	/* (4338) last move */
	-116,	/* (4339) has sibling */
	-107,	/* (4340) has sibling */
	-118,	/* (4341) has sibling */
	11,
	1,
	3,
	4,
	2,
	72,	/* (4347) last move */
	1,
	3,
	8,
	4,
	6,
	62,
	-115,	/* (4354) has sibling */
	7,
	11,
	10,
	41,
	33,
	54,
	-25,-126,	/* has siblings */
	5,
	2,
	26,
	63,-110,
	86,	/* (4368) last move */
	5,
	103,	/* (4370) last move */
	103,	/* (4371) last move */
	10,
	-45,-126,	/* has siblings */
	80,	/* (4375) last move */
	1,
	-60,-126,	/* has siblings */
	0,
	6,
	9,
	17,
	28,
	27,
	84,	/* (4385) last move */
	3,
	-60,-126,	/* has siblings */
	2,
	8,
	109,	/* (4391) last move */
	8,
	4,
	6,
	-30,-126,	/* has siblings */
	40,
	16,
	27,
	109,	/* (4400) last move */
	-48,-112,	/* has siblings */	/* last move */
	-19,-112,	/* has siblings */	/* last move */
	103,	/* (4405) last move */
	-63,-126,	/* has siblings */
	-60,-126,	/* has siblings */
	0,
	6,
	9,
	17,
	14,
	28,
	109,	/* (4416) last move */
	3,
	4,
	-62,-126,	/* has siblings */
	8,
	80,	/* (4422) last move */
	8,
	-123,	/* (4424) has sibling */
	0,
	15,
	1,
	11,
	14,
	6,
	23,
	28,
	82,	/* (4433) last move */
	2,
	0,
	7,
	1,
	-123,	/* (4438) has sibling */
	14,
	39,
	87,	/* (4441) last move */
	12,
	23,
	14,
	94,	/* (4445) last move */
	62,
	-122,	/* (4447) has sibling */
	-55,-126,	/* has siblings */
	12,
	0,
	1,
	17,
	27,
	-30,-126,	/* has siblings */
	28,
	-45,-126,	/* has siblings */
	40,
	4,
	5,
	2,
	10,
	11,
	16,
	8,
	22,
	15,
	21,
	13,
	60,
	41,
	63,97,
	26,
	118,	/* (4477) last move */
	14,
	19,
	24,
	48,
	36,
	111,	/* (4483) last move */
	28,
	83,	/* (4485) last move */
	-60,-125,	/* has siblings */
	-128,	/* (4488) has sibling */
	5,
	1,
	73,	/* (4491) last move */
	5,
	0,
	-119,	/* (4494) has sibling */
	-126,	/* (4495) has sibling */
	1,
	8,
	22,
	13,
	15,
	3,
	55,
	7,
	8,
	10,
	12,
	103,	/* (4507) last move */
	1,
	13,
	-125,	/* (4510) has sibling */
	78,	/* (4511) last move */
	86,	/* (4512) last move */
	1,
	-126,	/* (4514) has sibling */
	12,
	8,
	22,
	13,
	15,
	3,
	55,
	7,
	8,
	33,
	78,	/* (4525) last move */
	-110,	/* (4526) has sibling */
	17,
	24,
	44,
	36,
	51,
	63,59,
	109,	/* (4534) last move */
	-44,1,
	32,
	12,
	10,
	88,	/* (4540) last move */
	0,
	4,
	1,
	14,
	25,
	-110,	/* (4546) has sibling */
	30,
	35,
	36,
	88,	/* (4550) last move */
	30,
	18,
	9,
	35,
	-61,23,	/* last move */
	62,
	-128,	/* (4558) has sibling */
	-123,	/* (4559) has sibling */
	-124,	/* (4560) has sibling */
	12,
	-126,	/* (4562) has sibling */
	-122,	/* (4563) has sibling */
	80,	/* (4564) last move */
	16,
	70,	/* (4566) last move */
	6,
	2,
	9,
	103,	/* (4570) last move */
	-62,2,
	15,
	8,
	4,
	1,
	74,	/* (4577) last move */
	6,
	10,
	-100,	/* (4580) has sibling */
	32,
	47,
	-80,	/* (4583) has sibling */
	34,
	20,
	19,
	5,
	12,
	31,
	127,120,	/* (4590) last move */
	-33,2,
	112,	/* (4594) last move */
	17,
	-89,	/* (4596) has sibling */
	61,
	48,
	104,	/* (4599) last move */
	-30,-125,	/* has siblings */
	-99,	/* (4602) has sibling */
	16,
	-17,-126,	/* has siblings */
	63,120,
	63,-121,
	27,
	19,
	5,
	12,
	42,
	104,	/* (4615) last move */
	42,
	39,
	47,
	63,120,
	127,-121,	/* (4621) last move */
	-37,2,
	47,
	40,
	127,-121,	/* (4627) last move */
	31,
	-32,-125,	/* has siblings */
	52,
	44,
	24,
	63,90,
	63,105,
	48,
	51,
	36,
	46,
	50,
	35,
	63,42,
	63,43,
	63,59,
	61,
	63,91,
	63,76,
	127,121,	/* (4656) last move */
	34,
	125,	/* (4659) last move */
	-60,-125,	/* has siblings */
	-63,-125,	/* has siblings */
	-62,-125,	/* has siblings */
	-51,-126,	/* has siblings */
	-38,-112,	/* has siblings */	/* last move */
	11,
	26,
	85,	/* (4672) last move */
	5,
	13,
	-118,	/* (4675) has sibling */
	-121,	/* (4676) has sibling */
	-64,-126,	/* has siblings */
	6,
	23,
	25,
	15,
	18,
	11,
	3,
	22,
	56,
	55,
	26,
	41,
	53,
	63,17,
	21,
	54,
	117,	/* (4696) last move */
	3,
	0,
	-53,-126,	/* has siblings */
	86,	/* (4701) last move */
	86,	/* (4702) last move */
	-64,-125,	/* has siblings */
	22,
	-57,2,
	3,
	-107,	/* (4709) has sibling */
	15,
	23,
	59,
	-112,	/* (4713) has sibling */
	17,
	29,
	27,
	36,
	24,
	35,
	-84,	/* (4720) has sibling */
	50,
	46,
	63,42,
	63,59,
	18,
	20,
	14,
	127,-121,	/* (4730) last move */
	30,
	50,
	63,23,
	108,	/* (4736) last move */
	-55,-126,	/* has siblings */
	17,
	20,
	28,
	24,
	36,
	50,
	44,
	63,42,
	103,	/* (4748) last move */
	-44,-126,	/* has siblings */
	92,	/* (4751) last move */
	-47,-126,	/* has siblings */
	16,
	33,
	31,
	39,
	9,
	12,
	28,
	6,
	111,	/* (4762) last move */
	-52,2,
	16,
	33,
	31,
	39,
	9,
	6,
	91,	/* (4771) last move */
	15,
	75,	/* (4773) last move */
	21,
	16,
	33,
	9,
	-56,-126,	/* has siblings */
	0,
	-58,-126,	/* has siblings */
	15,
	23,
	22,
	12,
	-37,-112,	/* has siblings */	/* last move */
	95,	/* (4789) last move */
	15,
	6,
	3,
	23,
	7,
	78,	/* (4795) last move */
	-64,-126,	/* has siblings */
	8,
	-113,	/* (4799) has sibling */
	78,	/* (4800) last move */
	30,
	18,
	15,
	-39,-126,	/* has siblings */
	117,	/* (4806) last move */
	63,21,
	25,
	35,
	59,
	55,
	7,
	14,
	63,24,
	63,25,
	127,22,	/* (4819) last move */
	6,
	12,
	15,
	8,
	25,
	-98,	/* (4826) has sibling */
	18,
	35,
	20,
	28,
	24,
	36,
	14,
	32,
	17,
	27,
	50,
	63,42,
	7,
	63,21,
	59,
	55,
	3,
	0,
	23,
	9,
	63,22,
	63,25,
	86,	/* (4853) last move */
	-46,2,
	30,
	35,
	7,
	3,
	120,	/* (4860) last move */
	-116,	/* (4861) has sibling */
	-64,-125,	/* has siblings */
	8,
	-46,-126,	/* has siblings */
	11,
	26,
	21,
	41,
	54,
	33,
	31,
	39,
	127,-110,	/* (4875) last move */
	7,
	-114,	/* (4878) has sibling */
	-113,	/* (4879) has sibling */
	-1,21,-125,	/* has siblings */
	25,
	30,
	-55,-125,	/* has siblings */
	18,
	17,
	27,
	6,
	28,
	-109,	/* (4892) has sibling */
	10,
	29,
	16,
	31,
	21,
	42,
	34,
	41,
	33,
	39,
	54,
	63,-110,
	26,
	63,-127,
	63,113,
	63,-111,
	11,
	63,97,
	63,81,
	60,
	3,
	127,97,	/* (4920) last move */
	-42,2,
	59,
	3,
	119,	/* (4926) last move */
	23,
	63,23,
	18,
	9,
	35,
	24,
	86,	/* (4934) last move */
	25,
	22,
	6,
	67,	/* (4938) last move */
	25,
	-34,-125,	/* has siblings */
	-55,-125,	/* has siblings */
	18,
	17,
	27,
	6,
	29,
	28,
	48,
	-32,-126,	/* has siblings */
	52,
	44,
	34,
	24,
	23,
	15,
	63,22,
	20,
	59,
	37,
	115,	/* (4964) last move */
	15,
	96,	/* (4966) last move */
	-105,	/* (4967) has sibling */
	3,
	56,
	20,
	63,49,
	11,
	26,
	21,
	41,
	97,	/* (4977) last move */
	15,
	-1,21,32,19,21,	/* reconverge */
	18,
	15,
	30,
	86,	/* (4987) last move */
	25,
	14,
	18,
	15,
	9,
	86,	/* (4993) last move */
	8,
	0,
	15,
	6,
	3,
	-121,	/* (4999) has sibling */
	60,
	4,
	21,
	22,
	63,49,
	23,
	53,
	78,	/* (5008) last move */
	14,
	23,
	31,
	71,	/* (5012) last move */
	9,
	8,
	-49,-126,	/* has siblings */
	0,
	22,
	55,
	53,
	3,
	59,
	8,
	55,
	7,
	8,
	87,	/* (5027) last move */
	0,
	6,
	15,
	-41,-126,	/* has siblings */
	12,
	3,
	22,
	7,
	55,
	63,21,
	59,
	56,
	14,
	53,
	127,17,	/* (5044) last move */
	19,
	10,
	16,
	11,
	21,
	26,
	41,
	7,
	12,
	63,97,
	58,
	-39,23,	/* last move */
	-54,2,
	2,
	-53,-126,	/* has siblings */
	5,
	12,
	19,
	17,
	29,
	-94,	/* (5070) has sibling */
	97,	/* (5071) last move */
	31,
	98,	/* (5073) last move */
	27,
	85,	/* (5075) last move */
	2,
	1,
	3,
	8,
	-95,	/* (5080) has sibling */
	5,
	10,
	19,
	16,
	115,	/* (5085) last move */
	16,
	62,
	-110,	/* (5088) has sibling */
	86,	/* (5089) last move */
	62,
	22,
	-29,-112,	/* has siblings */	/* last move */
	73,	/* (5094) last move */
	-119,	/* (5095) has sibling */
	-52,-125,	/* has siblings */
	-47,-126,	/* has siblings */
	19,
	-124,	/* (5101) has sibling */
	2,
	1,
	3,
	8,
	97,	/* (5106) last move */
	27,
	29,
	-63,-126,	/* has siblings */
	3,
	4,
	8,
	-123,	/* (5114) has sibling */
	0,
	6,
	75,	/* (5117) last move */
	2,
	64,	/* (5119) last move */
	-124,	/* (5120) has sibling */
	2,
	1,
	3,
	72,	/* (5124) last move */
	-16,2,
	125,	/* (5127) last move */
	-63,-126,	/* has siblings */
	3,
	4,
	8,
	5,
	0,
	15,
	1,
	19,
	11,
	10,
	85,	/* (5140) last move */
	4,
	-123,	/* (5142) has sibling */
	-62,-126,	/* has siblings */
	11,
	13,
	1,
	26,
	17,
	0,
	8,
	-43,-126,	/* has siblings */
	25,
	-110,	/* (5155) has sibling */
	24,
	30,
	35,
	15,
	59,
	63,21,
	55,
	63,24,
	-1,25,-126,	/* has siblings */
	127,22,	/* (5169) last move */
	63,22,
	63,23,
	127,42,	/* (5175) last move */
	14,
	30,
	15,
	59,
	63,21,
	55,
	23,
	20,
	71,	/* (5186) last move */
	15,
	21,
	-57,-126,	/* has siblings */
	41,
	86,	/* (5192) last move */
	41,
	33,
	7,
	20,
	62,
	-39,7,
	86,	/* (5200) last move */
	1,
	-61,-125,	/* has siblings */
	7,
	2,
	-51,-126,	/* has siblings */
	8,
	0,
	56,
	11,
	117,	/* (5212) last move */
	22,
	6,
	0,
	13,
	17,
	27,
	34,
	40,
	32,
	47,
	116,	/* (5223) last move */
	13,
	-111,	/* (5225) has sibling */
	-101,	/* (5226) has sibling */
	34,
	-36,-126,	/* has siblings */
	32,
	20,
	67,	/* (5232) last move */
	40,
	32,
	3,
	108,	/* (5236) last move */
	3,
	40,
	62,
	-53,-126,	/* has siblings */
	6,
	0,
	2,
	82,	/* (5245) last move */
	125,	/* (5246) last move */
	3,
	81,	/* (5248) last move */
	2,
	-59,-126,	/* has siblings */
	10,
	1,
	21,
	26,
	11,
	67,	/* (5257) last move */
	1,
	-123,	/* (5259) has sibling */
	3,
	81,	/* (5261) last move */
	3,
	5,
	-54,-125,	/* has siblings */
	11,
	-102,	/* (5267) has sibling */
	21,
	16,
	33,
	31,
	39,
	-79,	/* (5273) has sibling */
	22,
	8,
	0,
	13,
	-1,49,-126,	/* has siblings */
	45,
	63,-94,
	127,-78,	/* (5284) last move */
	45,
	56,
	57,
	34,
	99,	/* (5290) last move */
	8,
	0,
	22,
	-36,-126,	/* has siblings */
	49,
	45,
	63,-75,
	63,-61,
	47,
	32,
	115,	/* (5304) last move */
	49,
	34,
	35,
	50,
	36,
	24,
	63,42,
	30,
	63,25,
	18,
	44,
	32,
	127,106,	/* (5319) last move */
	8,
	0,
	13,
	26,
	63,81,
	33,
	-36,-120,	/* has siblings */
	-101,	/* (5330) has sibling */
	17,
	19,
	16,
	106,	/* (5334) last move */
	-45,-126,	/* has siblings */
	16,
	17,
	31,
	39,
	49,
	45,
	27,
	-35,2,
	40,
	-22,2,
	127,-106,	/* (5349) last move */
	-47,2,
	91,	/* (5353) last move */
	34,
	-101,	/* (5355) has sibling */
	-97,	/* (5356) has sibling */
	-100,	/* (5357) has sibling */
	-32,-126,	/* has siblings */
	80,	/* (5360) last move */
	16,
	32,
	39,
	108,	/* (5364) last move */
	-48,-126,	/* has siblings */
	19,
	29,
	81,	/* (5369) last move */
	-45,2,
	16,
	111,	/* (5373) last move */
	-112,	/* (5374) has sibling */
	-100,	/* (5375) has sibling */
	-97,	/* (5376) has sibling */
	-96,	/* (5377) has sibling */
	39,
	108,	/* (5379) last move */
	-25,2,
	32,
	49,
	104,	/* (5384) last move */
	32,
	31,
	40,
	47,
	48,
	29,
	19,
	58,
	17,
	20,
	40,
	22,
	42,
	56,
	63,-107,
	62,
	-107,	/* (5402) has sibling */
	41,
	63,-110,
	54,
	39,
	63,97,
	49,
	124,	/* (5411) last move */
	45,
	57,
	-1,-77,-126,	/* has siblings */
	63,-110,
	54,
	39,
	49,
	63,-127,
	63,-94,
	105,	/* (5426) last move */
	127,-110,	/* (5427) last move */
	17,
	31,
	39,
	49,
	45,
	47,
	24,
	36,
	35,
	108,	/* (5438) last move */
	40,
	29,
	19,
	17,
	16,
	42,
	31,
	49,
	63,-107,
	58,
	39,
	57,
	21,
	111,	/* (5453) last move */
	-45,-126,	/* has siblings */
	16,
	-101,	/* (5457) has sibling */
	31,
	39,
	111,	/* (5460) last move */
	17,
	31,
	39,
	27,
	29,
	122,	/* (5466) last move */
	-29,18,	/* last move */
	11,
	10,
	16,
	-120,	/* (5472) has sibling */
	62,
	-106,	/* (5474) has sibling */
	29,
	19,
	40,
	-43,2,
	33,
	41,
	26,
	54,
	39,
	-57,-126,	/* has siblings */
	127,97,	/* (5487) last move */
	-65,97,	/* (5489) has sibling */
	63,81,
	7,
	63,113,
	63,-127,
	63,-110,
	127,97,	/* (5500) last move */
	-1,-110,2,
	63,-94,
	7,
	127,113,	/* (5508) last move */
	29,
	19,
	104,	/* (5512) last move */
	29,
	31,
	72,	/* (5515) last move */
	-112,	/* (5516) has sibling */
	-82,	/* (5517) has sibling */
	52,
	63,76,
	8,
	27,
	12,
	17,
	63,121,
	58,
	113,	/* (5528) last move */
	-89,	/* (5529) has sibling */
	-120,	/* (5530) has sibling */
	27,
	12,
	17,
	42,
	48,
	125,	/* (5536) last move */
	0,
	15,
	8,
	4,
	1,
	5,
	11,
	91,	/* (5544) last move */
	-63,-126,	/* has siblings */
	3,
	4,
	2,
	8,
	127,-77,	/* (5551) last move */
	8,
	-30,-120,	/* has siblings */
	39,
	63,-60,
	63,-107,
	76,	/* (5561) last move */
	62,
	-53,-121,	/* has siblings */
	-107,	/* (5565) has sibling */
	2,
	19,
	68,	/* (5568) last move */
	10,
	2,
	5,
	7,
	4,
	1,
	85,	/* (5575) last move */
	127,-77,	/* (5576) last move */
	-128,	/* (5578) has sibling */
	-123,	/* (5579) has sibling */
	-124,	/* (5580) has sibling */
	2,
	6,
	12,
	14,
	18,
	23,
	-44,-126,	/* has siblings */
	17,
	28,
	27,
	16,
	30,
	24,
	34,
	32,
	112,	/* (5597) last move */
	17,
	30,
	121,	/* (5600) last move */
	-62,2,
	4,
	1,
	10,
	85,	/* (5606) last move */
	15,
	8,
	4,
	1,
	5,
	11,
	62,
	-54,-126,	/* has siblings */
	19,
	29,
	27,
	40,
	98,	/* (5620) last move */
	62,
	-43,-121,	/* has siblings */
	62,
	-30,-112,	/* has siblings */	/* last move */
	91,	/* (5627) last move */
	-99,	/* (5628) has sibling */
	85,	/* (5629) last move */
	-54,-126,	/* has siblings */
	21,
	16,
	31,
	42,
	63,-107,
	58,
	113,	/* (5639) last move */
	-48,7,
	62,
	-118,	/* (5643) has sibling */
	90,	/* (5644) last move */
	-118,	/* (5645) has sibling */
	19,
	-35,-126,	/* has siblings */
	2,
	-51,2,
	26,
	21,
	71,	/* (5654) last move */
	2,
	40,
	42,
	29,
	33,
	31,
	103,	/* (5661) last move */
	-43,2,
	31,
	-31,2,
	113,	/* (5667) last move */
	10,
	-82,	/* (5669) has sibling */
	52,
	62,
	-37,-112,	/* has siblings */	/* last move */
	63,76,
	64,	/* (5676) last move */
	62,
	-128,	/* (5678) has sibling */
	27,
	95,	/* (5680) last move */
	-127,	/* (5681) has sibling */
	3,
	4,
	-56,-126,	/* has siblings */
	2,
	0,
	7,
	1,
	69,	/* (5690) last move */
	2,
	8,
	109,	/* (5693) last move */
	8,
	-19,1,
	62,
	-46,-121,	/* has siblings */
	62,
	-41,-126,	/* has siblings */
	25,
	14,
	-124,	/* (5705) has sibling */
	15,
	0,
	30,
	2,
	11,
	77,	/* (5711) last move */
	-49,2,
	6,
	0,
	68,	/* (5716) last move */
	62,
	-124,	/* (5718) has sibling */
	-61,-126,	/* has siblings */
	-50,-126,	/* has siblings */
	23,
	70,	/* (5724) last move */
	23,
	-49,-126,	/* has siblings */
	89,	/* (5728) last move */
	25,
	14,
	15,
	6,
	127,21,	/* (5733) last move */
	23,
	67,	/* (5736) last move */
	3,
	7,
	25,
	30,
	76,	/* (5741) last move */
	-37,-121,	/* has siblings */
	95,	/* (5744) last move */
	-3,-105,	/* has siblings */	/* last move */
	-18,23,	/* last move */
	-59,-125,	/* has siblings */
	-124,	/* (5751) has sibling */
	6,
	12,
	-62,-126,	/* has siblings */
	13,
	-57,-126,	/* has siblings */
	3,
	1,
	60,
	0,
	7,
	16,
	17,
	-42,-126,	/* has siblings */
	21,
	10,
	95,	/* (5770) last move */
	21,
	8,
	15,
	22,
	35,
	114,	/* (5776) last move */
	26,
	7,
	16,
	64,	/* (5780) last move */
	-54,-126,	/* has siblings */
	17,
	8,
	26,
	21,
	22,
	15,
	41,
	39,
	3,
	35,
	50,
	-28,-126,	/* has siblings */
	30,
	24,
	18,
	44,
	96,	/* (5800) last move */
	24,
	36,
	25,
	18,
	30,
	44,
	63,24,
	93,	/* (5809) last move */
	-47,-126,	/* has siblings */
	19,
	20,
	32,
	-37,2,
	29,
	-24,2,
	106,	/* (5820) last move */
	20,
	-54,-126,	/* has siblings */
	17,
	19,
	96,	/* (5826) last move */
	-32,-126,	/* has siblings */
	19,
	17,
	28,
	27,
	34,
	40,
	24,
	10,
	112,	/* (5837) last move */
	-109,	/* (5838) has sibling */
	28,
	62,
	-115,	/* (5841) has sibling */
	-62,-126,	/* has siblings */
	74,	/* (5844) last move */
	21,
	-62,-126,	/* has siblings */
	71,	/* (5848) last move */
	7,
	2,
	65,	/* (5851) last move */
	21,
	62,
	-104,	/* (5854) has sibling */
	62,
	-46,-112,	/* has siblings */	/* last move */
	18,
	-1,59,-126,	/* has siblings */
	72,	/* (5862) last move */
	79,	/* (5863) last move */
	24,
	62,
	-56,-105,	/* has siblings */	/* last move */
	-49,23,	/* last move */
	17,
	28,
	10,
	-37,-126,	/* has siblings */
	98,	/* (5875) last move */
	62,
	-110,	/* (5877) has sibling */
	-40,-126,	/* has siblings */
	35,
	36,
	73,	/* (5882) last move */
	9,
	115,	/* (5884) last move */
	-104,	/* (5885) has sibling */
	62,
	-37,-121,	/* has siblings */
	19,
	40,
	31,
	127,-106,	/* (5892) last move */
	-30,7,
	48,
	63,120,
	63,105,
	111,	/* (5901) last move */
	115,	/* (5902) last move */
	2,
	-112,	/* (5904) has sibling */
	10,
	19,
	11,
	40,
	-22,-125,	/* has siblings */
	29,
	33,
	51,
	48,
	63,106,
	63,-120,
	31,
	39,
	127,-91,	/* (5921) last move */
	0,
	9,
	61,
	85,	/* (5926) last move */
	10,
	-117,	/* (5928) has sibling */
	16,
	63,60,
	21,
	79,	/* (5933) last move */
	21,
	-77,	/* (5935) has sibling */
	-111,	/* (5936) has sibling */
	4,
	-127,	/* (5938) has sibling */
	16,
	97,	/* (5940) last move */
	16,
	11,
	26,
	1,
	29,
	67,	/* (5946) last move */
	48,
	91,	/* (5948) last move */
	61,
	-47,1,
	4,
	1,
	16,
	33,
	104,	/* (5956) last move */
	19,
	-58,-125,	/* has siblings */
	9,
	-47,-120,	/* has siblings */
	-124,	/* (5963) has sibling */
	12,
	5,
	0,
	1,
	15,
	2,
	14,
	8,
	23,
	80,	/* (5973) last move */
	-52,2,
	62,
	-60,-126,	/* has siblings */
	28,
	78,	/* (5980) last move */
	4,
	28,
	14,
	27,
	82,	/* (5985) last move */
	-52,-126,	/* has siblings */
	17,
	-37,-126,	/* has siblings */
	0,
	-94,	/* (5992) has sibling */
	4,
	5,
	2,
	10,
	11,
	16,
	18,
	21,
	71,	/* (6001) last move */
	-127,	/* (6002) has sibling */
	29,
	4,
	18,
	34,
	100,	/* (6007) last move */
	2,
	65,	/* (6009) last move */
	14,
	18,
	25,
	-64,-126,	/* has siblings */
	30,
	4,
	5,
	2,
	10,
	-117,	/* (6020) has sibling */
	16,
	7,
	21,
	27,
	20,
	58,
	13,
	1,
	26,
	23,
	121,	/* (6031) last move */
	7,
	11,
	13,
	27,
	28,
	93,	/* (6037) last move */
	-105,	/* (6038) has sibling */
	30,
	24,
	-29,-120,	/* has siblings */
	36,
	27,
	-94,	/* (6045) has sibling */
	40,
	8,
	50,
	46,
	47,
	32,
	48,
	28,
	52,
	20,
	63,42,
	63,59,
	-125,	/* (6060) has sibling */
	22,
	56,
	-118,	/* (6063) has sibling */
	2,
	11,
	26,
	41,
	0,
	15,
	13,
	63,43,
	63,22,
	95,	/* (6075) last move */
	-64,2,
	4,
	59,
	63,43,
	53,
	127,60,	/* (6083) last move */
	-42,-126,	/* has siblings */
	1,
	3,
	4,
	5,
	2,
	0,
	74,	/* (6093) last move */
	-1,43,2,
	10,
	2,
	67,	/* (6099) last move */
	-56,2,
	20,
	28,
	44,
	46,
	98,	/* (6106) last move */
	0,
	27,
	-29,-126,	/* has siblings */
	36,
	11,
	21,
	41,
	10,
	2,
	97,	/* (6117) last move */
	-108,	/* (6118) has sibling */
	35,
	15,
	44,
	32,
	110,	/* (6123) last move */
	62,
	-49,7,
	8,
	66,	/* (6128) last move */
	-108,	/* (6129) has sibling */
	23,
	104,	/* (6131) last move */
	24,
	23,
	91,	/* (6134) last move */
	0,
	12,
	4,
	-36,-126,	/* has siblings */
	14,
	18,
	23,
	-108,	/* (6143) has sibling */
	30,
	88,	/* (6145) last move */
	25,
	85,	/* (6147) last move */
	-108,	/* (6148) has sibling */
	-104,	/* (6149) has sibling */
	18,
	21,
	32,
	52,
	48,
	115,	/* (6155) last move */
	32,
	34,
	21,
	89,	/* (6159) last move */
	34,
	-105,	/* (6161) has sibling */
	-50,2,
	89,	/* (6164) last move */
	14,
	18,
	-44,-126,	/* has siblings */
	25,
	17,
	14,
	27,
	40,
	28,
	29,
	36,
	15,
	8,
	50,
	63,42,
	35,
	46,
	55,
	53,
	59,
	3,
	112,	/* (6188) last move */
	23,
	20,
	30,
	88,	/* (6192) last move */
	-48,-125,	/* has siblings */
	-62,-126,	/* has siblings */
	4,
	10,
	6,
	21,
	82,	/* (6201) last move */
	-118,	/* (6202) has sibling */
	11,
	21,
	33,
	41,
	-74,	/* (6207) has sibling */
	-38,-125,	/* has siblings */
	2,
	31,
	29,
	40,
	42,
	49,
	58,
	39,
	27,
	17,
	47,
	6,
	28,
	8,
	13,
	63,-110,
	22,
	15,
	3,
	35,
	117,	/* (6231) last move */
	29,
	31,
	2,
	26,
	13,
	63,113,
	4,
	-3,23,	/* last move */
	26,
	54,
	39,
	1,
	-124,	/* (6246) has sibling */
	0,
	63,-110,
	29,
	31,
	7,
	13,
	3,
	-3,23,	/* last move */
	-61,2,
	4,
	13,
	31,
	29,
	49,
	27,
	109,	/* (6265) last move */
	29,
	-33,-126,	/* has siblings */
	2,
	4,
	10,
	6,
	42,
	-15,-126,	/* has siblings */
	63,-75,
	63,-77,
	9,
	-114,	/* (6281) has sibling */
	18,
	25,
	20,
	87,	/* (6285) last move */
	17,
	14,
	-100,	/* (6288) has sibling */
	99,	/* (6289) last move */
	12,
	99,	/* (6291) last move */
	18,
	121,	/* (6293) last move */
	10,
	-52,-126,	/* has siblings */
	95,	/* (6297) last move */
	-59,-126,	/* has siblings */
	2,
	42,
	31,
	63,-107,
	109,	/* (6305) last move */
	42,
	12,
	17,
	9,
	6,
	5,
	20,
	-100,	/* (6313) has sibling */
	14,
	27,
	9,
	48,
	24,
	44,
	36,
	51,
	63,59,
	39,
	47,
	34,
	127,-119,	/* (6327) last move */
	14,
	18,
	25,
	28,
	-34,-126,	/* has siblings */
	-29,-126,	/* has siblings */
	24,
	50,
	23,
	52,
	103,	/* (6341) last move */
	24,
	35,
	36,
	103,	/* (6345) last move */
	23,
	95,	/* (6347) last move */
	-52,-125,	/* has siblings */
	17,
	5,
	-108,	/* (6352) has sibling */
	-101,	/* (6353) has sibling */
	-30,-126,	/* has siblings */
	40,
	47,
	58,
	24,
	127,-121,	/* (6360) last move */
	32,
	34,
	44,
	29,
	88,	/* (6366) last move */
	32,
	27,
	21,
	61,
	63,120,
	127,-107,	/* (6373) last move */
	-110,	/* (6375) has sibling */
	27,
	28,
	-88,	/* (6378) has sibling */
	125,	/* (6379) last move */
	29,
	34,
	40,
	62,
	-104,	/* (6384) has sibling */
	62,
	-98,	/* (6386) has sibling */
	25,
	20,
	9,
	78,	/* (6390) last move */
	30,
	79,	/* (6392) last move */
	-3,-105,	/* has siblings */	/* last move */
	-17,-105,	/* has siblings */	/* last move */
	-14,23,	/* last move */
	9,
	27,
	34,
	40,
	32,
	-77,	/* (6404) has sibling */
	-48,-126,	/* has siblings */
	31,
	33,
	113,	/* (6409) last move */
	-35,-126,	/* has siblings */
	42,
	16,
	31,
	21,
	-31,-126,	/* has siblings */
	54,
	63,-110,
	26,
	63,-127,
	41,
	45,
	7,
	3,
	66,	/* (6428) last move */
	39,
	63,-110,
	63,-94,
	58,
	47,
	33,
	57,
	63,-106,
	127,120,	/* (6440) last move */
	11,
	-33,-126,	/* has siblings */
	10,
	33,
	7,
	3,
	13,
	1,
	2,
	4,
	79,	/* (6453) last move */
	-35,-126,	/* has siblings */
	10,
	33,
	7,
	3,
	13,
	1,
	2,
	4,
	54,
	63,-110,
	41,
	39,
	127,97,	/* (6469) last move */
	10,
	29,
	-70,	/* (6473) has sibling */
	-86,	/* (6474) has sibling */
	63,-107,
	21,
	-112,	/* (6478) has sibling */
	31,
	33,
	49,
	54,
	63,-106,
	63,120,
	63,-91,
	52,
	3,
	-63,-126,	/* has siblings */
	2,
	4,
	7,
	22,
	8,
	15,
	55,
	59,
	0,
	53,
	8,
	55,
	6,
	72,	/* (6506) last move */
	2,
	65,	/* (6508) last move */
	-15,2,
	33,
	39,
	7,
	3,
	13,
	1,
	2,
	4,
	63,-110,
	63,-94,
	127,-111,	/* (6523) last move */
	16,
	21,
	33,
	41,
	63,-107,
	54,
	39,
	63,-110,
	45,
	63,-106,
	127,-91,	/* (6538) last move */
	-22,-126,	/* has siblings */
	16,
	31,
	21,
	33,
	2,
	4,
	7,
	3,
	13,
	1,
	54,
	63,-110,
	41,
	45,
	127,97,	/* (6557) last move */
	16,
	42,
	39,
	-1,-91,-112,	/* has siblings */	/* last move */
	-1,-107,-112,	/* has siblings */	/* last move */
	63,-121,
	127,-107,	/* (6570) last move */
	29,
	108,	/* (6573) last move */
	-3,-126,	/* has siblings */
	75,	/* (6576) last move */
	21,
	62,
	-128,	/* (6579) has sibling */
	6,
	5,
	12,
	31,
	-1,-121,19,	/* last move */
	-127,	/* (6587) has sibling */
	3,
	8,
	-116,	/* (6590) has sibling */
	7,
	2,
	13,
	26,
	22,
	81,	/* (6596) last move */
	22,
	4,
	2,
	73,	/* (6600) last move */
	-97,	/* (6601) has sibling */
	-48,-125,	/* has siblings */
	29,
	42,
	-15,-126,	/* has siblings */
	58,
	34,
	127,-75,	/* (6610) last move */
	57,
	-15,-126,	/* has siblings */
	63,-107,
	33,
	58,
	63,-91,
	31,
	39,
	-65,-60,	/* (6623) has sibling */
	63,-76,
	63,-75,
	45,
	127,-90,	/* (6630) last move */
	63,-90,
	63,-75,
	63,-74,
	127,-59,	/* (6638) last move */
	58,
	-31,1,
	54,
	63,-110,
	41,
	98,	/* (6647) last move */
	33,
	49,
	39,
	57,
	-3,-110,	/* has siblings */	/* last move */
	63,-77,
	-3,-126,	/* has siblings */
	12,
	17,
	9,
	5,
	6,
	4,
	10,
	2,
	0,
	1,
	15,
	-114,	/* (6669) has sibling */
	20,
	8,
	13,
	7,
	11,
	43,
	29,
	27,
	47,
	40,
	58,
	42,
	34,
	16,
	28,
	93,	/* (6685) last move */
	7,
	78,	/* (6687) last move */
	5,
	2,
	62,
	-18,-112,	/* has siblings */	/* last move */
	-13,-112,	/* has siblings */	/* last move */
	125,	/* (6695) last move */
	-82,	/* (6696) has sibling */
	62,
	-123,	/* (6698) has sibling */
	2,
	31,
	16,
	29,
	42,
	58,
	63,-107,
	111,	/* (6707) last move */
	34,
	-123,	/* (6709) has sibling */
	2,
	31,
	16,
	29,
	42,
	58,
	63,-107,
	111,	/* (6718) last move */
	-119,	/* (6719) has sibling */
	-124,	/* (6720) has sibling */
	32,
	28,
	20,
	52,
	-16,-112,	/* has siblings */	/* last move */
	24,
	48,
	100,	/* (6729) last move */
	63,106,
	1,
	3,
	4,
	2,
	8,
	-52,2,
	5,
	10,
	91,	/* (6741) last move */
	-124,	/* (6742) has sibling */
	-127,	/* (6743) has sibling */
	2,
	13,
	26,
	11,
	5,
	7,
	42,
	-65,-121,	/* (6751) has sibling */
	33,
	-10,-126,	/* has siblings */
	103,	/* (6756) last move */
	41,
	127,-91,	/* (6758) last move */
	39,
	127,-121,	/* (6761) last move */
	2,
	1,
	3,
	10,
	-120,	/* (6767) has sibling */
	11,
	26,
	0,
	7,
	42,
	9,
	12,
	103,	/* (6775) last move */
	11,
	8,
	9,
	70,	/* (6779) last move */
	-127,	/* (6780) has sibling */
	-124,	/* (6781) has sibling */
	0,
	6,
	7,
	13,
	3,
	73,	/* (6787) last move */
	3,
	-53,-120,	/* has siblings */
	-128,	/* (6791) has sibling */
	16,
	6,
	97,	/* (6794) last move */
	-38,2,
	2,
	13,
	4,
	7,
	73,	/* (6801) last move */
	4,
	-120,	/* (6803) has sibling */
	2,
	0,
	7,
	1,
	-117,	/* (6808) has sibling */
	6,
	9,
	5,
	10,
	17,
	103,	/* (6814) last move */
	12,
	23,
	14,
	30,
	95,	/* (6819) last move */
	2,
	-56,-126,	/* has siblings */
	12,
	5,
	10,
	16,
	11,
	17,
	29,
	27,
	40,
	9,
	95,	/* (6833) last move */
	-54,32,26,111,	/* reconverge */
	-22,2,
	6,
	9,
	12,
	17,
	91,	/* (6844) last move */
	-77,	/* (6845) has sibling */
	-80,	/* (6846) has sibling */
	-22,-126,	/* has siblings */
	12,
	17,
	9,
	5,
	6,
	4,
	10,
	2,
	-128,	/* (6857) has sibling */
	1,
	15,
	7,
	29,
	-37,2,
	40,
	20,
	14,
	18,
	25,
	28,
	88,	/* (6870) last move */
	29,
	0,
	91,	/* (6873) last move */
	-97,	/* (6874) has sibling */
	-112,	/* (6875) has sibling */
	29,
	42,
	2,
	-70,	/* (6879) has sibling */
	4,
	13,
	7,
	1,
	-38,-126,	/* has siblings */
	69,	/* (6886) last move */
	11,
	26,
	3,
	113,	/* (6890) last move */
	-51,-125,	/* has siblings */
	5,
	-121,	/* (6894) has sibling */
	-81,	/* (6895) has sibling */
	49,
	58,
	63,-107,
	-12,3,
	32,
	63,121,
	46,
	63,59,
	61,
	63,91,
	63,76,
	36,
	63,42,
	44,
	50,
	24,
	35,
	18,
	127,61,	/* (6921) last move */
	-15,2,
	58,
	28,
	-94,	/* (6927) has sibling */
	32,
	52,
	44,
	17,
	20,
	9,
	18,
	78,	/* (6935) last move */
	-32,2,
	111,	/* (6938) last move */
	-15,-125,	/* has siblings */
	58,
	63,-107,
	26,
	7,
	11,
	64,	/* (6947) last move */
	26,
	1,
	7,
	47,
	63,-107,
	58,
	127,106,	/* (6955) last move */
	-122,	/* (6957) has sibling */
	-95,	/* (6958) has sibling */
	41,
	4,
	5,
	10,
	0,
	11,
	14,
	118,	/* (6966) last move */
	-55,2,
	5,
	10,
	75,	/* (6971) last move */
	4,
	11,
	6,
	97,	/* (6975) last move */
	33,
	49,
	39,
	57,
	63,-77,
	5,
	2,
	127,106,	/* (6984) last move */
	-124,	/* (6986) has sibling */
	1,
	2,
	13,
	26,
	11,
	5,
	7,
	106,	/* (6994) last move */
	1,
	-124,	/* (6996) has sibling */
	0,
	6,
	7,
	13,
	3,
	73,	/* (7002) last move */
	3,
	4,
	-120,	/* (7005) has sibling */
	2,
	0,
	7,
	1,
	-117,	/* (7010) has sibling */
	6,
	9,
	5,
	10,
	17,
	103,	/* (7016) last move */
	12,
	23,
	14,
	30,
	95,	/* (7021) last move */
	2,
	10,
	11,
	8,
	9,
	70,	/* (7027) last move */
	62,
	-59,-96,26,43,	/* reconverge */	/* has siblings */
	-30,32,26,53,	/* reconverge */
	-3,32,27,116,	/* reconverge */
	-44,-115,	/* has siblings */
	-59,-126,	/* has siblings */
	4,
	6,
	12,
	17,
	19,
	9,
	10,
	82,	/* (7052) last move */
	62,
	-122,	/* (7054) has sibling */
	0,
	4,
	14,
	73,	/* (7058) last move */
	-110,	/* (7059) has sibling */
	-104,	/* (7060) has sibling */
	37,
	14,
	30,
	28,
	9,
	17,
	25,
	5,
	44,
	109,	/* (7070) last move */
	9,
	14,
	6,
	-60,-125,	/* has siblings */
	-128,	/* (7076) has sibling */
	5,
	1,
	24,
	26,
	16,
	98,	/* (7082) last move */
	-59,2,
	0,
	66,	/* (7086) last move */
	-27,-126,	/* has siblings */
	0,
	100,	/* (7090) last move */
	0,
	4,
	37,
	1,
	100,	/* (7095) last move */
	-111,	/* (7096) has sibling */
	-123,	/* (7097) has sibling */
	28,
	-91,	/* (7099) has sibling */
	9,
	32,
	48,
	63,105,
	63,121,
	63,120,
	34,
	63,106,
	63,-120,
	97,	/* (7114) last move */
	0,
	23,
	31,
	101,	/* (7118) last move */
	-36,-126,	/* has siblings */
	27,
	-30,-126,	/* has siblings */
	11,
	40,
	69,	/* (7126) last move */
	21,
	34,
	116,	/* (7129) last move */
	-118,	/* (7130) has sibling */
	-127,	/* (7131) has sibling */
	-124,	/* (7132) has sibling */
	0,
	9,
	6,
	12,
	27,
	25,
	14,
	18,
	23,
	31,
	48,
	115,	/* (7144) last move */
	3,
	4,
	2,
	-36,-112,	/* has siblings */	/* last move */
	8,
	45,
	62,
	28,
	29,
	16,
	42,
	31,
	127,-107,	/* (7158) last move */
	28,
	-128,	/* (7161) has sibling */
	6,
	31,
	115,	/* (7164) last move */
	37,
	9,
	96,	/* (7167) last move */
	-55,-126,	/* has siblings */
	69,	/* (7170) last move */
	85,	/* (7171) last move */
	-116,	/* (7172) has sibling */
	10,
	-100,	/* (7174) has sibling */
	32,
	34,
	44,
	31,
	19,
	9,
	18,
	58,
	8,
	127,-120,	/* (7184) last move */
	-33,-126,	/* has siblings */
	-112,	/* (7188) has sibling */
	42,
	27,
	1,
	3,
	17,
	-36,-126,	/* has siblings */
	19,
	29,
	40,
	34,
	66,	/* (7200) last move */
	40,
	28,
	64,	/* (7203) last move */
	19,
	17,
	0,
	23,
	40,
	28,
	-20,19,	/* last move */
	44,
	34,
	18,
	9,
	14,
	6,
	-60,-126,	/* has siblings */
	0,
	5,
	2,
	19,
	29,
	11,
	27,
	16,
	17,
	10,
	31,
	42,
	40,
	33,
	49,
	13,
	7,
	41,
	127,81,	/* (7238) last move */
	0,
	-127,	/* (7241) has sibling */
	4,
	5,
	37,
	83,	/* (7245) last move */
	68,	/* (7246) last move */
	-60,-112,	/* has siblings */	/* last move */
	1,
	-125,	/* (7250) has sibling */
	4,
	-56,-110,	/* has siblings */	/* last move */
	2,
	8,
	16,
	62,
	-114,	/* (7258) has sibling */
	22,
	23,
	7,
	13,
	63,49,
	6,
	0,
	69,	/* (7267) last move */
	62,
	-106,	/* (7269) has sibling */
	94,	/* (7270) last move */
	22,
	127,-77,	/* (7272) last move */
	4,
	0,
	6,
	-55,-125,	/* has siblings */
	-114,	/* (7279) has sibling */
	17,
	-98,	/* (7281) has sibling */
	7,
	13,
	3,
	10,
	-100,	/* (7286) has sibling */
	23,
	37,
	15,
	18,
	25,
	35,
	72,	/* (7293) last move */
	23,
	28,
	27,
	34,
	40,
	95,	/* (7299) last move */
	18,
	7,
	13,
	3,
	10,
	40,
	95,	/* (7306) last move */
	17,
	14,
	28,
	24,
	121,	/* (7311) last move */
	14,
	9,
	24,
	121,	/* (7315) last move */
	-28,-115,	/* has siblings */
	62,
	-128,	/* (7319) has sibling */
	6,
	10,
	-76,	/* (7322) has sibling */
	-67,	/* (7323) has sibling */
	24,
	-29,2,
	37,
	63,42,
	2,
	5,
	4,
	1,
	75,	/* (7334) last move */
	32,
	48,
	28,
	19,
	63,105,
	16,
	11,
	21,
	63,90,
	4,
	1,
	26,
	13,
	2,
	5,
	12,
	2,
	122,	/* (7354) last move */
	32,
	-65,60,	/* (7356) has sibling */
	61,
	63,59,
	24,
	35,
	44,
	46,
	18,
	95,	/* (7366) last move */
	-76,	/* (7367) has sibling */
	63,105,
	63,90,
	37,
	24,
	18,
	63,60,
	63,120,
	95,	/* (7379) last move */
	51,
	127,106,	/* (7381) last move */
	-1,59,-121,	/* has siblings */
	5,
	9,
	52,
	37,
	44,
	72,	/* (7391) last move */
	-107,	/* (7392) has sibling */
	5,
	45,
	-128,	/* (7395) has sibling */
	-113,	/* (7396) has sibling */
	8,
	19,
	73,	/* (7399) last move */
	-58,-126,	/* has siblings */
	20,
	12,
	10,
	27,
	80,	/* (7406) last move */
	37,
	44,
	32,
	52,
	48,
	63,122,
	127,-120,	/* (7414) last move */
	9,
	18,
	-91,	/* (7418) has sibling */
	8,
	22,
	50,
	63,42,
	20,
	28,
	35,
	46,
	1,
	3,
	0,
	17,
	-50,7,
	89,	/* (7434) last move */
	-108,	/* (7435) has sibling */
	24,
	-20,-126,	/* has siblings */
	50,
	63,42,
	37,
	32,
	52,
	48,
	35,
	61,
	1,
	3,
	0,
	77,	/* (7451) last move */
	37,
	50,
	-18,-126,	/* has siblings */
	63,42,
	63,59,
	79,	/* (7460) last move */
	63,42,
	46,
	44,
	-29,-126,	/* has siblings */
	62,
	-120,	/* (7468) has sibling */
	22,
	1,
	3,
	13,
	7,
	0,
	115,	/* (7475) last move */
	127,59,	/* (7476) last move */
	63,43,
	78,	/* (7480) last move */
	14,
	25,
	20,
	24,
	37,
	50,
	63,42,
	46,
	44,
	35,
	63,59,
	8,
	0,
	15,
	6,
	23,
	115,	/* (7499) last move */
	-118,	/* (7500) has sibling */
	6,
	-119,	/* (7502) has sibling */
	12,
	0,
	-127,	/* (7505) has sibling */
	4,
	5,
	2,
	11,
	13,
	26,
	83,	/* (7512) last move */
	2,
	1,
	103,	/* (7515) last move */
	4,
	5,
	0,
	2,
	12,
	9,
	1,
	-125,	/* (7523) has sibling */
	22,
	21,
	30,
	37,
	19,
	7,
	-33,7,
	33,
	49,
	63,-94,
	63,120,
	127,90,	/* (7538) last move */
	11,
	19,
	33,
	-110,	/* (7543) has sibling */
	20,
	37,
	-114,	/* (7546) has sibling */
	25,
	30,
	24,
	23,
	63,22,
	63,23,
	28,
	63,21,
	25,
	63,22,
	17,
	25,
	35,
	3,
	8,
	22,
	123,	/* (7567) last move */
	24,
	14,
	96,	/* (7570) last move */
	30,
	37,
	42,
	57,
	63,120,
	127,90,	/* (7577) last move */
	-123,	/* (7579) has sibling */
	-60,-126,	/* has siblings */
	6,
	12,
	10,
	17,
	8,
	26,
	-46,23,	/* last move */
	2,
	-63,-120,	/* has siblings */
	-118,	/* (7593) has sibling */
	-64,-126,	/* has siblings */
	103,	/* (7596) last move */
	4,
	-61,-125,	/* has siblings */
	11,
	8,
	0,
	13,
	26,
	63,81,
	33,
	19,
	101,	/* (7609) last move */
	21,
	26,
	11,
	3,
	109,	/* (7614) last move */
	-125,	/* (7615) has sibling */
	10,
	4,
	6,
	0,
	27,
	127,60,	/* (7621) last move */
	-60,2,
	0,
	6,
	9,
	17,
	84,	/* (7629) last move */
	10,
	21,
	63,59,
	52,
	1,
	-61,2,
	11,
	26,
	41,
	13,
	97,	/* (7642) last move */
	-127,	/* (7643) has sibling */
	3,
	4,
	-120,	/* (7646) has sibling */
	2,
	0,
	7,
	1,
	12,
	62,
	25,
	13,
	26,
	60,
	75,	/* (7657) last move */
	2,
	8,
	-95,	/* (7660) has sibling */
	62,
	-45,-112,	/* has siblings */	/* last move */
	5,
	10,
	19,
	80,	/* (7667) last move */
	80,	/* (7668) last move */
	-128,	/* (7669) has sibling */
	62,
	6,
	10,
	52,
	61,
	88,	/* (7675) last move */
	62,
	-123,	/* (7677) has sibling */
	-120,	/* (7678) has sibling */
	9,
	14,
	20,
	18,
	37,
	67,	/* (7684) last move */
	37,
	44,
	32,
	24,
	82,	/* (7689) last move */
	-118,	/* (7690) has sibling */
	62,
	1,
	3,
	4,
	-120,	/* (7695) has sibling */
	2,
	0,
	7,
	1,
	5,
	6,
	12,
	9,
	81,	/* (7704) last move */
	5,
	72,	/* (7706) last move */
	-124,	/* (7707) has sibling */
	-63,-125,	/* has siblings */
	2,
	-51,-126,	/* has siblings */
	26,
	11,
	5,
	41,
	7,
	63,97,
	67,	/* (7720) last move */
	5,
	13,
	12,
	0,
	8,
	7,
	14,
	25,
	18,
	15,
	30,
	86,	/* (7732) last move */
	2,
	1,
	3,
	8,
	80,	/* (7737) last move */
	19,
	62,
	-117,	/* (7740) has sibling */
	62,
	-127,	/* (7742) has sibling */
	0,
	-124,	/* (7744) has sibling */
	15,
	8,
	87,	/* (7747) last move */
	15,
	4,
	8,
	70,	/* (7751) last move */
	0,
	6,
	23,
	25,
	79,	/* (7756) last move */
	-52,-125,	/* has siblings */
	17,
	5,
	-104,	/* (7761) has sibling */
	-84,	/* (7762) has sibling */
	27,
	21,
	99,	/* (7765) last move */
	27,
	44,
	29,
	110,	/* (7769) last move */
	9,
	27,
	-99,	/* (7772) has sibling */
	-88,	/* (7773) has sibling */
	42,
	58,
	11,
	10,
	21,
	2,
	16,
	0,
	96,	/* (7782) last move */
	34,
	101,	/* (7784) last move */
	-30,2,
	40,
	32,
	93,	/* (7789) last move */
	21,
	127,59,	/* (7791) last move */
	-40,-115,	/* has siblings */
	62,
	-128,	/* (7796) has sibling */
	6,
	10,
	-96,	/* (7799) has sibling */
	51,
	-62,2,
	5,
	4,
	1,
	11,
	17,
	20,
	21,
	41,
	118,	/* (7811) last move */
	28,
	32,
	48,
	52,
	20,
	-97,	/* (7817) has sibling */
	37,
	44,
	36,
	46,
	50,
	63,42,
	35,
	127,105,	/* (7826) last move */
	-82,	/* (7828) has sibling */
	2,
	11,
	5,
	13,
	26,
	7,
	19,
	41,
	16,
	21,
	95,	/* (7839) last move */
	-53,-126,	/* has siblings */
	65,	/* (7842) last move */
	63,59,
	2,
	5,
	4,
	1,
	11,
	16,
	21,
	33,
	54,
	63,-110,
	31,
	39,
	29,
	42,
	49,
	19,
	40,
	27,
	34,
	58,
	47,
	121,	/* (7867) last move */
	62,
	-122,	/* (7869) has sibling */
	-55,-125,	/* has siblings */
	14,
	-124,	/* (7873) has sibling */
	12,
	0,
	5,
	25,
	18,
	-62,-126,	/* has siblings */
	65,	/* (7881) last move */
	1,
	13,
	7,
	66,	/* (7885) last move */
	18,
	25,
	4,
	12,
	5,
	0,
	19,
	-105,	/* (7893) has sibling */
	17,
	38,
	27,
	10,
	16,
	11,
	21,
	90,	/* (7901) last move */
	17,
	1,
	23,
	2,
	10,
	7,
	16,
	105,	/* (7909) last move */
	-64,-126,	/* has siblings */
	4,
	18,
	20,
	14,
	37,
	1,
	7,
	3,
	109,	/* (7920) last move */
	4,
	-128,	/* (7922) has sibling */
	5,
	1,
	73,	/* (7925) last move */
	5,
	0,
	9,
	1,
	13,
	-114,	/* (7931) has sibling */
	-125,	/* (7932) has sibling */
	18,
	20,
	35,
	36,
	50,
	110,	/* (7938) last move */
	-108,	/* (7939) has sibling */
	7,
	66,	/* (7941) last move */
	18,
	67,	/* (7943) last move */
	86,	/* (7944) last move */
	-27,-126,	/* has siblings */
	-108,	/* (7947) has sibling */
	82,	/* (7948) last move */
	-110,	/* (7949) has sibling */
	12,
	10,
	44,
	0,
	23,
	20,
	28,
	36,
	51,
	-82,	/* (7959) has sibling */
	62,
	-111,	/* (7961) has sibling */
	-119,	/* (7962) has sibling */
	27,
	19,
	29,
	40,
	34,
	16,
	42,
	5,
	11,
	2,
	13,
	26,
	21,
	7,
	6,
	63,81,
	4,
	13,
	95,	/* (7982) last move */
	-109,	/* (7983) has sibling */
	70,	/* (7984) last move */
	-123,	/* (7985) has sibling */
	2,
	19,
	34,
	-96,	/* (7989) has sibling */
	6,
	4,
	9,
	1,
	29,
	15,
	27,
	0,
	80,	/* (7998) last move */
	16,
	11,
	29,
	96,	/* (8002) last move */
	27,
	9,
	6,
	83,	/* (8006) last move */
	-3,-126,	/* has siblings */
	17,
	5,
	2,
	19,
	34,
	16,
	11,
	29,
	96,	/* (8017) last move */
	19,
	-67,	/* (8019) has sibling */
	16,
	21,
	5,
	11,
	2,
	13,
	97,	/* (8026) last move */
	16,
	61,
	127,60,	/* (8029) last move */
	62,
	-65,92,	/* (8032) has sibling */
	127,60,	/* (8034) last move */
	62,
	-97,	/* (8037) has sibling */
	19,
	81,	/* (8039) last move */
	63,59,
	9,
	95,	/* (8043) last move */
	44,
	18,
	36,
	-43,-125,	/* has siblings */
	10,
	-127,	/* (8050) has sibling */
	-125,	/* (8051) has sibling */
	4,
	2,
	16,
	19,
	96,	/* (8056) last move */
	11,
	4,
	13,
	7,
	60,
	2,
	41,
	127,25,	/* (8064) last move */
	16,
	19,
	-117,	/* (8068) has sibling */
	6,
	0,
	4,
	20,
	1,
	48,
	93,	/* (8075) last move */
	32,
	0,
	70,	/* (8078) last move */
	-112,	/* (8079) has sibling */
	5,
	28,
	0,
	40,
	-23,23,	/* last move */
	10,
	-128,	/* (8087) has sibling */
	12,
	26,
	21,
	41,
	39,
	-41,7,
	25,
	6,
	73,	/* (8097) last move */
	-58,-126,	/* has siblings */
	0,
	4,
	73,	/* (8102) last move */
	-123,	/* (8103) has sibling */
	12,
	11,
	-124,	/* (8106) has sibling */
	2,
	1,
	3,
	22,
	16,
	19,
	-56,-126,	/* has siblings */
	0,
	56,
	21,
	33,
	41,
	53,
	39,
	29,
	118,	/* (8123) last move */
	71,	/* (8124) last move */
	-48,2,
	21,
	31,
	33,
	49,
	109,	/* (8131) last move */
	11,
	21,
	16,
	2,
	26,
	19,
	13,
	31,
	15,
	93,	/* (8141) last move */
	-100,	/* (8142) has sibling */
	-96,	/* (8143) has sibling */
	48,
	52,
	20,
	21,
	37,
	44,
	36,
	46,
	50,
	63,42,
	35,
	127,91,	/* (8156) last move */
	-59,-126,	/* has siblings */
	18,
	37,
	20,
	63,60,
	72,	/* (8165) last move */
	-122,	/* (8166) has sibling */
	-55,-126,	/* has siblings */
	12,
	0,
	1,
	-27,23,	/* last move */
	18,
	0,
	101,	/* (8176) last move */
	-55,2,
	5,
	2,
	10,
	21,
	81,	/* (8183) last move */
	-55,-96,0,-100,	/* reconverge */	/* has siblings */
	-107,	/* (8188) has sibling */
	-122,	/* (8189) has sibling */
	-124,	/* (8190) has sibling */
	-123,	/* (8191) has sibling */
	0,
	-63,-126,	/* has siblings */
	66,	/* (8195) last move */
	-52,-126,	/* has siblings */
	66,	/* (8198) last move */
	13,
	-119,	/* (8200) has sibling */
	12,
	17,
	27,
	37,
	44,
	32,
	18,
	14,
	8,
	125,	/* (8210) last move */
	18,
	-119,	/* (8212) has sibling */
	20,
	14,
	25,
	23,
	-113,	/* (8217) has sibling */
	30,
	63,21,
	92,	/* (8221) last move */
	35,
	15,
	98,	/* (8224) last move */
	37,
	-116,	/* (8226) has sibling */
	9,
	20,
	17,
	28,
	19,
	-94,	/* (8232) has sibling */
	125,	/* (8233) last move */
	51,
	34,
	32,
	48,
	116,	/* (8238) last move */
	28,
	-52,-126,	/* has siblings */
	84,	/* (8242) last move */
	1,
	12,
	19,
	9,
	74,	/* (8247) last move */
	0,
	5,
	1,
	73,	/* (8251) last move */
	-19,2,
	64,	/* (8254) last move */
	5,
	45,
	-128,	/* (8257) has sibling */
	-113,	/* (8258) has sibling */
	8,
	19,
	73,	/* (8261) last move */
	-58,-112,	/* has siblings */	/* last move */
	73,	/* (8264) last move */
	9,
	62,
	-57,-112,	/* has siblings */	/* last move */
	-56,7,
	-61,-126,	/* has siblings */
	30,
	35,
	6,
	12,
	22,
	56,
	1,
	13,
	14,
	18,
	127,23,	/* (8283) last move */
	-106,	/* (8285) has sibling */
	3,
	-63,-126,	/* has siblings */
	53,
	56,
	63,49,
	15,
	63,17,
	0,
	90,	/* (8297) last move */
	15,
	53,
	56,
	63,49,
	0,
	77,	/* (8304) last move */
	14,
	-105,	/* (8306) has sibling */
	3,
	22,
	56,
	6,
	12,
	25,
	30,
	1,
	13,
	123,	/* (8316) last move */
	3,
	-57,-126,	/* has siblings */
	87,	/* (8320) last move */
	23,
	77,	/* (8322) last move */
	-127,	/* (8323) has sibling */
	-60,-126,	/* has siblings */
	0,
	-58,2,
	94,	/* (8329) last move */
	3,
	4,
	-120,	/* (8332) has sibling */
	-123,	/* (8333) has sibling */
	0,
	15,
	1,
	11,
	63,59,
	73,	/* (8340) last move */
	2,
	0,
	7,
	-122,	/* (8344) has sibling */
	1,
	15,
	5,
	14,
	12,
	9,
	103,	/* (8351) last move */
	1,
	12,
	-41,2,
	14,
	89,	/* (8357) last move */
	2,
	8,
	16,
	62,
	-119,	/* (8362) has sibling */
	5,
	10,
	86,	/* (8365) last move */
	62,
	-106,	/* (8367) has sibling */
	73,	/* (8368) last move */
	22,
	127,-77,	/* (8370) last move */
	-123,	/* (8372) has sibling */
	62,
	-60,-126,	/* has siblings */
	62,
	6,
	12,
	-126,	/* (8379) has sibling */
	13,
	-57,-126,	/* has siblings */
	3,
	1,
	60,
	0,
	7,
	16,
	17,
	-106,	/* (8390) has sibling */
	21,
	10,
	95,	/* (8393) last move */
	21,
	22,
	20,
	28,
	37,
	9,
	18,
	14,
	25,
	23,
	-113,	/* (8404) has sibling */
	30,
	63,21,
	-78,	/* (8408) has sibling */
	32,
	47,
	55,
	46,
	8,
	105,	/* (8414) last move */
	108,	/* (8415) last move */
	35,
	63,22,
	30,
	79,	/* (8420) last move */
	26,
	71,	/* (8422) last move */
	10,
	-109,	/* (8424) has sibling */
	16,
	29,
	-33,-126,	/* has siblings */
	9,
	14,
	18,
	8,
	22,
	17,
	20,
	42,
	34,
	53,
	15,
	0,
	1,
	59,
	55,
	40,
	27,
	8,
	23,
	25,
	119,	/* (8449) last move */
	13,
	-128,	/* (8451) has sibling */
	20,
	31,
	33,
	39,
	7,
	23,
	18,
	8,
	54,
	63,-110,
	41,
	3,
	25,
	63,21,
	56,
	-1,49,-126,	/* has siblings */
	34,
	42,
	46,
	-1,-127,2,
	2,
	60,
	63,81,
	22,
	127,113,	/* (8483) last move */
	106,	/* (8485) last move */
	7,
	-56,-126,	/* has siblings */
	31,
	26,
	22,
	1,
	3,
	53,
	20,
	62,
	-128,	/* (8497) has sibling */
	15,
	87,	/* (8499) last move */
	0,
	127,-107,	/* (8501) last move */
	0,
	-56,-126,	/* has siblings */
	1,
	3,
	22,
	56,
	53,
	55,
	124,	/* (8512) last move */
	1,
	8,
	22,
	59,
	26,
	2,
	63,81,
	31,
	9,
	63,49,
	60,
	53,
	56,
	14,
	18,
	41,
	25,
	23,
	63,22,
	63,21,
	35,
	17,
	20,
	11,
	34,
	63,97,
	67,	/* (8544) last move */
	-47,2,
	15,
	26,
	20,
	9,
	82,	/* (8551) last move */
	2,
	10,
	21,
	46,
	32,
	-65,90,	/* (8557) has sibling */
	-111,	/* (8559) has sibling */
	-124,	/* (8560) has sibling */
	16,
	11,
	26,
	1,
	29,
	67,	/* (8566) last move */
	11,
	26,
	16,
	-124,	/* (8570) has sibling */
	6,
	12,
	41,
	13,
	33,
	72,	/* (8576) last move */
	33,
	4,
	1,
	84,	/* (8580) last move */
	-1,120,2,
	91,	/* (8584) last move */
	16,
	33,
	31,
	49,
	63,-107,
	-65,-91,	/* (8591) has sibling */
	11,
	26,
	13,
	41,
	1,
	-65,-106,	/* (8598) has sibling */
	7,
	106,	/* (8601) last move */
	7,
	57,
	39,
	63,-106,
	127,90,	/* (8607) last move */
	57,
	11,
	26,
	41,
	-74,	/* (8613) has sibling */
	13,
	63,113,
	1,
	7,
	127,90,	/* (8619) last move */
	13,
	63,90,
	63,120,
	122,	/* (8626) last move */
	-52,-126,	/* has siblings */
	10,
	63,59,
	32,
	19,
	80,	/* (8634) last move */
	19,
	21,
	-82,	/* (8637) has sibling */
	92,	/* (8638) last move */
	63,59,
	92,	/* (8641) last move */
	-46,13,
	-53,-126,	/* has siblings */
	5,
	10,
	81,	/* (8648) last move */
	62,
	-128,	/* (8650) has sibling */
	6,
	10,
	17,
	31,
	37,
	46,
	88,	/* (8657) last move */
	62,
	-122,	/* (8659) has sibling */
	62,
	-114,	/* (8661) has sibling */
	-64,2,
	65,	/* (8664) last move */
	-128,	/* (8665) has sibling */
	4,
	-50,-126,	/* has siblings */
	9,
	30,
	24,
	35,
	36,
	65,	/* (8674) last move */
	1,
	14,
	7,
	13,
	3,
	75,	/* (8680) last move */
	4,
	-128,	/* (8682) has sibling */
	5,
	1,
	9,
	87,	/* (8686) last move */
	5,
	0,
	9,
	1,
	13,
	-57,-126,	/* has siblings */
	2,
	62,
	3,
	22,
	60,
	56,
	59,
	25,
	30,
	127,21,	/* (8703) last move */
	67,	/* (8705) last move */
	-47,-126,	/* has siblings */
	-122,	/* (8708) has sibling */
	9,
	14,
	4,
	12,
	5,
	19,
	0,
	10,
	2,
	11,
	-80,	/* (8719) has sibling */
	13,
	7,
	26,
	3,
	44,
	27,
	109,	/* (8726) last move */
	13,
	98,	/* (8728) last move */
	10,
	-127,	/* (8730) has sibling */
	-61,-126,	/* has siblings */
	4,
	2,
	101,	/* (8735) last move */
	4,
	0,
	6,
	-71,	/* (8739) has sibling */
	73,	/* (8740) last move */
	9,
	14,
	67,	/* (8743) last move */
	37,
	0,
	6,
	31,
	24,
	23,
	25,
	15,
	94,	/* (8752) last move */
	-63,-126,	/* has siblings */
	-124,	/* (8755) has sibling */
	0,
	70,	/* (8757) last move */
	3,
	-42,-126,	/* has siblings */
	0,
	8,
	4,
	15,
	6,
	14,
	73,	/* (8767) last move */
	4,
	-120,	/* (8769) has sibling */
	2,
	0,
	-49,-126,	/* has siblings */
	-123,	/* (8774) has sibling */
	1,
	7,
	-108,	/* (8777) has sibling */
	12,
	24,
	6,
	14,
	64,	/* (8782) last move */
	6,
	75,	/* (8784) last move */
	-63,2,
	12,
	13,
	75,	/* (8789) last move */
	7,
	-122,	/* (8791) has sibling */
	1,
	15,
	5,
	-119,	/* (8795) has sibling */
	103,	/* (8796) last move */
	12,
	19,
	28,
	103,	/* (8800) last move */
	1,
	12,
	9,
	6,
	23,
	103,	/* (8806) last move */
	2,
	8,
	16,
	62,
	-106,	/* (8811) has sibling */
	20,
	37,
	9,
	36,
	5,
	10,
	91,	/* (8818) last move */
	22,
	109,	/* (8820) last move */
	-123,	/* (8821) has sibling */
	-107,	/* (8822) has sibling */
	2,
	13,
	-111,	/* (8825) has sibling */
	3,
	101,	/* (8827) last move */
	-38,2,
	7,
	11,
	4,
	6,
	12,
	19,
	17,
	8,
	0,
	15,
	23,
	25,
	14,
	63,21,
	9,
	23,
	41,
	63,97,
	16,
	10,
	40,
	3,
	29,
	1,
	27,
	124,	/* (8857) last move */
	-126,	/* (8858) has sibling */
	10,
	21,
	46,
	28,
	16,
	33,
	31,
	49,
	11,
	26,
	41,
	54,
	13,
	63,113,
	1,
	7,
	63,-107,
	116,	/* (8878) last move */
	4,
	6,
	12,
	17,
	19,
	20,
	-91,	/* (8885) has sibling */
	32,
	44,
	52,
	51,
	13,
	-120,	/* (8891) has sibling */
	-118,	/* (8892) has sibling */
	29,
	0,
	1,
	-113,	/* (8896) has sibling */
	2,
	11,
	26,
	41,
	63,97,
	31,
	7,
	103,	/* (8905) last move */
	33,
	14,
	9,
	23,
	113,	/* (8910) last move */
	2,
	23,
	0,
	1,
	15,
	14,
	9,
	59,
	25,
	63,21,
	30,
	24,
	27,
	29,
	3,
	-29,-126,	/* has siblings */
	53,
	55,
	22,
	63,23,
	7,
	127,22,	/* (8935) last move */
	-57,-126,	/* has siblings */
	22,
	60,
	53,
	55,
	127,81,	/* (8943) last move */
	53,
	40,
	42,
	33,
	62,
	7,
	35,
	22,
	119,	/* (8953) last move */
	-62,2,
	10,
	11,
	16,
	21,
	95,	/* (8960) last move */
	10,
	24,
	62,
	-101,	/* (8964) has sibling */
	62,
	-97,	/* (8966) has sibling */
	62,
	-117,	/* (8968) has sibling */
	21,
	16,
	2,
	26,
	41,
	29,
	5,
	118,	/* (8976) last move */
	-86,	/* (8977) has sibling */
	93,	/* (8978) last move */
	-35,-126,	/* has siblings */
	122,	/* (8981) last move */
	11,
	122,	/* (8983) last move */
	-24,-126,	/* has siblings */
	29,
	42,
	16,
	11,
	21,
	-15,-126,	/* has siblings */
	26,
	2,
	54,
	39,
	63,-110,
	-120,	/* (8999) has sibling */
	127,-77,	/* (9000) last move */
	-1,-77,2,
	72,	/* (9005) last move */
	26,
	113,	/* (9007) last move */
	42,
	40,
	95,	/* (9010) last move */
	-37,7,
	62,
	-100,	/* (9014) has sibling */
	9,
	32,
	37,
	116,	/* (9018) last move */
	-30,1,
	47,
	48,
	63,120,
	63,105,
	127,-77,	/* (9027) last move */
	-108,	/* (9029) has sibling */
	37,
	-100,	/* (9031) has sibling */
	14,
	-59,-120,	/* has siblings */
	-105,	/* (9035) has sibling */
	0,
	15,
	1,
	96,	/* (9039) last move */
	2,
	10,
	-117,	/* (9042) has sibling */
	9,
	-28,-125,	/* has siblings */
	33,
	21,
	16,
	54,
	39,
	63,-110,
	109,	/* (9053) last move */
	4,
	6,
	16,
	24,
	15,
	87,	/* (9059) last move */
	-43,2,
	23,
	36,
	11,
	26,
	13,
	-23,-126,	/* has siblings */
	7,
	-63,2,
	3,
	-42,2,
	8,
	-49,2,
	55,
	-5,2,
	0,
	-11,2,
	8,
	-9,2,
	4,
	-56,18,	/* last move */
	7,
	41,
	63,81,
	97,	/* (9094) last move */
	-58,32,14,37,	/* reconverge */
	-55,32,14,-89,	/* reconverge */
	12,
	-107,	/* (9104) has sibling */
	100,	/* (9105) last move */
	10,
	-33,-125,	/* has siblings */
	19,
	20,
	24,
	27,
	-86,	/* (9113) has sibling */
	-71,	/* (9114) has sibling */
	40,
	34,
	0,
	14,
	127,59,	/* (9119) last move */
	5,
	2,
	29,
	40,
	16,
	11,
	34,
	49,
	58,
	63,-107,
	47,
	33,
	127,59,	/* (9134) last move */
	40,
	-99,	/* (9137) has sibling */
	16,
	42,
	17,
	34,
	9,
	1,
	6,
	7,
	4,
	63,-121,
	-128,	/* (9149) has sibling */
	22,
	8,
	3,
	15,
	26,
	41,
	33,
	21,
	2,
	5,
	53,
	112,	/* (9161) last move */
	13,
	64,	/* (9163) last move */
	34,
	42,
	-1,59,-112,	/* has siblings */	/* last move */
	57,
	0,
	14,
	127,59,	/* (9172) last move */
	-28,-125,	/* has siblings */
	101,	/* (9176) last move */
	-27,1,
	-44,-125,	/* has siblings */
	28,
	32,
	-109,	/* (9183) has sibling */
	16,
	24,
	17,
	34,
	9,
	27,
	6,
	30,
	0,
	116,	/* (9193) last move */
	24,
	-29,-126,	/* has siblings */
	30,
	25,
	14,
	63,23,
	9,
	30,
	110,	/* (9204) last move */
	17,
	34,
	9,
	27,
	6,
	30,
	0,
	116,	/* (9212) last move */
	0,
	-113,	/* (9214) has sibling */
	8,
	4,
	1,
	84,	/* (9218) last move */
	23,
	-109,	/* (9220) has sibling */
	88,	/* (9221) last move */
	-44,-126,	/* has siblings */
	88,	/* (9224) last move */
	24,
	-20,32,31,19,	/* reconverge */
	62,
	-26,-122,	/* has siblings */
	-25,-121,	/* has siblings */
	-1,-77,7,
	11,
	62,
	-46,7,
	62,
	-63,-126,	/* has siblings */
	64,	/* (9245) last move */
	62,
	-128,	/* (9247) has sibling */
	-126,	/* (9248) has sibling */
	-127,	/* (9249) has sibling */
	6,
	23,
	25,
	15,
	14,
	4,
	5,
	76,	/* (9257) last move */
	-54,2,
	79,	/* (9260) last move */
	-49,-126,	/* has siblings */
	1,
	4,
	8,
	23,
	7,
	13,
	3,
	2,
	70,	/* (9271) last move */
	4,
	1,
	-114,	/* (9274) has sibling */
	7,
	13,
	3,
	2,
	26,
	63,81,
	-44,-126,	/* has siblings */
	15,
	8,
	12,
	23,
	25,
	59,
	74,	/* (9290) last move */
	15,
	84,	/* (9292) last move */
	-41,-126,	/* has siblings */
	7,
	13,
	3,
	2,
	6,
	15,
	26,
	12,
	9,
	60,
	63,97,
	63,81,
	10,
	5,
	27,
	22,
	19,
	8,
	17,
	127,49,	/* (9316) last move */
	6,
	7,
	13,
	3,
	2,
	23,
	25,
	15,
	78,	/* (9326) last move */
	76,	/* (9327) last move */
	-19,-121,	/* has siblings */
	33,
	-53,7,
	-112,	/* (9333) has sibling */
	-54,-126,	/* has siblings */
	19,
	-116,	/* (9337) has sibling */
	-123,	/* (9338) has sibling */
	26,
	29,
	7,
	1,
	2,
	4,
	3,
	22,
	127,81,	/* (9347) last move */
	17,
	-35,-126,	/* has siblings */
	27,
	42,
	-107,	/* (9354) has sibling */
	26,
	49,
	54,
	41,
	39,
	31,
	127,113,	/* (9361) last move */
	49,
	54,
	41,
	21,
	63,-110,
	31,
	39,
	26,
	127,-127,	/* (9372) last move */
	27,
	29,
	9,
	-58,-126,	/* has siblings */
	28,
	5,
	78,	/* (9381) last move */
	28,
	6,
	4,
	5,
	25,
	20,
	96,	/* (9388) last move */
	4,
	-123,	/* (9390) has sibling */
	2,
	12,
	1,
	7,
	-61,-126,	/* has siblings */
	0,
	60,
	13,
	26,
	6,
	63,81,
	4,
	54,
	63,-110,
	21,
	39,
	63,113,
	42,
	62,
	-72,	/* (9414) has sibling */
	63,49,
	8,
	22,
	117,	/* (9419) last move */
	79,	/* (9420) last move */
	0,
	3,
	87,	/* (9423) last move */
	0,
	12,
	17,
	27,
	29,
	9,
	-122,	/* (9430) has sibling */
	28,
	69,	/* (9432) last move */
	28,
	6,
	1,
	20,
	32,
	37,
	116,	/* (9439) last move */
	5,
	-47,-126,	/* has siblings */
	19,
	-54,-126,	/* has siblings */
	2,
	12,
	4,
	0,
	1,
	8,
	3,
	22,
	71,	/* (9454) last move */
	47,
	6,
	9,
	0,
	4,
	12,
	15,
	14,
	1,
	23,
	0,
	29,
	42,
	34,
	32,
	58,
	40,
	27,
	48,
	28,
	20,
	37,
	18,
	63,120,
	63,-121,
	24,
	25,
	30,
	14,
	127,105,	/* (9486) last move */
	-45,-126,	/* has siblings */
	4,
	0,
	1,
	7,
	17,
	-52,2,
	6,
	9,
	14,
	20,
	8,
	25,
	23,
	18,
	15,
	14,
	66,	/* (9507) last move */
	4,
	12,
	40,
	28,
	18,
	112,	/* (9513) last move */
	-22,2,
	19,
	63,-91,
	98,	/* (9519) last move */
	-1,-77,-121,	/* has siblings */
	-25,7,
	21,
	31,
	-48,-126,	/* has siblings */
	42,
	29,
	58,
	34,
	84,	/* (9533) last move */
	2,
	-115,	/* (9535) has sibling */
	7,
	3,
	4,
	1,
	26,
	60,
	16,
	42,
	91,	/* (9544) last move */
	-121,	/* (9545) has sibling */
	12,
	9,
	98,	/* (9548) last move */
	-59,-125,	/* has siblings */
	7,
	-60,-126,	/* has siblings */
	-118,	/* (9554) has sibling */
	-125,	/* (9555) has sibling */
	26,
	-109,	/* (9557) has sibling */
	16,
	29,
	63,49,
	56,
	60,
	72,	/* (9564) last move */
	60,
	91,	/* (9566) last move */
	-109,	/* (9567) has sibling */
	67,	/* (9568) last move */
	13,
	90,	/* (9570) last move */
	3,
	11,
	13,
	26,
	15,
	23,
	65,	/* (9577) last move */
	-127,	/* (9578) has sibling */
	-125,	/* (9579) has sibling */
	22,
	11,
	63,49,
	29,
	13,
	83,	/* (9586) last move */
	-53,-126,	/* has siblings */
	-125,	/* (9589) has sibling */
	93,	/* (9590) last move */
	19,
	3,
	22,
	56,
	8,
	63,-110,
	63,-94,
	54,
	109,	/* (9601) last move */
	10,
	13,
	-53,-126,	/* has siblings */
	67,	/* (9606) last move */
	90,	/* (9607) last move */
	13,
	11,
	3,
	4,
	1,
	12,
	-99,	/* (9614) has sibling */
	-109,	/* (9615) has sibling */
	47,
	92,	/* (9617) last move */
	10,
	0,
	28,
	111,	/* (9621) last move */
	10,
	16,
	19,
	29,
	91,	/* (9626) last move */
	4,
	19,
	58,
	98,	/* (9630) last move */
	62,
	-19,7,
	62,
	-77,	/* (9635) has sibling */
	12,
	-119,	/* (9637) has sibling */
	-57,-125,	/* has siblings */
	-126,	/* (9640) has sibling */
	-115,	/* (9641) has sibling */
	-127,	/* (9642) has sibling */
	11,
	21,
	10,
	41,
	26,
	106,	/* (9648) last move */
	11,
	1,
	0,
	-102,	/* (9652) has sibling */
	3,
	8,
	22,
	21,
	16,
	33,
	31,
	113,	/* (9660) last move */
	8,
	26,
	62,
	63,49,
	56,
	22,
	3,
	63,17,
	15,
	23,
	53,
	55,
	59,
	63,81,
	117,	/* (9678) last move */
	1,
	13,
	0,
	4,
	22,
	6,
	63,81,
	63,97,
	60,
	90,	/* (9690) last move */
	-124,	/* (9691) has sibling */
	-51,-126,	/* has siblings */
	-53,-126,	/* has siblings */
	26,
	21,
	72,	/* (9698) last move */
	3,
	21,
	93,	/* (9701) last move */
	21,
	13,
	26,
	2,
	93,	/* (9706) last move */
	-115,	/* (9707) has sibling */
	2,
	3,
	4,
	1,
	26,
	60,
	85,	/* (9714) last move */
	-61,2,
	-127,	/* (9717) has sibling */
	-115,	/* (9718) has sibling */
	4,
	2,
	64,	/* (9721) last move */
	8,
	2,
	4,
	63,49,
	22,
	90,	/* (9728) last move */
	4,
	-126,	/* (9730) has sibling */
	0,
	1,
	23,
	25,
	8,
	17,
	19,
	16,
	91,	/* (9739) last move */
	0,
	2,
	1,
	97,	/* (9743) last move */
	-124,	/* (9744) has sibling */
	0,
	2,
	7,
	97,	/* (9748) last move */
	2,
	4,
	5,
	21,
	16,
	31,
	42,
	-99,	/* (9756) has sibling */
	40,
	10,
	19,
	16,
	98,	/* (9761) last move */
	49,
	104,	/* (9763) last move */
	10,
	14,
	-58,-126,	/* has siblings */
	9,
	17,
	28,
	27,
	23,
	15,
	25,
	1,
	34,
	104,	/* (9777) last move */
	-128,	/* (9778) has sibling */
	92,	/* (9779) last move */
	-119,	/* (9780) has sibling */
	6,
	-128,	/* (9782) has sibling */
	84,	/* (9783) last move */
	18,
	-103,	/* (9785) has sibling */
	0,
	20,
	17,
	-40,-126,	/* has siblings */
	28,
	37,
	96,	/* (9793) last move */
	32,
	27,
	88,	/* (9796) last move */
	0,
	25,
	23,
	-60,-125,	/* has siblings */
	78,	/* (9802) last move */
	14,
	-62,-126,	/* has siblings */
	-124,	/* (9806) has sibling */
	5,
	65,	/* (9808) last move */
	5,
	4,
	13,
	1,
	75,	/* (9813) last move */
	4,
	2,
	1,
	-101,	/* (9817) has sibling */
	22,
	7,
	119,	/* (9820) last move */
	40,
	22,
	7,
	-5,-126,	/* has siblings */
	56,
	8,
	117,	/* (9828) last move */
	119,	/* (9829) last move */
	25,
	23,
	-119,	/* (9832) has sibling */
	18,
	6,
	-34,-126,	/* has siblings */
	15,
	127,22,	/* (9838) last move */
	-108,	/* (9840) has sibling */
	15,
	92,	/* (9842) last move */
	15,
	-104,	/* (9844) has sibling */
	-44,-126,	/* has siblings */
	94,	/* (9847) last move */
	30,
	20,
	35,
	100,	/* (9851) last move */
	-34,2,
	24,
	35,
	100,	/* (9856) last move */
	-110,	/* (9857) has sibling */
	9,
	15,
	-64,-126,	/* has siblings */
	63,21,
	1,
	4,
	6,
	23,
	5,
	2,
	11,
	21,
	26,
	67,	/* (9873) last move */
	-79,	/* (9874) has sibling */
	19,
	-89,	/* (9876) has sibling */
	27,
	30,
	63,21,
	24,
	20,
	28,
	37,
	44,
	32,
	52,
	48,
	100,	/* (9889) last move */
	27,
	42,
	30,
	63,22,
	24,
	20,
	32,
	35,
	28,
	63,23,
	101,	/* (9902) last move */
	30,
	63,21,
	88,	/* (9906) last move */
	15,
	6,
	-65,21,	/* (9909) has sibling */
	0,
	23,
	18,
	62,
	-127,	/* (9915) has sibling */
	4,
	-123,	/* (9917) has sibling */
	2,
	11,
	21,
	22,
	3,
	8,
	7,
	30,
	56,
	119,	/* (9927) last move */
	-56,-126,	/* has siblings */
	86,	/* (9930) last move */
	86,	/* (9931) last move */
	4,
	92,	/* (9933) last move */
	0,
	30,
	63,21,
	-44,-126,	/* has siblings */
	24,
	82,	/* (9941) last move */
	-46,-126,	/* has siblings */
	92,	/* (9944) last move */
	32,
	-46,7,
	24,
	9,
	17,
	35,
	100,	/* (9952) last move */
	-65,-61,	/* (9953) has sibling */
	-1,-91,-112,	/* has siblings */	/* last move */
	62,
	33,
	-1,-107,-126,	/* has siblings */
	2,
	5,
	11,
	7,
	93,	/* (9967) last move */
	-79,	/* (9968) has sibling */
	-126,	/* (9969) has sibling */
	-115,	/* (9970) has sibling */
	7,
	-125,	/* (9972) has sibling */
	4,
	1,
	26,
	60,
	29,
	11,
	21,
	5,
	12,
	41,
	10,
	127,97,	/* (9984) last move */
	26,
	4,
	1,
	0,
	8,
	6,
	11,
	84,	/* (9993) last move */
	-123,	/* (9994) has sibling */
	-57,-125,	/* has siblings */
	-115,	/* (9997) has sibling */
	11,
	-38,-125,	/* has siblings */
	-118,	/* (10001) has sibling */
	4,
	-61,-126,	/* has siblings */
	105,	/* (10005) last move */
	31,
	3,
	63,-107,
	63,-91,
	63,-90,
	63,-75,
	63,-74,
	63,-59,
	39,
	57,
	127,-106,	/* (10022) last move */
	4,
	1,
	12,
	-107,	/* (10027) has sibling */
	10,
	16,
	29,
	31,
	0,
	8,
	79,	/* (10034) last move */
	-128,	/* (10035) has sibling */
	10,
	41,
	95,	/* (10038) last move */
	10,
	21,
	0,
	-99,	/* (10042) has sibling */
	19,
	27,
	6,
	106,	/* (10046) last move */
	19,
	16,
	31,
	29,
	42,
	40,
	127,-76,	/* (10053) last move */
	3,
	4,
	1,
	26,
	-116,	/* (10059) has sibling */
	93,	/* (10060) last move */
	60,
	12,
	10,
	-48,-112,	/* has siblings */	/* last move */
	93,	/* (10066) last move */
	4,
	11,
	3,
	93,	/* (10070) last move */
	-54,-125,	/* has siblings */
	-117,	/* (10073) has sibling */
	13,
	16,
	19,
	21,
	29,
	42,
	58,
	31,
	4,
	12,
	6,
	17,
	9,
	28,
	1,
	20,
	14,
	18,
	89,	/* (10092) last move */
	12,
	7,
	-117,	/* (10095) has sibling */
	13,
	16,
	65,	/* (10098) last move */
	13,
	11,
	3,
	83,	/* (10102) last move */
	11,
	7,
	93,	/* (10105) last move */
	-121,	/* (10106) has sibling */
	93,	/* (10107) last move */
	4,
	7,
	3,
	29,
	63,-106,
	111,	/* (10114) last move */
	-33,-126,	/* has siblings */
	42,
	29,
	40,
	19,
	11,
	63,-107,
	58,
	63,-91,
	63,-76,
	57,
	127,-77,	/* (10130) last move */
	29,
	11,
	31,
	63,-107,
	47,
	127,-105,	/* (10138) last move */
	63,-91,
	42,
	63,-59,
	127,-121,	/* (10145) last move */
	7,
	-127,	/* (10148) has sibling */
	2,
	-123,	/* (10150) has sibling */
	11,
	10,
	3,
	22,
	4,
	-120,	/* (10156) has sibling */
	16,
	21,
	31,
	-122,	/* (10160) has sibling */
	105,	/* (10161) last move */
	41,
	76,	/* (10163) last move */
	0,
	56,
	8,
	21,
	16,
	127,-110,	/* (10169) last move */
	-125,	/* (10171) has sibling */
	97,	/* (10172) last move */
	21,
	3,
	-123,	/* (10175) has sibling */
	11,
	10,
	41,
	54,
	26,
	16,
	86,	/* (10182) last move */
	22,
	-52,-112,	/* has siblings */	/* last move */
	11,
	16,
	4,
	0,
	56,
	8,
	83,	/* (10192) last move */
	-125,	/* (10193) has sibling */
	4,
	-64,-126,	/* has siblings */
	2,
	1,
	97,	/* (10199) last move */
	1,
	10,
	2,
	91,	/* (10203) last move */
	-124,	/* (10204) has sibling */
	-63,-126,	/* has siblings */
	13,
	22,
	15,
	8,
	63,49,
	60,
	119,	/* (10214) last move */
	-51,-126,	/* has siblings */
	-53,-126,	/* has siblings */
	1,
	26,
	22,
	15,
	72,	/* (10223) last move */
	3,
	33,
	11,
	90,	/* (10227) last move */
	2,
	3,
	97,	/* (10230) last move */
	-115,	/* (10231) has sibling */
	2,
	3,
	4,
	1,
	26,
	60,
	29,
	11,
	21,
	5,
	12,
	41,
	10,
	63,97,
	97,	/* (10247) last move */
	2,
	-51,-125,	/* has siblings */
	3,
	75,	/* (10252) last move */
	-63,7,
	-115,	/* (10255) has sibling */
	0,
	6,
	23,
	25,
	15,
	18,
	67,	/* (10262) last move */
	0,
	-51,-125,	/* has siblings */
	3,
	-120,	/* (10267) has sibling */
	22,
	11,
	10,
	16,
	19,
	105,	/* (10273) last move */
	11,
	10,
	21,
	16,
	63,-110,
	39,
	33,
	31,
	63,-94,
	127,-77,	/* (10285) last move */
	8,
	13,
	127,49,	/* (10289) last move */
	-58,10,
	-59,-126,	/* has siblings */
	1,
	0,
	68,	/* (10297) last move */
	62,
	-127,	/* (10299) has sibling */
	-110,	/* (10300) has sibling */
	4,
	2,
	-61,-126,	/* has siblings */
	23,
	15,
	38,
	64,	/* (10308) last move */
	15,
	97,	/* (10310) last move */
	-82,	/* (10311) has sibling */
	4,
	2,
	3,
	33,
	24,
	127,90,	/* (10317) last move */
	3,
	0,
	13,
	36,
	62,
	-117,	/* (10324) has sibling */
	5,
	16,
	12,
	104,	/* (10328) last move */
	62,
	-112,	/* (10330) has sibling */
	-52,-125,	/* has siblings */
	5,
	4,
	38,
	17,
	2,
	7,
	21,
	8,
	15,
	55,
	82,	/* (10343) last move */
	11,
	74,	/* (10345) last move */
	-116,	/* (10346) has sibling */
	17,
	19,
	11,
	4,
	73,	/* (10351) last move */
	-48,-112,	/* has siblings */	/* last move */
	74,	/* (10354) last move */
	-91,	/* (10355) has sibling */
	2,
	13,
	5,
	26,
	1,
	71,	/* (10361) last move */
	62,
	-126,	/* (10363) has sibling */
	13,
	5,
	11,
	-121,	/* (10367) has sibling */
	3,
	26,
	60,
	85,	/* (10371) last move */
	26,
	41,
	-118,	/* (10374) has sibling */
	-65,97,	/* (10375) has sibling */
	85,	/* (10377) last move */
	-43,18,	/* last move */
	7,
	63,97,
	1,
	62,
	-112,	/* (10385) has sibling */
	103,	/* (10386) last move */
	10,
	62,
	-47,-112,	/* has siblings */	/* last move */
	19,
	29,
	27,
	-61,23,	/* last move */
	62,
	-118,	/* (10397) has sibling */
	62,
	-127,	/* (10399) has sibling */
	13,
	4,
	85,	/* (10402) last move */
	62,
	-115,	/* (10404) has sibling */
	23,
	0,
	88,	/* (10407) last move */
	38,
	-124,	/* (10409) has sibling */
	0,
	1,
	3,
	8,
	22,
	2,
	7,
	14,
	15,
	23,
	55,
	20,
	17,
	73,	/* (10423) last move */
	-64,-109,	/* has siblings */	/* last move */
	14,
	0,
	-103,	/* (10428) has sibling */
	26,
	104,	/* (10430) last move */
	-44,-112,	/* has siblings */	/* last move */
	9,
	62,
	-115,	/* (10435) has sibling */
	71,	/* (10436) last move */
	90,	/* (10437) last move */
	-110,	/* (10438) has sibling */
	1,
	-64,-125,	/* has siblings */
	4,
	38,
	14,
	-119,	/* (10445) has sibling */
	-39,1,
	-56,1,
	-61,1,
	-41,1,
	-34,-125,	/* has siblings */
	-126,	/* (10456) has sibling */
	10,
	22,
	24,
	-44,-125,	/* has siblings */
	37,
	34,
	5,
	32,
	63,21,
	15,
	11,
	7,
	13,
	63,49,
	115,	/* (10474) last move */
	69,	/* (10475) last move */
	20,
	24,
	69,	/* (10478) last move */
	2,
	30,
	17,
	84,	/* (10482) last move */
	25,
	-55,-126,	/* has siblings */
	2,
	30,
	23,
	35,
	8,
	101,	/* (10491) last move */
	2,
	-121,	/* (10493) has sibling */
	9,
	15,
	20,
	24,
	13,
	3,
	-7,-112,	/* has siblings */	/* last move */
	108,	/* (10502) last move */
	-50,2,
	71,	/* (10505) last move */
	3,
	0,
	13,
	17,
	-84,	/* (10510) has sibling */
	11,
	21,
	16,
	41,
	31,
	103,	/* (10516) last move */
	16,
	37,
	36,
	18,
	30,
	84,	/* (10522) last move */
	-116,	/* (10523) has sibling */
	-63,-109,	/* has siblings */	/* last move */
	17,
	5,
	20,
	62,
	-127,	/* (10530) has sibling */
	0,
	4,
	38,
	9,
	8,
	2,
	3,
	-109,	/* (10538) has sibling */
	10,
	16,
	11,
	21,
	13,
	26,
	2,
	93,	/* (10546) last move */
	7,
	1,
	74,	/* (10549) last move */
	-101,	/* (10550) has sibling */
	34,
	-81,	/* (10552) has sibling */
	40,
	19,
	48,
	63,120,
	63,121,
	58,
	62,
	127,106,	/* (10562) last move */
	104,	/* (10564) last move */
	38,
	14,
	23,
	40,
	95,	/* (10569) last move */
	-118,	/* (10570) has sibling */
	-63,1,
	-61,1,
	-64,1,
	-115,	/* (10577) has sibling */
	100,	/* (10578) last move */
	22,
	88,	/* (10580) last move */
	38,
	-60,-125,	/* has siblings */
	-64,1,
	-62,1,
	-115,	/* (10588) has sibling */
	7,
	1,
	26,
	9,
	10,
	-61,-112,	/* has siblings */	/* last move */
	81,	/* (10596) last move */
	-114,	/* (10597) has sibling */
	-119,	/* (10598) has sibling */
	-44,32,1,-31,	/* reconverge */
	7,
	1,
	74,	/* (10605) last move */
	-119,	/* (10606) has sibling */
	1,
	7,
	8,
	13,
	-53,-125,	/* has siblings */
	-111,	/* (10613) has sibling */
	19,
	15,
	14,
	25,
	18,
	22,
	91,	/* (10620) last move */
	26,
	14,
	12,
	5,
	21,
	10,
	83,	/* (10627) last move */
	10,
	-117,	/* (10629) has sibling */
	14,
	5,
	12,
	19,
	17,
	16,
	84,	/* (10636) last move */
	19,
	16,
	17,
	5,
	15,
	14,
	25,
	18,
	22,
	59,
	23,
	30,
	3,
	110,	/* (10650) last move */
	1,
	9,
	18,
	-96,	/* (10654) has sibling */
	62,
	-40,-112,	/* has siblings */	/* last move */
	88,	/* (10658) last move */
	74,	/* (10659) last move */
	-50,1,
	-64,1,
	-55,-96,1,-7,	/* reconverge */	/* has siblings */
	20,
	-111,	/* (10669) has sibling */
	9,
	-59,17,	/* last move */
	11,
	63,59,
	-39,-126,	/* has siblings */
	94,	/* (10678) last move */
	-40,3,
	-34,-127,	/* has siblings */
	-18,3,
	-67,	/* (10685) has sibling */
	52,
	5,
	2,
	104,	/* (10689) last move */
	63,42,
	50,
	51,
	108,	/* (10694) last move */
	-27,2,
	36,
	44,
	89,	/* (10699) last move */
	-50,-119,	/* has siblings */
	62,
	64,	/* (10703) last move */
	-52,-126,	/* has siblings */
	-126,	/* (10706) has sibling */
	62,
	102,	/* (10708) last move */
	102,	/* (10709) last move */
	-26,-114,	/* has siblings */
	-1,-76,-119,	/* has siblings */
	62,
	-123,	/* (10716) has sibling */
	43,
	2,
	7,
	62,
	-100,	/* (10721) has sibling */
	13,
	21,
	62,
	-40,-112,	/* has siblings */	/* last move */
	88,	/* (10727) last move */
	17,
	6,
	12,
	13,
	85,	/* (10732) last move */
	-21,22,	/* last move */
	-7,-87,41,-37,	/* reconverge */	/* has siblings */
	-123,	/* (10739) has sibling */
	43,
	2,
	-124,	/* (10742) has sibling */
	12,
	-79,	/* (10744) has sibling */
	14,
	25,
	6,
	15,
	18,
	30,
	88,	/* (10751) last move */
	9,
	109,	/* (10753) last move */
	7,
	39,
	-115,	/* (10756) has sibling */
	85,	/* (10757) last move */
	62,
	-51,7,
	62,
	-128,	/* (10762) has sibling */
	4,
	6,
	15,
	14,
	23,
	73,	/* (10768) last move */
	73,	/* (10769) last move */
	62,
	-59,-106,	/* has siblings */	/* last move */
	-127,	/* (10773) has sibling */
	5,
	2,
	-117,	/* (10776) has sibling */
	-124,	/* (10777) has sibling */
	6,
	26,
	10,
	105,	/* (10781) last move */
	-54,3,
	43,
	13,
	-124,	/* (10786) has sibling */
	7,
	21,
	0,
	6,
	19,
	113,	/* (10792) last move */
	12,
	7,
	80,	/* (10795) last move */
	10,
	11,
	16,
	62,
	-120,	/* (10800) has sibling */
	22,
	79,	/* (10802) last move */
	62,
	-21,-109,	/* has siblings */	/* last move */
	62,
	-43,-112,	/* has siblings */	/* last move */
	23,
	-114,	/* (10810) has sibling */
	25,
	18,
	4,
	6,
	64,	/* (10815) last move */
	-39,2,
	78,	/* (10818) last move */
	-21,-96,-120,23,	/* reconverge */	/* has siblings */
	62,
	-21,22,	/* last move */
	-58,-114,	/* has siblings */
	-21,-96,-88,58,	/* reconverge */	/* has siblings */
	62,
	-82,	/* (10833) has sibling */
	65,	/* (10834) last move */
	-63,-112,	/* has siblings */	/* last move */
	62,
	-21,22,	/* last move */
	-63,-117,	/* has siblings */
	-122,	/* (10842) has sibling */
	5,
	12,
	10,
	0,
	8,
	19,
	80,	/* (10849) last move */
	-119,	/* (10850) has sibling */
	62,
	-59,-112,	/* has siblings */	/* last move */
	-59,-112,	/* has siblings */	/* last move */
	-54,-112,	/* has siblings */	/* last move */
	11,
	108,	/* (10859) last move */
	-124,	/* (10860) has sibling */
	62,
	-64,-112,	/* has siblings */	/* last move */
	-21,1,
	-59,1,
	-128,	/* (10868) has sibling */
	9,
	38,
	98,	/* (10871) last move */
	23,
	20,
	-38,-112,	/* has siblings */	/* last move */
	62,
	66,	/* (10877) last move */
	62,
	-55,-122,	/* has siblings */
	43,
	4,
	5,
	-126,	/* (10884) has sibling */
	7,
	13,
	6,
	0,
	10,
	38,
	26,
	3,
	75,	/* (10893) last move */
	6,
	2,
	3,
	103,	/* (10897) last move */
	62,
	-58,22,	/* last move */
	-60,5,
	-25,-121,	/* has siblings */
	63,-77,
	11,
	2,
	10,
	73,	/* (10910) last move */
	62,
	-44,-126,	/* has siblings */
	62,
	-48,-62,	/* has siblings */
	-85,	/* (10917) has sibling */
	1,
	-125,	/* (10919) has sibling */
	0,
	2,
	86,	/* (10922) last move */
	-128,	/* (10923) has sibling */
	6,
	3,
	8,
	2,
	86,	/* (10928) last move */
	2,
	-64,-112,	/* has siblings */	/* last move */
	3,
	-64,-125,	/* has siblings */
	38,
	15,
	23,
	6,
	5,
	10,
	12,
	19,
	17,
	85,	/* (10944) last move */
	-59,2,
	70,	/* (10947) last move */
	1,
	43,
	0,
	38,
	23,
	14,
	25,
	30,
	7,
	13,
	3,
	75,	/* (10959) last move */
	43,
	-126,	/* (10961) has sibling */
	-64,-126,	/* has siblings */
	38,
	5,
	6,
	13,
	11,
	26,
	1,
	7,
	85,	/* (10972) last move */
	-63,-126,	/* has siblings */
	13,
	23,
	14,
	86,	/* (10978) last move */
	13,
	5,
	7,
	26,
	0,
	38,
	15,
	23,
	22,
	85,	/* (10988) last move */
	1,
	-128,	/* (10990) has sibling */
	6,
	3,
	8,
	2,
	86,	/* (10995) last move */
	2,
	3,
	19,
	6,
	97,	/* (11000) last move */
	62,
	-18,-121,	/* has siblings */
	11,
	62,
	-119,	/* (11006) has sibling */
	-62,-125,	/* has siblings */
	-116,	/* (11009) has sibling */
	90,	/* (11010) last move */
	10,
	12,
	13,
	7,
	5,
	6,
	43,
	65,	/* (11018) last move */
	10,
	16,
	19,
	2,
	6,
	3,
	73,	/* (11025) last move */
	-71,	/* (11026) has sibling */
	14,
	-116,	/* (11028) has sibling */
	1,
	43,
	0,
	28,
	101,	/* (11033) last move */
	9,
	20,
	-90,	/* (11036) has sibling */
	17,
	6,
	18,
	10,
	16,
	19,
	13,
	7,
	43,
	1,
	2,
	-61,23,	/* last move */
	17,
	-90,	/* (11051) has sibling */
	0,
	16,
	2,
	13,
	10,
	26,
	7,
	40,
	34,
	36,
	62,
	-61,-112,	/* has siblings */	/* last move */
	87,	/* (11065) last move */
	-100,	/* (11066) has sibling */
	34,
	-127,	/* (11068) has sibling */
	38,
	18,
	43,
	7,
	25,
	22,
	94,	/* (11075) last move */
	27,
	38,
	18,
	43,
	40,
	112,	/* (11081) last move */
	-128,	/* (11082) has sibling */
	38,
	23,
	6,
	30,
	1,
	15,
	28,
	37,
	62,
	-21,23,	/* last move */
	1,
	-90,	/* (11095) has sibling */
	-46,-125,	/* has siblings */
	107,	/* (11098) last move */
	43,
	82,	/* (11100) last move */
	43,
	0,
	38,
	23,
	6,
	18,
	25,
	15,
	28,
	62,
	-91,	/* (11111) has sibling */
	35,
	110,	/* (11113) last move */
	108,	/* (11114) last move */
	62,
	-50,-96,43,20,	/* reconverge */	/* has siblings */
	-83,	/* (11120) has sibling */
	1,
	43,
	0,
	-26,-126,	/* has siblings */
	7,
	13,
	3,
	2,
	26,
	63,81,
	70,	/* (11133) last move */
	6,
	7,
	13,
	3,
	2,
	62,
	-100,	/* (11140) has sibling */
	82,	/* (11141) last move */
	14,
	62,
	-45,-125,	/* has siblings */
	9,
	18,
	20,
	37,
	98,	/* (11150) last move */
	74,	/* (11151) last move */
	-110,	/* (11152) has sibling */
	-85,	/* (11153) has sibling */
	1,
	0,
	6,
	3,
	8,
	13,
	22,
	56,
	102,	/* (11162) last move */
	1,
	43,
	0,
	6,
	7,
	13,
	3,
	2,
	38,
	14,
	19,
	27,
	29,
	98,	/* (11176) last move */
	-50,-127,	/* has siblings */
	-127,	/* (11179) has sibling */
	43,
	7,
	13,
	3,
	2,
	23,
	25,
	38,
	6,
	0,
	92,	/* (11190) last move */
	7,
	1,
	39,
	62,
	-95,	/* (11195) has sibling */
	62,
	-125,	/* (11197) has sibling */
	22,
	15,
	8,
	38,
	6,
	89,	/* (11203) last move */
	-61,17,	/* last move */
	31,
	33,
	10,
	16,
	29,
	85,	/* (11211) last move */
	-118,	/* (11212) has sibling */
	-59,-126,	/* has siblings */
	-126,	/* (11215) has sibling */
	12,
	43,
	-43,-126,	/* has siblings */
	9,
	17,
	92,	/* (11222) last move */
	80,	/* (11223) last move */
	12,
	2,
	6,
	16,
	19,
	43,
	1,
	3,
	22,
	8,
	0,
	56,
	21,
	33,
	41,
	53,
	103,	/* (11240) last move */
	16,
	-126,	/* (11242) has sibling */
	19,
	5,
	-107,	/* (11245) has sibling */
	-35,-109,	/* has siblings */	/* last move */
	73,	/* (11248) last move */
	-99,	/* (11249) has sibling */
	-119,	/* (11250) has sibling */
	62,
	-23,7,
	-38,23,	/* last move */
	21,
	33,
	26,
	41,
	73,	/* (11260) last move */
	26,
	-99,	/* (11262) has sibling */
	13,
	-33,-125,	/* has siblings */
	21,
	1,
	27,
	42,
	103,	/* (11270) last move */
	7,
	31,
	27,
	42,
	40,
	1,
	-21,-126,	/* has siblings */
	-64,17,	/* last move */
	0,
	3,
	43,
	124,	/* (11284) last move */
	73,	/* (11285) last move */
	19,
	2,
	43,
	45,
	73,	/* (11290) last move */
	-126,	/* (11291) has sibling */
	10,
	9,
	109,	/* (11294) last move */
	-21,-110,	/* has siblings */	/* last move */
	-55,1,
	-40,-121,	/* has siblings */
	-65,59,	/* (11301) has sibling */
	-52,-120,	/* has siblings */
	6,
	28,
	-111,	/* (11307) has sibling */
	27,
	34,
	32,
	10,
	-112,	/* (11312) has sibling */
	29,
	40,
	106,	/* (11315) last move */
	40,
	66,	/* (11317) last move */
	-44,2,
	37,
	17,
	27,
	34,
	32,
	19,
	40,
	5,
	93,	/* (11328) last move */
	-100,	/* (11329) has sibling */
	-44,-126,	/* has siblings */
	37,
	32,
	34,
	52,
	94,	/* (11336) last move */
	-109,	/* (11337) has sibling */
	31,
	40,
	7,
	1,
	63,90,
	127,76,	/* (11344) last move */
	-118,	/* (11346) has sibling */
	16,
	19,
	-85,	/* (11349) has sibling */
	1,
	2,
	79,	/* (11352) last move */
	13,
	-121,	/* (11354) has sibling */
	43,
	1,
	2,
	3,
	-26,-126,	/* has siblings */
	14,
	25,
	23,
	15,
	0,
	63,21,
	70,	/* (11368) last move */
	45,
	78,	/* (11370) last move */
	2,
	43,
	5,
	41,
	1,
	7,
	29,
	31,
	47,
	52,
	63,91,
	127,121,	/* (11383) last move */
	27,
	16,
	17,
	52,
	127,91,	/* (11389) last move */
	32,
	27,
	63,91,
	63,77,
	16,
	66,	/* (11398) last move */
	-83,	/* (11399) has sibling */
	7,
	1,
	33,
	63,-91,
	42,
	28,
	63,59,
	16,
	31,
	63,-110,
	54,
	67,	/* (11414) last move */
	-100,	/* (11415) has sibling */
	-96,	/* (11416) has sibling */
	48,
	52,
	2,
	10,
	113,	/* (11421) last move */
	63,59,
	2,
	10,
	113,	/* (11426) last move */
	-46,2,
	37,
	-62,2,
	10,
	77,	/* (11433) last move */
	-21,-125,	/* has siblings */
	-63,1,
	-125,	/* (11438) has sibling */
	-62,-125,	/* has siblings */
	7,
	13,
	-128,	/* (11443) has sibling */
	5,
	38,
	78,	/* (11446) last move */
	5,
	26,
	10,
	21,
	16,
	33,
	-122,	/* (11453) has sibling */
	0,
	38,
	-105,	/* (11456) has sibling */
	14,
	25,
	18,
	86,	/* (11460) last move */
	22,
	17,
	-56,7,
	15,
	117,	/* (11466) last move */
	0,
	6,
	79,	/* (11469) last move */
	-106,	/* (11470) has sibling */
	-57,-126,	/* has siblings */
	0,
	39,
	110,	/* (11475) last move */
	13,
	63,49,
	-57,-126,	/* has siblings */
	64,	/* (11481) last move */
	-4,-126,	/* has siblings */
	71,	/* (11484) last move */
	5,
	-121,	/* (11486) has sibling */
	6,
	0,
	81,	/* (11489) last move */
	6,
	7,
	79,	/* (11492) last move */
	-64,2,
	77,	/* (11495) last move */
	-59,3,
	-122,	/* (11498) has sibling */
	3,
	7,
	13,
	66,	/* (11502) last move */
	7,
	-58,-125,	/* has siblings */
	2,
	0,
	43,
	12,
	38,
	14,
	15,
	84,	/* (11513) last move */
	77,	/* (11514) last move */
	-127,	/* (11515) has sibling */
	43,
	7,
	-128,	/* (11518) has sibling */
	22,
	8,
	67,	/* (11521) last move */
	13,
	-106,	/* (11523) has sibling */
	60,
	3,
	2,
	25,
	-98,	/* (11528) has sibling */
	23,
	82,	/* (11530) last move */
	74,	/* (11531) last move */
	3,
	2,
	23,
	-39,-125,	/* has siblings */
	38,
	14,
	6,
	12,
	17,
	0,
	15,
	19,
	63,21,
	-73,	/* (11547) has sibling */
	8,
	117,	/* (11549) last move */
	-65,22,	/* (11550) has sibling */
	119,	/* (11552) last move */
	-36,3,
	-37,19,	/* last move */
	10,
	82,	/* (11558) last move */
	-83,	/* (11559) has sibling */
	-82,	/* (11560) has sibling */
	62,
	-21,-112,	/* has siblings */	/* last move */
	-44,-125,	/* has siblings */
	-110,	/* (11566) has sibling */
	24,
	37,
	28,
	-50,-126,	/* has siblings */
	44,
	35,
	32,
	114,	/* (11575) last move */
	-47,-126,	/* has siblings */
	44,
	32,
	34,
	36,
	99,	/* (11582) last move */
	32,
	14,
	30,
	-26,-126,	/* has siblings */
	6,
	35,
	34,
	17,
	27,
	12,
	10,
	25,
	5,
	63,23,
	18,
	30,
	85,	/* (11601) last move */
	34,
	25,
	48,
	63,90,
	-1,106,23,	/* last move */
	17,
	44,
	51,
	52,
	63,90,
	63,105,
	63,123,
	127,-119,	/* (11620) last move */
	-63,7,
	-64,-126,	/* has siblings */
	107,	/* (11626) last move */
	43,
	7,
	-128,	/* (11629) has sibling */
	22,
	8,
	67,	/* (11632) last move */
	13,
	3,
	2,
	23,
	25,
	38,
	14,
	62,
	-1,21,-112,	/* has siblings */	/* last move */
	6,
	12,
	62,
	-1,21,-112,	/* has siblings */	/* last move */
	0,
	81,	/* (11651) last move */
	-31,-105,	/* has siblings */	/* last move */
	-57,-112,	/* has siblings */	/* last move */
	-62,2,
	74,	/* (11658) last move */
	-25,-126,	/* has siblings */
	2,
	74,	/* (11662) last move */
	-71,	/* (11663) has sibling */
	-18,-96,-83,40,	/* reconverge */	/* has siblings */
	62,
	7,
	65,	/* (11670) last move */
	-79,	/* (11671) has sibling */
	2,
	10,
	-1,-77,23,	/* last move */
	7,
	-18,-126,	/* has siblings */
	1,
	64,	/* (11681) last move */
	-95,	/* (11682) has sibling */
	-107,	/* (11683) has sibling */
	31,
	1,
	83,	/* (11686) last move */
	1,
	2,
	-59,-125,	/* has siblings */
	-118,	/* (11691) has sibling */
	12,
	43,
	6,
	0,
	38,
	15,
	16,
	-102,	/* (11699) has sibling */
	19,
	77,	/* (11701) last move */
	83,	/* (11702) last move */
	-21,2,
	13,
	10,
	16,
	19,
	85,	/* (11709) last move */
	13,
	10,
	-43,-126,	/* has siblings */
	16,
	54,
	63,-110,
	41,
	103,	/* (11719) last move */
	-112,	/* (11720) has sibling */
	-102,	/* (11721) has sibling */
	21,
	41,
	29,
	0,
	15,
	23,
	8,
	113,	/* (11729) last move */
	21,
	26,
	93,	/* (11732) last move */
	-103,	/* (11733) has sibling */
	26,
	102,	/* (11735) last move */
	26,
	80,	/* (11737) last move */
	1,
	62,
	-95,	/* (11740) has sibling */
	19,
	42,
	34,
	101,	/* (11744) last move */
	39,
	62,
	-125,	/* (11747) has sibling */
	62,
	-90,	/* (11749) has sibling */
	6,
	8,
	15,
	0,
	43,
	23,
	22,
	59,
	13,
	56,
	66,	/* (11760) last move */
	22,
	43,
	64,	/* (11763) last move */
	-33,-96,43,-57,	/* reconverge */	/* has siblings */
	62,
	-104,	/* (11769) has sibling */
	-61,17,	/* last move */
	46,
	-61,-96,45,-28,	/* reconverge */	/* has siblings */
	62,
	-33,32,43,-57,	/* reconverge */
	-7,-121,	/* has siblings */
	62,
	11,
	-118,	/* (11786) has sibling */
	-112,	/* (11787) has sibling */
	2,
	19,
	5,
	21,
	73,	/* (11792) last move */
	-59,3,
	-126,	/* (11795) has sibling */
	12,
	43,
	80,	/* (11798) last move */
	12,
	2,
	6,
	16,
	19,
	43,
	1,
	3,
	22,
	8,
	0,
	120,	/* (11810) last move */
	-55,32,-83,-108,	/* reconverge */
	-18,-121,	/* has siblings */
	62,
	11,
	14,
	1,
	43,
	7,
	13,
	3,
	2,
	23,
	-90,	/* (11827) has sibling */
	25,
	74,	/* (11829) last move */
	-39,2,
	38,
	6,
	-55,19,	/* last move */
	62,
	-1,-77,-121,	/* has siblings */
	11,
	2,
	10,
	9,
	39,
	62,
	1,
	-85,	/* (11847) has sibling */
	15,
	0,
	22,
	3,
	8,
	23,
	7,
	13,
	127,49,	/* (11856) last move */
	-102,	/* (11858) has sibling */
	41,
	7,
	23,
	38,
	25,
	15,
	100,	/* (11865) last move */
	-121,	/* (11866) has sibling */
	3,
	26,
	87,	/* (11869) last move */
	-128,	/* (11870) has sibling */
	62,
	-85,	/* (11872) has sibling */
	7,
	67,	/* (11874) last move */
	13,
	7,
	43,
	3,
	5,
	67,	/* (11880) last move */
	13,
	38,
	6,
	-103,	/* (11884) has sibling */
	14,
	23,
	30,
	86,	/* (11888) last move */
	14,
	0,
	15,
	18,
	30,
	8,
	23,
	3,
	24,
	84,	/* (11898) last move */
	62,
	-55,-121,	/* has siblings */
	-85,	/* (11902) has sibling */
	-126,	/* (11903) has sibling */
	-51,-125,	/* has siblings */
	-63,-125,	/* has siblings */
	7,
	11,
	3,
	38,
	15,
	8,
	5,
	6,
	22,
	23,
	3,
	59,
	-38,-112,	/* has siblings */	/* last move */
	-43,3,
	26,
	10,
	41,
	97,	/* (11927) last move */
	5,
	7,
	26,
	0,
	38,
	8,
	15,
	1,
	23,
	86,	/* (11937) last move */
	0,
	6,
	3,
	13,
	89,	/* (11942) last move */
	1,
	-62,-126,	/* has siblings */
	3,
	5,
	76,	/* (11948) last move */
	-59,32,42,-124,	/* reconverge */
	1,
	43,
	15,
	-103,	/* (11956) has sibling */
	7,
	13,
	86,	/* (11959) last move */
	3,
	22,
	7,
	89,	/* (11963) last move */
	62,
	-26,-119,	/* has siblings */
	1,
	43,
	7,
	-51,1,
	22,
	-49,17,	/* last move */
	62,
	-50,-105,	/* has siblings */	/* last move */
	62,
	-46,-119,	/* has siblings */
	-85,	/* (11981) has sibling */
	-126,	/* (11982) has sibling */
	-115,	/* (11983) has sibling */
	5,
	-57,32,46,-102,	/* reconverge */
	1,
	13,
	23,
	9,
	-42,17,	/* last move */
	1,
	2,
	3,
	5,
	6,
	97,	/* (12000) last move */
	1,
	43,
	0,
	62,
	-90,	/* (12005) has sibling */
	89,	/* (12006) last move */
	-61,3,
	14,
	9,
	6,
	12,
	38,
	20,
	22,
	7,
	30,
	35,
	63,22,
	100,	/* (12021) last move */
	62,
	-19,-121,	/* has siblings */
	62,
	-55,-119,	/* has siblings */
	-85,	/* (12028) has sibling */
	1,
	5,
	6,
	11,
	62,
	-57,-112,	/* has siblings */	/* last move */
	7,
	77,	/* (12037) last move */
	-63,32,46,-78,	/* reconverge */
	62,
	-90,	/* (12043) has sibling */
	-63,32,46,-64,	/* reconverge */
	62,
	-50,-119,	/* has siblings */
	1,
	43,
	7,
	13,
	22,
	60,
	63,49,
	67,	/* (12059) last move */
	62,
	-84,	/* (12061) has sibling */
	1,
	62,
	-64,-112,	/* has siblings */	/* last move */
	62,
	-57,-94,47,22,	/* reconverge */	/* has siblings */
	-21,1,
	-64,1,
	-26,1,
	-41,1,
	-103,	/* (12079) has sibling */
	-57,-125,	/* has siblings */
	13,
	2,
	6,
	26,
	3,
	63,81,
	86,	/* (12089) last move */
	14,
	6,
	30,
	-113,	/* (12093) has sibling */
	-63,-96,47,20,	/* reconverge */	/* has siblings */
	63,22,
	72,	/* (12100) last move */
	9,
	63,22,
	18,
	89,	/* (12105) last move */
	-50,1,
	25,
	18,
	7,
	13,
	3,
	75,	/* (12113) last move */
	62,
	-92,	/* (12115) has sibling */
	-63,32,47,31,	/* reconverge */
	62,
	-82,	/* (12121) has sibling */
	-63,32,47,31,	/* reconverge */
	-63,32,47,31,	/* reconverge */
	62,
	-7,-121,	/* has siblings */
	62,
	-55,-119,	/* has siblings */
	-85,	/* (12136) has sibling */
	-63,32,46,-2,	/* reconverge */
	-63,32,46,-78,	/* reconverge */
	62,
	-26,-119,	/* has siblings */
	-63,32,46,-64,	/* reconverge */
	62,
	-50,-119,	/* has siblings */
	-63,32,47,20,	/* reconverge */
	62,
	-46,-87,46,-51,	/* reconverge */	/* has siblings */
	62,
	-20,-119,	/* has siblings */
	62,
	-46,-87,46,-51,	/* reconverge */	/* has siblings */
	-63,32,47,31,	/* reconverge */
	62,
	-28,-119,	/* has siblings */
	62,
	-46,-87,46,-51,	/* reconverge */	/* has siblings */
	-63,32,47,31,	/* reconverge */
	62,
	-18,-119,	/* has siblings */
	62,
	-46,-87,46,-51,	/* reconverge */	/* has siblings */
	-63,32,47,31,	/* reconverge */
	-63,-94,47,31,	/* reconverge */	/* has siblings */
	-114,	/* (12204) has sibling */
	62,
	-118,	/* (12206) has sibling */
	9,
	20,
	28,
	38,
	5,
	3,
	92,	/* (12213) last move */
	-84,	/* (12214) has sibling */
	17,
	10,
	15,
	1,
	114,	/* (12219) last move */
	-104,	/* (12220) has sibling */
	1,
	43,
	0,
	62,
	-55,-126,	/* has siblings */
	38,
	23,
	15,
	8,
	25,
	59,
	-111,	/* (12233) has sibling */
	30,
	20,
	63,22,
	18,
	25,
	98,	/* (12240) last move */
	94,	/* (12241) last move */
	6,
	-125,	/* (12243) has sibling */
	73,	/* (12244) last move */
	25,
	92,	/* (12246) last move */
	-64,-110,	/* has siblings */	/* last move */
	10,
	15,
	1,
	100,	/* (12252) last move */
	62,
	-13,-55,	/* has siblings */
	-63,-94,47,31,	/* reconverge */	/* has siblings */
	11,
	-21,-110,	/* has siblings */	/* last move */
	62,
	-50,-64,	/* has siblings */
	-127,	/* (12266) has sibling */
	12,
	5,
	10,
	19,
	17,
	93,	/* (12272) last move */
	-55,32,43,27,	/* reconverge */
	2,
	10,
	-55,1,
	31,
	39,
	33,
	62,
	47,
	62,
	-63,32,46,71,	/* reconverge */
	11,
	62,
	-119,	/* (12293) has sibling */
	17,
	-127,	/* (12295) has sibling */
	-108,	/* (12296) has sibling */
	43,
	78,	/* (12298) last move */
	43,
	0,
	6,
	38,
	20,
	14,
	83,	/* (12305) last move */
	28,
	12,
	-127,	/* (12308) has sibling */
	43,
	0,
	3,
	6,
	10,
	84,	/* (12314) last move */
	37,
	107,	/* (12316) last move */
	-21,-110,	/* has siblings */	/* last move */
	-118,	/* (12319) has sibling */
	16,
	-126,	/* (12321) has sibling */
	19,
	-59,1,
	26,
	73,	/* (12326) last move */
	19,
	2,
	43,
	33,
	73,	/* (12331) last move */
	-62,32,47,-10,	/* reconverge */
	62,
	-61,-110,	/* has siblings */	/* last move */
	62,
	-21,-110,	/* has siblings */	/* last move */
	62,
	-63,-110,	/* has siblings */	/* last move */
	-63,-94,47,31,	/* reconverge */	/* has siblings */
	-54,-123,	/* has siblings */
	62,
	-55,-64,	/* has siblings */
	-128,	/* (12354) has sibling */
	27,
	89,	/* (12356) last move */
	12,
	1,
	0,
	43,
	17,
	5,
	84,	/* (12363) last move */
	-119,	/* (12364) has sibling */
	43,
	1,
	-125,	/* (12367) has sibling */
	2,
	7,
	5,
	64,	/* (12371) last move */
	2,
	67,	/* (12373) last move */
	-55,-111,	/* has siblings */	/* last move */
	11,
	21,
	66,	/* (12378) last move */
	-53,44,42,-3,	/* reconverge */
	0,
	};
