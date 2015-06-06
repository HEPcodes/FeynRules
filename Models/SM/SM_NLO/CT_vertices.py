# This file was automatically created by FeynRules =.2.2
# Mathematica version: 9.0 for Mac OS X x86 (64-bit) (January 24, 2013)
# Date: Tue 19 Aug 2014 14:20:18


from object_library import all_vertices, all_CTvertices, Vertex, CTVertex
import particles as P
import CT_couplings as C
import lorentz as L


V_1 = CTVertex(name = 'V_1',
               type = 'R2',
               particles = [ P.g, P.g, P.g ],
               color = [ 'f(1,2,3)' ],
               lorentz = [ L.VVV2 ],
               loop_particles = [ [ [P.b], [P.c], [P.d], [P.s], [P.t], [P.u] ], [ [P.g] ] ],
               couplings = {(0,0,0):C.R2GC_138_25,(0,0,1):C.R2GC_138_26})

V_2 = CTVertex(name = 'V_2',
               type = 'R2',
               particles = [ P.g, P.g, P.g, P.g ],
               color = [ 'd(-1,1,3)*d(-1,2,4)', 'd(-1,1,3)*f(-1,2,4)', 'd(-1,1,4)*d(-1,2,3)', 'd(-1,1,4)*f(-1,2,3)', 'd(-1,2,3)*f(-1,1,4)', 'd(-1,2,4)*f(-1,1,3)', 'f(-1,1,2)*f(-1,3,4)', 'f(-1,1,3)*f(-1,2,4)', 'f(-1,1,4)*f(-1,2,3)', 'Identity(1,2)*Identity(3,4)', 'Identity(1,3)*Identity(2,4)', 'Identity(1,4)*Identity(2,3)' ],
               lorentz = [ L.VVVV2, L.VVVV3, L.VVVV5, L.VVVV9 ],
               loop_particles = [ [ [P.b], [P.c], [P.d], [P.s], [P.t], [P.u] ], [ [P.g] ] ],
               couplings = {(2,0,0):C.R2GC_101_3,(2,0,1):C.R2GC_101_4,(0,0,0):C.R2GC_101_3,(0,0,1):C.R2GC_101_4,(4,0,0):C.R2GC_99_63,(4,0,1):C.R2GC_99_64,(3,0,0):C.R2GC_99_63,(3,0,1):C.R2GC_99_64,(8,0,0):C.R2GC_100_1,(8,0,1):C.R2GC_100_2,(7,0,0):C.R2GC_105_10,(7,0,1):C.R2GC_142_31,(6,0,0):C.R2GC_104_8,(6,0,1):C.R2GC_143_32,(5,0,0):C.R2GC_99_63,(5,0,1):C.R2GC_99_64,(1,0,0):C.R2GC_99_63,(1,0,1):C.R2GC_99_64,(11,3,0):C.R2GC_103_6,(11,3,1):C.R2GC_103_7,(10,3,0):C.R2GC_103_6,(10,3,1):C.R2GC_103_7,(9,3,1):C.R2GC_102_5,(2,1,0):C.R2GC_101_3,(2,1,1):C.R2GC_101_4,(0,1,0):C.R2GC_101_3,(0,1,1):C.R2GC_101_4,(6,1,0):C.R2GC_139_27,(6,1,1):C.R2GC_139_28,(4,1,0):C.R2GC_99_63,(4,1,1):C.R2GC_99_64,(3,1,0):C.R2GC_99_63,(3,1,1):C.R2GC_99_64,(8,1,0):C.R2GC_100_1,(8,1,1):C.R2GC_144_33,(7,1,0):C.R2GC_105_10,(7,1,1):C.R2GC_105_11,(5,1,0):C.R2GC_99_63,(5,1,1):C.R2GC_99_64,(1,1,0):C.R2GC_99_63,(1,1,1):C.R2GC_99_64,(2,2,0):C.R2GC_101_3,(2,2,1):C.R2GC_101_4,(0,2,0):C.R2GC_101_3,(0,2,1):C.R2GC_101_4,(4,2,0):C.R2GC_99_63,(4,2,1):C.R2GC_99_64,(3,2,0):C.R2GC_99_63,(3,2,1):C.R2GC_99_64,(8,2,0):C.R2GC_100_1,(8,2,1):C.R2GC_141_30,(6,2,0):C.R2GC_104_8,(6,2,1):C.R2GC_104_9,(7,2,0):C.R2GC_140_29,(7,2,1):C.R2GC_101_4,(5,2,0):C.R2GC_99_63,(5,2,1):C.R2GC_99_64,(1,2,0):C.R2GC_99_63,(1,2,1):C.R2GC_99_64})

V_3 = CTVertex(name = 'V_3',
               type = 'R2',
               particles = [ P.t__tilde__, P.b, P.G__plus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFS3, L.FFS5 ],
               loop_particles = [ [ [P.b, P.g, P.t] ] ],
               couplings = {(0,0,0):C.R2GC_153_38,(0,1,0):C.R2GC_154_39})

V_4 = CTVertex(name = 'V_4',
               type = 'R2',
               particles = [ P.b__tilde__, P.b, P.G0 ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFS1 ],
               loop_particles = [ [ [P.b, P.g] ] ],
               couplings = {(0,0,0):C.R2GC_135_21})

V_5 = CTVertex(name = 'V_5',
               type = 'R2',
               particles = [ P.b__tilde__, P.b, P.H ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFS2 ],
               loop_particles = [ [ [P.b, P.g] ] ],
               couplings = {(0,0,0):C.R2GC_134_20})

V_6 = CTVertex(name = 'V_6',
               type = 'R2',
               particles = [ P.t__tilde__, P.t, P.G0 ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFS1 ],
               loop_particles = [ [ [P.g, P.t] ] ],
               couplings = {(0,0,0):C.R2GC_156_41})

V_7 = CTVertex(name = 'V_7',
               type = 'R2',
               particles = [ P.t__tilde__, P.t, P.H ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFS2 ],
               loop_particles = [ [ [P.g, P.t] ] ],
               couplings = {(0,0,0):C.R2GC_157_42})

V_8 = CTVertex(name = 'V_8',
               type = 'R2',
               particles = [ P.b__tilde__, P.t, P.G__minus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFS3, L.FFS5 ],
               loop_particles = [ [ [P.b, P.g, P.t] ] ],
               couplings = {(0,0,0):C.R2GC_155_40,(0,1,0):C.R2GC_152_37})

V_9 = CTVertex(name = 'V_9',
               type = 'R2',
               particles = [ P.u__tilde__, P.u, P.a ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV1 ],
               loop_particles = [ [ [P.g, P.u] ] ],
               couplings = {(0,0,0):C.R2GC_109_15})

V_10 = CTVertex(name = 'V_10',
                type = 'R2',
                particles = [ P.c__tilde__, P.c, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.c, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_109_15})

V_11 = CTVertex(name = 'V_11',
                type = 'R2',
                particles = [ P.t__tilde__, P.t, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.R2GC_109_15})

V_12 = CTVertex(name = 'V_12',
                type = 'R2',
                particles = [ P.u__tilde__, P.u, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.g, P.u] ] ],
                couplings = {(0,0,0):C.R2GC_107_13})

V_13 = CTVertex(name = 'V_13',
                type = 'R2',
                particles = [ P.c__tilde__, P.c, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.c, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_107_13})

V_14 = CTVertex(name = 'V_14',
                type = 'R2',
                particles = [ P.t__tilde__, P.t, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.R2GC_107_13})

V_15 = CTVertex(name = 'V_15',
                type = 'R2',
                particles = [ P.d__tilde__, P.u, P.W__minus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.d, P.g, P.u] ] ],
                couplings = {(0,0,0):C.R2GC_126_16})

V_16 = CTVertex(name = 'V_16',
                type = 'R2',
                particles = [ P.s__tilde__, P.c, P.W__minus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.c, P.g, P.s] ] ],
                couplings = {(0,0,0):C.R2GC_126_16})

V_17 = CTVertex(name = 'V_17',
                type = 'R2',
                particles = [ P.b__tilde__, P.t, P.W__minus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.b, P.g, P.t] ] ],
                couplings = {(0,0,0):C.R2GC_126_16})

V_18 = CTVertex(name = 'V_18',
                type = 'R2',
                particles = [ P.u__tilde__, P.u, P.Z ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.g, P.u] ] ],
                couplings = {(0,0,0):C.R2GC_150_35,(0,1,0):C.R2GC_151_36})

V_19 = CTVertex(name = 'V_19',
                type = 'R2',
                particles = [ P.c__tilde__, P.c, P.Z ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.c, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_150_35,(0,1,0):C.R2GC_151_36})

V_20 = CTVertex(name = 'V_20',
                type = 'R2',
                particles = [ P.t__tilde__, P.t, P.Z ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.R2GC_150_35,(0,1,0):C.R2GC_151_36})

V_21 = CTVertex(name = 'V_21',
                type = 'R2',
                particles = [ P.d__tilde__, P.d, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.d, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_106_12})

V_22 = CTVertex(name = 'V_22',
                type = 'R2',
                particles = [ P.s__tilde__, P.s, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.g, P.s] ] ],
                couplings = {(0,0,0):C.R2GC_106_12})

V_23 = CTVertex(name = 'V_23',
                type = 'R2',
                particles = [ P.b__tilde__, P.b, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.b, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_106_12})

V_24 = CTVertex(name = 'V_24',
                type = 'R2',
                particles = [ P.d__tilde__, P.d, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.d, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_107_13})

V_25 = CTVertex(name = 'V_25',
                type = 'R2',
                particles = [ P.s__tilde__, P.s, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.g, P.s] ] ],
                couplings = {(0,0,0):C.R2GC_107_13})

V_26 = CTVertex(name = 'V_26',
                type = 'R2',
                particles = [ P.b__tilde__, P.b, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1 ],
                loop_particles = [ [ [P.b, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_107_13})

V_27 = CTVertex(name = 'V_27',
                type = 'R2',
                particles = [ P.u__tilde__, P.d, P.W__plus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.d, P.g, P.u] ] ],
                couplings = {(0,0,0):C.R2GC_126_16})

V_28 = CTVertex(name = 'V_28',
                type = 'R2',
                particles = [ P.c__tilde__, P.s, P.W__plus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.c, P.g, P.s] ] ],
                couplings = {(0,0,0):C.R2GC_126_16})

V_29 = CTVertex(name = 'V_29',
                type = 'R2',
                particles = [ P.t__tilde__, P.b, P.W__plus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.b, P.g, P.t] ] ],
                couplings = {(0,0,0):C.R2GC_126_16})

V_30 = CTVertex(name = 'V_30',
                type = 'R2',
                particles = [ P.d__tilde__, P.d, P.Z ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.d, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_132_18,(0,1,0):C.R2GC_133_19})

V_31 = CTVertex(name = 'V_31',
                type = 'R2',
                particles = [ P.s__tilde__, P.s, P.Z ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.g, P.s] ] ],
                couplings = {(0,0,0):C.R2GC_132_18,(0,1,0):C.R2GC_133_19})

V_32 = CTVertex(name = 'V_32',
                type = 'R2',
                particles = [ P.b__tilde__, P.b, P.Z ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.b, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_132_18,(0,1,0):C.R2GC_133_19})

V_33 = CTVertex(name = 'V_33',
                type = 'R2',
                particles = [ P.u__tilde__, P.u ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF1 ],
                loop_particles = [ [ [P.g, P.u] ] ],
                couplings = {(0,0,0):C.R2GC_108_14})

V_34 = CTVertex(name = 'V_34',
                type = 'R2',
                particles = [ P.c__tilde__, P.c ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF1 ],
                loop_particles = [ [ [P.c, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_108_14})

V_35 = CTVertex(name = 'V_35',
                type = 'R2',
                particles = [ P.t__tilde__, P.t ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF2, L.FF3, L.FF4, L.FF5 ],
                loop_particles = [ [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.R2GC_148_34,(0,2,0):C.R2GC_148_34,(0,1,0):C.R2GC_108_14,(0,3,0):C.R2GC_108_14})

V_36 = CTVertex(name = 'V_36',
                type = 'R2',
                particles = [ P.d__tilde__, P.d ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF1 ],
                loop_particles = [ [ [P.d, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_108_14})

V_37 = CTVertex(name = 'V_37',
                type = 'R2',
                particles = [ P.s__tilde__, P.s ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF1 ],
                loop_particles = [ [ [P.g, P.s] ] ],
                couplings = {(0,0,0):C.R2GC_108_14})

V_38 = CTVertex(name = 'V_38',
                type = 'R2',
                particles = [ P.b__tilde__, P.b ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF2, L.FF3, L.FF4, L.FF5 ],
                loop_particles = [ [ [P.b, P.g] ] ],
                couplings = {(0,0,0):C.R2GC_131_17,(0,2,0):C.R2GC_131_17,(0,1,0):C.R2GC_108_14,(0,3,0):C.R2GC_108_14})

V_39 = CTVertex(name = 'V_39',
                type = 'R2',
                particles = [ P.g, P.g ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.VV1, L.VV2, L.VV3 ],
                loop_particles = [ [ [P.b] ], [ [P.b], [P.c], [P.d], [P.s], [P.t], [P.u] ], [ [P.g] ], [ [P.t] ] ],
                couplings = {(0,0,2):C.R2GC_137_24,(0,1,0):C.R2GC_74_43,(0,1,3):C.R2GC_74_44,(0,2,1):C.R2GC_136_22,(0,2,2):C.R2GC_136_23})

V_40 = CTVertex(name = 'V_40',
                type = 'R2',
                particles = [ P.g, P.g, P.H ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.VVS1 ],
                loop_particles = [ [ [P.b] ], [ [P.t] ] ],
                couplings = {(0,0,0):C.R2GC_75_45,(0,0,1):C.R2GC_75_46})

V_41 = CTVertex(name = 'V_41',
                type = 'R2',
                particles = [ P.g, P.g, P.W__minus__, P.W__plus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.VVVV2, L.VVVV3, L.VVVV5 ],
                loop_particles = [ [ [P.b, P.t], [P.c, P.s], [P.d, P.u] ] ],
                couplings = {(0,0,0):C.R2GC_87_62,(0,1,0):C.R2GC_87_62,(0,2,0):C.R2GC_87_62})

V_42 = CTVertex(name = 'V_42',
                type = 'R2',
                particles = [ P.a, P.g, P.g, P.Z ],
                color = [ 'Identity(2,3)' ],
                lorentz = [ L.VVVV2, L.VVVV3, L.VVVV5 ],
                loop_particles = [ [ [P.b], [P.d], [P.s] ], [ [P.c], [P.t], [P.u] ] ],
                couplings = {(0,0,0):C.R2GC_79_53,(0,0,1):C.R2GC_79_54,(0,1,0):C.R2GC_79_53,(0,1,1):C.R2GC_79_54,(0,2,0):C.R2GC_79_53,(0,2,1):C.R2GC_79_54})

V_43 = CTVertex(name = 'V_43',
                type = 'R2',
                particles = [ P.g, P.g, P.Z, P.Z ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.VVVV2, L.VVVV3, L.VVVV5 ],
                loop_particles = [ [ [P.b], [P.d], [P.s] ], [ [P.c], [P.t], [P.u] ] ],
                couplings = {(0,0,0):C.R2GC_82_59,(0,0,1):C.R2GC_82_60,(0,1,0):C.R2GC_82_59,(0,1,1):C.R2GC_82_60,(0,2,0):C.R2GC_82_59,(0,2,1):C.R2GC_82_60})

V_44 = CTVertex(name = 'V_44',
                type = 'R2',
                particles = [ P.a, P.a, P.g, P.g ],
                color = [ 'Identity(3,4)' ],
                lorentz = [ L.VVVV2, L.VVVV3, L.VVVV5 ],
                loop_particles = [ [ [P.b], [P.d], [P.s] ], [ [P.c], [P.t], [P.u] ] ],
                couplings = {(0,0,0):C.R2GC_77_49,(0,0,1):C.R2GC_77_50,(0,1,0):C.R2GC_77_49,(0,1,1):C.R2GC_77_50,(0,2,0):C.R2GC_77_49,(0,2,1):C.R2GC_77_50})

V_45 = CTVertex(name = 'V_45',
                type = 'R2',
                particles = [ P.g, P.g, P.g, P.Z ],
                color = [ 'd(1,2,3)', 'f(1,2,3)' ],
                lorentz = [ L.VVVV1, L.VVVV2, L.VVVV3, L.VVVV5 ],
                loop_particles = [ [ [P.b], [P.d], [P.s] ], [ [P.c], [P.t], [P.u] ] ],
                couplings = {(1,0,0):C.R2GC_81_57,(1,0,1):C.R2GC_81_58,(0,1,0):C.R2GC_80_55,(0,1,1):C.R2GC_80_56,(0,2,0):C.R2GC_80_55,(0,2,1):C.R2GC_80_56,(0,3,0):C.R2GC_80_55,(0,3,1):C.R2GC_80_56})

V_46 = CTVertex(name = 'V_46',
                type = 'R2',
                particles = [ P.a, P.g, P.g, P.g ],
                color = [ 'd(2,3,4)' ],
                lorentz = [ L.VVVV2, L.VVVV3, L.VVVV5 ],
                loop_particles = [ [ [P.b], [P.d], [P.s] ], [ [P.c], [P.t], [P.u] ] ],
                couplings = {(0,0,0):C.R2GC_78_51,(0,0,1):C.R2GC_78_52,(0,1,0):C.R2GC_78_51,(0,1,1):C.R2GC_78_52,(0,2,0):C.R2GC_78_51,(0,2,1):C.R2GC_78_52})

V_47 = CTVertex(name = 'V_47',
                type = 'R2',
                particles = [ P.g, P.g, P.H, P.H ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.VVSS1 ],
                loop_particles = [ [ [P.b] ], [ [P.t] ] ],
                couplings = {(0,0,0):C.R2GC_76_47,(0,0,1):C.R2GC_76_48})

V_48 = CTVertex(name = 'V_48',
                type = 'R2',
                particles = [ P.g, P.g, P.G0, P.G0 ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.VVSS1 ],
                loop_particles = [ [ [P.b] ], [ [P.t] ] ],
                couplings = {(0,0,0):C.R2GC_76_47,(0,0,1):C.R2GC_76_48})

V_49 = CTVertex(name = 'V_49',
                type = 'R2',
                particles = [ P.g, P.g, P.G__minus__, P.G__plus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.VVSS1 ],
                loop_particles = [ [ [P.b, P.t] ] ],
                couplings = {(0,0,0):C.R2GC_86_61})

V_50 = CTVertex(name = 'V_50',
                type = 'UV',
                particles = [ P.g, P.g, P.g ],
                color = [ 'f(1,2,3)' ],
                lorentz = [ L.VVV1, L.VVV2, L.VVV3 ],
                loop_particles = [ [ [P.b] ], [ [P.c], [P.d], [P.s], [P.u] ], [ [P.g] ], [ [P.ghG] ], [ [P.t] ] ],
                couplings = {(0,1,0):C.UVGC_138_37,(0,1,1):C.UVGC_138_38,(0,1,4):C.UVGC_138_39,(0,2,2):C.UVGC_89_85,(0,0,3):C.UVGC_90_86})

V_51 = CTVertex(name = 'V_51',
                type = 'UV',
                particles = [ P.g, P.g, P.g, P.g ],
                color = [ 'd(-1,1,3)*d(-1,2,4)', 'd(-1,1,3)*f(-1,2,4)', 'd(-1,1,4)*d(-1,2,3)', 'd(-1,1,4)*f(-1,2,3)', 'd(-1,2,3)*f(-1,1,4)', 'd(-1,2,4)*f(-1,1,3)', 'f(-1,1,2)*f(-1,3,4)', 'f(-1,1,3)*f(-1,2,4)', 'f(-1,1,4)*f(-1,2,3)', 'Identity(1,2)*Identity(3,4)', 'Identity(1,3)*Identity(2,4)', 'Identity(1,4)*Identity(2,3)' ],
                lorentz = [ L.VVVV2, L.VVVV3, L.VVVV5, L.VVVV9 ],
                loop_particles = [ [ [P.b] ], [ [P.b], [P.c], [P.d], [P.s], [P.t], [P.u] ], [ [P.c], [P.d], [P.s], [P.u] ], [ [P.g] ], [ [P.ghG] ], [ [P.t] ] ],
                couplings = {(2,0,3):C.UVGC_100_2,(2,0,4):C.UVGC_100_1,(0,0,3):C.UVGC_100_2,(0,0,4):C.UVGC_100_1,(4,0,3):C.UVGC_99_90,(4,0,4):C.UVGC_99_91,(3,0,3):C.UVGC_99_90,(3,0,4):C.UVGC_99_91,(8,0,3):C.UVGC_100_1,(8,0,4):C.UVGC_100_2,(7,0,0):C.UVGC_142_51,(7,0,2):C.UVGC_142_52,(7,0,3):C.UVGC_142_53,(7,0,4):C.UVGC_142_54,(7,0,5):C.UVGC_142_55,(6,0,0):C.UVGC_142_51,(6,0,2):C.UVGC_142_52,(6,0,3):C.UVGC_143_56,(6,0,4):C.UVGC_143_57,(6,0,5):C.UVGC_142_55,(5,0,3):C.UVGC_99_90,(5,0,4):C.UVGC_99_91,(1,0,3):C.UVGC_99_90,(1,0,4):C.UVGC_99_91,(11,3,3):C.UVGC_103_5,(11,3,4):C.UVGC_103_6,(10,3,3):C.UVGC_103_5,(10,3,4):C.UVGC_103_6,(9,3,3):C.UVGC_102_3,(9,3,4):C.UVGC_102_4,(2,1,3):C.UVGC_100_2,(2,1,4):C.UVGC_100_1,(0,1,3):C.UVGC_100_2,(0,1,4):C.UVGC_100_1,(6,1,0):C.UVGC_139_40,(6,1,3):C.UVGC_139_41,(6,1,4):C.UVGC_139_42,(6,1,5):C.UVGC_139_43,(4,1,3):C.UVGC_99_90,(4,1,4):C.UVGC_99_91,(3,1,3):C.UVGC_99_90,(3,1,4):C.UVGC_99_91,(8,1,0):C.UVGC_144_58,(8,1,2):C.UVGC_144_59,(8,1,3):C.UVGC_144_60,(8,1,4):C.UVGC_144_61,(8,1,5):C.UVGC_144_62,(7,1,1):C.UVGC_104_7,(7,1,3):C.UVGC_105_9,(7,1,4):C.UVGC_105_10,(5,1,3):C.UVGC_99_90,(5,1,4):C.UVGC_99_91,(1,1,3):C.UVGC_99_90,(1,1,4):C.UVGC_99_91,(2,2,3):C.UVGC_100_2,(2,2,4):C.UVGC_100_1,(0,2,3):C.UVGC_100_2,(0,2,4):C.UVGC_100_1,(4,2,3):C.UVGC_99_90,(4,2,4):C.UVGC_99_91,(3,2,3):C.UVGC_99_90,(3,2,4):C.UVGC_99_91,(8,2,0):C.UVGC_141_46,(8,2,2):C.UVGC_141_47,(8,2,3):C.UVGC_141_48,(8,2,4):C.UVGC_141_49,(8,2,5):C.UVGC_141_50,(6,2,1):C.UVGC_104_7,(6,2,3):C.UVGC_104_8,(6,2,4):C.UVGC_102_3,(7,2,0):C.UVGC_139_40,(7,2,3):C.UVGC_140_44,(7,2,4):C.UVGC_140_45,(7,2,5):C.UVGC_139_43,(5,2,3):C.UVGC_99_90,(5,2,4):C.UVGC_99_91,(1,2,3):C.UVGC_99_90,(1,2,4):C.UVGC_99_91})

V_52 = CTVertex(name = 'V_52',
                type = 'UV',
                particles = [ P.t__tilde__, P.b, P.G__plus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFS3, L.FFS5 ],
                loop_particles = [ [ [P.b, P.g] ], [ [P.b, P.g, P.t] ], [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.UVGC_153_74,(0,0,2):C.UVGC_153_75,(0,0,1):C.UVGC_153_76,(0,1,0):C.UVGC_154_77,(0,1,2):C.UVGC_154_78,(0,1,1):C.UVGC_154_79})

V_53 = CTVertex(name = 'V_53',
                type = 'UV',
                particles = [ P.b__tilde__, P.b, P.G0 ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFS1 ],
                loop_particles = [ [ [P.b, P.g] ] ],
                couplings = {(0,0,0):C.UVGC_135_30})

V_54 = CTVertex(name = 'V_54',
                type = 'UV',
                particles = [ P.b__tilde__, P.b, P.H ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFS2 ],
                loop_particles = [ [ [P.b, P.g] ] ],
                couplings = {(0,0,0):C.UVGC_134_29})

V_55 = CTVertex(name = 'V_55',
                type = 'UV',
                particles = [ P.t__tilde__, P.t, P.G0 ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFS1 ],
                loop_particles = [ [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.UVGC_156_83})

V_56 = CTVertex(name = 'V_56',
                type = 'UV',
                particles = [ P.t__tilde__, P.t, P.H ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFS2 ],
                loop_particles = [ [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.UVGC_157_84})

V_57 = CTVertex(name = 'V_57',
                type = 'UV',
                particles = [ P.b__tilde__, P.t, P.G__minus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFS3, L.FFS5 ],
                loop_particles = [ [ [P.b, P.g] ], [ [P.b, P.g, P.t] ], [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.UVGC_155_80,(0,0,2):C.UVGC_155_81,(0,0,1):C.UVGC_155_82,(0,1,0):C.UVGC_152_71,(0,1,2):C.UVGC_152_72,(0,1,1):C.UVGC_152_73})

V_58 = CTVertex(name = 'V_58',
                type = 'UV',
                particles = [ P.u__tilde__, P.u, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.g, P.u] ] ],
                couplings = {(0,0,0):C.UVGC_109_14,(0,1,0):C.UVGC_92_88,(0,2,0):C.UVGC_92_88})

V_59 = CTVertex(name = 'V_59',
                type = 'UV',
                particles = [ P.c__tilde__, P.c, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.c, P.g] ] ],
                couplings = {(0,0,0):C.UVGC_109_14,(0,1,0):C.UVGC_92_88,(0,2,0):C.UVGC_92_88})

V_60 = CTVertex(name = 'V_60',
                type = 'UV',
                particles = [ P.t__tilde__, P.t, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.UVGC_109_14,(0,1,0):C.UVGC_146_64,(0,2,0):C.UVGC_146_64})

V_61 = CTVertex(name = 'V_61',
                type = 'UV',
                particles = [ P.u__tilde__, P.u, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.b] ], [ [P.c], [P.d], [P.s], [P.u] ], [ [P.g] ], [ [P.ghG] ], [ [P.g, P.u] ], [ [P.t] ] ],
                couplings = {(0,0,4):C.UVGC_107_12,(0,1,0):C.UVGC_110_15,(0,1,1):C.UVGC_110_16,(0,1,2):C.UVGC_110_17,(0,1,3):C.UVGC_110_18,(0,1,5):C.UVGC_110_19,(0,1,4):C.UVGC_110_20,(0,2,0):C.UVGC_110_15,(0,2,1):C.UVGC_110_16,(0,2,2):C.UVGC_110_17,(0,2,3):C.UVGC_110_18,(0,2,5):C.UVGC_110_19,(0,2,4):C.UVGC_110_20})

V_62 = CTVertex(name = 'V_62',
                type = 'UV',
                particles = [ P.c__tilde__, P.c, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.b] ], [ [P.c], [P.d], [P.s], [P.u] ], [ [P.c, P.g] ], [ [P.g] ], [ [P.ghG] ], [ [P.t] ] ],
                couplings = {(0,0,2):C.UVGC_107_12,(0,1,0):C.UVGC_110_15,(0,1,1):C.UVGC_110_16,(0,1,3):C.UVGC_110_17,(0,1,4):C.UVGC_110_18,(0,1,5):C.UVGC_110_19,(0,1,2):C.UVGC_110_20,(0,2,0):C.UVGC_110_15,(0,2,1):C.UVGC_110_16,(0,2,3):C.UVGC_110_17,(0,2,4):C.UVGC_110_18,(0,2,5):C.UVGC_110_19,(0,2,2):C.UVGC_110_20})

V_63 = CTVertex(name = 'V_63',
                type = 'UV',
                particles = [ P.t__tilde__, P.t, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.b] ], [ [P.c], [P.d], [P.s], [P.u] ], [ [P.g] ], [ [P.ghG] ], [ [P.g, P.t] ], [ [P.t] ] ],
                couplings = {(0,0,4):C.UVGC_107_12,(0,1,0):C.UVGC_110_15,(0,1,1):C.UVGC_110_16,(0,1,2):C.UVGC_110_17,(0,1,3):C.UVGC_110_18,(0,1,5):C.UVGC_110_19,(0,1,4):C.UVGC_147_65,(0,2,0):C.UVGC_110_15,(0,2,1):C.UVGC_110_16,(0,2,2):C.UVGC_110_17,(0,2,3):C.UVGC_110_18,(0,2,5):C.UVGC_110_19,(0,2,4):C.UVGC_147_65})

V_64 = CTVertex(name = 'V_64',
                type = 'UV',
                particles = [ P.d__tilde__, P.u, P.W__minus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.d, P.g], [P.g, P.u] ], [ [P.d, P.g, P.u] ] ],
                couplings = {(0,0,0):C.UVGC_126_21,(0,0,1):C.UVGC_126_22})

V_65 = CTVertex(name = 'V_65',
                type = 'UV',
                particles = [ P.s__tilde__, P.c, P.W__minus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.c, P.g], [P.g, P.s] ], [ [P.c, P.g, P.s] ] ],
                couplings = {(0,0,0):C.UVGC_126_21,(0,0,1):C.UVGC_126_22})

V_66 = CTVertex(name = 'V_66',
                type = 'UV',
                particles = [ P.b__tilde__, P.t, P.W__minus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.b, P.g] ], [ [P.b, P.g, P.t] ], [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.UVGC_149_67,(0,0,2):C.UVGC_149_68,(0,0,1):C.UVGC_126_22})

V_67 = CTVertex(name = 'V_67',
                type = 'UV',
                particles = [ P.t__tilde__, P.t, P.Z ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.UVGC_150_69,(0,1,0):C.UVGC_151_70})

V_68 = CTVertex(name = 'V_68',
                type = 'UV',
                particles = [ P.d__tilde__, P.d, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.d, P.g] ] ],
                couplings = {(0,0,0):C.UVGC_106_11,(0,1,0):C.UVGC_94_89,(0,2,0):C.UVGC_94_89})

V_69 = CTVertex(name = 'V_69',
                type = 'UV',
                particles = [ P.s__tilde__, P.s, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.g, P.s] ] ],
                couplings = {(0,0,0):C.UVGC_106_11,(0,1,0):C.UVGC_94_89,(0,2,0):C.UVGC_94_89})

V_70 = CTVertex(name = 'V_70',
                type = 'UV',
                particles = [ P.b__tilde__, P.b, P.a ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.b, P.g] ] ],
                couplings = {(0,0,0):C.UVGC_106_11,(0,1,0):C.UVGC_129_24,(0,2,0):C.UVGC_129_24})

V_71 = CTVertex(name = 'V_71',
                type = 'UV',
                particles = [ P.d__tilde__, P.d, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.b] ], [ [P.c], [P.d], [P.s], [P.u] ], [ [P.d, P.g] ], [ [P.g] ], [ [P.ghG] ], [ [P.t] ] ],
                couplings = {(0,0,2):C.UVGC_107_12,(0,1,0):C.UVGC_110_15,(0,1,1):C.UVGC_110_16,(0,1,3):C.UVGC_110_17,(0,1,4):C.UVGC_110_18,(0,1,5):C.UVGC_110_19,(0,1,2):C.UVGC_110_20,(0,2,0):C.UVGC_110_15,(0,2,1):C.UVGC_110_16,(0,2,3):C.UVGC_110_17,(0,2,4):C.UVGC_110_18,(0,2,5):C.UVGC_110_19,(0,2,2):C.UVGC_110_20})

V_72 = CTVertex(name = 'V_72',
                type = 'UV',
                particles = [ P.s__tilde__, P.s, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.b] ], [ [P.c], [P.d], [P.s], [P.u] ], [ [P.g] ], [ [P.ghG] ], [ [P.g, P.s] ], [ [P.t] ] ],
                couplings = {(0,0,4):C.UVGC_107_12,(0,1,0):C.UVGC_110_15,(0,1,1):C.UVGC_110_16,(0,1,2):C.UVGC_110_17,(0,1,3):C.UVGC_110_18,(0,1,5):C.UVGC_110_19,(0,1,4):C.UVGC_110_20,(0,2,0):C.UVGC_110_15,(0,2,1):C.UVGC_110_16,(0,2,2):C.UVGC_110_17,(0,2,3):C.UVGC_110_18,(0,2,5):C.UVGC_110_19,(0,2,4):C.UVGC_110_20})

V_73 = CTVertex(name = 'V_73',
                type = 'UV',
                particles = [ P.b__tilde__, P.b, P.g ],
                color = [ 'T(3,2,1)' ],
                lorentz = [ L.FFV1, L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.b] ], [ [P.b, P.g] ], [ [P.c], [P.d], [P.s], [P.u] ], [ [P.g] ], [ [P.ghG] ], [ [P.t] ] ],
                couplings = {(0,0,1):C.UVGC_107_12,(0,1,0):C.UVGC_110_15,(0,1,2):C.UVGC_110_16,(0,1,3):C.UVGC_110_17,(0,1,4):C.UVGC_110_18,(0,1,5):C.UVGC_110_19,(0,1,1):C.UVGC_130_25,(0,2,0):C.UVGC_110_15,(0,2,2):C.UVGC_110_16,(0,2,3):C.UVGC_110_17,(0,2,4):C.UVGC_110_18,(0,2,5):C.UVGC_110_19,(0,2,1):C.UVGC_130_25})

V_74 = CTVertex(name = 'V_74',
                type = 'UV',
                particles = [ P.u__tilde__, P.d, P.W__plus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.d, P.g], [P.g, P.u] ], [ [P.d, P.g, P.u] ] ],
                couplings = {(0,0,0):C.UVGC_126_21,(0,0,1):C.UVGC_126_22})

V_75 = CTVertex(name = 'V_75',
                type = 'UV',
                particles = [ P.c__tilde__, P.s, P.W__plus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.c, P.g], [P.g, P.s] ], [ [P.c, P.g, P.s] ] ],
                couplings = {(0,0,0):C.UVGC_126_21,(0,0,1):C.UVGC_126_22})

V_76 = CTVertex(name = 'V_76',
                type = 'UV',
                particles = [ P.t__tilde__, P.b, P.W__plus__ ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2 ],
                loop_particles = [ [ [P.b, P.g] ], [ [P.b, P.g, P.t] ], [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.UVGC_149_67,(0,0,2):C.UVGC_149_68,(0,0,1):C.UVGC_126_22})

V_77 = CTVertex(name = 'V_77',
                type = 'UV',
                particles = [ P.b__tilde__, P.b, P.Z ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FFV2, L.FFV3 ],
                loop_particles = [ [ [P.b, P.g] ] ],
                couplings = {(0,0,0):C.UVGC_132_27,(0,1,0):C.UVGC_133_28})

V_78 = CTVertex(name = 'V_78',
                type = 'UV',
                particles = [ P.u__tilde__, P.u ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF1, L.FF3, L.FF5 ],
                loop_particles = [ [ [P.g, P.u] ] ],
                couplings = {(0,0,0):C.UVGC_108_13,(0,1,0):C.UVGC_91_87,(0,2,0):C.UVGC_91_87})

V_79 = CTVertex(name = 'V_79',
                type = 'UV',
                particles = [ P.c__tilde__, P.c ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF1, L.FF3, L.FF5 ],
                loop_particles = [ [ [P.c, P.g] ] ],
                couplings = {(0,0,0):C.UVGC_108_13,(0,1,0):C.UVGC_91_87,(0,2,0):C.UVGC_91_87})

V_80 = CTVertex(name = 'V_80',
                type = 'UV',
                particles = [ P.t__tilde__, P.t ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF2, L.FF3, L.FF4, L.FF5 ],
                loop_particles = [ [ [P.g, P.t] ] ],
                couplings = {(0,0,0):C.UVGC_148_66,(0,2,0):C.UVGC_148_66,(0,1,0):C.UVGC_145_63,(0,3,0):C.UVGC_145_63})

V_81 = CTVertex(name = 'V_81',
                type = 'UV',
                particles = [ P.d__tilde__, P.d ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF1, L.FF3, L.FF5 ],
                loop_particles = [ [ [P.d, P.g] ] ],
                couplings = {(0,0,0):C.UVGC_108_13,(0,1,0):C.UVGC_91_87,(0,2,0):C.UVGC_91_87})

V_82 = CTVertex(name = 'V_82',
                type = 'UV',
                particles = [ P.s__tilde__, P.s ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF1, L.FF3, L.FF5 ],
                loop_particles = [ [ [P.g, P.s] ] ],
                couplings = {(0,0,0):C.UVGC_108_13,(0,1,0):C.UVGC_91_87,(0,2,0):C.UVGC_91_87})

V_83 = CTVertex(name = 'V_83',
                type = 'UV',
                particles = [ P.b__tilde__, P.b ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.FF2, L.FF3, L.FF4, L.FF5 ],
                loop_particles = [ [ [P.b, P.g] ] ],
                couplings = {(0,0,0):C.UVGC_131_26,(0,2,0):C.UVGC_131_26,(0,1,0):C.UVGC_128_23,(0,3,0):C.UVGC_128_23})

V_84 = CTVertex(name = 'V_84',
                type = 'UV',
                particles = [ P.g, P.g ],
                color = [ 'Identity(1,2)' ],
                lorentz = [ L.VV1, L.VV3 ],
                loop_particles = [ [ [P.b] ], [ [P.g] ], [ [P.ghG] ], [ [P.t] ] ],
                couplings = {(0,0,0):C.UVGC_137_33,(0,0,1):C.UVGC_137_34,(0,0,2):C.UVGC_137_35,(0,0,3):C.UVGC_137_36,(0,1,0):C.UVGC_136_31,(0,1,3):C.UVGC_136_32})

