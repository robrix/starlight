-- schema for the ephemerides table
create table bodies
  ( parentId integer
  , code integer not null
  , name text not null
  , population integer not null
  , radius real not null -- km
  , mass real not null -- kg
  , tilt real not null -- degrees
  , rotationalPeriod real not null -- days
  , colour integer not null -- 32-bit packed
  , eccentricity real not null
  , semimajor real not null -- km
  , longitudeOfAscendingNode real not null -- degrees
  , inclination real not null -- degrees
  , argumentOfPerifocus real not null -- degrees
  , orbitalPeriod real not null -- seconds
  , timeOfPeriapsis real not null -- seconds
  );

insert into bodies values
  ( null
  , 10
  , "Sol"
  , 0
  , 695500.0
  , 1.9885e30
  , 7.250000000000001
  , 2164320.0
  , 0xffff00ff
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 1
  , "Ceres"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 7.687465334611522e-2
  , 4.142797828963455e8
  , 80.30119119345687
  , 10.59127757568793
  , 73.80896788656509
  , 1.454335687445103e8
  , -5.264532228236385e7 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 2
  , "Pallas"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 0.2301826226609277
  , 4.148649005599506e8
  , 173.0576921768779
  , 34.83071541568217
  , 310.1337912641824
  , 1.457417872231981e8
  , -4.566396368252335e7 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 4
  , "Vesta"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 8.857261292194868e-2
  , 3.533365061725e8
  , 103.8092901558364
  , 7.14181457784487
  , 150.8357759675119
  , 1.145530921975954e8
  , -5.19865481452617e7 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 10
  , "Hygiea"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 0.1122156412271784
  , 4.700770633961165e8
  , 283.198782813623
  , 3.831711254371847
  , 312.3862263752777
  , 1.757833119743325e8
  , 7.999591572569396e7 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 199
  , "Mercury"
  , 1000
  , 2439.7
  , 3.302e23
  , 3.4e-2
  , 5067014.4
  , 0x808080ff
  , 0.2056502791763153
  , 5.790898564564321e7
  , 48.30652204584498
  , 7.003793158072573
  , 29.18253289128074
  , 7600513.799399658
  , 3647176.731493337 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 299
  , "Venus"
  , 10000
  , 6051.9
  , 4.8685e24
  , 177.3
  , 2.099736e7
  , 0xffff80ff
  , 6.744611968909772e-3
  , 1.082076210583038e8
  , 76.62474852253777
  , 3.394579441826207
  , 54.90669253984895
  , 1.941378848004525e7
  , 6837354.360531456 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 399
  , "Terra"
  , 10000
  , 6378.14
  , 5.97219e24
  , 23.4392911
  , 86164.10035200001
  , 0x0000ffff
  , 1.712127710968187e-2
  , 1.49653795867731e8
  , 159.6967974767415
  , 2.777040607882003e-3
  , 304.357324974872
  , 3.157584666605868e7
  , 365640.0869365698 );

insert into bodies values
  ( (select rowid from bodies where code = 399 and name = "Terra")
  , 301
  , "Luna"
  , 1000
  , 1737.5
  , 7.342e22
  , 6.687
  , 2360591.5104
  , 0x808080ff
  , 6.253105769694327e-2
  , 380829.3800328925
  , 98.14052087575739
  , 5.286231389299102
  , 81.65180631784104
  , 2324620.301163248
  , -1061255.352507076 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 499
  , "Mars"
  , 10000
  , 3397.0
  , 6.4171e23
  , 25.19
  , 88642.6848
  , 0xff0000ff
  , 9.350045592043167e-2
  , 2.279313797192045e8
  , 49.50122742914472
  , 1.848059606867434
  , 286.6772838275621
  , 5.935128965962236e7
  , 1.86182463924549e7 );

insert into bodies values
  ( (select rowid from bodies where code = 499 and name = "Mars")
  , 401
  , "Phobos"
  , 0
  , 11.2667
  , 1.0659e16
  , 0.0
  , 27572.64610941267
  , 0x83786eff
  , 1.504026960511869e-2
  , 9377.986046487064
  , 82.64903352403502
  , 25.63283988371116
  , 285.5078164983516
  , 27572.64610941267
  , -7359.393936325722 );

insert into bodies values
  ( (select rowid from bodies where code = 499 and name = "Mars")
  , 402
  , "Deimos"
  , 0
  , 6.2
  , 1.4762e15
  , 0.0
  , 109090.1488279104
  , 0xbcaa91ff
  , 1.812367163573328e-4
  , 23459.2598135558
  , 79.06303338963231
  , 24.99562642178741
  , 346.3332850165525
  , 109090.1488279104
  , 36323.40384905073 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 599
  , "Jupiter"
  , 100000
  , 69911.0
  , 1.89813e27
  , 3.13
  , 35730.0
  , 0x808000ff
  , 4.875572951666173e-2
  , 7.782907216845045e8
  , 100.5161987950841
  , 1.303774910017153
  , 273.6952858336796
  , 3.743084593574342e8
  , 9.657367791800812e7 );

insert into bodies values
  ( (select rowid from bodies where code = 599 and name = "Jupiter")
  , 501
  , "Io"
  , 0
  , 1821.3
  , 8.933e22
  , 1.0
  , 153044.9224124419
  , 0xfdf99cff
  , 4.308533987106821e-3
  , 422029.6861545355
  , 338.1570617661533
  , 2.253833580791092
  , 58.27560719107961
  , 153044.9224124419
  , 31096.91459476587 );

insert into bodies values
  ( (select rowid from bodies where code = 599 and name = "Jupiter")
  , 502
  , "Europa"
  , 0
  , 1560.8
  , 4.799844e22
  , 0.1
  , 306980.3679877407
  , 0xb8a482ff
  , 9.384745003012568e-3
  , 671224.2448003067
  , 332.4208633991324
  , 2.62294086696387
  , 253.9262022361536
  , 306980.3679877407
  , 35808.1887340765 );

insert into bodies values
  ( (select rowid from bodies where code = 599 and name = "Jupiter")
  , 503
  , "Ganymede"
  , 0
  , 2634.1
  , 1.4819e23
  , 0.33
  , 618345.1356412469
  , 0x8f8475ff
  , 1.953326482402826e-3
  , 1070587.506889862
  , 340.3571923433072
  , 2.306479229210245
  , 4.052026367095923
  , 618345.1356412469
  , 239432.7649516635 );

insert into bodies values
  ( (select rowid from bodies where code = 599 and name = "Jupiter")
  , 504
  , "Callisto"
  , 0
  , 2410.3
  , 1.075938e23
  , 0.0
  , 1442530.711597345
  , 0x6B5f4fff
  , 7.336996618225692e-3
  , 1883136.592827482
  , 336.8955159222581
  , 1.964828759620594
  , 27.14830988250611
  , 1442530.711597345
  , 155965.3272648114 );

insert into bodies values
  ( (select rowid from bodies where code = 599 and name = "Jupiter")
  , 505
  , "Amalthea"
  , 0
  , 83.5
  , 2.08e18
  , 0.0
  , 43340.77495758011
  , 0x9d9d9dff
  , 4.667333126224941e-3
  , 181993.0019974714
  , 339.1039117232312
  , 2.588002942784753
  , 169.2461127021286
  , 43340.77495758011
  , -5645.177367486724 );

insert into bodies values
  ( (select rowid from bodies where code = 599 and name = "Jupiter")
  , 506
  , "Himalia"
  , 0
  , 85.0
  , 4.2e18
  , 0.0
  , 28015.2
  , 0xcbcbcbff
  , 0.1536553566350128
  , 1.138484017285008e7
  , 44.27095704562344
  , 29.45601842614082
  , 27.07859930761108
  , 2.144396286483647e7
  , -5172959.736118329 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 699
  , "Saturn"
  , 0
  , 58232.0
  , 5.6834e26
  , 26.73
  , 38018.0
  , 0xe5d8a7ff
  , 5.092477649670003e-2
  , 1.433408166229137e9
  , 113.5931808555262
  , 2.489867712734118
  , 337.7632387903743
  , 9.358727623083861e8
  , 4.080541968441563e8 );

insert into bodies values
  ( (select rowid from bodies where code = 699 and name = "Saturn")
  , 601
  , "Mimas"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 1.768271225533981e-2
  , 186005.0643092228
  , 171.2368290690333
  , 29.39147083942005
  , 219.5767933952658
  , 81840.57652848132
  , -31777.5658728918 );

insert into bodies values
  ( (select rowid from bodies where code = 699 and name = "Saturn")
  , 602
  , "Enceladus"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 6.283099737647752e-3
  , 238413.989109994
  , 169.5308960731525
  , 28.04133886680621
  , 86.85717931061527
  , 118762.4372145815
  , -6810.430849417849 );

insert into bodies values
  ( (select rowid from bodies where code = 699 and name = "Saturn")
  , 603
  , "Tethys"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 1.178085353395868e-3
  , 294973.2649099416
  , 168.161627168348
  , 27.15958127165961
  , 265.3205930356025
  , 163438.8463501523
  , 425.8604045766257 );

insert into bodies values
  ( (select rowid from bodies where code = 699 and name = "Saturn")
  , 604
  , "Dione"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 2.838515990860641e-3
  , 377644.595268596
  , 169.5238289757
  , 28.07604759327597
  , 78.58216437914999
  , 236758.7470990574
  , -10041.01706544435 );

insert into bodies values
  ( (select rowid from bodies where code = 699 and name = "Saturn")
  , 605
  , "Rhea"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 4.853205436923233e-4
  , 527263.5989225593
  , 170.1616218349706
  , 27.90827952083789
  , 164.7473355990541
  , 390591.1011086715
  , 192202.0747725686 );

insert into bodies values
  ( (select rowid from bodies where code = 699 and name = "Saturn")
  , 606
  , "Titan"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 2.859698699916036e-2
  , 1221957.648883498
  , 169.0713358999649
  , 27.69806348411966
  , 174.6258664361575
  , 1377889.469064591
  , 662495.863666624 );

insert into bodies values
  ( (select rowid from bodies where code = 699 and name = "Saturn")
  , 607
  , "Hyperion"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 0.1284449417007376
  , 1481670.40735616
  , 169.1462151850471
  , 27.03429058072402
  , 179.4947562896795
  , 1839962.406207508
  , -885553.6018348014 );

insert into bodies values
  ( (select rowid from bodies where code = 699 and name = "Saturn")
  , 608
  , "Iapetus"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 2.763916825225168e-2
  , 3562504.259802739
  , 138.8516629169715
  , 17.08033500276849
  , 230.8031919116441
  , 6859839.265014797
  , 2378914.245752243 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 799
  , "Uranus"
  , 0
  , 25362.0
  , 8.6813e25
  , 97.77
  , 62063.712
  , 0xc4ddf0ff
  , 4.665373660443459e-2
  , 2.868310714157579e9
  , 74.04624221175592
  , 0.771164840840142
  , 98.97036289664372
  , 2.649438628752338e9
  , 9.884131774911809e8 );

insert into bodies values
  ( (select rowid from bodies where code = 799 and name = "Uranus")
  , 701
  , "Ariel"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 6.899161771096965e-4
  , 190947.7115318151
  , 167.6507267054484
  , 97.69653942856905
  , 168.8409666965092
  , 217801.5699756911
  , -55775.79522336353 );

insert into bodies values
  ( (select rowid from bodies where code = 799 and name = "Uranus")
  , 702
  , "Umbriel"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 3.989140541389371e-3
  , 265983.9474349588
  , 167.6918950587149
  , 97.70140931781421
  , 47.4743530104665
  , 358073.6792479662
  , -52140.46983507854 );

insert into bodies values
  ( (select rowid from bodies where code = 799 and name = "Uranus")
  , 703
  , "Titania"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 2.24292618854844e-3
  , 436271.9577242527
  , 167.6260210568743
  , 97.7785660189554
  , 271.0083875375138
  , 752176.7238978347
  , -35369.48906097154 );

insert into bodies values
  ( (select rowid from bodies where code = 799 and name = "Uranus")
  , 704
  , "Oberon"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 2.454535719522157e-3
  , 583546.6330251545
  , 167.7218416040001
  , 97.90960705569489
  , 172.5644307696369
  , 1163584.972114305
  , -43665.54864286464 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 899
  , "Neptune"
  , 0
  , 24624.0
  , 1.02413e26
  , 28.32
  , 58000.32
  , 0x8aa3d9ff
  , 9.681224208493813e-3
  , 4.515214544223115e9
  , 131.8675920558875
  , 1.774141711272706
  , 247.5349759521243
  , 5.232755638354544e9
  , 4.51967469154434e8 );

insert into bodies values
  ( (select rowid from bodies where code = 899 and name = "Neptune")
  , 801
  , "Triton"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 2.528542251943667e-5
  , 354766.2257905865
  , 221.0565081304727
  , 129.3253503499843
  , 64.44088661668926
  , 507779.8933982676
  , 16857.46529085825 );

insert into bodies values
  ( (select rowid from bodies where code = 899 and name = "Neptune")
  , 802
  , "Nereid"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 0.7450711784341466
  , 5506906.101883776
  , 319.5580690418183
  , 5.025571972381673
  , 296.7414047018111
  , 3.105767598950946e7
  , 3591043.471748701 );

insert into bodies values
  ( (select rowid from bodies where code = 899 and name = "Neptune")
  , 803
  , "Naiad"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 1.370539142152146e-3
  , 48293.7560374405
  , 58.89388251502994
  , 28.38518222309618
  , 150.9861822926834
  , 25506.0921013033
  , 419.9585093552943 );

insert into bodies values
  ( (select rowid from bodies where code = 899 and name = "Neptune")
  , 804
  , "Thalassa"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 1.486513973798955e-3
  , 50139.62146534695
  , 48.63016381920195
  , 28.48794350914951
  , 148.778443169133
  , 26982.30356800628
  , 67.2799400788768 );

insert into bodies values
  ( (select rowid from bodies where code = 899 and name = "Neptune")
  , 805
  , "Despina"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 9.428120784636008e-4
  , 52588.04830601005
  , 49.04204075478262
  , 28.53224425810494
  , 63.41540466992091
  , 28982.64568214761
  , -556.5191337225276 );

insert into bodies values
  ( (select rowid from bodies where code = 10 and name = "Sol")
  , 999
  , "Pluto"
  , 0
  , 1188.3
  , 1.303e22
  , 122.53000000000002
  , 551856.672
  , 0xa59d90ff
  , 0.2495757207945894
  , 5.929522234007778e9
  , 110.2671060907729
  , 16.88141924355131
  , 114.6181255467904
  , 7.875055128040093e9
  , -9.453228443913183e8 );

insert into bodies values
  ( (select rowid from bodies where code = 999 and name = "Pluto")
  , 901
  , "Charon"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 1.475239653181569e-4
  , 19596.85077073687
  , 227.4021177813657
  , 112.8958415422894
  , 189.792660412797
  , 551855.8179519066
  , 12944.31317734176 );

insert into bodies values
  ( (select rowid from bodies where code = 999 and name = "Pluto")
  , 902
  , "Nix"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 0.1234296597828262
  , 48319.45614175033
  , 227.4106194534253
  , 112.9054214721053
  , 162.900159702043
  , 2263433.74425041
  , 488584.9151400468 );

insert into bodies values
  ( (select rowid from bodies where code = 999 and name = "Pluto")
  , 903
  , "Hydra"
  , 0
  , 1000.0
  , 1.307e22
  , 5.0
  , 86400.0
  , 0xffffffff
  , 0.2331037322936403
  , 52366.03574556949
  , 227.2502075578881
  , 113.2117678632148
  , 165.2096040848483
  , 2553635.450983411
  , -1032044.001985174 );
