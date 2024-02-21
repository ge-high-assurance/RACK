# Copyright (c) 2020, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

from typing import List

from ontology_changes.ontology_change import Commit
from rack.commits import (
    # <CHANGE_CRAWLER_IMPORTS> DO NOT EDIT OR MOVE THIS LINE
    commit000edc6e33bc50fc4ee4d3bd01268d297e48dce6,
    commit05a03cd687e3bdce425794763e0957d3ccaf8ff0,
    commit096749fb6a984d801d9ace5ccf5ec269de390a66,
    commit09962dd9ab9d252639d9e4324288fd6b47cbd91f,
    commit09b79d6c0e7f72b533a3ad21e776b200a973698a,
    commit0a89f70ff929380269a79fe2fc82f5dde346ed8c,
    commit10da69db606ebdc721fd3f8e003ef2099a5fdc43,
    commit13ed266ba5730cebe75c0c48f6ba83af69429122,
    commit16f6fe3e2bb5c8c6fae59b10f400380a76863452,
    commit1834d0201254907fa50a32945716a3e0de985cad,
    commit183dbba72623c2585a0451a19ac1ddb30f8a0ea6,
    commit2439da7fb602f020e9a711511f84cd75e1522cdf,
    commit278481ce335c98723597eadf89052a4b28f2eeec,
    commit27fa0d8fe813d341918465a7102bd2a8a859fa5a,
    commit2e079bb2a32b3cc1b3153d44ad0c21e27507937f,
    commit353e88dceeff899655321ed0817d5d849f71bec8,
    commit389424cb974164f552b6b6bf8aab504d23bf079b,
    commit38d1e00f36dacfccf9cff8d7793cd39f55a83682,
    commit38f6e7fe0fc0337e46a75c8fa1f49d1f4a689858,
    commit3908d68df1143537a49e1df9556dae8066b0e25f,
    commit3f5eb7d36ac11d31a45dc95523646e7aff554860,
    commit3fa536a8bced20ab2990a4efdfac1deaf5bdf6f5,
    commit40955e24b4e38d45df2ffd0ad8aa47a827a4c72f,
    commit42ceaff2d4c837e766d5481599a4ab94556255a2,
    commit44393cc30bb0ba7482acd21b2e68576b577179f9,
    commit44da44c6877c881240c418d084ecb17de9443373,
    commit4687eafdd03e7c4ff6888691ed51c8ef388935b2,
    commit4aff1ff6e25ec99d9acfc6863498c7b32241f9d4,
    commit4f60f85168ff8ef2513fa0e2f144c2ea5c3f87a3,
    commit4f9fce78e36a6dc75f1702ab50da6a4ac801dd5e,
    commit500958dae09d88f0b82c40faf93a634d108d360f,
    commit5329c949815afea87d8bae3768bf132258aad9a0,
    commit581f1820855eee2445d9e8bfdbb639e169e9391e,
    commit5c7920fe44a3a60c76fefddd2b88cd27851f37ed,
    commit5db0d118642b541b811d23d32c5f3410d0507618,
    commit5dd1a584e19b8716f0f13dc3a2cb2ba2d409c325,
    commit620b89db747b9834013502061040f179da67f123,
    commit643839e7d8036731ba1da767942c8e74c2876e2e,
    commit698bd1306d2e6efdc7b53bf0b6792ab2054d5389,
    commit6a647ff8342a1cca6bdef8620a5bc29b4243e794,
    commit6ca3b8884f159233b417917f5a6dfc3ed699d1f7,
    commit7202dbdb81274e521b0e2cdd3afedeb2a6204567,
    commit76de25ee930683871febc1b4cc1e4386aca16d42,
    commit78eaae3db5ed184c90f4f14d34a4fc000f04bdac,
    commit815f98911956aafea98b81787eec328b2833ec72,
    commit823e9eada16b98c896f9afc30a505683a7500276,
    commit833ef18f5024fee255f77887de2c8e9bc136e56d,
    commit84bad08fee850046ef1b328b2b393322b48d5e09,
    commit8a01ff1b53e0b4979f0120a362b8fd3776a6586c,
    commit90f2d4f55668786ffa01bba2a646c7468849c97d,
    commit96c4d5d8672bbb8e8b5ff44ea928638092f91b82,
    commit9af9030fe191d564875c067f6e0319ca6b52b798,
    commita9210534a2ceb9ea5595df9eb5cd02df3abe3cb3,
    commita95a46dec5162e65979d96ba140559dfb3013d23,
    commitae0a7660b0afdd53ff334577fbdea7749abe6cf6,
    commitb10136c4d5c962d237c936758684fd3dd05e4b2d,
    commitb25d07626e4693cd370a2070e17f6baa825a1d43,
    commitb6796936abe054edc9c4f9657c34bb0eadf0757a,
    commitb721c16f0f7420a8ccd92bda0d98a96c16dc62b8,
    commitb85a66b005f4105ac5195cfd2cefec475f9e1f21,
    commitb865c663351f39c275f5fb9985b681a6ae432cac,
    commitbdfef3d7ea9b3c9fc085defa8e26256f646097d9,
    commitc41222325db52df0eb5c1e7cb3a091f8c62f5b57,
    commitc47f4e58c0cb3d0925d7894e301e6a1f83e22580,
    commitc5306ce176984770b93070da829f60769cb19628,
    commitc6692fed3e150e7df53d4a2a8f8c84f760087420,
    commitcafce30763b5332106340cc8cbeb8fdac3b8132d,
    commitd48e208669c589d070c7c5fb7e3129ababbb9193,
    commitd497287426ac99acffbd51858ebf4722af06dae2,
    commitd8271d216704351cf0007a04abac47f4abc993ad,
    commitda9c143c53933d7eeb51f19c3ddc4ccf5fda95a8,
    commitdf67562c4e5305fc9082fc369570de0a49089ccf,
    commite18de6ebaa298881aab7e8e69580905ffb97e0c4,
    commite454889c706f02818f4badc3360a3c068fa014a0,
    commite485aa4268867521cc8d6f9c3a0f6fd2aef4311b,
    commite5e8a35322fab104a42cc0f46d16c27ffc10adbb,
    commite696969a9d85ca8f894eea12305412bdc05521b3,
    commiteb2f51ed862f33d2a56bbd6d43af27d9524912c9,
    commitee148bca649a1b451085832a7e2a488ce4127de7,
    commitef72564bbc4887c2d6f6654671427ba2780e0f67,
    commitefb8faf291a895dcce35300ae3341dd4a5c3f0bd,
    commitf801242e4a8a763620571481fd83cc2af5aac2ac,
    commitfa603aad886439eb6a94e44e2c6f4851af16c9a3,
    commitff31a28051a5e348fd2474fce5360195999ddb3a,
)

# WARNING: commit dates may not reflect the history order, and are not even the
# same on Github and in git, use them as vague hints of where a commit could be.
#
# You can use:
#
#   git log | grep "commit1\|commit2\|commit3..."
#
# to figure out the respective order of some commits, and use the reverse of
# that order in this list.

commits_in_chronological_order: List[Commit] = [
    # <CHANGE_CRAWLER_COMMITS> DO NOT EDIT OR MOVE THIS LINE
    # oldest (in history)

    commita9210534a2ceb9ea5595df9eb5cd02df3abe3cb3.commit, # v4.0

    commitae0a7660b0afdd53ff334577fbdea7749abe6cf6.commit, # 2020 Dec 15

    commite5e8a35322fab104a42cc0f46d16c27ffc10adbb.commit, # v4.1

    commit2e079bb2a32b3cc1b3153d44ad0c21e27507937f.commit, # 2021 Jan 5
    commit9af9030fe191d564875c067f6e0319ca6b52b798.commit, # 2021 Jan 5
    commit05a03cd687e3bdce425794763e0957d3ccaf8ff0.commit, # 2020 Dec 22
    commitfa603aad886439eb6a94e44e2c6f4851af16c9a3.commit, # 2020 Dec 22
    commit10da69db606ebdc721fd3f8e003ef2099a5fdc43.commit, # 2020 Dec 22
    commitd48e208669c589d070c7c5fb7e3129ababbb9193.commit, # 2021 Jan 5
    commit90f2d4f55668786ffa01bba2a646c7468849c97d.commit, # 2020 Dec 22
    commit833ef18f5024fee255f77887de2c8e9bc136e56d.commit, # 2021 Jan 12
    commitd8271d216704351cf0007a04abac47f4abc993ad.commit, # 2020 Dec 22
    commit13ed266ba5730cebe75c0c48f6ba83af69429122.commit, # 2021 Jan 12
    commitff31a28051a5e348fd2474fce5360195999ddb3a.commit, # 2021 Jan 12
    commit44393cc30bb0ba7482acd21b2e68576b577179f9.commit, # 2020 Dec 22
    commit581f1820855eee2445d9e8bfdbb639e169e9391e.commit, # 2021 Jan 15
    commitb721c16f0f7420a8ccd92bda0d98a96c16dc62b8.commit, # 2021 Jan 15
    commitc6692fed3e150e7df53d4a2a8f8c84f760087420.commit, # 2021 Jan 19
    commit4f9fce78e36a6dc75f1702ab50da6a4ac801dd5e.commit, # 2021 Jan 19
    commit643839e7d8036731ba1da767942c8e74c2876e2e.commit, # 2021 Jan 21
    commitbdfef3d7ea9b3c9fc085defa8e26256f646097d9.commit, # 2021 Feb 4

    commita95a46dec5162e65979d96ba140559dfb3013d23.commit, # v4.8

    commit78eaae3db5ed184c90f4f14d34a4fc000f04bdac.commit, # 2021 Feb 26

    commit44da44c6877c881240c418d084ecb17de9443373.commit, # v5.0

    commitb25d07626e4693cd370a2070e17f6baa825a1d43.commit, # 2021 Mar 15

    commit389424cb974164f552b6b6bf8aab504d23bf079b.commit, # v5.1

    commit09b79d6c0e7f72b533a3ad21e776b200a973698a.commit, # 2021 Apr 20
    commite18de6ebaa298881aab7e8e69580905ffb97e0c4.commit, # 2021 Apr 21
    commit7202dbdb81274e521b0e2cdd3afedeb2a6204567.commit, # 2021 Apr 22
    commit0a89f70ff929380269a79fe2fc82f5dde346ed8c.commit, # 2021 Apr 23

    commit4f60f85168ff8ef2513fa0e2f144c2ea5c3f87a3.commit, # v5.9

    commit76de25ee930683871febc1b4cc1e4386aca16d42.commit, # v6.0

    commit183dbba72623c2585a0451a19ac1ddb30f8a0ea6.commit, # 2021 May 21
    commit5329c949815afea87d8bae3768bf132258aad9a0.commit, # 2021 May 21
    commit620b89db747b9834013502061040f179da67f123.commit, # 2021 May 25
    commitb6796936abe054edc9c4f9657c34bb0eadf0757a.commit, # 2021 May 26

    commitcafce30763b5332106340cc8cbeb8fdac3b8132d.commit, # v7.0

    commit42ceaff2d4c837e766d5481599a4ab94556255a2.commit, # 2021 Aug 10
    commitdf67562c4e5305fc9082fc369570de0a49089ccf.commit, # 2021 Sep 2
    commitc41222325db52df0eb5c1e7cb3a091f8c62f5b57.commit, # 2021 Sep 7
    commit698bd1306d2e6efdc7b53bf0b6792ab2054d5389.commit, # 2021 Sep 27

    commit500958dae09d88f0b82c40faf93a634d108d360f.commit, # v8.0

    commit3908d68df1143537a49e1df9556dae8066b0e25f.commit, # 2021 Oct 26
    commit38d1e00f36dacfccf9cff8d7793cd39f55a83682.commit, # 2021 Oct 29
    commit278481ce335c98723597eadf89052a4b28f2eeec.commit, # 2021 Nov 10
    commit6a647ff8342a1cca6bdef8620a5bc29b4243e794.commit, # 2021 Nov 11
    commit40955e24b4e38d45df2ffd0ad8aa47a827a4c72f.commit, # 2021 Nov 24
    commite696969a9d85ca8f894eea12305412bdc05521b3.commit, # 2021 Nov 29
    commitee148bca649a1b451085832a7e2a488ce4127de7.commit, # 2021 Nov 29
    commit27fa0d8fe813d341918465a7102bd2a8a859fa5a.commit, # 2021 Nov 29
    commit1834d0201254907fa50a32945716a3e0de985cad.commit, # 2021 Nov 30
    commit3f5eb7d36ac11d31a45dc95523646e7aff554860.commit, # 2021 Dec 01
    commit84bad08fee850046ef1b328b2b393322b48d5e09.commit, # 2021 Dec 01
    commit2439da7fb602f020e9a711511f84cd75e1522cdf.commit, # 2021 Dec 02
    commit38f6e7fe0fc0337e46a75c8fa1f49d1f4a689858.commit, # 2021 Dec 02
    commit09962dd9ab9d252639d9e4324288fd6b47cbd91f.commit, # 2021 Dec 02
    commitb10136c4d5c962d237c936758684fd3dd05e4b2d.commit, # 2021 Dec 03
    commit3fa536a8bced20ab2990a4efdfac1deaf5bdf6f5.commit, # 2021 Dec 03
    commit6ca3b8884f159233b417917f5a6dfc3ed699d1f7.commit, # 2021 Dec 13

    commit5dd1a584e19b8716f0f13dc3a2cb2ba2d409c325.commit, # v9.0

    commit823e9eada16b98c896f9afc30a505683a7500276.commit, # 2022 Jan 24
    commit353e88dceeff899655321ed0817d5d849f71bec8.commit, # 2022 Feb 02
    commitf801242e4a8a763620571481fd83cc2af5aac2ac.commit, # 2022 Feb 04
    commitb865c663351f39c275f5fb9985b681a6ae432cac.commit, # 2022 Feb 09
    commit5db0d118642b541b811d23d32c5f3410d0507618.commit, # 2022 Feb 10
    commitc5306ce176984770b93070da829f60769cb19628.commit, # 2022 Feb 10
    commit815f98911956aafea98b81787eec328b2833ec72.commit, # 2022 Feb 18
    commit4687eafdd03e7c4ff6888691ed51c8ef388935b2.commit, # 2022 Feb 28

    commitef72564bbc4887c2d6f6654671427ba2780e0f67.commit, # v10.0

    commit5c7920fe44a3a60c76fefddd2b88cd27851f37ed.commit, # 2022 Apr 20
    commit4aff1ff6e25ec99d9acfc6863498c7b32241f9d4.commit, # 2022 Aug 03
    commitc47f4e58c0cb3d0925d7894e301e6a1f83e22580.commit, # 2022 Aug 25
    commitb85a66b005f4105ac5195cfd2cefec475f9e1f21.commit, # 2022 Oct 05
    commit16f6fe3e2bb5c8c6fae59b10f400380a76863452.commit, # 2022 Oct 05

    commitefb8faf291a895dcce35300ae3341dd4a5c3f0bd.commit, # v11

    commiteb2f51ed862f33d2a56bbd6d43af27d9524912c9.commit, # 2022 Nov 07
    commit096749fb6a984d801d9ace5ccf5ec269de390a66.commit, # 2022 Dec 13
    commit8a01ff1b53e0b4979f0120a362b8fd3776a6586c.commit, # 2023 Jan 10
    commit96c4d5d8672bbb8e8b5ff44ea928638092f91b82.commit, # 2023 Feb 21

    commite485aa4268867521cc8d6f9c3a0f6fd2aef4311b.commit, # v12.0

    commitd497287426ac99acffbd51858ebf4722af06dae2.commit, # 2023 Apr 11
    commit000edc6e33bc50fc4ee4d3bd01268d297e48dce6.commit, # 2023 May 01
    commite454889c706f02818f4badc3360a3c068fa014a0.commit, # 2023 May 02
    commitda9c143c53933d7eeb51f19c3ddc4ccf5fda95a8.commit, # 2023 May 12

    commiteda11bc3abcd3d3876cfb9d9434541fb6e8fb6d4.commit, # v13.0

    # most recent (in history)
]

commits_in_git_log_order: List[Commit] = list(
    reversed(commits_in_chronological_order)
)
