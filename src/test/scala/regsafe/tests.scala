package regsafe

import org.junit.Test
import org.junit.Assert._
import scala.annotation.experimental

@experimental
class Tests {
  @Test def QT3TS: Unit = {
    val r1 = Regex("((((((((((a))))))))))"); "a" match { case r1(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) => assert((g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) == ("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")) }
    val r2 = Regex("((((((((((a))))))))))\\10"); "aa" match { case r2(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) => assert((g0, g1, g2, g3, g4, g5, g6, g7, g8, g9) == ("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")) }
    val r3 = Regex("(((((((((a)))))))))"); "a" match { case r3(g0, g1, g2, g3, g4, g5, g6, g7, g8) => assert((g0, g1, g2, g3, g4, g5, g6, g7, g8) == ("a", "a", "a", "a", "a", "a", "a", "a", "a")) }
    val r4 = Regex("((?:aaaa|bbbb)cccc)?"); "aaaacccc" match { case r4(Some(g0)) => assert((g0) == ("aaaacccc")) }
    val r5 = Regex("((?:aaaa|bbbb)cccc)?"); "bbbbcccc" match { case r5(Some(g0)) => assert((g0) == ("bbbbcccc")) }
    val r6 = Regex("((?i)a)b"); "ab" match { case r6(g0) => assert((g0) == ("a")) }
    val r7 = Regex("((?i)a)b"); "Ab" match { case r7(g0) => assert((g0) == ("A")) }
    val r8 = Regex("((?i:a))b"); "ab" match { case r8(g0) => assert((g0) == ("a")) }
    val r9 = Regex("((?i:a))b"); "Ab" match { case r9(g0) => assert((g0) == ("A")) }
    val r10 = Regex("(([a-c])b*?\\2)*"); "ababbbcbc" match { case r10(Some(g0), Some(g1)) => assert((g0, g1) == ("cbc", "c")) }
    val r11 = Regex("(([a-c])b*?\\2){3}"); "ababbbcbc" match { case r11(g0, g1) => assert((g0, g1) == ("cbc", "c")) }
    val r12 = Regex("((a)(b)c)(d)"); "abcd" match { case r12(g0, g1, g2, g3) => assert((g0, g1, g2, g3) == ("abc", "a", "b", "d")) }
    val r13 = Regex("((foo)|(bar))*"); "foobar" match { case r13(Some(g0), Some(g1), Some(g2)) => assert((g0, g1, g2) == ("bar", "foo", "bar")) }
    val r14 = Regex("(.*)c(.*)"); "abcde" match { case r14(g0, g1) => assert((g0, g1) == ("ab", "de")) }
    val r15 = Regex("(?:(f)(o)(o)|(b)(a)(r))*"); "foobar" match { case r15(Some(g0), Some(g1), Some(g2), Some(g3), Some(g4), Some(g5)) => assert((g0, g1, g2, g3, g4, g5) == ("f", "o", "o", "b", "a", "r")) }
    val r16 = Regex("([[:digit:]-[:alpha:]]+)"); "-" match { case r16(g0) => assert((g0) == ("-")) }
    val r17 = Regex("([[:digit:]-z]+)"); "-" match { case r17(g0) => assert((g0) == ("-")) }
    val r18 = Regex("([\\d-\\s]+)"); "-" match { case r18(g0) => assert((g0) == ("-")) }
    val r19 = Regex("([\\d-z]+)"); "-" match { case r19(g0) => assert((g0) == ("-")) }
    val r20 = Regex("([\\w:]+::)?(\\w+)$"); "abcd" match { case r20(None, g1) => assert((null, g1) == (null, "abcd")) }
    val r21 = Regex("([\\w:]+::)?(\\w+)$"); "xy:z:::abcd" match { case r21(Some(g0), g1) => assert((g0, g1) == ("xy:z:::", "abcd")) }
    val r22 = Regex("([a-c]*)\\1"); "abcabc" match { case r22(g0) => assert((g0) == ("abc")) }
    val r23 = Regex("([abc])*bcd"); "abcd" match { case r23(Some(g0)) => assert((g0) == ("a")) }
    val r24 = Regex("([abc])*d"); "abbbcd" match { case r24(Some(g0)) => assert((g0) == ("c")) }
    val r25 = Regex("([yX].|WORDS|[yX].|WORD)+S"); "WORDS" match { case r25(g0) => assert((g0) == ("WORD")) }
    val r26 = Regex("([yX].|WORDS|[yX].|WORD)S"); "WORDS" match { case r26(g0) => assert((g0) == ("WORD")) }
    val r27 = Regex("([yX].|WORDS|WORD|[xY].)+S"); "WORDS" match { case r27(g0) => assert((g0) == ("WORD")) }
    val r28 = Regex("([yX].|WORDS|WORD|[xY].)S"); "WORDS" match { case r28(g0) => assert((g0) == ("WORD")) }
    val r29 = Regex("([zx].|foo|fool|[zq].|money|parted|[yx].)$"); "fool" match { case r29(g0) => assert((g0) == ("fool")) }
    val r30 = Regex("([zx].|foo|fool|[zq].|money|parted|[yx].)+$"); "fool" match { case r30(g0) => assert((g0) == ("fool")) }
    val r31 = Regex("(\\d+\\.\\d+)"); "3.1415926" match { case r31(g0) => assert((g0) == ("3.1415926")) }
    val r32 = Regex("(\\w+:)+"); "one:" match { case r32(g0) => assert((g0) == ("one:")) }
    val r33 = Regex("(^|a)b"); "ab" match { case r33(g0) => assert((g0) == ("a")) }
    val r34 = Regex("(a)?(a)+"); "a" match { case r34(None, g1) => assert((null, g1) == (null, "a")) }
    val r35 = Regex("(a)b(c)"); "abc" match { case r35(g0, g1) => assert((g0, g1) == ("a", "c")) }
    val r36 = Regex("(a)|(b)"); "b" match { case r36(None, Some(g1)) => assert((null, g1) == (null, "b")) }
    val r37 = Regex("(a)|\\1"); "a" match { case r37(Some(g0)) => assert((g0) == ("a")) }
    val r38 = Regex("(a+|b)*"); "ab" match { case r38(Some(g0)) => assert((g0) == ("b")) }
    val r39 = Regex("(a+|b)+"); "ab" match { case r39(g0) => assert((g0) == ("b")) }
    val r40 = Regex("(a+|b){0,}"); "ab" match { case r40(Some(g0)) => assert((g0) == ("b")) }
    val r41 = Regex("(a+|b){1,}"); "ab" match { case r41(g0) => assert((g0) == ("b")) }
    val r42 = Regex("(aA)*+b"); "aAaAaAaAaAb" match { case r42(Some(g0)) => assert((g0) == ("aA")) }
    val r43 = Regex("(aA)++b"); "aAaAaAaAaAb" match { case r43(g0) => assert((g0) == ("aA")) }
    val r44 = Regex("(aA)?+b"); "aAb" match { case r44(Some(g0)) => assert((g0) == ("aA")) }
    val r45 = Regex("(aA){1,5}+b"); "aAaAaAaAaAb" match { case r45(g0) => assert((g0) == ("aA")) }
    val r46 = Regex("(aA|bB)*+b"); "bBbBbBbBbBb" match { case r46(Some(g0)) => assert((g0) == ("bB")) }
    val r47 = Regex("(aA|bB)++b"); "aAbBaAaAbBb" match { case r47(g0) => assert((g0) == ("bB")) }
    val r48 = Regex("(aA|bB)?+b"); "bBb" match { case r48(Some(g0)) => assert((g0) == ("bB")) }
    val r49 = Regex("(aA|bB){1,5}+b"); "bBaAbBaAbBb" match { case r49(g0) => assert((g0) == ("bB")) }
    val r50 = Regex("(ab)?(ab)+"); "ab" match { case r50(None, g1) => assert((null, g1) == (null, "ab")) }
    val r51 = Regex("(abc)?(abc)+"); "abc" match { case r51(None, g1) => assert((null, g1) == (null, "abc")) }
    val r52 = Regex("(abc)\\1"); "abcabc" match { case r52(g0) => assert((g0) == ("abc")) }
    val r53 = Regex("(ab|a)b*c"); "abc" match { case r53(g0) => assert((g0) == ("ab")) }
    val r54 = Regex("(ab|ab*)bc"); "abc" match { case r54(g0) => assert((g0) == ("a")) }
    val r55 = Regex("(a|(bc)){0,0}+xyz"); "xyz" match { case r55(None, None) => assert((null, null) == (null, null)) }
    val r56 = Regex("(a|(bc)){0,0}?xyz"); "xyz" match { case r56(None, None) => assert((null, null) == (null, null)) }
    val r57 = Regex("(a|b|c|d|e)f"); "ef" match { case r57(g0) => assert((g0) == ("e")) }
    val r58 = Regex("(bc+d$|ef*g.|h?i(j|k))"); "effgz" match { case r58(g0, None) => assert((g0, null) == ("effgz", null)) }
    val r59 = Regex("(bc+d$|ef*g.|h?i(j|k))"); "ij" match { case r59(g0, Some(g1)) => assert((g0, g1) == ("ij", "j")) }
    val r60 = Regex("(foo[1x]|bar[2x]|baz[3x])*y"); "foo1bar2baz3y" match { case r60(Some(g0)) => assert((g0) == ("baz3")) }
    val r61 = Regex("(foo[1x]|bar[2x]|baz[3x])+y"); "foo1bar2baz3y" match { case r61(g0) => assert((g0) == ("baz3")) }
    val r62 = Regex("(foo|fool|[zx].|money|parted)$"); "fool" match { case r62(g0) => assert((g0) == ("fool")) }
    val r63 = Regex("(foo|fool|[zx].|money|parted)+$"); "fool" match { case r63(g0) => assert((g0) == ("fool")) }
    val r64 = Regex("(foo|fool|money|parted)$"); "fool" match { case r64(g0) => assert((g0) == ("fool")) }
    val r65 = Regex("(foo|fool|x.|money|parted)$"); "fool" match { case r65(g0) => assert((g0) == ("fool")) }
    val r66 = Regex("(q1|.)*(q2|.)*(x(a|bc)*y){2,3}"); "xayxay" match { case r66(None, None, g2, Some(g3)) => assert((null, null, g2, g3) == (null, null, "xay", "a")) }
    val r67 = Regex("(q1|.)*(q2|.)*(x(a|bc)*y){2,}"); "xayxay" match { case r67(None, None, g2, Some(g3)) => assert((null, null, g2, g3) == (null, null, "xay", "a")) }
    val r68 = Regex("(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z"); "zzzzzzzzzzzzzzzz-xayxayxayxayZ" match { case r68(Some(g0), None, g2, Some(g3)) => assert((g0, null, g2, g3) == ("z", null, "xay", "a")) }
    val r69 = Regex("(WORDS|WORD)S"); "WORDS" match { case r69(g0) => assert((g0) == ("WORD")) }
    val r70 = Regex("(WORDS|WORLD|WORD)+S"); "WORDS" match { case r70(g0) => assert((g0) == ("WORD")) }
    val r71 = Regex("(WORDS|WORLD|WORD)S"); "WORDS" match { case r71(g0) => assert((g0) == ("WORD")) }
    val r72 = Regex("(x.|foo|fool|x.|money|parted|y.)$"); "fool" match { case r72(g0) => assert((g0) == ("fool")) }
    val r73 = Regex("(X.|WORDS|WORD|Y.)S"); "WORDS" match { case r73(g0) => assert((g0) == ("WORD")) }
    val r74 = Regex("(X.|WORDS|X.|WORD)S"); "WORDS" match { case r74(g0) => assert((g0) == ("WORD")) }
    val r75 = Regex("(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*"); "xyzQzWlongishoverblownW" match { case r75(Some(g0), Some(g1)) => assert((g0, g1) == ("zW", "overblownW")) }
    val r76 = Regex("(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+"); "xyzQzWlongishoverblownW" match { case r76(Some(g0), Some(g1)) => assert((g0, g1) == ("zW", "overblownW")) }
    val r77 = Regex("(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+"); "xyzQzWlongishoverblownW" match { case r77(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
    val r78 = Regex("(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++"); "xyzQzWlongishoverblownW" match { case r78(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
    val r79 = Regex("(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}"); "xyzQzWlongishoverblownW" match { case r79(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
    val r80 = Regex("(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+"); "xyzQzWlongishoverblownW" match { case r80(g0, g1) => assert((g0, g1) == ("zW", "overblownW")) }
    val r81 = Regex(".*?(?:(\\w)|(\\w))x"); "abx" match { case r81(Some(g0), None) => assert((g0, null) == ("b", null)) }
    val r82 = Regex("2(]*)?$\\1"); "2" match { case r82(Some(g0)) => assert((g0) == ("")) }
    val r83 = Regex("\\((.*), (.*)\\)"); "(a, b)" match { case r83(g0, g1) => assert((g0, g1) == ("a", "b")) }
    val r84 = Regex("^((?:aa)*)(?:X+((?:\\d+|-)(?:X+(.+))?))?$"); "aaaaX5" match { case r84(g0, Some(g1), None) => assert((g0, g1, null) == ("aaaa", "5", null)) }
    val r85 = Regex("^((a|b)+)*ax"); "aax" match { case r85(Some(g0), Some(g1)) => assert((g0, g1) == ("a", "a")) }
    val r86 = Regex("^((a|bc)+)*ax"); "aax" match { case r86(Some(g0), Some(g1)) => assert((g0, g1) == ("a", "a")) }
    val r87 = Regex("^(.*?)\\s*\\|\\s*(?:\\/\\s*|)\'(.+)\'$"); "text|\'sec\'" match { case r87(g0, g1) => assert((g0, g1) == ("text", "sec")) }
    val r88 = Regex("^(.+)?B"); "AB" match { case r88(Some(g0)) => assert((g0) == ("A")) }
    val r89 = Regex("^(.,){2}c"); "a,b,c" match { case r89(g0) => assert((g0) == ("b,")) }
    val r90 = Regex("^(0+)?(?:x(1))?"); "x1" match { case r90(None, Some(g1)) => assert((null, g1) == (null, "1")) }
    val r91 = Regex("^(?:(\\d)x)?\\d$"); "1" match { case r91(None) => assert((null) == (null)) }
    val r92 = Regex("^(?:(X)?(\\d)|(X)?(\\d\\d))$"); "X12" match { case r92(None, None, Some(g2), Some(g3)) => assert((null, null, g2, g3) == (null, null, "X", "12")) }
    val r93 = Regex("^(?:(XX)?(\\d)|(XX)?(\\d\\d))$"); "XX12" match { case r93(None, None, Some(g2), Some(g3)) => assert((null, null, g2, g3) == (null, null, "XX", "12")) }
    val r94 = Regex("^(?:f|o|b){2,3}?((?:b|a|r)+)\\1$"); "foobarbar" match { case r94(g0) => assert((g0) == ("bar")) }
    val r95 = Regex("^(?:f|o|b){2,3}?((?:b|a|r)+?)\\1$"); "foobarbar" match { case r95(g0) => assert((g0) == ("bar")) }
    val r96 = Regex("^(?:f|o|b){2,3}?(.+)\\1$"); "foobarbar" match { case r96(g0) => assert((g0) == ("bar")) }
    val r97 = Regex("^(?:f|o|b){2,3}?(.+?)\\1$"); "foobarbar" match { case r97(g0) => assert((g0) == ("bar")) }
    val r98 = Regex("^(?:f|o|b){3,4}((?:b|a|r)+)\\1$"); "foobarbar" match { case r98(g0) => assert((g0) == ("bar")) }
    val r99 = Regex("^(?:f|o|b){3,4}((?:b|a|r)+?)\\1$"); "foobarbar" match { case r99(g0) => assert((g0) == ("bar")) }
    val r100 = Regex("^(?:f|o|b){3,4}(.+)\\1$"); "foobarbar" match { case r100(g0) => assert((g0) == ("bar")) }
    val r101 = Regex("^(?:f|o|b){3,4}(.+?)\\1$"); "foobarbar" match { case r101(g0) => assert((g0) == ("bar")) }
    val r102 = Regex("^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?"); "012cxx0190" match { case r102(g0, None, Some(g2)) => assert((g0, null, g2) == ("012c", null, "0190")) }
    val r103 = Regex("^([^,]*,){0,3}d"); "aaa,b,c,d" match { case r103(Some(g0)) => assert((g0) == ("c,")) }
    val r104 = Regex("^([^,]*,){2}c"); "a,b,c" match { case r104(g0) => assert((g0) == ("b,")) }
    val r105 = Regex("^([^,]*,){3,}d"); "aaa,b,c,d" match { case r105(g0) => assert((g0) == ("c,")) }
    val r106 = Regex("^([^,]*,){3}d"); "aaa,b,c,d" match { case r106(g0) => assert((g0) == ("c,")) }
    val r107 = Regex("^([^,]{0,3},){0,3}d"); "aaa,b,c,d" match { case r107(Some(g0)) => assert((g0) == ("c,")) }
    val r108 = Regex("^([^,]{0,3},){3,}d"); "aaa,b,c,d" match { case r108(g0) => assert((g0) == ("c,")) }
    val r109 = Regex("^([^,]{0,3},){3}d"); "aaa,b,c,d" match { case r109(g0) => assert((g0) == ("c,")) }
    val r110 = Regex("^([^,]{1,3},){0,3}d"); "aaa,b,c,d" match { case r110(Some(g0)) => assert((g0) == ("c,")) }
    val r111 = Regex("^([^,]{1,3},){3,}d"); "aaa,b,c,d" match { case r111(g0) => assert((g0) == ("c,")) }
    val r112 = Regex("^([^,]{1,3},){3}d"); "aaa,b,c,d" match { case r112(g0) => assert((g0) == ("c,")) }
    val r113 = Regex("^([^,]{1,},){0,3}d"); "aaa,b,c,d" match { case r113(Some(g0)) => assert((g0) == ("c,")) }
    val r114 = Regex("^([^,]{1,},){3,}d"); "aaa,b,c,d" match { case r114(g0) => assert((g0) == ("c,")) }
    val r115 = Regex("^([^,]{1,},){3}d"); "aaa,b,c,d" match { case r115(g0) => assert((g0) == ("c,")) }
    val r116 = Regex("^([^a-z])|(\\^)$"); "." match { case r116(Some(g0), None) => assert((g0, null) == (".", null)) }
    val r117 = Regex("^([a]{1})*$"); "aa" match { case r117(Some(g0)) => assert((g0) == ("a")) }
    val r118 = Regex("^([ab]*?)(b)?(c)$"); "abac" match { case r118(g0, None, g2) => assert((g0, null, g2) == ("aba", null, "c")) }
    val r119 = Regex("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r119(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    val r120 = Regex("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r120(g0) => assert((g0) == ("ZEQQQX")) }
    val r121 = Regex("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r121(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    val r122 = Regex("^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQX:" match { case r122(g0) => assert((g0) == ("ZEQQQX")) }
    val r123 = Regex("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r123(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    val r124 = Regex("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r124(g0) => assert((g0) == ("ZEQQQX")) }
    val r125 = Regex("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r125(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    val r126 = Regex("^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):"); "ZEQQQX:" match { case r126(g0) => assert((g0) == ("ZEQQQX")) }
    val r127 = Regex("^(a(b)?)+$"); "aba" match { case r127(g0, Some(g1)) => assert((g0, g1) == ("a", "b")) }
    val r128 = Regex("^(a)?a$"); "a" match { case r128(None) => assert((null) == (null)) }
    val r129 = Regex("^(a+)*ax"); "aax" match { case r129(Some(g0)) => assert((g0) == ("a")) }
    val r130 = Regex("^(a\\1?)(a\\1?)(a\\2?)(a\\3?)$"); "aaaaaa" match { case r130(g0, g1, g2, g3) => assert((g0, g1, g2, g3) == ("a", "aa", "a", "aa")) }
    val r131 = Regex("^(a\\1?){4}$"); "aaaaaa" match { case r131(g0) => assert((g0) == ("aa")) }
    val r132 = Regex("^(a\\1?){4}$"); "aaaaaaaaaa" match { case r132(g0) => assert((g0) == ("aaaa")) }
    val r133 = Regex("^(aa(bb)?)+$"); "aabbaa" match { case r133(g0, Some(g1)) => assert((g0, g1) == ("aa", "bb")) }
    val r134 = Regex("^(b+?|a){1,2}c"); "bbbac" match { case r134(g0) => assert((g0) == ("a")) }
    val r135 = Regex("^(b+?|a){1,2}c"); "bbbbac" match { case r135(g0) => assert((g0) == ("a")) }
    val r136 = Regex("^(foo|)bar$"); "bar" match { case r136(g0) => assert((g0) == ("")) }
    val r137 = Regex("^(foo||baz)bar$"); "bar" match { case r137(g0) => assert((g0) == ("")) }
    val r138 = Regex("^(foo||baz)bar$"); "bazbar" match { case r138(g0) => assert((g0) == ("baz")) }
    val r139 = Regex("^(foo||baz)bar$"); "foobar" match { case r139(g0) => assert((g0) == ("foo")) }
    val r140 = Regex("^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r140(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    val r141 = Regex("^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r141(g0) => assert((g0) == ("ZEQQQX")) }
    val r142 = Regex("^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQQQQQQQQQQQQQQQQP:" match { case r142(g0) => assert((g0) == ("ZEQQQQQQQQQQQQQQQQQQP")) }
    val r143 = Regex("^(XXX|YYY|Z.Q*X|Z[TE]Q*P):"); "ZEQQQX:" match { case r143(g0) => assert((g0) == ("ZEQQQX")) }
    val r144 = Regex("^.{2,3}?((?:b|a|r)+)\\1$"); "foobarbar" match { case r144(g0) => assert((g0) == ("bar")) }
    val r145 = Regex("^.{2,3}?((?:b|a|r)+?)\\1$"); "foobarbar" match { case r145(g0) => assert((g0) == ("bar")) }
    val r146 = Regex("^.{2,3}?(.+)\\1$"); "foobarbar" match { case r146(g0) => assert((g0) == ("bar")) }
    val r147 = Regex("^.{2,3}?(.+?)\\1$"); "foobarbar" match { case r147(g0) => assert((g0) == ("bar")) }
    val r148 = Regex("^.{3,4}((?:b|a|r)+)\\1$"); "foobarbar" match { case r148(g0) => assert((g0) == ("bar")) }
    val r149 = Regex("^.{3,4}((?:b|a|r)+?)\\1$"); "foobarbar" match { case r149(g0) => assert((g0) == ("bar")) }
    val r150 = Regex("^.{3,4}(.+)\\1$"); "foobarbar" match { case r150(g0) => assert((g0) == ("bar")) }
    val r151 = Regex("^.{3,4}(.+?)\\1$"); "foobarbar" match { case r151(g0) => assert((g0) == ("bar")) }
    val r152 = Regex("^m?(\\d)(.*)\\1$"); "5b5" match { case r152(g0, g1) => assert((g0, g1) == ("5", "b")) }
    val r153 = Regex("^m?(\\D)(.*)\\1$"); "aba" match { case r153(g0, g1) => assert((g0, g1) == ("a", "b")) }
    val r154 = Regex("^m?(\\S)(.*)\\1$"); "aba" match { case r154(g0, g1) => assert((g0, g1) == ("a", "b")) }
    val r155 = Regex("^m?(\\W)(.*)\\1$"); ":b:" match { case r155(g0, g1) => assert((g0, g1) == (":", "b")) }
    val r156 = Regex("^m?(\\w)(.*)\\1$"); "aba" match { case r156(g0, g1) => assert((g0, g1) == ("a", "b")) }
    val r157 = Regex("a(?:b|(c|e){1,2}?|d)+?(.)"); "ace" match { case r157(Some(g0), g1) => assert((g0, g1) == ("c", "e")) }
    val r158 = Regex("a(?:b|c|d)(.)"); "ace" match { case r158(g0) => assert((g0) == ("e")) }
    val r159 = Regex("a(?:b|c|d)*(.)"); "ace" match { case r159(g0) => assert((g0) == ("e")) }
    val r160 = Regex("a(?:b|c|d)+(.)"); "acdbcdbe" match { case r160(g0) => assert((g0) == ("e")) }
    val r161 = Regex("a(?:b|c|d)+?(.)"); "acdbcdbe" match { case r161(g0) => assert((g0) == ("e")) }
    val r162 = Regex("a(?:b|c|d)+?(.)"); "ace" match { case r162(g0) => assert((g0) == ("e")) }
    val r163 = Regex("a(?:b|c|d){5,6}(.)"); "acdbcdbe" match { case r163(g0) => assert((g0) == ("e")) }
    val r164 = Regex("a(?:b|c|d){5,6}?(.)"); "acdbcdbe" match { case r164(g0) => assert((g0) == ("e")) }
    val r165 = Regex("a(?:b|c|d){5,7}(.)"); "acdbcdbe" match { case r165(g0) => assert((g0) == ("e")) }
    val r166 = Regex("a(?:b|c|d){5,7}?(.)"); "acdbcdbe" match { case r166(g0) => assert((g0) == ("e")) }
    val r167 = Regex("a(?:b|c|d){6,7}(.)"); "acdbcdbe" match { case r167(g0) => assert((g0) == ("e")) }
    val r168 = Regex("a(?:b|c|d){6,7}?(.)"); "acdbcdbe" match { case r168(g0) => assert((g0) == ("e")) }
    val r169 = Regex("a([bc]*)(c*d)"); "abcd" match { case r169(g0, g1) => assert((g0, g1) == ("bc", "d")) }
    val r170 = Regex("a([bc]*)(c+d)"); "abcd" match { case r170(g0, g1) => assert((g0, g1) == ("b", "cd")) }
    val r171 = Regex("a([bc]*)c*"); "abc" match { case r171(g0) => assert((g0) == ("bc")) }
    val r172 = Regex("a([bc]+)(c*d)"); "abcd" match { case r172(g0, g1) => assert((g0, g1) == ("bc", "d")) }
    val r173 = Regex("a(bc)d"); "abcd" match { case r173(g0) => assert((g0) == ("bc")) }
    val r174 = Regex("foo(aA)*+b"); "fooaAaAaAaAaAb" match { case r174(Some(g0)) => assert((g0) == ("aA")) }
    val r175 = Regex("foo(aA)++b"); "fooaAaAaAaAaAb" match { case r175(g0) => assert((g0) == ("aA")) }
    val r176 = Regex("foo(aA)?+b"); "fooaAb" match { case r176(Some(g0)) => assert((g0) == ("aA")) }
    val r177 = Regex("foo(aA){1,5}+b"); "fooaAaAaAaAaAb" match { case r177(g0) => assert((g0) == ("aA")) }
    val r178 = Regex("foo(aA|bB)*+b"); "foobBbBaAaAaAb" match { case r178(Some(g0)) => assert((g0) == ("aA")) }
    val r179 = Regex("foo(aA|bB)++b"); "foobBaAbBaAbBb" match { case r179(g0) => assert((g0) == ("bB")) }
    val r180 = Regex("foo(aA|bB)?+b"); "foobBb" match { case r180(Some(g0)) => assert((g0) == ("bB")) }
    val r181 = Regex("foo(aA|bB){1,5}+b"); "foobBaAaAaAaAb" match { case r181(g0) => assert((g0) == ("aA")) }
    val r182 = Regex("X(\\w+)(?=\\s)|X(\\w+)"); "Xab" match { case r182(None, Some(g1)) => assert((null, g1) == (null, "ab")) }
    val r183 = Regex("x(~~)*(?:(?:F)?)?"); "x~~" match { case r183(Some(g0)) => assert((g0) == ("~~")) }
  }

  @Test def t8022CharSequence(): Unit = {
    val full = Regex(""".*: (.)$""")
    val text = "   When I use this operator: *"
    // Testing 2.10.x compatibility of the return types of unapplySeq
    val y = full.unapply(text: CharSequence).get: @unchecked
    assertEquals("*", y)
  }

  @Test def t8022Match(): Unit = {
    val R = Regex("""(\d)""")
    val matchh = R.findFirstMatchIn("a1").get
    // Testing 2.10.x compatibility of the return types of unapplySeq
    val y = R.unapply(matchh).get: @unchecked
    assertEquals("1", y)
  }

  @Test def `t9666: use inline group names`(): Unit = {
    val r = new Regex("a(?<Bee>b*)c")
    val ms = r findAllIn "stuff abbbc more abc and so on"
    assertTrue(ms.hasNext)
    assertEquals("abbbc", ms.next())
    assertEquals("bbb", ms group "Bee")
    assertTrue(ms.hasNext)
    assertEquals("abc", ms.next())
    assertEquals("b", ms group "Bee")
    assertFalse(ms.hasNext)
  }

  @Test def `t9666: use explicit group names`(): Unit = {
    val r = new Regex("a(b*)c", "Bee")
    val ms = r findAllIn "stuff abbbc more abc and so on"
    assertTrue(ms.hasNext)
    assertEquals("abbbc", ms.next())
    assertEquals("bbb", ms group "Bee")
    assertTrue(ms.hasNext)
    assertEquals("abc", ms.next())
    assertEquals("b", ms group "Bee")
    assertFalse(ms.hasNext)
  }

  @Test def `t9666: fall back to explicit group names`(): Unit = {
    val r = new Regex("a(?<Bar>b*)c", "Bee")
    val ms = r findAllIn "stuff abbbc more abc and so on"
    assertTrue(ms.hasNext)
    assertEquals("abbbc", ms.next())
    assertEquals("bbb", ms group "Bee")
    assertEquals("bbb", ms group "Bar")
    assertTrue(ms.hasNext)
    assertEquals("abc", ms.next())
    assertEquals("b", ms group "Bee")
    assertEquals("b", ms group "Bar")
    assertFalse(ms.hasNext)
  }

  type NoGroup = IllegalArgumentException
  type NoMatch = NoSuchElementException
  type NoData  = IllegalStateException

  @Test def `t9666: throw on bad name`(): Unit = {
    try {
      val r = new Regex("a(?<Bar>b*)c")
      val ms = r findAllIn "stuff abbbc more abc and so on"
      assertTrue(ms.hasNext)
      ms group "Bee"
      assert(false)
    } catch {
      case e: NoGroup => () // OK
    }
    try {
      val r = new Regex("a(?<Bar>b*)c", "Bar")
      val ms = r findAllIn "stuff abbbc more abc and so on"
      assertTrue(ms.hasNext)
      ms group "Bee"
      assert(false)
    } catch {
      case e: NoGroup => () // OK
    }
    try {
      val r = new Regex("a(b*)c", "Bar")
      val ms = r findAllIn "stuff abbbc more abc and so on"
      assertTrue(ms.hasNext)
      ms group "Bee"
      assert(false)
    } catch {
      case e: NoGroup => () // OK
    }
  }

  @Test def `t9827 MatchIterator ergonomics`(): Unit = {
    val r = Regex("(ab)(cd)")
    val s = "xxxabcdyyyabcdzzz"
    assertEquals(3, r.findAllIn(s).start)
    assertEquals(5, r.findAllIn(s).start(2))
    locally {
      val mi = r.findAllIn(s)
      assertTrue(mi.hasNext)
      assertEquals(3, mi.start)
      assertEquals("abcd", mi.next())
      assertEquals(3, mi.start)
      assertTrue(mi.hasNext)
      assertEquals(10, mi.start)
    }
    locally {
      val mi = r.findAllIn(s)
      assertEquals("abcd", mi.next())
      assertEquals(3, mi.start)
      assertEquals("abcd", mi.next())
      assertEquals(10, mi.start)
      try {
        mi.next()
        assert(false)
      } catch {
        case e: NoMatch => () // OK
      }
      try {
        mi.start
        assert(false)
      } catch {
        case e: NoData => () // OK
      }
    }
    locally {
      val mi = r.findAllIn("")
      try {
        mi.start
        assert(false)
      } catch {
        case e: NoData => () // OK
      }
      try {
        mi.next()
        assert(false)
      } catch {
        case e: NoMatch => () // OK
      }
    }
    locally {
      val mi = r.findAllMatchIn(s)
      val x = mi.next()
      assertEquals("abcd", x.matched)
      assertEquals(3, x.start)
      val y = mi.next()
      assertEquals("abcd", y.matched)
      assertEquals(10, y.start)
      try {
        mi.next()
        assert(false)
      } catch {
        case e: NoMatch => () // OK
      }
      assertEquals(3, x.start)
      assertEquals(10, y.start)
    }
    locally {
      val regex = Regex("(foo)-(.*)")
      val s = "foo-abc-def"
      val result = regex.findAllIn(s)
      //result.toString // comment this line to make it not work
      val r = (result.group(1), result.group(2))
      assertEquals(("foo", "abc-def"), r)
    }
    locally {
      val t = "this is a test"
      val rx = Regex(" ")
      val m = rx.findAllIn(t)
      assertEquals(5, m.end)
    }
    locally {
      val data = "<a>aaaaa</a><b>bbbbbb</b><c>ccccccc</c>"
      val p = Regex("^<a>(.+)</a><b>(.+)</b><c>(.+)</c>$")
      val parts = p.findAllIn(data)
      val aes = parts.group(1)
      val bes = parts.group(2)
      val ces = parts.group(3)
      assertEquals("ccccccc", ces)
      assertEquals("bbbbbb", bes)
      assertEquals("aaaaa", aes)
    }
  }

  @Test def `t10827 matches method`(): Unit = {
    val r = Regex("""\d+""")
    assertTrue(r.matches("500"))
    assertFalse(r.matches("foo"))
    assertFalse(r.matches("123 123"))
    assertFalse(r.matches("foo2"))
    assertFalse(r.matches("2foo"))
  }

  @Test def `t10827 matches method for unanchored Regex`(): Unit = {
    val r = Regex("""\d+""").unanchored
    assertTrue(r.matches("500"))
    assertFalse(r.matches("abc"))
    assertTrue(r.matches("123 123"))
    assertTrue(r.matches("foo2"))
    assertTrue(r.matches("2foo"))
  }

  @Test def replacementMatching(): Unit = {
    val regex = Regex("""\$\{(.+?)\}""")
    val replaced = regex.replaceAllIn("Replacing: ${main}. And another method: ${foo}.",
        (m: Regex.Match) => {
      val identifier = m.group(1)
      identifier
    })
    assertEquals("Replacing: main. And another method: foo.", replaced)

    val regex3 = Regex("""\$\{(.+?)\}""")
    val replaced3 = regex3.replaceSomeIn("Replacing: ${main}. And another: ${foo}.", (m: Regex.Match) => {
      val id = m.group(1)
      if (id.startsWith("m")) Some(id) else None
    })
    assertEquals("Replacing: main. And another: ${foo}.", replaced3)
  }

  @Test def groupsMatching(): Unit = {
    val Date = Regex("""(\d+)/(\d+)/(\d+)""")
    for (Regex.Groups(a, b, c) <- Date findFirstMatchIn "1/1/2001 marks the start of the millennium. 31/12/2000 doesn't.") {
      assertEquals("1", a)
      assertEquals("1", b)
      assertEquals("2001", c)
    }
    for (Regex.Groups(a, b, c) <- Date.findAllIn("1/1/2001 marks the start of the millennium. 31/12/2000 doesn't.").matchData) {
      assertTrue(a == "1" || a == "31")
      assertTrue(b == "1" || b == "12")
      assertTrue(c == "2001" || c == "2000")
    }
  }

  @Test def `t6406 no longer unapply any`(): Unit = {
    val r = Regex("(\\d+)")
    val q = Regex("""(\d)""")
    val ns = List("1,2","x","3,4")
    val u = r.unanchored

    val is = ns collect { case u(x) => x } map { case r(x) => x }
    assertTrue(List("1", "3").sameElements(is))
    assertEquals(List("1", "3"), is)
    // Match from same pattern
    val js = ns.map(u.findFirstMatchIn(_)).flatten.map { case r(x) => x }
    assertEquals(List("1", "3"), js)
    // Match not from same pattern
    val ks = ns.map(q.findFirstMatchIn(_)).flatten.map { case r(x) => x }
    assertEquals(List("1", "3"), ks)

    val t = "Last modified 2011-07-15"
    val p1 = Regex("""(\d\d\d\d)-(\d\d)-(\d\d)""")
    val y1: Option[String] = for {
      p1(year, month, day) <- p1.findFirstIn(t)
    } yield year
    assertEquals(Some("2011"), y1)
    val y2: Option[String] = for {
      p1(year, month, day) <- p1.findFirstMatchIn(t)
    } yield year
    assertEquals(Some("2011"), y2)
  }
}
