<?php

$levenshtein = function($str1, $str2) {
    // PHP's levenshtein() is limited to 255 chars
    if (strlen($str1) > 255 || strlen($str2) > 255) {
        // fallback for long strings
        $str1Len = strlen($str1);
        $str2Len = strlen($str2);
        if ($str1Len === 0) return $str2Len;
        if ($str2Len === 0) return $str1Len;

        $prevRow = range(0, $str2Len);
        for ($i = 0; $i < $str1Len; $i++) {
            $nextCol = $i + 1;
            $char1 = $str1[$i];
            for ($j = 0; $j < $str2Len; $j++) {
                $curCol = $nextCol;
                $strCmp = $char1 === $str2[$j];
                $nextCol = $prevRow[$j] + ($strCmp ? 0 : 1);
                $tmp = $curCol + 1;
                if ($nextCol > $tmp) $nextCol = $tmp;
                $tmp = $prevRow[$j + 1] + 1;
                if ($nextCol > $tmp) $nextCol = $tmp;
                $prevRow[$j] = $curCol;
            }
            $prevRow[$str2Len] = $nextCol;
        }
        return $nextCol;
    }
    return \levenshtein($str1, $str2);
};

$sorensenDiceCoefficient = function($l, $r) {
    $l_len = mb_strlen($l);
    $r_len = mb_strlen($r);
    if ($l_len < 2 || $r_len < 2) return 0.0;
    
    $lBigrams = [];
    for ($i = 0; $i < $l_len - 1; $i++) {
        $lBigram = mb_substr($l, $i, 2);
        if (!isset($lBigrams[$lBigram])) {
            $lBigrams[$lBigram] = 0;
        }
        $lBigrams[$lBigram]++;
    }
    
    $intersectionSize = 0;
    for ($j = 0; $j < $r_len - 1; $j++) {
        $rBigram = mb_substr($r, $j, 2);
        if (isset($lBigrams[$rBigram]) && $lBigrams[$rBigram] > 0) {
            $lBigrams[$rBigram]--;
            $intersectionSize++;
        }
    }
    
    return (2.0 * $intersectionSize) / ($l_len + $r_len - 2);
};

$exports['levenshtein'] = $levenshtein;
$exports['sorensenDiceCoefficient'] = $sorensenDiceCoefficient;
return $exports;
