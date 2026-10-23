<?php

$levenshtein = function ($str1, $str2) {
    // Both paths measure byte edits, including for UTF-8 input.
    if (strlen($str1) > 255 || strlen($str2) > 255) {
        // fallback for long strings
        $str1Len = strlen($str1);
        $str2Len = strlen($str2);
        if ($str1Len === 0) {
            return $str2Len;
        }
        if ($str2Len === 0) {
            return $str1Len;
        }

        $prevRow = range(0, $str2Len);
        for ($i = 0; $i < $str1Len; $i++) {
            $nextCol = $i + 1;
            $char1 = $str1[$i];
            for ($j = 0; $j < $str2Len; $j++) {
                $curCol = $nextCol;
                $strCmp = $char1 === $str2[$j];
                $nextCol = $prevRow[$j] + ($strCmp ? 0 : 1);
                $tmp = $curCol + 1;
                if ($nextCol > $tmp) {
                    $nextCol = $tmp;
                }
                $tmp = $prevRow[$j + 1] + 1;
                if ($nextCol > $tmp) {
                    $nextCol = $tmp;
                }
                $prevRow[$j] = $curCol;
            }
            $prevRow[$str2Len] = $nextCol;
        }
        return $nextCol;
    }
    return \levenshtein($str1, $str2);
};

$sorensenDiceCoefficient = function ($left, $right) {
    $leftLength = mb_strlen($left);
    $rightLength = mb_strlen($right);
    if ($leftLength < 2 || $rightLength < 2) {
        return 0.0;
    }

    $bigramCounts = [];
    for ($i = 0; $i < $leftLength - 1; $i++) {
        $leftBigram = mb_substr($left, $i, 2);
        if (!isset($bigramCounts[$leftBigram])) {
            $bigramCounts[$leftBigram] = 0;
        }
        $bigramCounts[$leftBigram]++;
    }

    $intersectionSize = 0;
    for ($j = 0; $j < $rightLength - 1; $j++) {
        $rightBigram = mb_substr($right, $j, 2);
        if (isset($bigramCounts[$rightBigram]) && $bigramCounts[$rightBigram] > 0) {
            $bigramCounts[$rightBigram]--;
            $intersectionSize++;
        }
    }

    return (2.0 * $intersectionSize) / ($leftLength + $rightLength - 2);
};

$exports['levenshtein'] = $levenshtein;
$exports['sorensenDiceCoefficient'] = $sorensenDiceCoefficient;
return $exports;
