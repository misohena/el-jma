;;; jma-weather-code.el --- JMA Weather Code         -*- lexical-binding: t; -*-

;; Copyright (C) 2022  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;;; 天気コード

(defconst jma-weather-code-list
  ;; コード 昼画像 夜画像 ? 日本語 英語
  '((100 "100.svg" "500.svg" "100" "晴" "CLEAR")
    (101 "101.svg" "501.svg" "100" "晴時々曇" "PARTLY CLOUDY")
    (102 "102.svg" "502.svg" "300" "晴一時雨" "CLEAR  OCCASIONAL SCATTERED SHOWERS")
    (103 "102.svg" "502.svg" "300" "晴時々雨" "CLEAR  FREQUENT SCATTERED SHOWERS")
    (104 "104.svg" "504.svg" "400" "晴一時雪" "CLEAR  SNOW FLURRIES")
    (105 "104.svg" "504.svg" "400" "晴時々雪" "CLEAR  FREQUENT SNOW FLURRIES")
    (106 "102.svg" "502.svg" "300" "晴一時雨か雪" "CLEAR  OCCASIONAL SCATTERED SHOWERS OR SNOW FLURRIES")
    (107 "102.svg" "502.svg" "300" "晴時々雨か雪" "CLEAR  FREQUENT SCATTERED SHOWERS OR SNOW FLURRIES")
    (108 "102.svg" "502.svg" "300" "晴一時雨か雷雨" "CLEAR  OCCASIONAL SCATTERED SHOWERS AND/OR THUNDER")
    (110 "110.svg" "510.svg" "100" "晴後時々曇" "CLEAR  PARTLY CLOUDY LATER")
    (111 "110.svg" "510.svg" "100" "晴後曇" "CLEAR  CLOUDY LATER")
    (112 "112.svg" "512.svg" "300" "晴後一時雨" "CLEAR  OCCASIONAL SCATTERED SHOWERS LATER")
    (113 "112.svg" "512.svg" "300" "晴後時々雨" "CLEAR  FREQUENT SCATTERED SHOWERS LATER")
    (114 "112.svg" "512.svg" "300" "晴後雨" "CLEAR RAIN LATER")
    (115 "115.svg" "515.svg" "400" "晴後一時雪" "CLEAR  OCCASIONAL SNOW FLURRIES LATER")
    (116 "115.svg" "515.svg" "400" "晴後時々雪" "CLEAR  FREQUENT SNOW FLURRIES LATER")
    (117 "115.svg" "515.svg" "400" "晴後雪" "CLEAR SNOW LATER")
    (118 "112.svg" "512.svg" "300" "晴後雨か雪" "CLEAR  RAIN OR SNOW LATER")
    (119 "112.svg" "512.svg" "300" "晴後雨か雷雨" "CLEAR  RAIN AND/OR THUNDER LATER")
    (120 "102.svg" "502.svg" "300" "晴朝夕一時雨" "OCCASIONAL SCATTERED SHOWERS IN THE MORNING AND EVENING  CLEAR DURING THE DAY")
    (121 "102.svg" "502.svg" "300" "晴朝の内一時雨" "OCCASIONAL SCATTERED SHOWERS IN THE MORNING  CLEAR DURING THE DAY")
    (122 "112.svg" "512.svg" "300" "晴夕方一時雨" "CLEAR  OCCASIONAL SCATTERED SHOWERS IN THE EVENING")
    (123 "100.svg" "500.svg" "100" "晴山沿い雷雨" "CLEAR IN THE PLAINS  RAIN AND THUNDER NEAR MOUTAINOUS AREAS")
    (124 "100.svg" "500.svg" "100" "晴山沿い雪" "CLEAR IN THE PLAINS  SNOW NEAR MOUTAINOUS AREAS")
    (125 "112.svg" "512.svg" "300" "晴午後は雷雨" "CLEAR  RAIN AND THUNDER IN THE AFTERNOON")
    (126 "112.svg" "512.svg" "300" "晴昼頃から雨" "CLEAR  RAIN IN THE AFTERNOON")
    (127 "112.svg" "512.svg" "300" "晴夕方から雨" "CLEAR  RAIN IN THE EVENING")
    (128 "112.svg" "512.svg" "300" "晴夜は雨" "CLEAR  RAIN IN THE NIGHT")
    (130 "100.svg" "500.svg" "100" "朝の内霧後晴" "FOG IN THE MORNING  CLEAR LATER")
    (131 "100.svg" "500.svg" "100" "晴明け方霧" "FOG AROUND DAWN  CLEAR LATER")
    (132 "101.svg" "501.svg" "100" "晴朝夕曇" "CLOUDY IN THE MORNING AND EVENING  CLEAR DURING THE DAY")
    (140 "102.svg" "502.svg" "300" "晴時々雨で雷を伴う" "CLEAR  FREQUENT SCATTERED SHOWERS AND THUNDER")
    (160 "104.svg" "504.svg" "400" "晴一時雪か雨" "CLEAR  SNOW FLURRIES OR OCCASIONAL SCATTERED SHOWERS")
    (170 "104.svg" "504.svg" "400" "晴時々雪か雨" "CLEAR  FREQUENT SNOW FLURRIES OR SCATTERED SHOWERS")
    (181 "115.svg" "515.svg" "400" "晴後雪か雨" "CLEAR  SNOW OR RAIN LATER")
    (200 "200.svg" "200.svg" "200" "曇" "CLOUDY")
    (201 "201.svg" "601.svg" "200" "曇時々晴" "MOSTLY CLOUDY")
    (202 "202.svg" "202.svg" "300" "曇一時雨" "CLOUDY  OCCASIONAL SCATTERED SHOWERS")
    (203 "202.svg" "202.svg" "300" "曇時々雨" "CLOUDY  FREQUENT SCATTERED SHOWERS")
    (204 "204.svg" "204.svg" "400" "曇一時雪" "CLOUDY  OCCASIONAL SNOW FLURRIES")
    (205 "204.svg" "204.svg" "400" "曇時々雪" "CLOUDY FREQUENT SNOW FLURRIES")
    (206 "202.svg" "202.svg" "300" "曇一時雨か雪" "CLOUDY  OCCASIONAL SCATTERED SHOWERS OR SNOW FLURRIES")
    (207 "202.svg" "202.svg" "300" "曇時々雨か雪" "CLOUDY  FREQUENT SCCATERED SHOWERS OR SNOW FLURRIES")
    (208 "202.svg" "202.svg" "300" "曇一時雨か雷雨" "CLOUDY  OCCASIONAL SCATTERED SHOWERS AND/OR THUNDER")
    (209 "200.svg" "200.svg" "200" "霧" "FOG")
    (210 "210.svg" "610.svg" "200" "曇後時々晴" "CLOUDY  PARTLY CLOUDY LATER")
    (211 "210.svg" "610.svg" "200" "曇後晴" "CLOUDY  CLEAR LATER")
    (212 "212.svg" "212.svg" "300" "曇後一時雨" "CLOUDY  OCCASIONAL SCATTERED SHOWERS LATER")
    (213 "212.svg" "212.svg" "300" "曇後時々雨" "CLOUDY  FREQUENT SCATTERED SHOWERS LATER")
    (214 "212.svg" "212.svg" "300" "曇後雨" "CLOUDY  RAIN LATER")
    (215 "215.svg" "215.svg" "400" "曇後一時雪" "CLOUDY  SNOW FLURRIES LATER")
    (216 "215.svg" "215.svg" "400" "曇後時々雪" "CLOUDY  FREQUENT SNOW FLURRIES LATER")
    (217 "215.svg" "215.svg" "400" "曇後雪" "CLOUDY  SNOW LATER")
    (218 "212.svg" "212.svg" "300" "曇後雨か雪" "CLOUDY  RAIN OR SNOW LATER")
    (219 "212.svg" "212.svg" "300" "曇後雨か雷雨" "CLOUDY  RAIN AND/OR THUNDER LATER")
    (220 "202.svg" "202.svg" "300" "曇朝夕一時雨" "OCCASIONAL SCCATERED SHOWERS IN THE MORNING AND EVENING  CLOUDY DURING THE DAY")
    (221 "202.svg" "202.svg" "300" "曇朝の内一時雨" "CLOUDY OCCASIONAL SCCATERED SHOWERS IN THE MORNING")
    (222 "212.svg" "212.svg" "300" "曇夕方一時雨" "CLOUDY  OCCASIONAL SCCATERED SHOWERS IN THE EVENING")
    (223 "201.svg" "601.svg" "200" "曇日中時々晴" "CLOUDY IN THE MORNING AND EVENING  PARTLY CLOUDY DURING THE DAY ")
    (224 "212.svg" "212.svg" "300" "曇昼頃から雨" "CLOUDY  RAIN IN THE AFTERNOON")
    (225 "212.svg" "212.svg" "300" "曇夕方から雨" "CLOUDY  RAIN IN THE EVENING")
    (226 "212.svg" "212.svg" "300" "曇夜は雨" "CLOUDY  RAIN IN THE NIGHT")
    (228 "215.svg" "215.svg" "400" "曇昼頃から雪" "CLOUDY  SNOW IN THE AFTERNOON")
    (229 "215.svg" "215.svg" "400" "曇夕方から雪" "CLOUDY  SNOW IN THE EVENING")
    (230 "215.svg" "215.svg" "400" "曇夜は雪" "CLOUDY  SNOW IN THE NIGHT")
    (231 "200.svg" "200.svg" "200" "曇海上海岸は霧か霧雨" "CLOUDY  FOG OR DRIZZLING ON THE SEA AND NEAR SEASHORE")
    (240 "202.svg" "202.svg" "300" "曇時々雨で雷を伴う" "CLOUDY  FREQUENT SCCATERED SHOWERS AND THUNDER")
    (250 "204.svg" "204.svg" "400" "曇時々雪で雷を伴う" "CLOUDY  FREQUENT SNOW AND THUNDER")
    (260 "204.svg" "204.svg" "400" "曇一時雪か雨" "CLOUDY  SNOW FLURRIES OR OCCASIONAL SCATTERED SHOWERS")
    (270 "204.svg" "204.svg" "400" "曇時々雪か雨" "CLOUDY  FREQUENT SNOW FLURRIES OR SCATTERED SHOWERS")
    (281 "215.svg" "215.svg" "400" "曇後雪か雨" "CLOUDY  SNOW OR RAIN LATER")
    (300 "300.svg" "300.svg" "300" "雨" "RAIN")
    (301 "301.svg" "701.svg" "300" "雨時々晴" "RAIN  PARTLY CLOUDY")
    (302 "302.svg" "302.svg" "300" "雨時々止む" "SHOWERS THROUGHOUT THE DAY")
    (303 "303.svg" "303.svg" "400" "雨時々雪" "RAIN FREQUENT SNOW FLURRIES")
    (304 "300.svg" "300.svg" "300" "雨か雪" "RAINORSNOW")
    (306 "300.svg" "300.svg" "300" "大雨" "HEAVYRAIN")
    (308 "308.svg" "308.svg" "300" "雨で暴風を伴う" "RAINSTORM")
    (309 "303.svg" "303.svg" "400" "雨一時雪" "RAIN OCCASIONAL SNOW")
    (311 "311.svg" "711.svg" "300" "雨後晴" "RAIN CLEAR LATER")
    (313 "313.svg" "313.svg" "300" "雨後曇" "RAIN CLOUDY LATER")
    (314 "314.svg" "314.svg" "400" "雨後時々雪" "RAIN  FREQUENT SNOW FLURRIES LATER")
    (315 "314.svg" "314.svg" "400" "雨後雪" "RAIN SNOW LATER")
    (316 "311.svg" "711.svg" "300" "雨か雪後晴" "RAIN OR SNOW  CLEAR LATER")
    (317 "313.svg" "313.svg" "300" "雨か雪後曇" "RAIN OR SNOW  CLOUDY LATER")
    (320 "311.svg" "711.svg" "300" "朝の内雨後晴" "RAIN IN THE MORNING  CLEAR LATER")
    (321 "313.svg" "313.svg" "300" "朝の内雨後曇" "RAIN IN THE MORNING  CLOUDY LATER")
    (322 "303.svg" "303.svg" "400" "雨朝晩一時雪" "OCCASIONAL SNOW IN THE MORNING AND EVENING  RAIN DURING THE DAY")
    (323 "311.svg" "711.svg" "300" "雨昼頃から晴" "RAIN  CLEAR IN THE AFTERNOON")
    (324 "311.svg" "711.svg" "300" "雨夕方から晴" "RAIN  CLEAR IN THE EVENING")
    (325 "311.svg" "711.svg" "300" "雨夜は晴" "RAIN  CLEAR IN THE NIGHT")
    (326 "314.svg" "314.svg" "400" "雨夕方から雪" "RAIN  SNOW IN THE EVENING")
    (327 "314.svg" "314.svg" "400" "雨夜は雪" "RAIN SNOW IN THE NIGHT")
    (328 "300.svg" "300.svg" "300" "雨一時強く降る" "RAIN  EXPECT OCCASIONAL HEAVY RAINFALL")
    (329 "300.svg" "300.svg" "300" "雨一時みぞれ" "RAIN  OCCASIONAL SLEET")
    (340 "400.svg" "400.svg" "400" "雪か雨" "SNOWORRAIN")
    (350 "300.svg" "300.svg" "300" "雨で雷を伴う" "RAIN AND THUNDER")
    (361 "411.svg" "811.svg" "400" "雪か雨後晴" "SNOW OR RAIN  CLEAR LATER")
    (371 "413.svg" "413.svg" "400" "雪か雨後曇" "SNOW OR RAIN  CLOUDY LATER")
    (400 "400.svg" "400.svg" "400" "雪" "SNOW")
    (401 "401.svg" "801.svg" "400" "雪時々晴" "SNOW  FREQUENT CLEAR")
    (402 "402.svg" "402.svg" "400" "雪時々止む" "SNOWTHROUGHOUT THE DAY")
    (403 "403.svg" "403.svg" "400" "雪時々雨" "SNOW FREQUENT SCCATERED SHOWERS")
    (405 "400.svg" "400.svg" "400" "大雪" "HEAVYSNOW")
    (406 "406.svg" "406.svg" "400" "風雪強い" "SNOWSTORM")
    (407 "406.svg" "406.svg" "400" "暴風雪" "HEAVYSNOWSTORM")
    (409 "403.svg" "403.svg" "400" "雪一時雨" "SNOW  OCCASIONAL SCCATERED SHOWERS")
    (411 "411.svg" "811.svg" "400" "雪後晴" "SNOW CLEAR LATER")
    (413 "413.svg" "413.svg" "400" "雪後曇" "SNOW CLOUDY LATER")
    (414 "414.svg" "414.svg" "400" "雪後雨" "SNOW RAIN LATER")
    (420 "411.svg" "811.svg" "400" "朝の内雪後晴" "SNOW IN THE MORNING  CLEAR LATER")
    (421 "413.svg" "413.svg" "400" "朝の内雪後曇" "SNOW IN THE MORNING  CLOUDY LATER")
    (422 "414.svg" "414.svg" "400" "雪昼頃から雨" "SNOW  RAIN IN THE AFTERNOON")
    (423 "414.svg" "414.svg" "400" "雪夕方から雨" "SNOW  RAIN IN THE EVENING")
    (425 "400.svg" "400.svg" "400" "雪一時強く降る" "SNOW  EXPECT OCCASIONAL HEAVY SNOWFALL")
    (426 "400.svg" "400.svg" "400" "雪後みぞれ" "SNOW  SLEET LATER")
    (427 "400.svg" "400.svg" "400" "雪一時みぞれ" "SNOW  OCCASIONAL SLEET")
    (450 "400.svg" "400.svg" "400" "雪で雷を伴う" "SNOW AND THUNDER")))

;;;;; 天気コードデータの取得とアクセッサ

(defun jma-weather-code-data (code)
  (assq (if (stringp code) (string-to-number code) code) jma-weather-code-list))

(defun jma-weather-code-data-image-daytime (data) (nth 1 data))
(defun jma-weather-code-data-image-night (data) (nth 2 data))
(defun jma-weather-code-data-image (data night-p) (nth (if night-p 2 1) data))
(defun jma-weather-code-data-text-ja (data) (nth 4 data))
(defun jma-weather-code-data-text-en (data) (nth 5 data))

;;;;; 天気コードから各種情報の取得

(defun jma-weather-code-image-daytime (code)
  (jma-weather-code-data-image-daytime (jma-weather-code-data code)))

(defun jma-weather-code-image-night (code)
  (jma-weather-code-data-image-night (jma-weather-code-data code)))

(defun jma-weather-code-image (code night-p)
  (jma-weather-code-data-image (jma-weather-code-data code) night-p))

(defun jma-weather-code-text-ja (code)
  (jma-weather-code-data-text-ja (jma-weather-code-data code)))

(defun jma-weather-code-text-en (code)
  (jma-weather-code-data-text-en (jma-weather-code-data code)))

;;;;; 画像

(defvar jma-weather-code-image-dir (locate-user-emacs-file ".jma-weather-cache"))

(defun jma-weather-code-image-delete-all ()
  (interactive)
  (when jma-weather-code-image-dir
    (let ((files (directory-files jma-weather-code-image-dir t "\\.svg$")))
      (unless files
        (error "No file is found"))
      (when (yes-or-no-p (format "%s\nDelete above files in `%s'? "
                                 (mapconcat #'file-name-nondirectory files " ")
                                 jma-weather-code-image-dir))
        (dolist (path files)
          (delete-file path))))))

(defun jma-weather-code-image-download-file (filename &optional override-p)
  "画像をダウンロードします。"
  (let ((url (concat "https://www.jma.go.jp/bosai/forecast/img/" filename))
        (dst-file (expand-file-name filename jma-weather-code-image-dir)))
    (when (or override-p
              (not (file-exists-p dst-file)))
      (make-directory jma-weather-code-image-dir t)
      (url-copy-file url dst-file t))))

(defun jma-weather-code-image-download-all (&optional override-p)
  "全ての天気コード画像をダウンロードします。"
  (interactive)
  (dolist (data jma-weather-code-list)
    (jma-weather-code-image-download-file
     (jma-weather-code-data-image-daytime data) override-p)
    (jma-weather-code-image-download-file
     (jma-weather-code-data-image-night data) override-p)))

(defun jma-weather-code-image-download (code night-p &optional override-p)
  (jma-weather-code-image-download-file
   (jma-weather-code-image code night-p)
   override-p))

(defun jma-weather-code-image-file-path (code night-p)
  (expand-file-name
   (jma-weather-code-image code night-p)
   jma-weather-code-image-dir))

(defvar jma-weather-code-image-default-height nil
  "デフォルトの画像の高さ。nilのとき (default-font-height) を使う")

(defun jma-weather-code-image-create (code &optional night-p image-height)
  "指定された天気コードに対応するImage Descriptorを返します。

テキスト端末の場合やSVGをサポートしていない場合はnilを返します。"
  (when (jma-support-svg-p)
    (jma-weather-code-image-download code night-p)
    (create-image (jma-weather-code-image-file-path code night-p)
                  'svg nil
                  :height (or image-height
                              jma-weather-code-image-default-height
                              (default-font-height))
                  :scale 1
                  :ascent 'center)))
;;Usage: (insert-image (jma-weather-code-image-create 300 nil))

(defun jma-weather-code-image-string (code &optional night-p image-height)
  "指定された天気コードに対応する画像を表示する文字列を返します。

テキスト端末の場合やSVGをサポートしていない場合はnilを返します。"
  (when (jma-support-svg-p)
    (propertize "_"
                'display
                (jma-weather-code-image-create code night-p image-height))))
;;Usage: (insert (jma-weather-code-image-string 100 nil))

(defun jma-support-svg-p ()
  (and
   (display-graphic-p)
   (fboundp 'image-type-available-p)
   (image-type-available-p 'svg)))



(provide 'jma-weather-code)
;;; jma-weather-code.el ends here
