;;; jma-forecast.el --- JMA Forecast                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Japan,Weather,Diary,Org

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

;; * 使用例
;;
;; ** 今日の東京の天気予報を取得する
;;
;;   (jma-forecast-weather-for-date
;;     (jma-forecast-get "130000") ;;130000はoffice-code(府県予報区)
;;     "130010" ;;class10-code(一次細分区域)
;;     "44132" ;;amedas-code(アメダス観測所番号)
;;     "130010" ;;week-area-code(週間予報区域。府県予報区を少し細分化したもの)
;;     "44132" ;;week-amedas-code(週間予報でのアメダス観測所番号)
;;     (jma-date-today)) ;; 日付はcalendarパッケージと同じ形式(MM DD YYYY)
;;
;; ** 東京の生の予報データを取得する
;;
;;   (let ((forecast (jma-forecast-get "130000")))
;;     ....)
;;
;; ** 長野県の特定の日の天気予報を取得し、文字列化し、バッファに挿入する
;;
;;   (let* ((forecast (jma-forecast-get "200000"))
;;          (weather (jma-forecast-weather-for-date
;;                    forecast
;;                    "200020" ;;中部
;;                    "48491" ;;諏訪
;;                    '("200100" "200000") ;;中部・南部 または 長野県全域
;;                    '("48361" "48156") ;;松本 または 長野
;;                    (jma-date 2022 2 23))) ;;2022年2月23日
;;          (weather-str (jma-weather-to-string weather)))
;;     (when weather-str
;;       (insert weather-str)))

;; * エリアコード
;;
;; 天気予報で使用する場所を指定するためのコードには次のものがある。
;;
;; - office : 府県予報区 (概ね都道府県)
;; - class10 : 一次細分区域 (府県の中を大ざっぱに分割したもの)
;; - week-area : 週間予報区域(府県予報区を少し細分化したもの)(※独自呼称)
;; - amedas : アメダス観測所
;;
;; 天気予報データは府県予報区(office)単位で取得できる。
;;
;; 天気予報(明後日までの詳細)は一次細分区域(class10)毎に行われている。
;; (参考: https://www.jma.go.jp/jma/kishou/know/saibun/)
;;
;; 週間天気予報は原則として府県予報区毎に行われているが、地形や季節等
;; の都合で多少の細分化が行われている。ここでは細分化後の区域を週間予
;; 報区域(week-area)と呼ぶことにする。
;; (参考: https://www.jma.go.jp/jma/kishou/know/kurashi/shukan.html)
;;
;; 気温や降水量に関する情報はアメダス観測所(amedas)毎に発表されている。
;; (参考: https://www.jma.go.jp/jma/kishou/know/amedas/kaisetsu.html)
;;
;; 例:
;; 長野県(office=200000)
;; - 詳細予報
;;   - 一次細分区域(class10)
;;     - 200010 北部
;;     - 200020 中部
;;     - 200030 南部
;;   - アメダス観測所(amedas)
;;     - 48156 長野 (北部)
;;     - 48361 松本 (中部)
;;     - 48491 諏訪 (中部)
;;     - 48767 飯田 (南部)
;;     - 48331 軽井沢 (中部)
;; - 週間予報
;;   - 週間予報区域(week-area) (※季節によっては分割されないことに注意)
;;     - 200010 北部
;;     - 200100 中部・南部
;;   - アメダス観測所(amedas)
;;     - 48156 長野 (北部)
;;     - 48361 松本 (中部・南部)
;; (2022-02-23現在)
;; 人間閲覧用URL: https://www.jma.go.jp/bosai/forecast/#area_type=offices&area_code=200000
;; JSON URL: https://www.jma.go.jp/bosai/forecast/data/forecast/200000.json
;;
;; 各エリア間の関係性は次のURLから得られる。
;; - https://www.jma.go.jp/bosai/forecast/const/forecast_area.json (class10とamedas)
;; - https://www.jma.go.jp/bosai/forecast/const/week_area.json (week-areaとamedas)
;; - https://www.jma.go.jp/bosai/common/const/area.json (officeとclass10)
;;

;; * 天気予報のミニバッファ表示
;;
;; M-x jma-forecast-show を実行するとミニバッファに天気予報を表示する。
;;
;; 事前に次の変数で予報する地域を設定しておく必要がある。
;; - jma-forecast-location-office
;; - jma-forecast-location-class10
;; - jma-forecast-location-amedas
;; - jma-forecast-location-week-area
;; - jma-forecast-location-week-amedas
;;
;; M-x jma-forecast-location-setup を実行すると入力補完付きで予報地域
;; を設定できる。

;; * Diary連携
;;
;; ダイアリーエントリーやorg-modeのagendaファイルに次の文を追加すると、
;; 現在の日付から向こう8日間に天気予報のエントリーが追加される。
;;
;; %%(jma-diary-weathers "130000" "130010" "44132" "130010" "44132")
;;

;;; Code:

(require 'jma-utils)
(require 'jma-weather-code)

;;;; 天気予報

;;;;; 天気予報データの取得

(defun jma-forecast-get (office-code)
  "OFFICE-CODE で指定される府県予報区の天気予報データを返します。"
  (when (symbolp office-code)
    (setq office-code (symbol-name office-code)))
  (when (string-match-p "\\`[0-9]+\\'" office-code)
    (or (jma-forecast-cache-get office-code)
        (jma-forecast-cache-put
         office-code
         ;; Download
         (jma-json-get
          (format "https://www.jma.go.jp/bosai/forecast/data/forecast/%s.json"
                  office-code))))))

;; キャッシュ

(defvar jma-forecast-cache nil)

(defun jma-forecast-cache-make-key (office-code)
  (cond
   ((symbolp office-code)
    office-code)
   ((stringp office-code)
    (intern office-code))
   (t
    (error "Invalid type office-code=%s" office-code))))

(defun jma-forecast-cache-put (office-code forecast)
  (let ((key (jma-forecast-cache-make-key office-code)))
    (setf (alist-get key jma-forecast-cache)
          ;; (CACHE-TIME FORECAST)
          (list (current-time) forecast))
    forecast))

(defun jma-forecast-cache-get (office-code)
  (let* ((key (jma-forecast-cache-make-key office-code))
         (cache (alist-get key jma-forecast-cache)))
    (when cache
      (let* (;; (CACHE-TIME FORECAST)
             (cache-time (nth 0 cache))
             (forecast (nth 1 cache))
             (next-time (jma-forecast-next-report-time forecast))
             (curr-time (current-time))
             (ttl (if (time-less-p curr-time next-time)
                      10800 ;;3*60*60 次回更新予定時刻を過ぎていないとき
                    120))) ;;2*60 次回更新予定時刻を過ぎているとき
        (when (time-less-p curr-time (time-add cache-time ttl))
          forecast)))))



;;;;; 天気予報データの中身へのアクセス

;; 天気予報データには次の二つの発表データが含まれています。
;; - 詳細予報(明後日までの詳細)
;; - 週間予報(翌日から7日間)

(defun jma-forecast-report-at (forecast report-index)
  "天気予報データ FORECAST 内の REPORT-INDEX 番目の発表データを返します。"
  (elt forecast report-index))

(defun jma-forecast-detailed-report (forecast)
  "天気予報データ FORECAST 内の詳細予報発表データを返します。"
  (jma-forecast-report-at forecast 0))

(defun jma-forecast-week-report (forecast)
  "天気予報データ FORECAST 内の週間予報発表データを返します。"
  (jma-forecast-report-at forecast 1))

(defun jma-forecast-next-report-time (forecast)
  "予報の次の更新日時を返します。"
  (let ((t0 (jma-forecast-report-datetime-next
             (jma-forecast-detailed-report forecast) nil))
        (t1 (jma-forecast-report-datetime-next
             (jma-forecast-week-report forecast) t)))
    (if (time-less-p t0 t1) t0 t1)))

;;;;; 発表データの中身へのアクセス

(defun jma-forecast-report-publisher (report)
  "発表データ REPORT の発表者を返します。"
  (alist-get 'publishingOffice report))

(defun jma-forecast-report-datetime-string (report)
  "発表データ REPORT の発表日時を文字列で返します。"
  (alist-get 'reportDatetime report))

(defun jma-forecast-report-datetime (report)
  "発表データ REPORT の発表日時を返します。"
  (parse-iso8601-time-string (jma-forecast-report-datetime-string report)))

(defun jma-forecast-report-time-series-vector (report)
  "発表データ REPORT 内の全時系列データベクトルを返します。"
  (alist-get 'timeSeries report))

(defun jma-forecast-report-time-series-at (report time-series-index)
  "発表データ REPORT の TIME-SERIES-INDEX 番目の時系列データを返します。"
  (elt (jma-forecast-report-time-series-vector report) time-series-index))

(defun jma-forecast-report-datetime-next (report week-p)
  (jma-time-next-hour
   (jma-forecast-report-datetime report)
   (if week-p '(11 17) '(5 11 17))))

;;(jma-forecast-report-datetime-next (elt tokyo-forecast 0) '(5 11 17))

;;;;; 特定位置・特定日の予報へのアクセス

(defun jma-forecast-weather-for-date (forecast class10-code amedas-code week-area-code week-amedas-code date)
  "天気予報データ FORECAST から指定した場所・日付の予報を取り出します。

CLASS10-CODE 一次細分区域コード。

AMEDAS-CODE 気温を取得するためのAMEDAS観測所コード。

WEEK-AREA-CODE 週間予報のための区域コード。

WEEK-AMEDAS-CODE 週間予報における気温を取得するためのAMEDAS観測所コード。"
  (or (jma-forecast-weather-for-date-from-detailed-report
       (jma-forecast-detailed-report forecast) class10-code amedas-code date)
      (jma-forecast-weather-for-date-from-week-report
       (jma-forecast-week-report forecast) week-area-code week-amedas-code date)))

;;(jma-forecast-weather-for-date tokyo-forecast "130010" "130010" "44132" (jma-date 2022 2 23))

;;;;;; (2日間)詳細予報

(defun jma-forecast-weather-for-date-from-detailed-report (detailed-report class10-code amedas-code date)
  "指定した場所・日付の詳細予報を返します。"
  (let* ((time-series-0 (jma-forecast-report-time-series-at detailed-report 0))
         (range (jma-time-series-range-in-date time-series-0 date)))

    (when (jma-not-empty-range-p range)
      (let ((time-index (car range)))
        ;; 注意: 明後日の天気は詳細予報に含まれるが、降水確率や気温の情報は無い。
        ;; 全情報無ければnilを返して週間予報を使うようにする。
        (when-let ((area (jma-time-series-area time-series-0 class10-code))
                   (weather (jma-time-series-area-value-at area 'weathers time-index))
                   (weather-code (jma-time-series-area-value-at area 'weatherCodes time-index))
                   (wind (jma-time-series-area-value-at area 'winds time-index))
                   (pops (jma-forecast-pops-for-date-from-detailed-report detailed-report class10-code date)))
          (let (;; 波は海に面していない区域では含まれない。
                (wave (jma-time-series-area-value-at area 'waves time-index)))
            (append
             (list
              (cons 'report-type "detailed")
              (cons 'class10-area (alist-get 'area area))
              (cons 'weather weather)
              (cons 'weather-code weather-code)
              (cons 'wind wind)
              (cons 'wave wave)
              (cons 'pops pops)
              (cons 'pop (mapconcat #'identity pops "/")))
             (jma-forecast-temps-for-date-from-detailed-report detailed-report amedas-code date))))))))

(defun jma-forecast-pops-for-date-from-detailed-report (detailed-report class10-code date)
  (let* ((time-series-1 (jma-forecast-report-time-series-at detailed-report 1))
         (range (jma-time-series-range-in-date time-series-1 date)))

    (when (jma-not-empty-range-p range)
      (when-let ((area (jma-time-series-area time-series-1 class10-code)))
        (cl-loop for time-index from (car range) to (1- (cdr range))
                 collect (jma-time-series-area-value-at area 'pops time-index))))))

(defun jma-forecast-temps-for-date-from-detailed-report (detailed-report amedas-code date)
  (let* ((time-series-2 (jma-forecast-report-time-series-at detailed-report 2))
         (range (jma-time-series-range-in-date time-series-2 date)))
    (when (jma-not-empty-range-p range)
      (let ((time-index (car range)))
        (when-let ((area (jma-time-series-area time-series-2 amedas-code))
                   (tempMin (jma-time-series-area-value-at area 'temps time-index))
                   (tempMax (jma-time-series-area-value-at area 'temps (1+ time-index))))
          (list
           (cons 'amedas-point (alist-get 'area area))
           (cons 'temp-min tempMin)
           (cons 'temp-max tempMax)))))))

;;;;;; (翌日以降7日間)週間予報

(defun jma-forecast-weather-for-date-from-week-report (week-report week-area-code amedas-code date)
  "指定した場所・日付の週間予報を返します。"
  (let* ((time-series-0 (jma-forecast-report-time-series-at week-report 0))
         (range (jma-time-series-range-in-date time-series-0 date)))
    (when (jma-not-empty-range-p range)
      (let ((time-index (car range)))
        (when-let ((area (jma-time-series-area time-series-0 week-area-code))
                   (weather-code (jma-time-series-area-value-at area 'weatherCodes time-index))
                   (p-o-p (jma-time-series-area-value-at area 'pops time-index))
                   (reliability (jma-time-series-area-value-at area 'reliabilities time-index)))
          (append
           (list
            (cons 'report-type "week")
            (cons 'week-area (alist-get 'area area))
            (cons 'weather (jma-weather-code-text-ja weather-code))
            (cons 'weather-code weather-code)
            (cons 'pop p-o-p)
            (cons 'reliability reliability))
           (jma-forecast-temps-for-date-from-week-report week-report amedas-code date)))))))

(defun jma-forecast-temps-for-date-from-week-report (week-report amedas-code date)
  (let* ((time-series-1 (jma-forecast-report-time-series-at week-report 1))
         (range (jma-time-series-range-in-date time-series-1 date)))
    (when (jma-not-empty-range-p range)
      (let ((time-index (car range)))
        (when-let ((area (jma-time-series-area time-series-1 amedas-code)))
          (list
           (cons 'amedas-point (alist-get 'area area))
           (cons 'temp-min (jma-time-series-area-value-at area 'tempsMin time-index))
           (cons 'temp-min-lower (jma-time-series-area-value-at area 'tempsMinLower time-index))
           (cons 'temp-min-upper (jma-time-series-area-value-at area 'tempsMinUpper time-index))
           (cons 'temp-max (jma-time-series-area-value-at area 'tempsMax time-index))
           (cons 'temp-max-lower (jma-time-series-area-value-at area 'tempsMaxLower time-index))
           (cons 'temp-max-upper (jma-time-series-area-value-at area 'tempsMaxUpper time-index))))))))



;;;;; 天気予報の文字列化

(defcustom jma-weather-default-template
  "{{{weather-image:%s }}}{{{weather}}}{{{pop: %s%%}}}{{{temp-min: %s～}}}{{{temp-max:%s℃}}}"
  "テンプレート文字列(see: `jma-expand-template')"
  :group 'jma
  :type '(choice (string :tag "テンプレート文字列")
                 (function :tag "文字列化関数")))

(defconst jma-weather-default-template-params
  '((weather-image . jma-weather-to-weather-image))) ;;{{{weather-image}}}を指定可能にする

(defun jma-weather-to-weather-image (weather)
  (when-let ((code (alist-get 'weather-code weather)))
    (jma-weather-code-image-string code)))

(defun jma-weather-to-string (weather &optional template)
  (when weather
    (jma-expand-template
     (or template jma-weather-default-template)
     (append
      weather
      jma-weather-default-template-params))))

;;(setq tokyo-forecast (jma-forecast-get "130000"))
;;(insert (jma-weather-to-string (jma-forecast-weather-for-date (jma-forecast-get "130000") "130010" "130010" "44132" (jma-date-today)) nil))

;;;;; 天気予報エリア選択

(defun jma-choose-from-alist (prompt alist) ;;@todo jma-utils.elへ移動する。
  (cdr (assoc (completing-read prompt alist nil t) alist)))

(defvar jma-forecast-cache-area nil)
(defvar jma-forecast-cache-forecast-area nil)
(defvar jma-forecast-cache-week-area nil)
(defvar jma-forecast-cache-week-area-name nil)
(defvar jma-forecast-cache-week-area05 nil)
(defvar jma-forecast-cache-amedas-table nil)

(defun jma-forecast-cache-area-data ()
  (let ((data-list
         '((jma-forecast-cache-area . "https://www.jma.go.jp/bosai/common/const/area.json")
           (jma-forecast-cache-forecast-area . "https://www.jma.go.jp/bosai/forecast/const/forecast_area.json")
           (jma-forecast-cache-week-area . "https://www.jma.go.jp/bosai/forecast/const/week_area.json")
           (jma-forecast-cache-week-area-name . "https://www.jma.go.jp/bosai/forecast/const/week_area_name.json")
           (jma-forecast-cache-week-area05 . "https://www.jma.go.jp/bosai/forecast/const/week_area05.json")
           (jma-forecast-cache-amedas-table . "https://www.jma.go.jp/bosai/amedas/const/amedastable.json"))))
    (dolist (data data-list)
      (let ((var (car data))
            (url (cdr data)))
        (unless (symbol-value var)
          (set var (jma-json-get url)))))))

(defun jma-forecast-area-office-name (office-sym)
  (cdar (alist-get office-sym (alist-get 'offices jma-forecast-cache-area))))

(defun jma-forecast-area-class10-name (class10-sym)
  (cdar (alist-get class10-sym (alist-get 'class10s jma-forecast-cache-area))))

(defun jma-forecast-area-amedas-name (amedas-sym)
  (alist-get 'kjName (alist-get amedas-sym jma-forecast-cache-amedas-table)))

;; jma-forecast-cache-forecast-area

(defun jma-forecast-area-read ()
  (interactive)
  (jma-forecast-cache-area-data) ;; 必要なデータをダウンロード(キャッシュ)

  (let* (;; 府県予報区を選択
         (offices jma-forecast-cache-forecast-area)
         (office
          (jma-choose-from-alist
           "府県予報区: "
           (mapcar (lambda (office)
                     (cons
                      (jma-forecast-area-office-name (car office))
                      office))
                   offices)))
         (office-sym (car office))
         (office-code (symbol-name office-sym))
         ;; 一次細分区域を選択
         (class10
          (jma-choose-from-alist
           "一次細分区域: "
           (mapcar (lambda (class10)
                     (cons
                      (jma-forecast-area-class10-name
                       (intern (alist-get 'class10 class10)))
                      class10))
                   (cdr office))))
         (class10-code (alist-get 'class10 class10))
         ;; アメダス観測所を選択
         (amedas-code
          (jma-choose-from-alist
           "アメダス観測所: "
           (mapcar (lambda (amedas)
                     (cons
                      (jma-forecast-area-amedas-name (intern amedas))
                      amedas))
                   (alist-get 'amedas class10))))
         ;; 現在の週間予報区域を取得(季節によって異なる可能性あり。長野等)
         (forecast (jma-forecast-get office-code))
         (week-report (jma-forecast-week-report forecast))
         (week-ts0 (jma-forecast-report-time-series-at week-report 0))
         (week-areas-in-forecast (alist-get 'areas week-ts0))
         ;; 週間予報区域を選択
         (week-area-code
          (jma-choose-from-alist
           "週間予報区域: "
           (mapcar (lambda (week-area)
                     (cons
                      (alist-get 'name (alist-get 'area week-area))
                      (alist-get 'code (alist-get 'area week-area))))
                   week-areas-in-forecast)))
         ;; 週間予報区域用のアメダス観測所を割り出す。
         (week-areas (alist-get office-sym jma-forecast-cache-week-area))
         (week-area (seq-find (lambda (area) (equal (alist-get 'week area) week-area-code)) week-areas))
         (week-amedas-code (alist-get 'amedas week-area))
         (result (list office-code
                       class10-code
                       amedas-code
                       week-area-code
                       week-amedas-code)))
    (when (interactive-p)
      (message "%s" (prin1-to-string result)))
    result))

;;;;; お手軽表示

(defcustom jma-forecast-location-office nil;;"130000"
  "府県予報区"
  :group 'jma
  :type '(string :tag "番号"))

(defcustom jma-forecast-location-class10 nil;;"130010"
  "一次細分区域"
  :group 'jma
  :type '(choice (string :tag "番号")
                 (repeat :tag "優先順リスト" (string :tag "番号"))))

(defcustom jma-forecast-location-amedas nil;;"44132"
  "アメダス観測所"
  :group 'jma
  :type '(choice (string :tag "番号")
                 (repeat :tag "優先順リスト" (string :tag "番号"))))

(defcustom jma-forecast-location-week-area nil;;"130010"
  "週間予報区域"
  :group 'jma
  :type '(choice (string :tag "番号")
                 (repeat :tag "優先順リスト" (string :tag "番号"))))

(defcustom jma-forecast-location-week-amedas nil;;"44132"
  "アメダス観測所(週間)"
  :group 'jma
  :type '(choice (string :tag "番号")
                 (repeat :tag "優先順リスト" (string :tag "番号"))))

(defcustom jma-forecast-date-format "%m/%d(%a)"
  "日付の形式"
  :group 'jma
  :type '(choice (string :tag "`format-time-string'の書式")
                 (function :tag "文字列化関数")))

(defun jma-forecast-date-format (date)
  (let ((time (jma-date-to-time date)))
    (cond
     ((functionp jma-forecast-date-format)
      (funcall jma-forecast-date-format time))
     ((stringp jma-forecast-date-format)
      (format-time-string jma-forecast-date-format time))
     (t ""))))

(defun jma-forecast-show ()
  (interactive)
  (unless (and jma-forecast-location-office
               jma-forecast-location-class10
               jma-forecast-location-amedas
               jma-forecast-location-week-area
               jma-forecast-location-week-amedas)
    (call-interactively #'jma-forecast-location-setup))
  (let ((forecast (jma-forecast-get jma-forecast-location-office)))
    (message "%s"
             (mapconcat
              #'identity
              (cl-loop for n from 0 to 7
                       for date = (jma-date-inc-day (jma-date-today) n)
                       collect
                       (concat
                        (jma-forecast-date-format date)
                        " "
                        (jma-weather-to-string
                         (jma-forecast-weather-for-date
                          forecast
                          jma-forecast-location-class10
                          jma-forecast-location-amedas
                          jma-forecast-location-week-area
                          jma-forecast-location-week-amedas
                          date))))
              "\n"))))

(defun jma-forecast-location-setup (office-code class10-code amedas-code week-area-code week-amedas-code)
  "デフォルトの予報地点を設定します。

次の変数を変更します。

`jma-forecast-location-office'
`jma-forecast-location-class10'
`jma-forecast-location-amedas'
`jma-forecast-location-week-area'
`jma-forecast-location-week-amedas'

注意: 季節的な要因によってエリアコードは変わる可能性があります。
そのような地域を指定する場合は、手動で設定を調整する必要がありま
す。これらの変数にリストを指定することで、複数のエリアコードの中
から最初に有効なものを使わせることができます。"
  (interactive
   (jma-forecast-area-read))
  (customize-set-variable 'jma-forecast-location-office office-code)
  (customize-set-variable 'jma-forecast-location-class10 class10-code)
  (customize-set-variable 'jma-forecast-location-amedas amedas-code)
  (customize-set-variable 'jma-forecast-location-week-area week-area-code)
  (customize-set-variable 'jma-forecast-location-week-amedas week-amedas-code)
  (when (y-or-n-p "変更を保存しますか？")
    (customize-save-variable 'jma-forecast-location-office office-code)
    (customize-save-variable 'jma-forecast-location-class10 class10-code)
    (customize-save-variable 'jma-forecast-location-amedas amedas-code)
    (customize-save-variable 'jma-forecast-location-week-area week-area-code)
    (customize-save-variable 'jma-forecast-location-week-amedas week-amedas-code)))

;;;;; diary連携

(defun jma-diary-weathers (&optional office-code class10-code amedas-code week-area-code week-amedas-code template)
  "`date'変数が示す日付の天気予報文字列を返します。

予報する場所は次の引数で指定します。

OFFICE-CODE : 府県予報区

CLASS10-CODE : 一次細分区域

AMEDAS-CODE : アメダス観測所(詳細予報で使用)

WEEK-AREA-CODE : 週間予報区域(府県予報区を必要に応じて多少細分化したもの)

WEEK-AMEDAS-CODE : アメダス観測所(週間予報で使用)

天気予報からデータから文字列への変換にはTEMPLATE引数を使用します。
テンプレートの書き方は`jma-expand-template'関数を参照してください。
指定しなかった場合、`jma-weather-default-template'変数の値が使われます。"
  (with-no-warnings (defvar date))
  (let* ((forecast (jma-forecast-get (or office-code jma-forecast-location-office)))
         (weather (jma-forecast-weather-for-date
                   forecast
                   (or class10-code jma-forecast-location-class10)
                   (or amedas-code jma-forecast-location-amedas)
                   (or week-area-code jma-forecast-location-week-area)
                   (or week-amedas-code jma-forecast-location-week-amedas)
                   date)))
    (jma-weather-to-string weather template)))


(provide 'jma-forecast)
;;; jma-forecast.el ends here
