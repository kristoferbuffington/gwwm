;;;
;;; gwwm/packages/dock.scm - A simple dock for gwwm.
;;;
;;; Copyright (C) 2023 Jules
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; To use this dock, add the following to your ~/.config/gwwm/init.scm:
;;;
;;; (use-modules (gwwm packages dock) (gwwm hooks))
;;; (add-hook! gwwm-after-init-hook (lambda (h) (start-dock)))
;;;

(define-module (gwwm packages dock)
  #:use-module (oop goops)
  #:use-module (wlroots types scene)
  #:use-module (wlroots util box)
  #:use-module (gwwm)
  #:use-module (gwwm monitor)
  #:use-module (gwwm listener)
  #:use-module (cairo)
  #:use-module (gwwm buffer)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 control)
  #:use-module (system base dirent)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (gwwm hooks)
  #:use-module (gwwm commands)
  #:use-module (srfi srfi-1)

  #:export (start-dock stop-dock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dock-height 48)
(define icon-cache (make-hash-table))

;; State variables for the dock
(define dock-scene-node #f)
(define dock-cairo-buffer #f)
(define dock-timer-thread #f)
(define dock-click-handler #f)
(define applications (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Information Gathering & Application Discovery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-battery-info)
  (catch #t
    (lambda ()
      (let* ((base-path "/sys/class/power_supply/")
             (dir (opendir base-path)))
        (let loop ()
          (let ((entry (readdir dir)))
            (if (eof-object? entry)
                (begin (closedir dir) #f)
                (let ((path (string-append base-path (dirent:name entry))))
                  (if (and (not (string=? (dirent:name entry) "."))
                           (not (string=? (dirent:name entry) ".."))
                           (file-is-directory? path)
                           (file-exists? (string-append path "/type"))
                           (string=? (call-with-input-file (string-append path "/type") read-line)
                                     "Battery"))
                      (let ((capacity (call-with-input-file (string-append path "/capacity") read-line))
                            (status (call-with-input-file (string-append path "/status") read-line)))
                        (closedir dir)
                        (list (string->number capacity) status))
                      (loop))))))))
    (lambda (key . args) #f)))

(define (get-wifi-info)
  (catch #t
    (lambda ()
      (let ((pipe (open-input-pipe "nmcli -t -f active,ssid dev wifi")))
        (let loop ()
          (let ((line (read-line pipe)))
            (if (eof-object? line)
                (begin (close-pipe pipe) #f)
                (let ((parts (string-split line #\:)))
                  (if (and (>= (length parts) 2)
                           (string=? (car parts) "yes")
                           (not (string-null? (cadr parts))))
                      (begin (close-pipe pipe) (cadr parts))
                      (loop))))))))
    (lambda (key . args) #f)))

(define (string-suffix? suffix s)
  (let ((slen (string-length s)) (suflen (string-length suffix)))
    (and (>= slen suflen) (string=? suffix (substring s (- slen suflen))))))

(define (parse-desktop-file path)
  (catch #t
    (lambda ()
      (let ((port (open-input-file path)))
        (let loop ((line (read-line port)) (in-entry #f) (result '()))
          (if (eof-object? line)
              (begin
                (close-port port)
                (if (and (assoc-ref result "Name") (assoc-ref result "Exec")
                         (let ((nodisplay (assoc-ref result "NoDisplay")))
                           (or (not nodisplay) (not (string=? nodisplay "true"))))
                         (equal? (assoc-ref result "Type" "Application") "Application"))
                    result
                    #f))
              (let ((trimmed-line (string-trim line)))
                (cond
                  ((string=? trimmed-line "[Desktop Entry]")
                   (loop (read-line port) #t result))
                  ((and in-entry (string-index trimmed-line #\=))
                   (let* ((pos (string-index trimmed-line #\=))
                          (key (string-trim (substring trimmed-line 0 pos)))
                          (value (string-trim (substring trimmed-line (+ pos 1)))))
                     (loop (read-line port) #t (acons key value result))))
                  (else (loop (read-line port) in-entry result))))))))
    (lambda (key . args) #f)))

(define (find-desktop-files-in-dir dir)
  (if (not (file-exists? dir)) '()
      (catch #t
        (lambda ()
          (let ((d (opendir dir)))
            (let loop ((files '()))
              (let ((f (readdir d)))
                (if (eof-object? f)
                    (begin (closedir d)
                           (map (lambda (file) (string-append dir "/" file))
                                (filter (lambda (fname) (string-suffix? ".desktop" fname))
                                        (reverse files))))
                    (loop (cons (dirent:name f) files)))))))
        (lambda args '()))))

(define (find-applications)
  (let* ((home (getenv "HOME"))
         (dirs (list "/usr/share/applications"
                     (if home (string-append home "/.local/share/applications") #f)))
         (desktop-files (append-map find-desktop-files-in-dir (filter identity dirs))))
    (filter-map parse-desktop-file desktop-files)))

(define (get-applications)
  (if (applications) (applications)
      (let ((apps (find-applications)))
        (applications apps)
        apps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (load-icon icon-name)
  (or (hash-ref icon-cache icon-name)
      (let* ((path (string-append "/usr/share/icons/hicolor/48x48/apps/" icon-name ".png"))
             (surface (if (file-exists? path)
                          (catch #t
                            (lambda () (cairo-image-surface-create-from-png path))
                            (lambda args #f))
                          #f)))
        (if surface (begin (hash-set! icon-cache icon-name surface) surface) #f))))

(define (draw-dock cr width height battery-info wifi-info applications)
  (cairo-set-source-rgba cr 0.1 0.1 0.1 0.8)
  (cairo-paint cr)
  (let ((time-str (strftime "%H:%M:%S" (localtime (current-time)))))
    (cairo-set-source-rgb cr 1 1 1)
    (cairo-select-font-face cr "sans-serif" 'normal 'normal)
    (cairo-set-font-size cr 16)
    (cairo-move-to cr 10 30)
    (cairo-show-text cr time-str))
  (when battery-info
    (let ((battery-str (format #f "BAT: ~a% (~a)" (car battery-info) (cadr battery-info))))
      (cairo-set-source-rgb cr 1 1 1)
      (cairo-move-to cr 120 30)
      (cairo-show-text cr battery-str)))
  (when wifi-info
    (let ((wifi-str (format #f "WIFI: ~a" wifi-info)))
      (cairo-set-source-rgb cr 1 1 1)
      (cairo-move-to cr 280 30)
      (cairo-show-text cr wifi-str)))
  (let* ((icon-size 40) (padding 4)
         (num-apps (length applications))
         (grid-width (* num-apps (+ icon-size padding)))
         (grid-start-x (- width grid-width 10)))
    (let loop ((apps applications) (i 0))
      (when (not (null? apps))
        (let* ((app (car apps))
               (x (+ grid-start-x (* i (+ icon-size padding))))
               (icon-name (assoc-ref app "Icon"))
               (icon-surface (if icon-name (load-icon icon-name) #f)))
          (if icon-surface
              (begin (cairo-set-source-surface cr icon-surface x 4) (cairo-paint cr))
              (begin
                (cairo-set-source-rgb cr 0.3 0.3 0.8)
                (cairo-rectangle cr x 4 icon-size icon-size)
                (cairo-fill cr))))
        (loop (cdr apps) (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dock Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (redraw-dock battery-info wifi-info)
  (when dock-cairo-buffer
    (let* ((width (cairo-image-surface-get-width dock-cairo-buffer))
           (height (cairo-image-surface-get-height dock-cairo-buffer))
           (cr (cairo-buffer-cairo dock-cairo-buffer))
           (apps (get-applications)))
      (draw-dock cr width height battery-info wifi-info apps)
      (wlr-scene-buffer-set-buffer dock-scene-node dock-cairo-buffer))))

(define (start-dock-timer)
  (call-with-new-thread
   (lambda ()
     (let ((counter 0) (battery-info #f) (wifi-info #f))
       (let loop ()
         (if (zero? (modulo counter 5))
             (set! battery-info (get-battery-info)))
         (if (zero? (modulo counter 10))
             (set! wifi-info (get-wifi-info)))
         (redraw-dock battery-info wifi-info)
         (sleep 1)
         (set! counter (+ counter 1))
         (loop))))))

(define (stop-dock . args)
  (when dock-scene-node
    (if (thread? dock-timer-thread) (leave-thread dock-timer-thread))
    (set! dock-timer-thread #f)
    (if dock-click-handler (remove-hook! cursor-button-event-hook dock-click-handler))
    (set! dock-click-handler #f)
    (wlr-scene-node-destroy (.node dock-scene-node))
    (set! dock-scene-node #f)
    (if dock-cairo-buffer (wlr-buffer-drop dock-cairo-buffer))
    (set! dock-cairo-buffer #f)
    (remove-hook! gwwm-cleanup-hook stop-dock)))

(define (start-dock)
  (if dock-scene-node
      (send-log WARNING "Dock already running.")
      (let ((monitor (current-monitor)))
        (when monitor
          (let* ((m-area (monitor-area monitor))
                 (width (box-width m-area))
                 (height dock-height)
                 (apps (get-applications)))
            (set! dock-cairo-buffer (cairo-buffer-create width height))
            (set! dock-scene-node (wlr-scene-buffer-create top-layer dock-cairo-buffer))
            (wlr-scene-node-set-position (.node dock-scene-node) (box-x m-area)
                                         (- (+ (box-y m-area) (box-height m-area)) height))
            (set! dock-click-handler
                  (lambda (event)
                    (let* ((cursor (gwwm-cursor)) (x (.x cursor)) (y (.y cursor))
                           (m-height (box-height m-area)) (m-width (box-width m-area))
                           (icon-size 40) (padding 4) (num-apps (length apps))
                           (grid-width (* num-apps (+ icon-size padding)))
                           (grid-start-x (- m-width grid-width 10)))
                      (when (and (eq? (.state event) 'WLR_BUTTON_PRESSED)
                                 (> y (- m-height height)) (> x grid-start-x))
                        (let* ((index (floor->exact (/ (- x grid-start-x) (+ icon-size padding)))))
                          (when (and (>= index 0) (< index num-apps))
                            (let ((app (list-ref apps index)))
                              (when app
                                (let* ((exec-str (assoc-ref app "Exec"))
                                       (parts (string-split exec-str #\ )))
                                  (spawn (car parts) (cdr parts)))))))))))
            (add-hook! cursor-button-event-hook dock-click-handler)
            (set! dock-timer-thread (start-dock-timer))
            (add-hook! gwwm-cleanup-hook stop-dock))))))
