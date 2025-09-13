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
;;; (use-modules (gwwm packages dock))
;;; (start-dock)
;;;

(define-module (gwwm packages dock)
  #:use-module (oop goops)
  #:use-module (wayland server protocol wayland)
  #:use-module (wlroots types layer-shell)
  #:use-module (wlroots types output)
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

  #:export (start-dock))

;; The height of the dock in pixels.
(define-once dock-height 48)
;; A cache for loaded application icons.
(define-once icon-cache (make-hash-table))

;;;
;;; Information Gathering
;;;

;; Get battery information from /sys/class/power_supply.
;; Returns a list of (capacity status) or #f if no battery is found.
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
    (lambda (key . args)
      #f)))

;; Get Wi-Fi information using nmcli.
;; Returns the SSID of the active connection or #f.
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
    (lambda (key . args)
      #f)))

;;;
;;; Application Discovery
;;;

;; Check if a string ends with a given suffix.
(define (string-suffix? suffix s)
  (let ((slen (string-length s))
        (suflen (string-length suffix)))
    (and (>= slen suflen)
         (string=? suffix (substring s (- slen suflen))))))

;; Parse a .desktop file and return an alist of its properties.
(define (parse-desktop-file path)
  (catch #t
    (lambda ()
      (let ((port (open-input-file path)))
        (let loop ((line (read-line port)) (in-entry #f) (result '()))
          (if (eof-object? line)
              (begin
                (close-port port)
                (if (and (assoc-ref result "Name")
                         (assoc-ref result "Exec")
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
                  (else
                   (loop (read-line port) in-entry result))))))))
    (lambda (key . args)
      #f)))

;; Find all .desktop files in a directory.
(define (find-desktop-files-in-dir dir)
  (if (not (file-exists? dir))
      '()
      (catch #t
        (lambda ()
          (let ((d (opendir dir)))
            (let loop ((files '()))
              (let ((f (readdir d)))
                (if (eof-object? f)
                    (begin
                      (closedir d)
                      (map (lambda (file) (string-append dir "/" file))
                           (filter (lambda (fname) (string-suffix? ".desktop" fname))
                                   (reverse files))))
                    (loop (cons (dirent:name f) files)))))))
        (lambda args '()))))

;; Find all applications by scanning standard .desktop file locations.
(define (find-applications)
  (let* ((home (getenv "HOME"))
         (dirs (list "/usr/share/applications"
                     (if home (string-append home "/.local/share/applications") #f)))
         (desktop-files (append-map find-desktop-files-in-dir (filter identity dirs))))
    (filter-map parse-desktop-file desktop-files)))

;; Get the list of applications, caching the result.
(define-once applications (make-parameter #f))
(define (get-applications)
  (if (applications)
      (applications)
      (let ((apps (find-applications)))
        (applications apps)
        apps)))

;;;
;;; Drawing
;;;

;; Load an application icon.
;; NOTE: This is a simplified implementation. It does not follow the full
;; icon theme specification. It only looks for 48x48 PNG icons in hicolor.
(define (load-icon icon-name)
  (or (hash-ref icon-cache icon-name)
      (let* ((path (string-append "/usr/share/icons/hicolor/48x48/apps/" icon-name ".png"))
             (surface (if (file-exists? path)
                          (catch #t
                            (lambda () (cairo-image-surface-create-from-png path))
                            (lambda args #f))
                          #f)))
        (if surface
            (begin
              (hash-set! icon-cache icon-name surface)
              surface)
            #f))))

;; Draw the entire dock.
(define (draw-dock cr width height battery-info wifi-info applications)
  ;; Draw background
  (cairo-set-source-rgba cr 0.1 0.1 0.1 0.8)
  (cairo-paint cr)

  ;; Draw Clock (left-aligned)
  (let ((time-str (strftime "%H:%M:%S" (localtime (current-time)))))
    (cairo-set-source-rgb cr 1 1 1)
    (cairo-select-font-face cr "sans-serif" 'normal 'normal)
    (cairo-set-font-size cr 16)
    (cairo-move-to cr 10 30)
    (cairo-show-text cr time-str))

  ;; Draw Battery (left-aligned)
  (when battery-info
    (let ((battery-str (format #f "BAT: ~a% (~a)" (car battery-info) (cadr battery-info))))
      (cairo-set-source-rgb cr 1 1 1)
      (cairo-move-to cr 120 30)
      (cairo-show-text cr battery-str)))

  ;; Draw WIFI (left-aligned)
  (when wifi-info
    (let ((wifi-str (format #f "WIFI: ~a" wifi-info)))
      (cairo-set-source-rgb cr 1 1 1)
      (cairo-move-to cr 280 30)
      (cairo-show-text cr wifi-str)))

  ;; Draw applications (right-aligned)
  (let* ((icon-size 40)
         (padding 4)
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
              (begin
                (cairo-set-source-surface cr icon-surface x 4)
                (cairo-paint cr))
              (begin ;; Fallback placeholder
                (cairo-set-source-rgb cr 0.3 0.3 0.8)
                (cairo-rectangle cr x 4 icon-size icon-size)
                (cairo-fill cr))))
        (loop (cdr apps) (+ i 1))))))

;;;
;;; Dock Creation and Management
;;;

;; Create the dock's layer surface and set up all its behavior.
(define (create-layer-surface output)
  (let* ((width (box-width (monitor-area (wlr-output->monitor output))))
         (height dock-height)
         (surface (wlr-layer-surface-v1-create
                   (gwwm-layer-shell) output "dock"
                   'ZWLR_LAYER_SHELL_V1_LAYER_TOP))
         (cairo-buffer (cairo-buffer-create width height))
         (scene-buffer (wlr-scene-buffer-create top-layer cairo-buffer))
         (apps (get-applications))
         (click-handler #f))

    (wlr-layer-surface-v1-set-anchor surface 'ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM)
    (wlr-layer-surface-v1-set-size surface width height)
    (wlr-layer-surface-v1-set-exclusive-zone surface height)

    ;; Set up a hook to handle clicks on the application grid.
    (set! click-handler
          (lambda (event)
            (let* ((cursor (gwwm-cursor))
                   (x (.x cursor))
                   (y (.y cursor))
                   (monitor (current-monitor))
                   (m-width (box-width (monitor-area monitor)))
                   (m-height (box-height (monitor-area monitor)))
                   (icon-size 40)
                   (padding 4)
                   (num-apps (length apps))
                   (grid-width (* num-apps (+ icon-size padding)))
                   (grid-start-x (- m-width grid-width 10)))
              (when (and (eq? (.state event) 'WLR_BUTTON_PRESSED)
                         (> y (- m-height dock-height))
                         (> x grid-start-x))
                (let* ((index (floor->exact (/ (- x grid-start-x) (+ icon-size padding)))))
                  (when (and (>= index 0) (< index num-apps))
                    (let ((app (list-ref apps index)))
                      (when app
                        (let* ((exec-str (assoc-ref app "Exec"))
                               ;; NOTE: This is a simplified command parser. It does not
                               ;; handle quoted arguments or format specifiers like %U.
                               (parts (string-split exec-str #\ )))
                          (spawn (car parts) (cdr parts)))))))))))
    (add-hook! cursor-button-event-hook click-handler)

    (add-listen surface 'map
                (lambda (listener data)
                  (display "Dock mapped\n")
                  (wlr-surface-commit (.surface surface))))
    (add-listen surface 'unmap
                (lambda (listener data)
                  (display "Dock unmapped\n")))
    (let ((clock-thread #f)
          (battery-thread #f)
          (wifi-thread #f)
          (battery-info #f)
          (wifi-info #f))
      (add-listen surface 'destroy
                  (lambda (listener data)
                    (remove-hook! cursor-button-event-hook click-handler)
                    (if (thread? clock-thread)
                        (leave-thread clock-thread))
                    (if (thread? battery-thread)
                        (leave-thread battery-thread))
                    (if (thread? wifi-thread)
                        (leave-thread wifi-thread))
                    (wlr-buffer-drop cairo-buffer)
                    (display "Dock destroyed\n")))

      ;; Thread to update the clock every second.
      (set! clock-thread
            (call-with-new-thread
             (lambda ()
               (let loop ()
                 (sleep 1)
                 (wlr-surface-commit (.surface surface))
                 (loop)))))

      ;; Thread to update the battery status every 5 seconds.
      (set! battery-thread
            (call-with-new-thread
             (lambda ()
               (let loop ()
                 (set! battery-info (get-battery-info))
                 (wlr-surface-commit (.surface surface))
                 (sleep 5)
                 (loop)))))

      ;; Thread to update the Wi-Fi status every 10 seconds.
      (set! wifi-thread
            (call-with-new-thread
             (lambda ()
               (let loop ()
                 (set! wifi-info (get-wifi-info))
                 (wlr-surface-commit (.surface surface))
                 (sleep 10)
                 (loop)))))

      ;; Listen for surface commits (redraw requests).
      (add-listen (.surface surface) 'commit
                  (lambda (listener data)
                  (let* ((state (.current surface))
                         (m (wlr-output->monitor output))
                         (new-width (box-width (monitor-area m))))
                    ;; Resize the buffer if the monitor resolution changes.
                    (when (or (not (= (.configured-width state) new-width))
                              (not (= (.configured-height state) height)))
                      (wlr-layer-surface-v1-set-size surface new-width height)
                      (wlr-buffer-drop cairo-buffer)
                      (set! cairo-buffer (cairo-buffer-create new-width height))
                      (wlr-scene-buffer-set-buffer scene-buffer cairo-buffer)
                      (wlr-surface-commit (.surface surface))
                      (return)))

                  ;; Draw the dock contents.
                  (let ((cr (cairo-buffer-cairo cairo-buffer)))
                    (draw-dock cr
                               (.configured-width (.current surface))
                               (.configured-height (.current surface))
                               battery-info
                               wifi-info
                               apps)
                    (wlr-scene-buffer-set-buffer scene-buffer cairo-buffer))))

    (wlr-surface-commit (.surface surface))
    surface))

;; The main entry point for the dock package.
(define (start-dock)
  (let ((monitor (current-monitor)))
    (when monitor
      (create-layer-surface (monitor-output monitor)))))
