;;; publish.el --- Build systemcrafters.cc

;; Copyright (C) 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;; Copyright (C) 2021 David Wilson <david@daviwil.com>

;; Author: David Wilson <david@daviwil.com>
;; Maintainer: David Wilson <david@daviwil.com>
;; URL: https://sr.ht/~systemcrafters/site
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: hypermedia, blog, feed, rss

;; This file is not part of GNU Emacs.

;; This file is loosely based on Pierre Neidhardt's publish.el, here's his
;; authorship details:

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://gitlab.com/Ambrevar/ambrevar.gitlab.io

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Docs License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Docs License for more details.
;;
;; You should have received a copy of the GNU General Docs License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Usage:
;; emacs -Q --batch -l ./publish.el --funcall dw/publish

;; Initialize package sources
(require 'package)

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(setq package-user-dir (expand-file-name "./.packages"))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Unfortunately this is necessary for now...
(load-file "./ox-slimhtml.el")

;; Install other dependencies
(use-package esxml
  :pin melpa-stable
  :ensure t)

(use-package ox-gemini
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package webfeeder
  :ensure t)

(require 'ox-publish)

(defvar yt-iframe-format
  ;; TODO: Change this after switching from Bootstrap
  (concat "<div class=\"embed-responsive embed-responsive-16by9\">"
          " <iframe class=\"embed-responsive-item\" src=\"https://www.youtube.com/embed/%s\" allowfullscreen></iframe>"
          " </div>"))

(setq dw/site-url "https://systemcrafters.net")

(org-link-set-parameters
 "yt"
 :follow
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/watch?v="
            handle)))
 :export
 (lambda (path desc backend channel)
   (when (eq backend 'html)
     (format yt-iframe-format
             path (or desc "")))))

(setq dw/site-title   "System Crafters")
(setq dw/site-tagline "")

(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./.org-cache/"
      org-export-with-section-numbers nil
      org-export-use-babel nil
      org-export-with-smart-quotes t
      org-export-with-sub-superscripts nil
      org-export-with-tags 'not-in-toc
      org-export-with-toc t)

;; We're using Git, we don't need no steenking backups
(setq make-backup-files nil)

(defun dw/site-header (info)
  (let* ((file (plist-get info :output-file)))
    `(div (div (@ (class "blog-header"))
               (div (@ (class "container"))
                    (div (@ (class "row align-items-center justify-content-between"))
                         (div (@ (class "col-sm-12 col-md-8"))
                              (div (@ (class "blog-title"))
                                   ,dw/site-title))
                         (div (@ (class "col-sm col-md"))
                              (div (@ (class "blog-description text-sm-left text-md-right text-lg-right text-xl-right"))
                                   ,dw/site-tagline)))))

          (div (@ (class "blog-masthead"))
               (div (@ (class "container"))
                    (div (@ (class "row align-items-center justify-content-between"))
                         (div (@ (class "col-sm-12 col-md-12"))
                              (nav (@ (class "nav"))
                                   (a (@ (class "nav-link") (href "/")) "Home") " "
                                   ;; (a (@ (class "nav-link") (href "/articles")) "Articles")
                                   (a (@ (class "nav-link") (href "/videos")) "Videos") " "
                                   (a (@ (class "nav-link") (href "https://wiki.systemcrafters.net")) "Wiki") " "
                                   (a (@ (class "nav-link") (href "https://store.systemcrafters.net?utm_source=sc-site-nav")) "Merch Store") " "
                                   (a (@ (class "nav-link") (href "/support-the-channel")) "Support The Channel")))))))))

(defun dw/site-footer (info)
  (list
   `(footer (@ (class "blog-footer"))
            (div (@ (class "container"))
                 (div (@ (class "row"))
                      (div (@ (class "col-sm col-md text-sm-left text-md-right text-lg-right text-xl-right"))
                           (p "Made with " ,(plist-get info :creator))
                           (p (a (@ (href ,(concat dw/site-url "/privacy-policy/"))) "Privacy Policy"))))))
   `(script (@ (src "/js/bootstrap.bundle.min.js")))))

(defun get-article-output-path (org-file pub-dir)
  (let ((article-dir (concat pub-dir
                             (downcase
                              (file-name-as-directory
                               (file-name-sans-extension
                                (file-name-nondirectory org-file)))))))

    (if (string-match "\\/index.org$" org-file)
        pub-dir
        (progn
          (unless (file-directory-p article-dir)
            (make-directory article-dir t))
          article-dir))))

(defun dw/org-html-template (contents info)
  (concat
   "<!-- " (org-export-data (org-export-get-date info "%Y-%m-%d") info) " -->\n"
   "<!DOCTYPE html>"
   (sxml-to-xml
    `(html (@ (lang "en"))
           (head
            (meta (@ (charset "utf-8")))
            (meta (@ (author "David Wilson")))
            (meta (@ (name "viewport")
                     (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
            (link (@ (rel "stylesheet")
                     (href "/css/bootstrap.min.css")))
            (link (@ (rel "stylesheet")
                     (href "/fonts/iosevka-aile/iosevka-aile.css")))
            (link (@ (rel "stylesheet")
                     (href "/fonts/jetbrains-mono/jetbrains-mono.css")))
            (link (@ (rel "stylesheet")
                     (href "/css/code.css")))
            (link (@ (rel "stylesheet")
                     (href "/css/site.css")))
            (script (@ (defer "defer")
                       (data-domain "systemcrafters.net")
                       (src "https://plausible.io/js/plausible.js"))
                    ;; Empty string to cause a closing </script> tag
                    "")
            (title ,(concat (org-export-data (plist-get info :title) info) " - System Crafters")))
           (body
            ,@(dw/site-header info)
            (div (@ (class "container"))
                 (div (@ (class "row"))
                      (div (@ (class "col-sm-12 blog-main"))
                           (div (@ (class "blog-post"))
                                (h1 (@ (class "blog-post-title"))
                                    ,(org-export-data (plist-get info :title) info))
                                (p (@ (class "blog-post-meta"))
                                   ,(org-export-data (org-export-get-date info "%B %e, %Y") info))
                                ,contents
                                ,(let ((tags (plist-get info :filetags)))
                                   (when (and tags (> (list-length tags) 0))
                                     `(p (@ (class "blog-post-tags"))
                                         "Tags: "
                                         ,(mapconcat (lambda (tag) tag)
                                                     ;; TODO: We don't have tag pages yet
                                                     ;; (format "<a href=\"/tags/%s/\">%s</a>" tag tag))
                                                     (plist-get info :filetags)
                                                     ", "))))
                                ,(when (equal "article" (plist-get info :page-type))
                                   ;; TODO: Link to mailing list
                                   "<script src=\"https://utteranc.es/client.js\"
                                              repo=\"daviwil/harmonicschemes.com\"
                                              issue-term=\"title\"
                                              label=\"comments\"
                                              theme=\"photon-dark\"
                                              crossorigin=\"anonymous\"
                                              async>
                                     </script>")))))

            ,@(dw/site-footer info))))))

;; Thanks Ashraz!
(defun dw/org-html-link (link contents info)
  "Removes file extension and changes the path into lowercase file:// links."
  (when (string= 'file (org-element-property :type link))
    (org-element-put-property link :path
                              (downcase
                               (file-name-sans-extension
                                (org-element-property :path link)))))

  (let ((exported-link (org-export-custom-protocol-maybe link contents 'html info)))
    (cond
     (exported-link exported-link)
     ((equal contents nil)
      (format "<a href=\"%s\">%s</a>"
              (org-element-property :raw-link link)
              (org-element-property :raw-link link)))
     (t (org-export-with-backend 'slimhtml link contents info)))))

;; Make sure we have thread-last
(require 'subr-x)

(defun dw/make-heading-anchor-name (headline-text)
  (thread-last headline-text
    (downcase)
    (replace-regexp-in-string " " "-")
    (replace-regexp-in-string "[^[:alnum:]_-]" "")))

(defun dw/org-html-headline (headline contents info)
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (level (org-export-get-relative-level headline info))
         (level (min 7 (when level (1+ level))))
         (anchor-name (dw/make-heading-anchor-name text))
         (attributes (org-element-property :ATTR_HTML headline))
         (container (org-element-property :HTML_CONTAINER headline))
         (container-class (and container (org-element-property :HTML_CONTAINER_CLASS headline))))
    (when attributes
      (setq attributes
            (format " %s" (org-html--make-attribute-string
                           (org-export-read-attribute 'attr_html `(nil
                                                                   (attr_html ,(split-string attributes))))))))
    (concat
     (when (and container (not (string= "" container)))
       (format "<%s%s>" container (if container-class (format " class=\"%s\"" container-class) "")))
     (if (not (org-export-low-level-p headline info))
         (format "<h%d%s><a id=\"%s\" class=\"anchor\" href=\"#%s\">¶</a>%s</h%d>%s"
                 level
                 (or attributes "")
                 anchor-name
                 anchor-name
                 text
                 level
                 (or contents ""))
       (concat
        (when (org-export-first-sibling-p headline info) "<ul>")
        (format "<li>%s%s</li>" text (or contents ""))
        (when (org-export-last-sibling-p headline info) "</ul>")))
     (when (and container (not (string= "" container)))
       (format "</%s>" (cl-subseq container 0 (cl-search " " container)))))))

(org-export-define-derived-backend 'site-html
    'slimhtml
  :translate-alist
  '((template . dw/org-html-template)
    (link . dw/org-html-link)
    (code . ox-slimhtml-verbatim)
    (headline . dw/org-html-headline))
  :options-alist
  '((:page-type "PAGE-TYPE" nil nil t)
    (:html-use-infojs nil nil nil)))

(defun org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, using the FILENAME as the output directory."
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 (concat article-path "index" extension))))
      (org-publish-org-to 'site-html
                          filename
                          (concat "." (or (plist-get plist :html-extension)
                                          "html"))
                          plist
                          article-path))))

(defun dw/org-gemini-publish-to-gemini (plist filename pub-dir)
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 (concat article-path "index" extension))))
      (org-publish-org-to
       'gemini filename ".gmi" plist pub-dir))))

(defun dw/sitemap-entry (entry style project)
  (format "<h4><em>%s</em> - <a href=\"%s\">%s</a></h4>"
          (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
          (concat (file-name-sans-extension entry) "/")
          (org-publish-find-title entry project)))

(defun dw/generate-sitemap (title list)
  (concat
    "#+TITLE: " title "\n\n"
    "#+BEGIN_EXPORT html\n"
    (mapconcat (lambda (item)
                 (car item))
               (cdr list)
               "\n")
    "\n#+END_EXPORT\n"))

(setq org-html-preamble  #'dw/site-header
      org-html-postamble #'dw/site-footer
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-checkbox-type 'site-html
      org-html-html5-fancy nil
      org-html-htmlize-output-type 'css
      org-html-self-link-headlines t
      org-html-validation-link nil
      org-html-doctype "html5")

(setq org-publish-project-alist
      (list
       (list "systemcrafters:main"
             :recursive t
             :base-extension "org"
             :base-directory "./content"
             :publishing-function '(org-html-publish-to-html)
             :publishing-directory "./public"
             :with-timestamps t
             :with-title nil)
       (list "systemcrafters:gemini"
             :recursive t
             :base-extension "org"
             :base-directory "./content"
             :publishing-function '(dw/org-gemini-publish-to-gemini)
             :publishing-directory "./gemini"
             :with-timestamps t)))

(defun dw/publish ()
  (interactive)
  (org-publish-all t))
