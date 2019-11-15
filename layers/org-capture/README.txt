                          ━━━━━━━━━━━━━━━━━━━
                           ORG-CAPTURE LAYER

                             Hippie Hacker
                          ━━━━━━━━━━━━━━━━━━━


Table of Contents
─────────────────

1. Description
.. 1. Features:
2. Install
3. Combined Concepts
.. 1. [The Org Manual: Capture]
.. 2. [org-protocol.el – Intercept calls from emacsclient to trigger custom actions]
.. 3. [alphapapa/org-protocol-capture-html: Capture HTML from the browser selection into Emacs as org-mode content]
4. Configuration
.. 1. Getting your browser to call emacsclient for `org-protocol://' links
..... 1. [Linux setup (Gnome)]
..... 2. xdg protocol handler
.. 2. [The Org Manual: Capture templates]
5. Mapping Bookmarklets to Capture Templates
.. 1. org-capture-html (oc-H)
..... 1. bookmarklet
..... 2. capture template
.. 2. org-capture-paragraph (oc-p)
.. 3. org-capture-link (oc-L)
6. A bookmarklet to save the current title/link
7. Linux setup (Gnome)


1 Description
═════════════

  This layer adds support for capturing urls, page titles, and selected
  content from your web browser via bookmarklets that call emacsclient
  with a special `org-protocol://' uri.


1.1 Features:
─────────────

  • Capture Current Page (Link + Page Title)
  • Capture Selected Text
  • Capture Html (as org via pandoc)


2 Install
═════════

  To use this configuration layer, add it to your `~/.spacemacs'. You
  will need to add `org-capture' to the existing
  `dotspacemacs-configuration-layers' list in this file.


3 Combined Concepts
═══════════════════

[The Org Manual: Capture]
<https://orgmode.org/manual/Capture.html#Capture>

3.1 [The Org Manual: Capture]
─────────────────────────────

  This is usually just used within emacs.

  Capture lets you quickly store notes with little interruption of your
  work flow. Org's method for capturing new items is heavily inspired by
  John Wiegley's excellent Remember package.


[The Org Manual: Capture]
<https://orgmode.org/manual/Capture.html#Capture>


3.2 [org-protocol.el – Intercept calls from emacsclient to trigger custom actions]
──────────────────────────────────────────────────────────────────────────────────

  This is what connects org-mode to the “outside world” using a MIME
  protocol handler.

  org-protocol intercepts calls from emacsclient to trigger custom
  actions without external dependencies. Only one protocol has to be
  configured with your external applications or the operating system, to
  trigger an arbitrary number of custom actions. Just register your
  custom sub-protocol and handler with the variable
  `org-protocol-protocol-alist'.


[org-protocol.el – Intercept calls from emacsclient to trigger custom
actions] <https://orgmode.org/worg/org-contrib/org-protocol.html>


3.3 [alphapapa/org-protocol-capture-html: Capture HTML from the browser selection into Emacs as org-mode content]
─────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  org-protocol is awesome, but browsers do a pretty poor job of turning
  a page's HTML content into plain-text. However, Pandoc supports
  converting /from/ HTML /to/ org-mode, so we can use it to turn HTML
  into Org-mode content! It can even turn HTML tables into Org tables!


[alphapapa/org-protocol-capture-html: Capture HTML from the browser
selection into Emacs as org-mode content]
<https://github.com/alphapapa/org-protocol-capture-html>


4 Configuration
═══════════════

4.1 Getting your browser to call emacsclient for `org-protocol://' links
────────────────────────────────────────────────────────────────────────

[Linux setup (Gnome)]
<https://orgmode.org/worg/org-contrib/org-protocol.html#org4166fc4>

4.1.1 [Linux setup (Gnome)]
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  ┌────
  │ gconftool-2 -s /desktop/gnome/url-handlers/org-protocol/command '/usr/local/bin/emacsclient %s' --type String
  │ gconftool-2 -s /desktop/gnome/url-handlers/org-protocol/enabled --type Boolean true
  └────


[Linux setup (Gnome)]
<https://orgmode.org/worg/org-contrib/org-protocol.html#org4166fc4>


4.1.2 xdg protocol handler
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  Save the following to
  `$HOME/.local/share/applications/org-protocol.desktop'
  ┌────
  │ [Desktop Entry]
  │ Name=org-protocol
  │ Exec=emacsclient %u
  │ Type=Application
  │ Terminal=false
  │ Categories=System;
  │ MimeType=x-scheme-handler/org-protocol;
  └────
  Then run the following:
  ┌────
  │ # On KDE:
  │ kbuildsycoca4
  │ # On GNOME:
  │ update-desktop-database ~/.local/share/applications/
  └────


4.2 [The Org Manual: Capture templates]
───────────────────────────────────────

   You can use templates for different types of capture items, and for
  different target locations. The ones you want to use with org-protocol
  require

  ┌────
  │ 
  │ (setq org-capture-templates
  │       '(
  │         ("t" "TODO"
  │          entry (file+headline "~/org/TODO.org" "Inbox")
  │          "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
  │          )
  │         ))
  └────


[The Org Manual: Capture templates]
<https://orgmode.org/manual/Capture-templates.html>


5 Mapping Bookmarklets to Capture Templates
═══════════════════════════════════════════

5.1 org-capture-html (oc-H)
───────────────────────────

5.1.1 bookmarklet
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  This requires pandoc installed to convert the html into org-mode.
  ┌────
  │ javascript:location.href = 'org-protocol://capture-html?template=w&url=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title || "[untitled page]") + '&body=' + encodeURIComponent(function () {var html = ""; if (typeof document.getSelection != "undefined") {var sel = document.getSelection(); if (sel.rangeCount) {var container = document.createElement("div"); for (var i = 0, len = sel.rangeCount; i < len; ++i) {container.appendChild(sel.getRangeAt(i).cloneContents());} html = container.innerHTML;}} else if (typeof document.selection != "undefined") {if (document.selection.type == "Text") {html = document.selection.createRange().htmlText;}} var relToAbs = function (href) {var a = document.createElement("a"); a.href = href; var abs = a.protocol + "//" + a.host + a.pathname + a.search + a.hash; a.remove(); return abs;}; var elementTypes = [['a', 'href'], ['img', 'src']]; var div = document.createElement('div'); div.innerHTML = html; elementTypes.map(function(elementType) {var elements = div.getElementsByTagName(elementType[0]); for (var i = 0; i < elements.length; i++) {elements[i].setAttribute(elementType[1], relToAbs(elements[i].getAttribute(elementType[1])));}}); return div.innerHTML;}());
  └────


5.1.2 capture template
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  ┌────
  │ (add-to-list 'org-capture-templates
  │              ("w" "Default template" entry
  │               (file+headline "~/org/capture.org" "Notes")
  │               "* %^{Title}
  │ 
  │   Source: %u, %c
  │ 
  │   %i" :empty-lines 1)
  │ )
  └────


5.2 org-capture-paragraph (oc-p)
────────────────────────────────

  ┌────
  │ javascript:location.href='org-protocol://capture?template=p&url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title)+'&body='+encodeURIComponent(window.getSelection())
  └────

  ┌────
  │ (add-to-list 'org-capture-templates
  │              ("p" "Paragraph" entry
  │               (file+headline "~/ii/org/agenda/notes.org" "Inbox")
  │               "* [[%:link][%:description]] %^G
  │ Time: %u
  │  #+BEGIN_QUOTE
  │ %:initial
  │ #+END_QUOTE
  │ 
  │ 
  │ %?")
  │              )
  └────


5.3 org-capture-link (oc-L)
───────────────────────────

  ┌────
  │ javascript:location.href='org-protocol://capture?template=L&url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title)+'?body='+encodeURIComponent(window.getSelection())
  └────

  ┌────
  │ (add-to-list 'org-capture-templates
  │              ("L" "Capture a Link" entry
  │               (file+headline "~/ii/org/agenda/notes.org" "Inbox")
  │               "* %? [[%:link][%:description]]
  │ Captured On: %U")
  │              ;; javascript:location.href='org-protocol://store-link?url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title);
  │              )
  │ )
  └────


6 A bookmarklet to save the current title/link
══════════════════════════════════════════════

  Similar to using `aol' to call *org-store-link* and later retrieval
  with `,il' *org-insert-link*
  ┌────
  │ javascript:location.href='org-protocol://capture?template=p&url='+ encodeURIComponent(location.href)+'&title='+ encodeURIComponent(document.title)+'?body='+encodeURIComponent(window.getSelection())
  └────


7 Linux setup (Gnome)
═════════════════════
