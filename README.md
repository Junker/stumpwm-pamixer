# StumpWM Pamixer

Minimalistic Pulseaudio volume and microphone control module for StumpWM.

## Requirements

- [pamixer](https://github.com/cdemoulins/pamixer) CLI program installed

## Installation

```bash
cd ~/.stumpwm.d/modules/
git clone https://github.com/Junker/stumpwm-pamixer pamixer
```

```lisp
(stumpwm:add-to-load-path "~/.stumpwm.d/modules/pamixer")
(load-module "pamixer")
```

## Usage

```lisp
  (define-key *top-map* (kbd "XF86AudioRaiseVolume") "pamixer-volume-up")
  (define-key *top-map* (kbd "XF86AudioLowerVolume") "pamixer-volume-down")
  (define-key *top-map* (kbd "XF86AudioMute") "pamixer-toggle-mute")
```

### Additional commands

- pamixer-mute
- pamixer-unmute
- pamixer-set-volume volume

**control of source, e.g. microphone:**

- pamixer-source-volume-up
- pamixer-source-volume-down
- pamixer-source-mute
- pamixer-source-unmute
- pamixer-source-toggle-mute
- pamixer-source-set-volume volume

### Parameters

- pamixer:\*step\* - volume increase/decrease step
- pamixer:\*allow-boost\* - allow volume to go above 100%
- pamixer:\*source-allow-boost\* - allow source volume to go above 100%

### Modeline

%P - pamixer formatter

**Modeline mouse interaction:**

- left button: mute/unmute
- wheel up: volume up
- wheel down: volume down

#### Parameters for modeline

- pamixer:\*modeline-fmt\* - format of pamixer modeline (default: "%b(%v)")
  - %b - volume bar
  - %v - volume value
