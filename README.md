# sub-wave

### About
TTS podcasting experimental project currently publishing front-page articles from https://www.meduza.io/en.

- R for scraping, text processing, API integrations, etc. 

- Google Wavenet for TTS services

- Transistor.fm for podcast publishing and hosting

- FFMPEG for audio-file post-processing

#### Files
- check_requirements.R: Dependency check.

- meduza_processing-control.R: main script, executes all below.

- meduza_check-home.R: Grab articles from target site. Is there anything new?

- meduza_synthesize-article.R: For new articles, grab text, prep, process, publish.

