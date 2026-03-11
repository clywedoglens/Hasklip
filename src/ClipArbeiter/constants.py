from pathlib import Path

TASKS_FIFO = Path("/tmp/cliparbeitinfo.fifo")
RESULTS_FIFO = Path("/tmp/cliparbeitresults.fifo")
CLIPS_DIR = Path('/home/george/Videos/Clips/')
DEFAULT_OPTIONS = {
                    "c:v": "libvpx-vp9",
                    "crf": 40,
                    "b:v": 0,
                    "c:a": "libopus",
                    "b:a": "64k",
                    "af": "volume=0.85"
                }