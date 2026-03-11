import ffmpeg
from dataclasses import dataclass
from pathlib import Path

from constants import *

@dataclass
class ClipArbeitInfo:
    task_id: str
    input_path: str
    output_path: str
    start: int
    end: int
    game: str

@dataclass
class ClipArbeitResult:
    task_id: str
    success: bool
    message: str = ""

@dataclass
class ClipArbeit:
    tasks: list[ClipArbeitInfo|ClipArbeitResult]

    def arbeiten(self) -> list[ClipArbeitResult]:
        def clip(task: ClipArbeitInfo | ClipArbeitResult) -> ClipArbeitResult:
            if isinstance(task, ClipArbeitResult):
                return task
            input_path = CLIPS_DIR / task.input_path
            output_path = CLIPS_DIR / task.game / task.output_path
            print(f"input: {input_path}\noutput: {output_path}")
            try:
                (
                    ffmpeg
                    .input(str(input_path), ss=task.start, to=task.end)
                    .output(str(output_path), **DEFAULT_OPTIONS)
                    .run()
                )
                return ClipArbeitResult(task.task_id, True)
            except Exception as e:
                print(f"ERROR: {e}")
                return ClipArbeitResult(task.task_id, False, str(e))

        results = [clip(task) for task in self.tasks]
        return results