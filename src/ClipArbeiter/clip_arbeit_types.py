import ffmpeg
from dataclasses import dataclass
from multiprocessing import Pool

from constants import *

@dataclass
class ClipArbeitInfo:
    clip_arbeit_id: str
    input_path: str
    output_path: str
    start: int
    end: int


@dataclass
class ClipArbeitResult:
    task_id: str
    success: bool
    message: str = ""


@dataclass
class ClipArbeit:
    tasks: list[ClipArbeitInfo|ClipArbeitResult]    

    def arbeiten(self) -> list[ClipArbeitResult]:
        with Pool(processes=4) as pool:
            results = pool.map(clip, self.tasks)
        return results
    

def clip(task: ClipArbeitInfo | ClipArbeitResult) -> ClipArbeitResult:
    if isinstance(task, ClipArbeitResult):
        return task   
    print(f"input: {task.input_path}\noutput: {task.output_path}")
    try:
        (
            ffmpeg
            .input(task.input_path, ss=task.start, to=task.end)
            .output(task.output_path, **DEFAULT_OPTIONS)
            .run()
        )
        return ClipArbeitResult(task.clip_arbeit_id, True)
    except Exception as e:
        print(f"ERROR: {e}")
        return ClipArbeitResult(task.clip_arbeit_id, False, str(e))