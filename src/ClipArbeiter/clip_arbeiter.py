#!/usr/bin/env python3

import os
import json
from dataclasses import asdict

from clip_arbeit_types import ClipArbeit, ClipArbeitInfo, ClipArbeitResult
from constants import *

def arbeiten():
    while True:
        try:
            arbeit = listen_for_arbeit()
        except ValueError as e:
            print(f"error in parsing json input: {str(e)}")
            continue
        results = arbeit.arbeiten()
        json_results = [asdict(res) for res in results]
        write_data(json.dumps(json_results))
        print("finished writing results...")


def listen_for_arbeit() -> ClipArbeit:
    print("listening for clip tasks...")
    raw_data = json.loads(read_data())
    if not isinstance(raw_data, list) :
        raise ValueError("Incorrect input, must be in list")
    def validate_task(raw_task: dict) -> ClipArbeitInfo | ClipArbeitResult:
        required = {"clip_arbeit_id", "input_path", "output_path", "start", "end"}
        missing = required - set(raw_task.keys())
        if missing:
            print('MISSING FIELDS')
            arbeit_id = raw_task.get("clip_arbeit_id", "BLANK_ID")
            msg = f"Missing fields: {missing}"
            return ClipArbeitResult(arbeit_id, False, msg)
        # validate the filepaths
        input_path = CLIPS_DIR / raw_task['input_path']
        game = raw_task.get("game", False)
        if game:
            output_path = CLIPS_DIR / game / raw_task['output_path']
        else:
            output_path = CLIPS_DIR / raw_task['output_path']
        if not input_path.exists():
            msg = "Input file does not exist"
            return ClipArbeitResult(raw_task['clip_arbeit_id'], False, msg)
        if output_path.exists():
            msg = "Output file already exists, choose a different filename"
            return ClipArbeitResult(raw_task['clip_arbeit_id'], False, msg)
        return ClipArbeitInfo(raw_task['clip_arbeit_id'],
                          str(input_path),
                          str(output_path),
                          raw_task['start'],
                          raw_task['end'])
    print(f"raw_data: {raw_data}")
    tasks = [validate_task(data) for data in raw_data]
    return ClipArbeit(tasks=tasks)


def read_data() -> str:
    with open(TASKS_FIFO, 'r') as pipe:
        data = pipe.read()
        # logger.info(f'data: {data}')
        return data
    

def write_data(data: str) -> None:
    print("writing to results pipe...")
    try:
        with open(RESULTS_FIFO, 'w') as pipe:
            pipe.write(data)
            pipe.flush()
    except Exception as e:
        print(f"Error writing to pipe: {e}")


if __name__ == "__main__":
    if not os.path.exists(RESULTS_FIFO):
        os.mkfifo(RESULTS_FIFO)
    if not os.path.exists(TASKS_FIFO):
        os.mkfifo(TASKS_FIFO)
    arbeiten()