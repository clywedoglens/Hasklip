#!/usr/bin/env python3

import os
import json
from dataclasses import asdict

from ClipArbeiter.clip_arbeit_types import ClipArbeit, ClipArbeitInfo, ClipArbeitResult
from constants import *

def arbeiten() -> None:
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
    # currently a list of dictionary objects. the fields are key values in the dict
    if not isinstance(raw_data, list) :
        raise ValueError("Incorrect input, must be in list")
    def validate_task(raw_task: dict) -> ClipArbeitInfo | ClipArbeitResult:
        required = {"task_id", "input_path", "output_path", "start", "end"}
        missing = required - set(raw_task.keys())
        if missing:
            return ClipArbeitResult(raw_task.get("task_id", "BLANK_ID"), False, f"Missing fields: {missing}")
        return ClipArbeitInfo(raw_task['task_id'], 
                          raw_task['input_path'], 
                          raw_task['output_path'], 
                          raw_task['start'], 
                          raw_task['end'],
                          raw_task.get("game", ""))
        
    print(f"raw_data: {raw_data}")
    tasks = [validate_task(data) for data in raw_data]
    return ClipArbeit(tasks=tasks)


def read_data() -> str:
    if not os.path.exists(TASKS_FIFO):
        os.mkfifo(TASKS_FIFO)
    with open(TASKS_FIFO, 'r') as pipe:
        data = pipe.read()
        # logger.info(f'data: {data}')
        return data
    

def write_data(data: str) -> None:
    print("writing to results pipe...")
    try:
        if not os.path.exists(RESULTS_FIFO):
            os.mkfifo(RESULTS_FIFO)
        with open(RESULTS_FIFO, 'w') as pipe:
            pipe.write(data)
            pipe.flush()
    except Exception as e:
        print(f"Error writing to pipe: {e}")


if __name__ == "__main__":
    arbeiten() 