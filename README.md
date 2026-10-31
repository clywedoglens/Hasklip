# Hasklip
A command-line interface tool that parses a YAML manifest file and performs a variety of tasks involving converting clips and managing the metadata of the converted clips in SQLite database. 

## Manifest
Each task has a type field that determines the other expected fields.

| Field | Type   |
|-------|--------|
| type  | string |

---

### Clip Task
Clip a video file given by the input and save to output. Specifies a start and an end, can also specify a game and add any tags listed in a sequence
| Field | Type   |
|-------|--------|
| id    | string |
| input | string |
| output| string |
| start | int    |
| end   | int    |
| game  | string |
| tags  | list   |

**Example**

```yaml
- type: clip
  id: clip3
  input: "Replay 2026-03-30 14-46-31.mkv"
  output: "whamming failure.webm"
  start: 1
  end: 10
  game: "Team Fortress 2"
  tags: ["tf2 scout", "tf2 brazil"]
```

---

### Tag Task
Associate an existing clip with new tags
| Field | Type   |
|-------|--------|
| id    | string |
| clip | string |
| tags | string |

**Example**
```yaml
 - type: tag
   id: tag1
   clip: "paige kill on geist.webm"
   tags: ['deadlock kill', 'paige deadlock']
```
---

### Manage Task
Perform an operation on the associations between clips and tags.
| Field | Type   |
|-------|--------|
| id    | string |
| operation | string |

Operation types: `deleteClip`, `deleteTag`, `removeClipTags`

**Examples**
```yaml
 - type: manage
   id: manage1
   operation:
     type: deleteClip
     filepath: "another_test.webm"
 - type: manage
   id: manage2
   operation:
     type: deleteTag
     tag: "test1"
 - type: manage
   id: manage3
   operation:
     type: removeClipTags
     filepath: "test1.webm"
     tags: ["test1"]
```
---

### List Task
Performs a search given by the type and its parameters
| Field | Type   |
|-------|--------|
| id    | string |
| operation | string |

Operation types: `tagSearch`, `nameSearch`

**Examples**
```yaml
- type: list
  id: list1
  operation:
    type: tagSearch
    tags: ["tf2 airshot"]
- type: list
  id: list2
  operation:
    type: nameSearch
    search: "flub"
```