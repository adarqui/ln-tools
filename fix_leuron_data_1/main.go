package main



import (
  "encoding/json"
  "encoding/csv"
  "bufio"
  "log"
  "io"
  "os"
)



type OldRecord struct {
  Tag string                      `json:"tag"`
  Contents map[string]interface{} `json:"contents"`
}



type NewRecord struct {
  Tag string                        `json:"tag"`
  Contents []map[string]interface{} `json:"contents"`
}



func main() {

  if len(os.Args) < 2 {
    log.Fatal("fix_leuron_data_1 <csv:format=id,data_json>")
  }

  f, err := os.Open(os.Args[1])
  if err != nil {
    log.Fatal(err)
  }

  r := csv.NewReader(bufio.NewReader(f))
  w := csv.NewWriter(bufio.NewWriter(os.Stdout))

  for {
    line, err := r.Read()
    if line == nil && err == io.EOF {
      break
    }

    if err != nil {
      continue
    }

    old_record := OldRecord{}
    err = json.Unmarshal([]byte(line[1]), &old_record)
    if err != nil {
      log.Fatal(err)
    }

    new_record := NewRecord{Tag: old_record.Tag, Contents: []map[string]interface{}{old_record.Contents}}

    js, err := json.Marshal(new_record)
    if err != nil {
      log.Fatal(err)
    }

    line[1] = string(js)
    w.Write(line)
    w.Flush()
  }

}
