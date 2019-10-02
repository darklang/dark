package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"runtime"
	"sort"
	"sync"
	"time"

	"cloud.google.com/go/pubsub"
)

// flags used for configuration
var (
	flagProject      = flag.String("project", "", "Google project ID")
	flagSubscription = flag.String("subscription", "", "Google PubSub subscription name")

	flagReceiveGoroutines = flag.Int(
		"recv-routines",
		runtime.NumCPU(),
		"Number of goroutines to use to receive messages from the pubsub Subscription. default: runtime.NumCPUs()",
	)

	// flagFlushInterval is the duration used for ticker that flushes the
	// buffer to STDOUT. A smaller duration will mean less memory usage but a
	// higher probability of out-of-order messages in the output.
	//
	// Importantly, this duration also controls the data loss you are willing
	// to tolerate in the event of an unclean shutdown. Messages are acked once
	// put into this buffer, so if the program exits unexpectedly, any messages
	// in buffer and not flushed to STDOUT will be lost.
	flagFlushInterval = flag.Duration(
		"flush-interval",
		5*time.Second,
		"Time between flushes of message buffer to STDOUT.",
	)
)

// mx is the mutex used to proect the global message buffer
var mx sync.Mutex

// globalBuffer is the buffer used to store messages until flush
var globalBuffer []ParsedMessage

// pgTimestamp is the format that postgres usually uses for timestamps, and
// consequently what honeytail expects. It's almost but not quite
// time.RFC3339Nano.
const pgTimestamp = "2006-01-02 15:04:05.999999999 UTC"

// ParsedMessage holds the relevant details from a parsed Cloud Subscription message.
type ParsedMessage struct {
	TextPayload string    `json:"textPayload"`
	Timestamp   time.Time `json:"timestamp"`
}

func main() {
	flag.Parse()

	switch "" {
	case *flagProject:
		fmt.Fprintf(os.Stderr, "must provide -project")
		os.Exit(1)
	case *flagSubscription:
		fmt.Fprintf(os.Stderr, "must provide -subscription")
		os.Exit(1)
	}
	if *flagFlushInterval < 1 {
		fmt.Fprintf(os.Stderr, "flush internal '%s' must be > 0", *flagFlushInterval)
		os.Exit(1)
	} else if *flagFlushInterval < time.Second {
		fmt.Fprintf(
			os.Stderr,
			`WARNING: using an small flush interval may result in more out-of-order output. Are you sure you didn't mean "%ds"?`,
			*flagFlushInterval)
	}

	ctx := context.Background()
	c, err := pubsub.NewClient(ctx, *flagProject)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%+v\n", err)
		os.Exit(1)
	}

	sub := c.Subscription(*flagSubscription)
	sub.ReceiveSettings.NumGoroutines = *flagReceiveGoroutines

	// this is a forever ticker that periodically
	go flushBuffer(*flagFlushInterval)

	err = sub.Receive(ctx, func(ctx context.Context, msg *pubsub.Message) {
		bufferMessage(msg.Data)
		msg.Ack()
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "%+v\n", err)
		os.Exit(1)
	}
}

// bufferMessage takes a message's unparsed JSON data, parses it, and appends a
// ParsedMessage to the global message buffer.
func bufferMessage(data []byte) {
	var pm ParsedMessage
	if err := json.Unmarshal(data, &pm); err != nil {
		return
	}

	mx.Lock()
	globalBuffer = append(globalBuffer, pm)
	mx.Unlock()
}

// flushBuffer sets up a ticker to flush the message buffer every 'dur'.
// Each tick locks then sorts the message buffer based on the Timestamp,
// then outputs the buffer sequentially to to STDOUT. Some notes:
//
// Subscriptions make no guarantee about the ordering of delivered
// messages, so we sort all buffered messages before output. This does
// not guarantee that we don't ever miss or mangle messages, but it
// gets to mostly correct.
//
// Postgres query logs can span multiple lines.
// The first line always has a prefix, which we know starts with the
// [PID] in brackets, while the following lines are \t indented.
// Add the timestamp from the pubsub message to initial lines and
// pass-through remaining lines unmodified.
func flushBuffer(dur time.Duration) {
	tick := time.NewTicker(dur)
	for {
		<-tick.C
		mx.Lock()

		sort.Slice(globalBuffer, func(i, j int) bool {
			return globalBuffer[i].Timestamp.Before(globalBuffer[j].Timestamp)
		})

		for i := range globalBuffer {
			msg := &globalBuffer[i]

			if msg.TextPayload[0] == '[' {
				timestamp := globalBuffer[i].Timestamp.Format(pgTimestamp)
				fmt.Printf("[%s]: %s\n", timestamp, globalBuffer[i].TextPayload)
			} else {
				fmt.Println(globalBuffer[i].TextPayload)
			}
		}

		// Reset the global entries slice, pre-allocating enough capacity to
		// fit the same number of messages as we saw last time. This trades
		// some memory usage for not having to copy the slice a bunch when we
		// append() in bufferMessage.
		globalBuffer = make([]ParsedMessage, 0, len(globalBuffer))
		mx.Unlock()
	}
}
