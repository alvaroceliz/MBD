#!/usr/bin/python3
from confluent_kafka import Consumer, KafkaError, KafkaException, OFFSET_END
from datetime import datetime, timedelta
import argparse 

def my_assign (consumer, partitions):
  for p in partitions:
    p.offset = OFFSET_END
  print('assign', partitions)
  consumer.assign(partitions)


def display_message(message):
  print("- '%s' %s %d %d" %
        (message.value(), message.topic(), message.offset(),
         message.timestamp()[1]))

parser = argparse.ArgumentParser()
parser.add_argument("topic_name", help="name of the topic to consume from")
parser.add_argument("group_id", help="group identifier this consumer belongs to")
parser.add_argument("secs", type=int, help="number of seconds reading from the topic")

args = parser.parse_args()

dt_start = datetime.now()
# Consumer setup
#
conf = {'bootstrap.servers': "localhost:9092",
        'auto.offset.reset': 'latest',
        'group.id': args.group_id}
consumer = Consumer(conf)

# Consumer subscription and message processing
#
try:
  consumer.subscribe([args.topic_name], on_assign=my_assign)

  while (dt_start + timedelta(seconds=args.secs))>datetime.now():
    message = consumer.poll(timeout=1.0)
    if message is None: continue
    print ("message")

    if message.error():
      if message.error().code() == KafkaError._PARTITION_EOF:
        # End of partition event
        sys.stderr.write('%% %s [%d] reached end at offset %d\n' %
                         (message.topic(), message.partition(), 
                          message.offset()))
      elif message.error():
        raise KafkaException(message.error())
    else:
      display_message(message)
finally:
  # Close down consumer to commit final offsets.
  consumer.close()
