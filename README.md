# expcc_query

This is a REST application developed using Spring Boot 2 with gradle as build tool. 
It uses:
1. MongoDB as datastore
2. RabbitMQ as message queueing system

<b>Setup</b><br/>
This application support 2 spring profiles:
1. dev - This is mean for development where it uses embedded mongodb.
2. sampleData - This is to populate sample data into demographic and person collections in expcc database.<br/>
**DO NOTE that it will drop existing demographic and person collections before inserting the sample data.

Use one of these commands to run the application:<br/>
1. Using dev and sampleData profiles (embedded Mongodb) <br/>
`gradlew.bat bootRun -Pargs=--spring.profiles.active=dev,sampleData;--spring.rabbitmq.addresses=amqp://usernama:password@localhost:5672/`

2. Without spring profiles<br/>
`gradlew.bat bootRun -Pargs=--spring.rabbitmq.addresses=amqp://usernama:password@localhost:5672/;--expcc.mongodb.uri=mongodb://localhost:27017/admin`

Do replace the spring.rabbitmq.addresses and expcc.mongodb.uri with the RabbitMQ and MongoDB connection string that you have setup/installed.

<b>Demographic filtering</b><br/>
This is a sample request to perform query on Demographic collection.

```json
{
   "query":[{
         "$and":{
            "ID":{"$in":[1,2, 3]}
         }
      }]
}
```
Supported logical operators: $and, $or <br/>
Supported comparison operators: $eq, $gt, $gte, $lt, $lte, $in

There is an example of sample request using postman in the doc folder. 
