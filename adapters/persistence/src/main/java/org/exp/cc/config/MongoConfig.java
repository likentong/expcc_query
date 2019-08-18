package org.exp.cc.config;

import com.mongodb.MongoClient;
import com.mongodb.MongoClientOptions;
import com.mongodb.MongoClientURI;
import cz.jirutka.spring.embedmongo.EmbeddedMongoFactoryBean;
import org.exp.cc.properties.MongoProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.mongodb.core.MongoTemplate;

import java.io.IOException;

import static org.exp.cc.constant.Constant.Profile.DEV;
import static org.exp.cc.constant.Constant.Profile.NOT_DEV;

@Configuration
public class MongoConfig {

    private static final Logger logger = LoggerFactory.getLogger(MongoConfig.class);

    private final MongoProperties mongoProperties;

    @Autowired
    public MongoConfig(final MongoProperties mongoProperties) {
        this.mongoProperties = mongoProperties;
    }

    @Bean(destroyMethod = "close")
    @Profile(DEV)
    public MongoClient embeddedMongoClient() throws IOException {
        logger.info("Creating Embedded Mongo Client.");

        EmbeddedMongoFactoryBean mongo = new EmbeddedMongoFactoryBean();
        mongo.setBindIp("localhost");

        return mongo.getObject();
    }

    @Bean(destroyMethod = "close")
    @Profile(NOT_DEV)
    public MongoClient mongoClient() {
        logger.info("Creating Mongo Client.");

        final MongoClientOptions.Builder mongoClientOptions = MongoClientOptions.builder()
                .connectionsPerHost(this.mongoProperties.getConnectionPoolSize())
                .socketTimeout(this.mongoProperties.getSocketTimeout())
                .minHeartbeatFrequency(this.mongoProperties.getMinHeartBeatFrequency())
                .maxConnectionIdleTime(this.mongoProperties.getConnectionIdleTime())
                .heartbeatSocketTimeout(this.mongoProperties.getHeartBeatSocketTimeout());

        return new MongoClient(new MongoClientURI(this.mongoProperties.getUri(), mongoClientOptions));
    }

    @Bean
    public MongoTemplate mongoTemplate(final MongoClient mongoClient) {
        return new MongoTemplate(mongoClient, this.mongoProperties.getDatabase());
    }
}
