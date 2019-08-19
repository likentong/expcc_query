package org.exp.cc.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * MongoDB properties.
 */
@Configuration
@ConfigurationProperties(prefix = "expcc.mongodb")
public class MongoProperties {

    private String uri;
    private String database;

    private boolean keepAlive = true;
    private int minHeartBeatFrequency = 25;
    private int heartBeatSocketTimeout = 3000;
    private int socketTimeout = 3000;
    private int connectionPoolSize = 100;
    private int connectionIdleTime = 10000;

    public String getUri() {
        return uri;
    }

    public void setUri(final String uri) {
        this.uri = uri;
    }

    public String getDatabase() {
        return database;
    }

    public void setDatabase(final String database) {
        this.database = database;
    }

    public boolean isKeepAlive() {
        return keepAlive;
    }

    public void setKeepAlive(final boolean keepAlive) {
        this.keepAlive = keepAlive;
    }

    public int getMinHeartBeatFrequency() {
        return minHeartBeatFrequency;
    }

    public void setMinHeartBeatFrequency(final int minHeartBeatFrequency) {
        this.minHeartBeatFrequency = minHeartBeatFrequency;
    }

    public int getHeartBeatSocketTimeout() {
        return heartBeatSocketTimeout;
    }

    public void setHeartBeatSocketTimeout(final int heartBeatSocketTimeout) {
        this.heartBeatSocketTimeout = heartBeatSocketTimeout;
    }

    public int getSocketTimeout() {
        return socketTimeout;
    }

    public void setSocketTimeout(final int socketTimeout) {
        this.socketTimeout = socketTimeout;
    }

    public int getConnectionPoolSize() {
        return connectionPoolSize;
    }

    public void setConnectionPoolSize(final int connectionPoolSize) {
        this.connectionPoolSize = connectionPoolSize;
    }

    public int getConnectionIdleTime() {
        return connectionIdleTime;
    }

    public void setConnectionIdleTime(final int connectionIdleTime) {
        this.connectionIdleTime = connectionIdleTime;
    }
}
