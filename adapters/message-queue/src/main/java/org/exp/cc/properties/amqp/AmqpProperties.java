package org.exp.cc.properties.amqp;

/**
 * Amqp properties.
 */
public class AmqpProperties {
    private String exchange;
    private String queue;
    private String deadLetter;
    private String routingKey;

    public String getExchange() {
        return exchange;
    }

    public void setExchange(final String exchange) {
        this.exchange = exchange;
    }

    public String getQueue() {
        return queue;
    }

    public void setQueue(final String queue) {
        this.queue = queue;
    }

    public String getDeadLetter() {
        return deadLetter;
    }

    public void setDeadLetter(final String queueDeadLetter) {
        this.deadLetter = queueDeadLetter;
    }

    public String getRoutingKey() {
        return routingKey;
    }

    public void setRoutingKey(final String routingKey) {
        this.routingKey = routingKey;
    }
}
