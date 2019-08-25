package org.exp.cc.impl.amqp;

import org.exp.cc.MessageQueue;
import org.exp.cc.properties.amqp.PersonAmqpProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.stereotype.Component;

import java.util.Map;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Person message queue.
 */
@Component
public class PersonMessageQueue implements MessageQueue {
    private static final Logger logger = LoggerFactory.getLogger(PersonMessageQueue.class);

    private final PersonAmqpProperties personAmqpProperties;
    private final RabbitTemplate rabbitTemplate;

    public PersonMessageQueue(final PersonAmqpProperties personAmqpProperties, final RabbitTemplate rabbitTemplate) {
        this.personAmqpProperties = personAmqpProperties;
        this.rabbitTemplate = rabbitTemplate;
    }

    @Override
    public void sendMessage(final Map<String, Object> message) {
        checkArgument(message != null, "message cannot be null.");
        checkArgument(!message.isEmpty(), "message cannot be empty.");

        debugLogging("Sending message to queue using exchange {}, routingKey {}. Message={}",
                this.personAmqpProperties.getExchange(),
                this.personAmqpProperties.getRoutingKey(),
                message);

        this.rabbitTemplate.convertAndSend(this.personAmqpProperties.getExchange(), this.personAmqpProperties.getRoutingKey(), message);

        debugLogging("Message sent successfully to queue using exchange {}, routingKey {}. Message={}",
                this.personAmqpProperties.getExchange(),
                this.personAmqpProperties.getRoutingKey(),
                message);
    }

    private void debugLogging(final String loggingMessage, Object... objects) {
        if (logger.isDebugEnabled()) {
            logger.debug(loggingMessage, objects);
        }
    }

}
