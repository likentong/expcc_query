package org.exp.cc.config.amqp;

import org.exp.cc.constant.MessageConstant;
import org.exp.cc.properties.amqp.PersonAmqpProperties;
import org.springframework.amqp.core.*;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Person amqp config.
 */
@Configuration
public class PersonAmqpConfig {
    private final PersonAmqpProperties personAmqpProperties;

    public PersonAmqpConfig(final PersonAmqpProperties personAmqpProperties) {
        this.personAmqpProperties = personAmqpProperties;
    }

    @Bean
    public Exchange personExchange() {
        return ExchangeBuilder.directExchange(this.personAmqpProperties.getExchange()).build();
    }

    @Bean
    public Queue personDeadLetterQueue() {
        return QueueBuilder.durable(this.personAmqpProperties.getDeadLetter()).build();
    }

    @Bean(MessageConstant.PERSON_QUEUE_BEAN_NAME)
    public Queue personQueue() {
        return QueueBuilder.durable(this.personAmqpProperties.getQueue())
                .withArgument(MessageConstant.Ampq.DEAD_LETTER_EXCHANGER, "")
                .withArgument(MessageConstant.Ampq.DEAD_LETTER_ROUTING_KEY, this.personAmqpProperties.getDeadLetter())
                .build();
    }

    @Bean
    public Binding personBinding(@Qualifier(MessageConstant.PERSON_QUEUE_BEAN_NAME) final Queue queue, final DirectExchange exchange) {
        return BindingBuilder.bind(queue).to(exchange).with(this.personAmqpProperties.getRoutingKey());
    }

}
