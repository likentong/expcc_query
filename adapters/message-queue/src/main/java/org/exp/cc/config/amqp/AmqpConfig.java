package org.exp.cc.config.amqp;

import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.amqp.support.converter.MessageConverter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Amqp config.
 */
@Configuration
public class AmqpConfig {

    @Bean
    public MessageConverter producerJacksonMessageConverter() {
        return new Jackson2JsonMessageConverter();
    }
}
