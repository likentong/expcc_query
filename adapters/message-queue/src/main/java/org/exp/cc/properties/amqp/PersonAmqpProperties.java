package org.exp.cc.properties.amqp;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * Person amqp properties.
 */
@Configuration
@ConfigurationProperties(prefix = "expcc.amqp.person")
public class PersonAmqpProperties extends AmqpProperties {
}
