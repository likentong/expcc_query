package org.exp.cc.impl.amqp.steps;

import com.google.common.collect.ImmutableSet;
import org.exp.cc.impl.amqp.PersonMessageQueue;
import org.exp.cc.properties.amqp.PersonAmqpProperties;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import uk.org.lidalia.slf4jext.Level;
import uk.org.lidalia.slf4jtest.LoggingEvent;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.Mockito.verify;

/**
 * Person message queue step.
 */
public class PersonMessageQueueStep {
    private final TestLogger logger = TestLoggerFactory.getTestLogger(PersonMessageQueue.class);

    private PersonAmqpProperties personAmqpProperties;
    private RabbitTemplate rabbitTemplate;
    private Map<String, Object> message;
    private PersonMessageQueue personMessageQueue;

    /**
     * Step to setup logging level.
     * @param enabledLevels enabled logging level
     * @return this instance
     */
    public PersonMessageQueueStep givenISetLoggingLevel(final ImmutableSet<Level> enabledLevels) {
        this.logger.setEnabledLevels(enabledLevels);
        return this;
    }

    /**
     * Step to setup PersonMessageQueue.
     * @param personAmqpProperties mock PersonAmqpProperties
     * @param rabbitTemplate mock RabbitTemplate
     * @return this instance
     */
    public PersonMessageQueueStep givenISetUpPersonMessageQueue(final PersonAmqpProperties personAmqpProperties,
                                                                final RabbitTemplate rabbitTemplate) {
        this.personAmqpProperties = personAmqpProperties;
        this.rabbitTemplate = rabbitTemplate;
        this.personMessageQueue = new PersonMessageQueue(this.personAmqpProperties, this.rabbitTemplate);
        return this;
    }

    /**
     * Step to call sendMessage from PersonMessageQueue.
     * @param message message to send to queue
     * @return this instance
     */
    public PersonMessageQueueStep whenISendMessage(final Map<String, Object> message) {
        this.message = message;
        this.personMessageQueue.sendMessage(message);
        return this;
    }

    /**
     * Step to verify RabbitTemplate convertAndSend is called once.
     * @return this instance
     */
    public PersonMessageQueueStep thenRabbitTemplateConvertAndSendIsCalled() {
        verify(this.rabbitTemplate).convertAndSend(this.personAmqpProperties.getExchange(), this.personAmqpProperties.getRoutingKey(), this.message);
        return this;
    }

    /**
     * Step to assert expected logging.
     * @param loggingEvent expected logging event
     * @return this instance
     */
    public PersonMessageQueueStep thenLoggingEventShouldBe(final List<LoggingEvent> loggingEvent) {
        assertThat(this.logger.getLoggingEvents()).isEqualTo(loggingEvent);
        return this;
    }

    /**
     * Step to assert no logging.
     * @return this instance
     */
    public PersonMessageQueueStep thenWithNoLogging() {
        assertThat(this.logger.getLoggingEvents().isEmpty());
        return this;
    }

}
