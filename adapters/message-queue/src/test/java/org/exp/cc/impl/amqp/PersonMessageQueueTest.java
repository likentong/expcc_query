package org.exp.cc.impl.amqp;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.exp.cc.impl.amqp.steps.PersonMessageQueueStep;
import org.exp.cc.properties.amqp.PersonAmqpProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import uk.org.lidalia.slf4jext.ConventionalLevelHierarchy;
import uk.org.lidalia.slf4jext.Level;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

import java.util.Map;
import java.util.stream.Stream;

import static org.assertj.core.util.Lists.newArrayList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static uk.org.lidalia.slf4jtest.LoggingEvent.debug;

/**
 * Person message queue test.
 */
public class PersonMessageQueueTest {
    private static final Map<String, Object> MESSAGE = ImmutableMap.of(
            "field1", "value1",
            "field2", "value2");

    private static final String EXCHANGE = "exchange";
    private static final String ROUTING_KEY = "routingKey";

    private final PersonMessageQueueStep step;

    public PersonMessageQueueTest() {
        this.step = new PersonMessageQueueStep();
    }

    @BeforeAll
    public static void beforeAll() {
        TestLoggerFactory.clearAll();
    }

    /**
     * Test Initialization.
     */
    @BeforeEach
    public void init() {
        final PersonAmqpProperties personAmqpProperties = mock(PersonAmqpProperties.class);
        final RabbitTemplate rabbitTemplate = mock(RabbitTemplate.class);

        when(personAmqpProperties.getExchange()).thenReturn(EXCHANGE);
        when(personAmqpProperties.getRoutingKey()).thenReturn(ROUTING_KEY);

        this.step.givenISetUpPersonMessageQueue(personAmqpProperties, rabbitTemplate);
    }

    @AfterEach
    public void cleanUp() {
        TestLoggerFactory.clear();
    }

    /**
     * Send message with logging levels other than debug, no logging.
     * @param enabledLevels enabled logging levels
     */
    @ParameterizedTest
    @MethodSource("loggingLevels")
    public void sendMessage_NotDebugLoggingLevel_MessageSent_NoLogging(final ImmutableSet<Level> enabledLevels) {
        this.step.givenISetLoggingLevel(enabledLevels)
                .whenISendMessage(MESSAGE)
                .thenRabbitTemplateConvertAndSendIsCalled()
                .thenWithNoLogging();
    }

    /**
     * Send message with debug logging level, correct message is logged.
     */
    @Test
    public void sendMessage_DebugLoggingLevel_MessageSent_WithLogging() {
        this.step.givenISetLoggingLevel(ConventionalLevelHierarchy.DEBUG_LEVELS)
                .whenISendMessage(MESSAGE)
                .thenRabbitTemplateConvertAndSendIsCalled()
                .thenLoggingEventShouldBe(newArrayList(
                        debug("Sending message to queue using exchange {}, routingKey {}. Message={}", EXCHANGE, ROUTING_KEY, MESSAGE),
                        debug("Message sent successfully to queue using exchange {}, routingKey {}. Message={}", EXCHANGE, ROUTING_KEY, MESSAGE)
                ));
    }

    private static Stream<ImmutableSet<Level>> loggingLevels() {
        return Stream.of(
                ConventionalLevelHierarchy.ERROR_LEVELS,
                ConventionalLevelHierarchy.INFO_LEVELS,
                ConventionalLevelHierarchy.TRACE_LEVELS,
                ConventionalLevelHierarchy.WARN_LEVELS
        );
    }

}
