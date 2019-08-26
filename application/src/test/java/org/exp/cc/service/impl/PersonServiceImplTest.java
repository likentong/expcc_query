package org.exp.cc.service.impl;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.exp.cc.constant.ApplicationConstant;
import org.exp.cc.model.service.Result;
import org.exp.cc.service.impl.steps.PersonServiceImplStep;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import static org.assertj.core.util.Lists.newArrayList;

/**
 * Person service implementation test.
 */
public class PersonServiceImplTest {
    private static final String ID_EXCEPTION_MESSAGE = "id cannot be null or empty.";
    private static final Map<String, Object> PERSON_DATA = ImmutableMap.of("ID", 1, "idPerson", 1, "data", "test");
    private PersonServiceImplStep step;

    public PersonServiceImplTest() {
        this.step = new PersonServiceImplStep();
    }

    /**
     * getPerson with invalid id, should throws {@link IllegalArgumentException}.
     * @param testClassName test class name
     * @param id id
     */
    @ParameterizedTest
    @MethodSource("invalidId")
    public void getPerson_WithInvalidId_ThrowsIllegalArgumentException(final String testClassName, final Set<Object> id) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIGetPerson(id))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(ID_EXCEPTION_MESSAGE);
    }

    /**
     * getPerson with valid id, should return result.
     */
    @Test
    public void getPerson_ValidId_ShouldReturnResult() {
        final Set<Object> id = ImmutableSet.of(1, 2);
        final List<Map<String, Object>> data = newArrayList(PERSON_DATA);

        this.step.givenISetupPersonDAOGetPersonByDemographicId(id, data)
                .whenIGetPerson(id)
                .thenResultShouldBe(new Result(data, ImmutableMap.of(ApplicationConstant.Summary.RECORD_COUNT, 1)));
    }

    /**
     * getPersonCountGroupByDemographicID with invalid id, should throws {@link IllegalArgumentException}.
     * @param testClassName test class name
     * @param id id
     */
    @ParameterizedTest
    @MethodSource("invalidId")
    public void getPersonCountGroupByDemographicID_WithInvalidId_ThrowsIllegalArgumentException(final String testClassName, final Set<Object> id) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIGetPersonCountByDemographicId(id))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(ID_EXCEPTION_MESSAGE);
    }

    /**
     * getPersonCountGroupByDemographicID with valid id, should return result.
     */
    @Test
    public void getPersonCountGroupByDemographicID_ValidId_ShouldReturnResult() {
        final Set<Object> id = ImmutableSet.of(1, 2);
        final List<Map<String, Object>> data = newArrayList(ImmutableMap.of("ID", 1, "sum", 5));

        this.step.givenISetupPersonDAOGetPersonCountByDemographicId(id, data)
                .whenIGetPersonCountByDemographicId(id)
                .thenResultShouldBe(new Result(data, ImmutableMap.of(ApplicationConstant.Summary.RECORD_COUNT, 1)));
    }

    /**
     * sendToPersonQueue with invalid person, should throws {@link IllegalArgumentException}.
     * @param testClassName test class name
     * @param person person
     */
    @ParameterizedTest
    @MethodSource("invalidPerson")
    public void sendToPersonQueue_WithInvalidPerson_ThrowsIllegalArgumentException(final String testClassName, final Map<String, Object> person) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenISendToPersonQueue(person))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage("person cannot be null or empty.");
    }

    /**
     * sendToPersonQueue with valid person, should called message queue send message.
     */
    @Test
    public void sendToPersonQueue_ValidPerson_MessageQueueIsCalled() {
        this.step.givenIHaveNoSetup()
                .whenISendToPersonQueue(PERSON_DATA)
                .thenPersonMessageQueueSendMessageIsCalled();
    }

    private static Stream<Arguments> invalidId() {
        return Stream.of(
                Arguments.of("null_id", null),
                Arguments.of("empty_id", Collections.emptySet())
        );
    }

    private static Stream<Arguments> invalidPerson() {
        return Stream.of(
                Arguments.of("null_person", null),
                Arguments.of("empty_person", Collections.emptyMap())
        );
    }

}
