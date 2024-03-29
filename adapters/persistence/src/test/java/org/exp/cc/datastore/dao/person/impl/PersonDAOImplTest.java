package org.exp.cc.datastore.dao.person.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.datastore.dao.person.impl.steps.PersonDAOImplStep;
import org.exp.cc.enums.AggregationOperator;
import org.exp.cc.enums.ComparisonOperator;
import org.exp.cc.model.AggregationCriteria;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.model.persistence.QueryOperator;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.*;
import java.util.stream.Stream;

import static de.flapdoodle.embed.process.collections.Collections.newArrayList;

/**
 * PersonDAO implementation test.
 */
public class PersonDAOImplTest {
    private static final String ID_EXCEPTION_MESSAGE = "id cannot be null or empty.";
    private final PersonDAOImplStep step;

    public PersonDAOImplTest() {
        this.step = new PersonDAOImplStep();
    }

    /**
     * getPersonCountByDemographicId null or empty id, should throws {@link IllegalArgumentException}.
     * @param id id
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("invalidArgument")
    public void getPersonCountByDemographicId_NullOrEmptyId_ThrowsIllegalArgumentException(final String testCaseName, final Set<Object> id) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIGetPersonCountByDemographicId(id))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(ID_EXCEPTION_MESSAGE);
    }

    /**
     * getPersonCountByDemographicId with given Id, returns expected results.
     */
    @Test
    public void getPersonCountByDemographicId_WithId_ReturnsExpectedResults() {
        final Set<Object> id = new HashSet<>(Arrays.asList(1, 2));

        final List<Map<String, Object>> results = newArrayList(
                ImmutableMap.of(PersistenceConstant.Demographic.ID, 1, "idPerson count", 3),
                ImmutableMap.of(PersistenceConstant.Demographic.ID, 2, "idPerson count", 6)
        );

        final QueryOperator queryOperator = new QueryOperator(ImmutableMap.of(ComparisonOperator.IN.getOperator(), id));
        final QueryFields queryFields = new QueryFields(ImmutableMap.of(PersistenceConstant.Demographic.ID, queryOperator));
        final Map<String, AggregationOperator> fieldsToAggregate = ImmutableMap.of(PersistenceConstant.Person.ID_PERSON, AggregationOperator.COUNT);

        final AggregationCriteria aggregationCriteria = new AggregationCriteria(
                queryFields,
                Collections.singletonList(PersistenceConstant.Demographic.ID),
                fieldsToAggregate,
                PersistenceConstant.Demographic.ID);

        this.step.givenIMockDataStoreDAOAggregateData(results)
                .whenIGetPersonCountByDemographicId(id)
                .thenResultsShouldBe(results)
                .thenAggregationCriteriaShouldBe(aggregationCriteria);
    }

    /**
     * getPersonByDemographicId null or empty id, should throws {@link IllegalArgumentException}.
     * @param id id
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("invalidArgument")
    public void getPersonByDemographicId_NullOrEmptyId_ThrowIllegalArgumentException(final String testCaseName, final Set<Object> id) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIGetPersonByDemographicId(id))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(ID_EXCEPTION_MESSAGE);
    }

    /**
     * getPersonByDemographicId with given Id, returns expected results.
     */
    @Test
    public void getPersonByDemographicId_WithId_ReturnsExcpectedResults() {
        final Set<Object> id = new HashSet<>(Arrays.asList(1, 2));

        final List<Map<String, Object>> results = newArrayList(
                ImmutableMap.of(PersistenceConstant.Demographic.ID, 1, "idPerson", 1),
                ImmutableMap.of(PersistenceConstant.Demographic.ID, 2, "idPerson", 2)
        );

        this.step.givenIMockDataStoreDAOQueryData(results)
                .whenIGetPersonByDemographicId(id)
                .thenResultsShouldBe(results)
                .thenQueryStringShouldBe("{ ID : { $in : [1,2] }}");
    }

    private static Stream<Arguments> invalidArgument() {
        return Stream.of(
                Arguments.of("null_id", null),
                Arguments.of("empty_id", Collections.emptySet())
        );
    }
}
