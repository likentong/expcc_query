package org.exp.cc.processor.impl.mongo;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.exp.cc.enums.ComparisonOperator;
import org.exp.cc.exception.InvalidQueryException;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.model.persistence.QueryOperator;
import org.exp.cc.processor.impl.mongo.steps.ComparisonOperatorProcessorImplStep;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.data.mongodb.core.query.Criteria;

import java.util.Collections;
import java.util.stream.Stream;

/**
 * Comparison operator processor implementation test.
 */
public class ComparisonOperatorProcessorImplTest {
    private static final String FIELD_NAME = "fieldName";
    private static final String FIELD_NAME_2 = "fieldName2";
    private static final int FIELD_VALUE = 1;
    private static final String FIELD_VALUE_2 = "2";

    private ComparisonOperatorProcessorImplStep step;

    public ComparisonOperatorProcessorImplTest() {
        this.step = new ComparisonOperatorProcessorImplStep();
    }

    /**
     * generateCriteria with invalid query fields, should throw {@link IllegalArgumentException}.
     * @param testCaseName test case name
     * @param queryFields query fields
     * @param exceptionMessage exception message
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("invalidQueryFields")
    public void generateCriteria_WithInvalidQueryFields_ThrowsIllegalArgumentException(final String testCaseName, final QueryFields queryFields, final String exceptionMessage) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIGenerateCriteria(queryFields))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(exceptionMessage);
    }

    /**
     * generateCriteria with invalid query operator, should throw {@link InvalidQueryException}.
     * @param testCaseName test case name
     * @param queryFields query fields
     * @param exceptionMessage exception message
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("invalidQueryOperator")
    public void generateCriteria_WithInvalidQueryOperator_ThrowsInvalidQueryException(final String testCaseName, final QueryFields queryFields, final String exceptionMessage) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIGenerateCriteria(queryFields))
                .thenExceptionMatchCorrectType(InvalidQueryException.class)
                .thenExceptionWithCorrectMessage(exceptionMessage);
    }

    /**
     * generateCriteria with valid query fields, should return correct result.
     * @param testCaseName test case name
     * @param queryFields query fields
     * @param expectedCriteria expected criteria
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("validQueryFields")
    public void generateCriteria_WithValidQueryFields_ShouldReturnsCriteria(final String testCaseName, final QueryFields queryFields, final Criteria[] expectedCriteria) {
        this.step.givenIHaveNoSetup()
                .whenIGenerateCriteria(queryFields)
                .thenCriteriaShouldBe(expectedCriteria);
    }

    /**
     * generateCriteria with valid query fields and comparison operator, should return correct results.
     * @param testCaseName test case name
     * @param operator comparison operator
     * @param value value
     * @param expectedCriteria expected criteria
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("validQueryOperator")
    public void generateCriteria_WithValidQueryFields_WithDifferentComparisonOperator_ShouldReturnCriteria(final String testCaseName,
                                                                                                           final String operator,
                                                                                                           final Object value,
                                                                                                           final Criteria[] expectedCriteria) {
        final QueryOperator queryOperator = new QueryOperator(ImmutableMap.of(operator, value));

        this.step.givenIHaveNoSetup()
                .whenIGenerateCriteria(new QueryFields(ImmutableMap.of(FIELD_NAME, queryOperator)))
                .thenCriteriaShouldBe(expectedCriteria);
    }

    private static Stream<Arguments> validQueryOperator() {
        return Stream.of(
                Arguments.of("$eq", ComparisonOperator.EQ.getOperator(), FIELD_VALUE_2, new Criteria[]{Criteria.where(FIELD_NAME).is(FIELD_VALUE_2)}),
                Arguments.of("$gt", ComparisonOperator.GT.getOperator(), FIELD_VALUE, new Criteria[]{Criteria.where(FIELD_NAME).gt(FIELD_VALUE)}),
                Arguments.of("$gte", ComparisonOperator.GTE.getOperator(), FIELD_VALUE, new Criteria[]{Criteria.where(FIELD_NAME).gte(FIELD_VALUE)}),
                Arguments.of("$lt", ComparisonOperator.LT.getOperator(), FIELD_VALUE, new Criteria[]{Criteria.where(FIELD_NAME).lt(FIELD_VALUE)}),
                Arguments.of("$lte", ComparisonOperator.LTE.getOperator(), FIELD_VALUE, new Criteria[]{Criteria.where(FIELD_NAME).lte(FIELD_VALUE)}),
                Arguments.of("$in_singleValue", ComparisonOperator.IN.getOperator(), FIELD_VALUE, new Criteria[]{Criteria.where(FIELD_NAME).in(FIELD_VALUE)}),
                Arguments.of("$in_hashSet", ComparisonOperator.IN.getOperator(), ImmutableSet.of(FIELD_VALUE, FIELD_VALUE_2), new Criteria[]{Criteria.where(FIELD_NAME).in(ImmutableSet.of(FIELD_VALUE, FIELD_VALUE_2))}),
                Arguments.of("$in_arrayList", ComparisonOperator.IN.getOperator(), ImmutableList.of(FIELD_VALUE, FIELD_VALUE_2), new Criteria[]{Criteria.where(FIELD_NAME).in(ImmutableList.of(FIELD_VALUE, FIELD_VALUE_2))})
        );
    }

    private static Stream<Arguments> validQueryFields() {
        final QueryOperator multipleQueryOperator = new QueryOperator(ImmutableMap.of(
                ComparisonOperator.GTE.getOperator(), FIELD_VALUE,
                ComparisonOperator.EQ.getOperator(), FIELD_VALUE_2
        ));

        final QueryFields multipleQueryFields = new QueryFields(ImmutableMap.of(
                FIELD_NAME, multipleQueryOperator,
                FIELD_NAME_2, multipleQueryOperator
        ));

        final Criteria QueryOperatorCriteriaFieldName = new Criteria().andOperator(Criteria.where(FIELD_NAME).gte(FIELD_VALUE),
                Criteria.where(FIELD_NAME).is(FIELD_VALUE_2));

        final Criteria QueryOperatorCriteriaFieldName2 = new Criteria().andOperator(Criteria.where(FIELD_NAME_2).gte(FIELD_VALUE),
                Criteria.where(FIELD_NAME_2).is(FIELD_VALUE_2));

        return Stream.of(
                Arguments.of("queryFields_SingleFieldWithEmptyFields", new QueryFields(Collections.emptyMap()), new Criteria[]{}),
                Arguments.of("queryFields_MultipleFieldsWithMultipleQueryOperator", new QueryFields(ImmutableMap.of(FIELD_NAME, multipleQueryOperator)), new Criteria[]{QueryOperatorCriteriaFieldName}),
                Arguments.of("queryFields_MultipleFieldsWithQueryOperator", multipleQueryFields, new Criteria[]{QueryOperatorCriteriaFieldName, QueryOperatorCriteriaFieldName2})
        );
    }

    private static Stream<Arguments> invalidQueryFields() {
        return Stream.of(
                Arguments.of("queryFields_IsNull", null, "queryFields cannot be null."),
                Arguments.of("queryFields_fields_IsNull", new QueryFields(null), "fields cannot be null.")
        );
    }

    private static Stream<Arguments> invalidQueryOperator() {
        final QueryOperator emptyKeyQueryOperator = new QueryOperator(ImmutableMap.of("", FIELD_VALUE));
        final QueryOperator spaceKeyQueryOperator = new QueryOperator(ImmutableMap.of(" ", FIELD_VALUE));
        final QueryOperator unknownQueryOperator = new QueryOperator(ImmutableMap.of("UnknownOperator", FIELD_VALUE));
        final QueryOperator emptyMapQueryOperator = new QueryOperator(Collections.emptyMap());

        return Stream.of(
                Arguments.of("queryOperator_EmptyKey", new QueryFields(ImmutableMap.of(FIELD_NAME, emptyKeyQueryOperator)), "Operator cannot be blank : [fieldName]"),
                Arguments.of("queryOperator_SpaceKey", new QueryFields(ImmutableMap.of(FIELD_NAME, spaceKeyQueryOperator)), "Operator cannot be blank : [fieldName]"),
                Arguments.of("queryOperator_UnknownOperatorKey", new QueryFields(ImmutableMap.of(FIELD_NAME, unknownQueryOperator)), "UnknownOperator comparison operator not supported."),
                Arguments.of("queryOperator_emptyMap", new QueryFields(ImmutableMap.of(FIELD_NAME, emptyMapQueryOperator)), "fieldName does not has a query operator.")
        );
    }
}
