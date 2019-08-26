package org.exp.cc.processor.impl.mongo;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.enums.ComparisonOperator;
import org.exp.cc.enums.LogicalOperator;
import org.exp.cc.exception.InvalidQueryException;
import org.exp.cc.model.persistence.QueryCriteria;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.model.persistence.QueryOperator;
import org.exp.cc.processor.impl.mongo.steps.LogicalOperatorProcessorImplStep;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.data.mongodb.core.query.Criteria;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static org.assertj.core.util.Lists.newArrayList;

/**
 * Logical operator processor implementation test.
 */
public class LogicalOperatorProcessorImplTest {
    private static final String QUERY_FIELD_NAME = "number of hours on fb";
    private static final String QUERY_FIELD_NAME_2 = "group";
    private static final int QUERY_VALUE = 4;
    private static final String QUERY_VALUE_2 = "B1";

    private static final QueryOperator QUERY_OPERATOR_GTE = new QueryOperator(ImmutableMap.of(ComparisonOperator.GTE.getOperator(), QUERY_VALUE));
    private static final QueryOperator QUERY_OPERATOR_EQ = new QueryOperator(ImmutableMap.of(ComparisonOperator.EQ.getOperator(), QUERY_VALUE_2));
    private static final QueryFields QUERY_FIELDS = new QueryFields(ImmutableMap.of(QUERY_FIELD_NAME, QUERY_OPERATOR_GTE));

    private static final QueryFields MULTIPLE_QUERY_FIELDS = new QueryFields(ImmutableMap.of(
            QUERY_FIELD_NAME, QUERY_OPERATOR_GTE,
            QUERY_FIELD_NAME_2, QUERY_OPERATOR_EQ
    ));

    private static final Criteria QUERY_FIELD_GTE_CRITERIA = Criteria.where(QUERY_FIELD_NAME).gte(QUERY_VALUE);
    private static final Criteria QUERY_FIELD_EQ_CRITERIA = Criteria.where(QUERY_FIELD_NAME_2).is(QUERY_VALUE_2);

    private LogicalOperatorProcessorImplStep step;

    public LogicalOperatorProcessorImplTest() {
        this.step = new LogicalOperatorProcessorImplStep();
    }

    /**
     * generateCriteria with invalid query criteria, should throws {@link IllegalArgumentException}.
     * @param testCaseName test case name
     * @param queryCriteria query criteria
     * @param exceptionMessage exception message
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("invalidQueryCriteria")
    public void generateCriteria_WithInvalidQueryCriteria_ThrowsIllegalArgumentException(final String testCaseName,
                                                                                         final QueryCriteria queryCriteria,
                                                                                         final String exceptionMessage) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIGenerateCriteria(queryCriteria))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(exceptionMessage);
    }

    /**
     * generateCriteria with invalid query criteria query, throws {@link InvalidQueryException}.
     * @param testCaseName test case name
     * @param queryCriteria query criteria
     * @param exceptionMessage exception message
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("invalidQueryCriteriaQuery")
    public void generateCriteria_WithInvalidQueryCriteriaQuery_ThrowsInvalidQueryException(final String testCaseName,
                                                                                           final QueryCriteria queryCriteria,
                                                                                           final String exceptionMessage) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIGenerateCriteria(queryCriteria))
                .thenExceptionMatchCorrectType(InvalidQueryException.class)
                .thenExceptionWithCorrectMessage(exceptionMessage);
    }

    /**
     * generateCriteria with valid query criteria, should return criteria.
     * @param testCaseName test case name
     * @param queryCriteria query criteria
     * @param expectedCriteria expected criteria
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("validQueryCriteria")
    public void generateCriteria_WithValidQueryCriteria_ShouldReturnsCorrectCriteria(final String testCaseName,
                                                                                     final QueryCriteria queryCriteria,
                                                                                     final Criteria expectedCriteria) {
        this.step.givenIMockComparisonOperatorGenerateCriteria(QUERY_FIELDS, new Criteria[]{QUERY_FIELD_GTE_CRITERIA})
                .givenIMockComparisonOperatorGenerateCriteria(MULTIPLE_QUERY_FIELDS, new Criteria[]{QUERY_FIELD_GTE_CRITERIA, QUERY_FIELD_EQ_CRITERIA})
                .whenIGenerateCriteria(queryCriteria)
                .thenCriteriaShouldBe(expectedCriteria);
    }

    private static Stream<Arguments> validQueryCriteria() {
        final List<Map<String, QueryFields>> orLogicalOperation = newArrayList(ImmutableMap.of(LogicalOperator.OR.getOperator(), QUERY_FIELDS));
        final List<Map<String, QueryFields>> andLogicalOperation = newArrayList(ImmutableMap.of(LogicalOperator.AND.getOperator(), QUERY_FIELDS));

        final List<Map<String, QueryFields>> mixedLogicalOperation = newArrayList(
                ImmutableMap.of(LogicalOperator.AND.getOperator(), QUERY_FIELDS),
                ImmutableMap.of(LogicalOperator.OR.getOperator(), QUERY_FIELDS)
        );

        final List<Map<String, QueryFields>> mixedLogicalOperationWithMultipleQueryFields = newArrayList(
                ImmutableMap.of(LogicalOperator.AND.getOperator(), QUERY_FIELDS),
                ImmutableMap.of(LogicalOperator.OR.getOperator(), MULTIPLE_QUERY_FIELDS)
        );

        return Stream.of(
                Arguments.of("queryCriteria_emptyList", new QueryCriteria(Collections.emptyList()), new Criteria()),
                Arguments.of("queryCriteria_orLogicalOperation", new QueryCriteria(orLogicalOperation), QUERY_FIELD_GTE_CRITERIA),
                Arguments.of("queryCriteria_andLogicalOperation", new QueryCriteria(andLogicalOperation), QUERY_FIELD_GTE_CRITERIA),
                Arguments.of("queryCriteria_mixedLogicalOperation", new QueryCriteria(mixedLogicalOperation), new Criteria().andOperator(QUERY_FIELD_GTE_CRITERIA, QUERY_FIELD_GTE_CRITERIA)),
                Arguments.of("queryCriteria_mixedLogicalOperation_multipleQueryFields", new QueryCriteria(mixedLogicalOperationWithMultipleQueryFields),
                        new Criteria().andOperator(QUERY_FIELD_GTE_CRITERIA, new Criteria().orOperator(QUERY_FIELD_GTE_CRITERIA, QUERY_FIELD_EQ_CRITERIA))
                )
        );
    }

    private static Stream<Arguments> invalidQueryCriteriaQuery() {
        final List<Map<String, QueryFields>> queryWithMoreThanOneElement = newArrayList(
                ImmutableMap.of(
                        LogicalOperator.AND.getOperator(), QUERY_FIELDS,
                        LogicalOperator.OR.getOperator(), QUERY_FIELDS)
        );

        final List<Map<String, QueryFields>> queryWithUnknonLogicalOperator = newArrayList(
                ImmutableMap.of("$unknown", QUERY_FIELDS)
        );

        return Stream.of(
                Arguments.of("queryCriteria_query_moreThanOneElement", new QueryCriteria(queryWithMoreThanOneElement), "Nested operators not supported. Found [$and,$or]"),
                Arguments.of("queryCriteria_query_unknownLogicalOperator", new QueryCriteria(queryWithUnknonLogicalOperator), "$unknown logical operator not supported.")
        );
    }

    private static Stream<Arguments> invalidQueryCriteria() {
        return Stream.of(
                Arguments.of("queryCriteria_IsNull", null, "queryCriteria cannot be null."),
                Arguments.of("queryCriteria_query_isNull", new QueryCriteria(null), "queryCriteria query cannot be null.")
        );
    }

}
