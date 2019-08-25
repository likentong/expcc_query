package org.exp.cc.processor.impl.mongo;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.StringUtils;
import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.enums.AggregationOperator;
import org.exp.cc.enums.ComparisonOperator;
import org.exp.cc.model.AggregationCriteria;
import org.exp.cc.model.persistence.QueryFields;
import org.exp.cc.model.persistence.QueryOperator;
import org.exp.cc.processor.ComparisonOperatorProcessor;
import org.exp.cc.processor.impl.mongo.steps.AggregationOperatorProcessorImplStep;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.aggregation.GroupOperation;
import org.springframework.data.mongodb.core.aggregation.MatchOperation;
import org.springframework.data.mongodb.core.aggregation.ProjectionOperation;
import org.springframework.data.mongodb.core.query.Criteria;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static org.assertj.core.util.Lists.newArrayList;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Aggregation operator processor implementation test.
 */
public class AggregationOperatorProcessorImplTest {
    private static final String GROUP_FIELD_NAME = "ID";
    private static final String QUERY_FIELD_NAME = "number of hours on fb";
    private static final int QUERY_VALUE = 4;
    private static final Criteria MATCH_CRITERIA = Criteria.where(QUERY_FIELD_NAME).gte(QUERY_VALUE);
    private static final QueryOperator QUERY_OPERATOR = new QueryOperator(ImmutableMap.of(
            ComparisonOperator.GTE.getOperator(), QUERY_VALUE
    ));

    private static final QueryFields QUERY_FIELDS = new QueryFields(ImmutableMap.of(
            QUERY_FIELD_NAME, QUERY_OPERATOR
    ));

    private static final List<String> FIELDS_TO_GROUP = newArrayList("ID");
    private static final Map<String, AggregationOperator> FIELDS_TO_AGGREGATE = ImmutableMap.of(
            "idPerson", AggregationOperator.SUM
    );
    private AggregationOperatorProcessorImplStep step;

    public AggregationOperatorProcessorImplTest() {
        this.step = new AggregationOperatorProcessorImplStep();
    }

    /**
     * Test Initialization.
     */
    @BeforeEach
    public void init() {
        final ComparisonOperatorProcessor comparisonOperatorProcessor = mock(ComparisonOperatorProcessor.class);
        final Criteria[] matchCriteria = new Criteria[]{MATCH_CRITERIA};

        this.step.givenISetupAggregationOperatorProcessorImpl(comparisonOperatorProcessor);
        when(comparisonOperatorProcessor.generateCriteria(any(QueryFields.class))).thenReturn(matchCriteria);
    }

    /**
     * generateAggregation with invalid aggregation criteria, should throw {@link IllegalArgumentException}.
     * @param testCaseName test case name
     * @param aggregationCriteria aggregation criteria
     * @param exceptionMessgae exception message
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("invalidAggregationCriteriaException")
    public void generateAggregation_WithInvalidAggregationCriteria_ThrowsIllegalArgumentException(final String testCaseName,
                                                                                                  final AggregationCriteria aggregationCriteria,
                                                                                                  final String exceptionMessgae) {
        this.step.givenISetupAThrowingCallable(
                () -> this.step.givenIHaveNoSetup()
                        .whenIGenerateAggregation(aggregationCriteria))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage(exceptionMessgae);
    }

    /**
     * generateAggregation with valid aggregation criteria, should return {@link Aggregation}.
     */
    @Test
    public void generateAggregation_WithValidAggregationCriteria_ShouldReturnsCorrectAggregation() {
        final AggregationCriteria aggregationCriteria = new AggregationCriteria(QUERY_FIELDS, FIELDS_TO_GROUP, FIELDS_TO_AGGREGATE, GROUP_FIELD_NAME);

        final MatchOperation expectedMatchOperation = Aggregation.match(MATCH_CRITERIA);

        final GroupOperation expectedGroupOperation = Aggregation.group(FIELDS_TO_GROUP.toArray(new String[0]))
                .sum("idPerson").as("idPerson sum");

        final ProjectionOperation expectedProjectionOperation = Aggregation.project()
                .andExpression(PersistenceConstant.MongoDB.MONGO_OBJECT_ID).as(GROUP_FIELD_NAME)
                .andExclude(PersistenceConstant.MongoDB.MONGO_OBJECT_ID)
                .andInclude("idPerson sum");

        final Aggregation expectedAggregation = Aggregation.newAggregation(expectedMatchOperation, expectedGroupOperation, expectedProjectionOperation);

        this.step.givenIHaveNoSetup()
                .whenIGenerateAggregation(aggregationCriteria)
                .thenGeneratedAggregationShouldBe(expectedAggregation);
    }

    private static Stream<Arguments> invalidAggregationCriteriaException() {
        return Stream.of(
                Arguments.of("aggregationCriteria_isNull", null, "aggregationCriteria cannot be null."),
                Arguments.of("aggregationCriteria_queryFields_isNull", new AggregationCriteria(null, FIELDS_TO_GROUP, FIELDS_TO_AGGREGATE, GROUP_FIELD_NAME), "queryFields cannot be null."),
                Arguments.of("aggregationCriteria_fieldsToGroup_isNull", new AggregationCriteria(QUERY_FIELDS, null, FIELDS_TO_AGGREGATE, GROUP_FIELD_NAME), "fieldsToGroup cannot be null or empty."),
                Arguments.of("aggregationCriteria_fieldsToGroup_isEmpty", new AggregationCriteria(QUERY_FIELDS, Collections.emptyList(), FIELDS_TO_AGGREGATE, GROUP_FIELD_NAME), "fieldsToGroup cannot be null or empty."),
                Arguments.of("aggregationCriteria_fieldsToAggregate_isNull", new AggregationCriteria(QUERY_FIELDS, FIELDS_TO_GROUP, null, GROUP_FIELD_NAME), "fieldsToAggregate cannot be null or empty."),
                Arguments.of("aggregationCriteria_fieldsToAggregate_isEmpty", new AggregationCriteria(QUERY_FIELDS, FIELDS_TO_GROUP, Collections.emptyMap(), GROUP_FIELD_NAME), "fieldsToAggregate cannot be null or empty."),
                Arguments.of("aggregationCriteria_queryFields_fields_isNull", new AggregationCriteria(new QueryFields(null), FIELDS_TO_GROUP, FIELDS_TO_AGGREGATE, GROUP_FIELD_NAME), "fields cannot be null or empty."),
                Arguments.of("aggregationCriteria_queryFields_fields_isEmpty", new AggregationCriteria(new QueryFields(Collections.emptyMap()), FIELDS_TO_GROUP, FIELDS_TO_AGGREGATE, GROUP_FIELD_NAME), "fields cannot be null or empty."),
                Arguments.of("aggregationCriteria_groupFieldName_isNull", new AggregationCriteria(QUERY_FIELDS, FIELDS_TO_GROUP, FIELDS_TO_AGGREGATE, null), "groupFieldName cannot be blank."),
                Arguments.of("aggregationCriteria_groupFieldName_isEmpty", new AggregationCriteria(QUERY_FIELDS, FIELDS_TO_GROUP, FIELDS_TO_AGGREGATE, StringUtils.EMPTY), "groupFieldName cannot be blank."),
                Arguments.of("aggregationCriteria_groupFieldName_isSpace", new AggregationCriteria(QUERY_FIELDS, FIELDS_TO_GROUP, FIELDS_TO_AGGREGATE, " "), "groupFieldName cannot be blank.")
        );
    }

}
