package org.exp.cc.processor.mongo.impl;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.enums.AggregationOperator;
import org.exp.cc.exception.ApplicationRuntimeException;
import org.exp.cc.model.AggregationCriteria;
import org.exp.cc.processor.mongo.AggregationOperatorProcessor;
import org.exp.cc.processor.mongo.ComparisonOperatorProcessor;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.aggregation.GroupOperation;
import org.springframework.data.mongodb.core.aggregation.MatchOperation;
import org.springframework.data.mongodb.core.aggregation.ProjectionOperation;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

@Component
public class MongoAggregationOperatorProcessor implements AggregationOperatorProcessor {
    private final Map<String, BiFunction<GroupOperation, ImmutablePair<String, String>, GroupOperation>> aggregatorOperatorProcessor =
            ImmutableMap.<String, BiFunction<GroupOperation, ImmutablePair<String, String>, GroupOperation>>builder()
                    .put(AggregationOperator.SUM.getOperator(), this::sumAggregationProcessor)
                    .build();

    private final ComparisonOperatorProcessor comparisonOperatorProcessor;

    public MongoAggregationOperatorProcessor(final ComparisonOperatorProcessor comparisonOperatorProcessor) {
        this.comparisonOperatorProcessor = comparisonOperatorProcessor;
    }

    @Override
    public Aggregation generateAggregation(final AggregationCriteria aggregationCriteria) {
        final Criteria criteria = new Criteria();
        final Criteria[] matchCriteria = this.comparisonOperatorProcessor.generateCriteria(aggregationCriteria.getQueryFields());
        final MatchOperation matchOperation = Aggregation.match(criteria.andOperator(matchCriteria));
        final String[] fields = aggregationCriteria.getFieldsToGroup().toArray(new String[0]);
        final List<String> fieldsToProject = new ArrayList<>();

        GroupOperation groupOperation = Aggregation.group(fields);

        for (Map.Entry<String, AggregationOperator> entry : aggregationCriteria.getFieldsToAggregate().entrySet()) {
            final String operator = entry.getValue().getOperator();

            if (this.aggregatorOperatorProcessor.get(operator) == null) {
                throw new ApplicationRuntimeException(String.format("Aggregation operator processor not found for %s", operator));
            }

            final String fieldName = entry.getKey() + " " + entry.getValue().toString().toLowerCase();
            fieldsToProject.add(fieldName);

            groupOperation = this.aggregatorOperatorProcessor.get(operator).apply(groupOperation, new ImmutablePair<>(entry.getKey(), fieldName));
        }

        ProjectionOperation projectionOperation = Aggregation.project();

        if (fields.length == 1) {
            projectionOperation = projectionOperation.andExpression(PersistenceConstant.MongoDB.MONGO_OBJECT_ID).as(fields[0]);
        }

        projectionOperation = projectionOperation.andExclude(PersistenceConstant.MongoDB.MONGO_OBJECT_ID)
                .andInclude(fieldsToProject.toArray(new String[0]));

        return Aggregation.newAggregation(matchOperation, groupOperation, projectionOperation);
    }

    private GroupOperation sumAggregationProcessor(final GroupOperation groupOperation, final ImmutablePair<String, String> fieldNameMapper) {
        return groupOperation.sum(fieldNameMapper.getKey()).as(fieldNameMapper.getValue());
    }
}
