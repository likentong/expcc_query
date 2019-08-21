package org.exp.cc.runner;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.bson.Document;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Profile;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.stereotype.Component;

import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.exp.cc.constant.Constant.Profile.SAMPLE_DATA;

/**
 * Sample data startup runner.
 */
@Component
@Profile(SAMPLE_DATA)
public class SampleDataStartupRunner implements CommandLineRunner {

    private static final Logger logger = LoggerFactory.getLogger(SampleDataStartupRunner.class);

    private final MongoTemplate mongoTemplate;
    private final ResourcePatternResolver resourcePatternResolver;

    public SampleDataStartupRunner(final MongoTemplate mongoTemplate, final ResourcePatternResolver resourcePatternResolver) {
        this.mongoTemplate = mongoTemplate;
        this.resourcePatternResolver = resourcePatternResolver;
    }

    @Override
    public void run(String... args) throws Exception {
        final Resource[] resources = resourcePatternResolver.getResources("classpath:sample_data/*.csv");

        for (Resource resource : resources) {
            final String filename = resource.getFilename();

            if (StringUtils.isNotBlank(filename)) {
                final String collectionName = FilenameUtils.getBaseName(filename);
                final List<Document> documents = new ArrayList<>();

                logger.info("Loading sample data: {}", filename);

                try (final Reader reader = new InputStreamReader(resource.getInputStream(), StandardCharsets.UTF_8);
                     final CSVParser parser = new CSVParser(reader, CSVFormat.EXCEL.withFirstRecordAsHeader())) {

                    for (CSVRecord csvRecord : parser) {
                        final Map<String, Object> record = new HashMap<>();

                        parser.getHeaderMap()
                                .keySet()
                                .forEach(key -> {
                                    final String value = csvRecord.get(key);

                                    // TODO: Pending metadata on field data type, currently any value that is parsable to number will be converted to Integer.
                                    if (NumberUtils.isParsable(value)) {
                                        record.put(key, Integer.parseInt(value));
                                    } else {
                                        record.put(key, value);
                                    }
                                });

                        documents.add(new Document(record));
                        record.clear();
                    }
                }

                logger.info("Dropping collection: {}.", collectionName);
                this.mongoTemplate.dropCollection(collectionName);

                this.mongoTemplate.insert(documents, collectionName);
                logger.info("Total of {} sample data from {} inserted into {}.", documents.size(), resource.getFilename(), collectionName);
            }
        }
    }
}
