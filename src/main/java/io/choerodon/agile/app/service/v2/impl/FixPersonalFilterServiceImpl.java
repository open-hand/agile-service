package io.choerodon.agile.app.service.v2.impl;

import java.io.IOException;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

import io.choerodon.agile.api.vo.business.TagVO;
import io.choerodon.agile.api.vo.search.*;
import io.choerodon.agile.app.service.v2.FixPersonalFilterService;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.search.SearchConstant;
import io.choerodon.core.exception.CommonException;

import static io.choerodon.agile.infra.enums.search.SearchConstant.Operation;
import static io.choerodon.agile.infra.enums.search.SearchConstant.Relationship;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.Pair;

/**
 * @author superlee
 * @since 2022-11-30
 */
@Service
public class FixPersonalFilterServiceImpl implements FixPersonalFilterService {

    private static final Logger logger = LoggerFactory.getLogger(FixPersonalFilterServiceImpl.class);
    @Autowired
    private ObjectMapper objectMapper;

    private static final List<String> CONDITION_KEYS = Arrays.asList("advancedSearchArgs", "otherArgs", "searchArgs");

    private static final List<String> PREDEFINED_NUMBER_FIELDS = Arrays.asList(
            FieldCode.STORY_POINTS,
            FieldCode.ESTIMATE_TIME,
            FieldCode.REMAINING_TIME);


    private static final String CUSTOM_FIELD = "customField";

    private static final String TAGS = "tags";

    private static final String YQ_CLOUD_FLAG = "yqCloudFlag";
    private static final String YQ_CLOUD_NUM = "yqCloudNum";
    private static final String ISSUE_TYPE_ID = "issueTypeId";
    private static final String STATUS_ID = "statusId";
    private static final String REPORTER_IDS = "reporterIds";
    private static final String PRIORITY_ID = "priorityId";
    private static final String MAIN_RESPONSIBLE_IDS = "mainResponsibleIds";
    private static final String ASSIGNEE_ID = "assigneeId";
    private static final String UPDATOR_IDS = "updatorIds";
    private static final String PRODUCT_IDS = "productIds";
    private static final String CREATOR_IDS = "creatorIds";
    private static final String PARTICIPANT_IDS = "participantIds";
    private static final String UPDATE = "update";
    private static final String CREATE = "create";

    private static final String SUFFIX_START_DATE = "StartDate";
    private static final String SUFFIX_END_DATE = "EndDate";
    private static final String SUFFIX_SCOPE_START = "ScopeStart";
    private static final String SUFFIX_SCOPE_END = "ScopeEnd";

    private static final List<String> YQ_CLOUD_FIELDS = Arrays.asList(YQ_CLOUD_FLAG, YQ_CLOUD_NUM);

    private static final List<String> DATE_SUFFIX = Arrays.asList(SUFFIX_START_DATE, SUFFIX_END_DATE, SUFFIX_SCOPE_START, SUFFIX_SCOPE_END);

    private static final Map<String, String> OPTION_FIELD_MAPPING = new HashMap<>();
    private static final Map<String, String> DATE_FIELD_MAPPING = new HashMap<>();

    private static final String OPTION = "option";
    private static final String DATE = "date";
    private static final String DATE_HMS = "date_hms";
    private static final String NUMBER = "number";
    private static final String STRING = "string";
    private static final String TEXT = "text";

    private static final String FIELD_ID = "fieldId";
    private static final String VALUE = "value";
    private static final String START_DATE = "startDate";
    private static final String END_DATE = "endDate";


    static {
        OPTION_FIELD_MAPPING.put(ISSUE_TYPE_ID, FieldCode.ISSUE_TYPE);
        OPTION_FIELD_MAPPING.put(STATUS_ID, FieldCode.STATUS);
        OPTION_FIELD_MAPPING.put(REPORTER_IDS, FieldCode.REPORTER);
        OPTION_FIELD_MAPPING.put(PRIORITY_ID, FieldCode.PRIORITY);
        OPTION_FIELD_MAPPING.put(MAIN_RESPONSIBLE_IDS, FieldCode.MAIN_RESPONSIBLE);
        OPTION_FIELD_MAPPING.put(FieldCode.SPRINT, FieldCode.SPRINT);
        OPTION_FIELD_MAPPING.put(FieldCode.EPIC, FieldCode.EPIC);
        OPTION_FIELD_MAPPING.put(FieldCode.LABEL, FieldCode.LABEL);
        OPTION_FIELD_MAPPING.put(FieldCode.FIX_VERSION, FieldCode.FIX_VERSION);
        OPTION_FIELD_MAPPING.put(ASSIGNEE_ID, FieldCode.ASSIGNEE);
        OPTION_FIELD_MAPPING.put(UPDATOR_IDS, FieldCode.UPDATOR);
        OPTION_FIELD_MAPPING.put(FieldCode.COMPONENT, FieldCode.COMPONENT);
        OPTION_FIELD_MAPPING.put(FieldCode.ENVIRONMENT, FieldCode.ENVIRONMENT);
        OPTION_FIELD_MAPPING.put(FieldCode.FEATURE, FieldCode.FEATURE);
        OPTION_FIELD_MAPPING.put(PRODUCT_IDS, FieldCode.PRODUCT);
        OPTION_FIELD_MAPPING.put(FieldCode.INFLUENCE_VERSION, FieldCode.INFLUENCE_VERSION);
        OPTION_FIELD_MAPPING.put(CREATOR_IDS, FieldCode.CREATOR);
        OPTION_FIELD_MAPPING.put(PARTICIPANT_IDS, FieldCode.PARTICIPANT);

        DATE_FIELD_MAPPING.put(CREATE, FieldCode.CREATION_DATE);
        DATE_FIELD_MAPPING.put(UPDATE, FieldCode.LAST_UPDATE_DATE);
        DATE_FIELD_MAPPING.put(FieldCode.ACTUAL_START_TIME, FieldCode.ACTUAL_START_TIME);
        DATE_FIELD_MAPPING.put(FieldCode.ACTUAL_END_TIME, FieldCode.ACTUAL_END_TIME);
        DATE_FIELD_MAPPING.put(FieldCode.ESTIMATED_START_TIME, FieldCode.ESTIMATED_START_TIME);
        DATE_FIELD_MAPPING.put(FieldCode.ESTIMATED_END_TIME, FieldCode.ESTIMATED_END_TIME);
    }


    @Override
    public void fix(Set<String> typeCodes) {
        logger.info("");
        //系统字段
        String json = "{\"advancedSearchArgs\":{\"storyPoints\":10,\"issueTypeId\":[\"144747088462131200\",\"144747088546017280\",\"144747088520851456\",\"144747088491491328\",\"319873186236702720\",\"306344094946205696\",\"268308777911906304\",\"327132212716625920\",\"144747088424382464\"],\"statusId\":[\"144747088789286912\",\"144747088818647040\",\"261935559202480128\",\"255629154333999104\",\"261936383987195904\",\"327126317152591872\",\"327160308446515200\",\"327164370671886336\",\"327164875976462336\",\"327224168541052928\",\"367974233189539840\",\"367974311799181312\",\"162197083096313856\",\"261935830162898944\",\"322876357745815552\",\"144747088801869824\",\"348121529307877376\"],\"estimateTime\":1,\"reporterIds\":[\"843\",\"1878\",\"3322\",\"4263\",\"11134\",\"144231620675907584\",\"1880\",\"7078\",\"9822\",\"10861\",\"11223\",\"11423\",\"16968\",\"17062\",\"17244\",\"17282\",\"202522563422482432\",\"213298218061824000\",\"250600150459596800\",\"317467668435714048\",\"334499961025118208\",\"372467755460739072\",\"493\",\"249\",\"302\",\"7121\",\"8802\",\"10489\",\"11051\",\"11059\",\"16741\",\"17024\",\"240453067333246976\",\"271320493771751424\",\"281064308296474624\",\"314017460070739968\",\"337210720888860672\",\"337210736365842432\",\"366563746443198464\",\"372862065662423040\",\"373092163510534144\"],\"priorityId\":[\"144747088688623616\",\"144747088717983744\",\"144747088751538176\"],\"remainingTime\":1},\"otherArgs\":{\"yqCloudFlag\":true,\"mainResponsibleIds\":[\"843\",\"3322\",\"1878\"],\"sprint\":[\"0\",\"359654690234966016\",\"359654690230771712\",\"359654690226577408\",\"346964447175106560\",\"346964447103803392\",\"346964447040888832\",\"334274908199526400\",\"334274908191137792\",\"334274908186943488\",\"332533145868861440\",\"332533145864667136\",\"332533145860472832\",\"317341593890684928\",\"317341593886490624\",\"317341593882296320\",\"305660095722586113\",\"305660095722586112\",\"293782479902232576\",\"293782479872872448\",\"293782479596048384\",\"279565953955512320\",\"279565953917763584\",\"279565953586413568\",\"266141498247921664\",\"266141498231144448\",\"266141498218561536\"],\"epic\":[\"0\",\"332620543814438912\",\"332620195729149952\",\"319932779696463872\"],\"label\":[\"385475252836245504\",\"385475282016018432\",\"385475301569863680\",\"385475324038750208\"],\"fixVersion\":[\"0\",\"344235909577687040\",\"329692791369097216\",\"319834264135770112\"],\"assigneeId\":[\"0\",\"843\",\"1878\",\"3322\",\"4263\",\"11134\",\"144231620675907584\",\"1880\",\"7078\",\"9822\",\"10489\",\"10861\",\"11051\",\"11223\",\"11423\",\"16968\",\"17062\",\"17244\",\"17282\",\"22488\",\"202522563422482432\",\"213298218061824000\",\"250600150459596800\",\"317467668435714048\",\"334499961025118208\",\"372467755460739072\",\"249\",\"302\",\"493\",\"7121\",\"8802\",\"11059\",\"16741\",\"17024\",\"17227\",\"240453067333246976\",\"271320493771751424\",\"281064308296474624\",\"314017460070739968\",\"337210720888860672\",\"337210736365842432\",\"366563746443198464\",\"372862065662423040\",\"373092163510534144\",\"3356\",\"12960\",\"211288352619266048\",\"248252140350189568\",\"291549888402751488\"],\"yqCloudNum\":\"asd\",\"customField\":{\"date\":[],\"number\":[],\"string\":[{\"value\":null,\"fieldId\":\"260776720293847040\"}],\"text\":[],\"date_hms\":[],\"option\":[{\"value\":null,\"fieldId\":\"343322379194183680\"},{\"value\":null,\"fieldId\":\"334708880255688704\"},{\"value\":null,\"fieldId\":\"333271704418570240\"},{\"value\":null,\"fieldId\":\"326651945564491776\"},{\"value\":null,\"fieldId\":\"302025667414745088\"},{\"value\":null,\"fieldId\":\"286912509234782208\"},{\"value\":null,\"fieldId\":\"282455734817263616\"},{\"value\":null,\"fieldId\":\"258695528425861120\"}]},\"updatorIds\":[\"843\",\"1878\"],\"tags\":null,\"component\":[\"357465906421321728\",\"359632479130361856\",\"357466008837832704\",\"357466190333759488\",\"357466147748990976\",\"359632416723308544\",\"359632534847492096\",\"361145026161545216\",\"359632716746072064\",\"358354456935288832\",\"329696945986592768\"],\"environment\":[\"pro\",\"other\"],\"feature\":[\"261850794243825664\",\"261852035069943808\"],\"productIds\":[\"344886758586957824\"],\"influenceVersion\":[\"0\",\"344235909577687040\"],\"creatorIds\":[\"843\",\"3322\"],\"participantIds\":[\"843\",\"1878\"]},\"searchArgs\":{\"estimatedStartTimeScopeStart\":\"2022-11-01 00:00:00\",\"estimatedStartTimeScopeEnd\":\"2022-11-23 00:00:00\",\"estimatedEndTimeScopeStart\":\"2022-11-15 00:00:00\",\"estimatedEndTimeScopeEnd\":\"2022-11-17 00:00:00\",\"actualStartTimeScopeStart\":\"2022-11-08 00:00:00\",\"actualStartTimeScopeEnd\":\"2022-11-18 00:00:00\",\"actualEndTimeScopeStart\":\"2022-11-08 00:00:00\",\"actualEndTimeScopeEnd\":\"2022-11-18 00:00:00\",\"createStartDate\":\"2022-11-08 00:00:00\",\"createEndDate\":\"2022-11-17 00:00:00\",\"updateStartDate\":\"2022-11-01 00:00:00\",\"updateEndDate\":\"2022-11-04 00:00:00\"}}";
        SearchParamVO searchParamVO = new SearchParamVO();
        try {
            JsonNode jsonNode = objectMapper.readTree(json);
            Iterator<String> fieldNameIterator = jsonNode.fieldNames();
            while (fieldNameIterator.hasNext()) {
                String fieldName = fieldNameIterator.next();
                JsonNode conditionNode = jsonNode.get(fieldName);
                if (CONDITION_KEYS.contains(fieldName)) {
                    processConditionDetails(conditionNode, searchParamVO);
                } else {
                    logger.info("error");
                }
            }
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }
        logger.info("result:{}", searchParamVO);
    }

    private void processConditionDetails(JsonNode conditionNode,
                                         SearchParamVO searchParamVO) {
        List<Condition> conditions = Optional.ofNullable(searchParamVO.getConditions()).orElse(new ArrayList<>());
        searchParamVO.setConditions(conditions);
        Iterator<String> conditionFieldNameIterator = conditionNode.fieldNames();
        boolean yqCloudFieldDone = false;
        Map<String, Boolean> dateFieldDoneMap = new HashMap<>();
        while (conditionFieldNameIterator.hasNext()) {
            String conditionFieldName = conditionFieldNameIterator.next();
            if (PREDEFINED_NUMBER_FIELDS.contains(conditionFieldName)) {
                //数字类型字段
                processNumberField(conditionNode, searchParamVO, conditions, conditionFieldName);
            } else if (YQ_CLOUD_FIELDS.contains(conditionFieldName)) {
                if (yqCloudFieldDone) {
                    continue;
                }
                processYqCloudField(conditionNode, conditions);
                yqCloudFieldDone = true;
            } else if (CUSTOM_FIELD.equals(conditionFieldName)) {
                //自定义字段
                processCustomField(conditionNode, searchParamVO, conditions, conditionFieldName);
            } else if (TAGS.equals(conditionFieldName)) {
                //tag需要特殊处理
                processTagField(conditionNode, searchParamVO, conditions, conditionFieldName);
            } else if (OPTION_FIELD_MAPPING.keySet().contains(conditionFieldName)) {
                //下拉框
                processDropDown(conditionNode, searchParamVO, conditions, conditionFieldName);
            } else {
                //日期
                processDate(conditionNode, conditions, dateFieldDoneMap, conditionFieldName);
            }
        }
    }

    private void processDate(JsonNode conditionNode, List<Condition> conditions, Map<String, Boolean> dateFieldDoneMap, String conditionFieldName) {
        String field = null;
        for (String suffix : DATE_SUFFIX) {
            if (conditionFieldName.endsWith(suffix)) {
                field = conditionFieldName.substring(0, conditionFieldName.length() - suffix.length());
                break;
            }
        }
        if (field == null) {
            //非日期字段，抛异常
            throw new CommonException("error.illegal.json.key");
        }
        //判断字段是否已经处理过
        boolean done = Boolean.TRUE.equals(dateFieldDoneMap.get(field));
        if (!done) {
            dateFieldDoneMap.put(field, true);
            Pair<Value, Value> pair = null;
            String pattern = BaseConstants.Pattern.DATETIME;
            if (CREATE.equals(field) || UPDATE.equals(field)) {
                JsonNode start = conditionNode.get(field + SUFFIX_START_DATE);
                JsonNode end = conditionNode.get(field + SUFFIX_END_DATE);
                if (start != null && !start.isNull() && end != null && !end.isNull()) {
                    pair = generatePairDate(start, end, pattern);
                }
            } else {
                JsonNode start = conditionNode.get(field + SUFFIX_SCOPE_START);
                JsonNode end = conditionNode.get(field + SUFFIX_SCOPE_END);
                if (start != null && !start.isNull() && end != null && !end.isNull()) {
                    pair = generatePairDate(start, end, pattern);
                }
            }
            if (pair != null) {
                String fieldCode = DATE_FIELD_MAPPING.get(field);
                if (fieldCode == null) {
                    throw new CommonException("error.illegal.date.field");
                }
                Condition condition =
                        new Condition()
                                .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                .setRelationship(Relationship.AND.toString())
                                .setOperation(Operation.BETWEEN.toString())
                                .setBetweenValues(pair);
                conditions.add(condition);
            }
        }
    }

    private void processCustomField(JsonNode conditionNode,
                                    SearchParamVO searchParamVO,
                                    List<Condition> conditions,
                                    String conditionFieldName) {
        JsonNode node = conditionNode.get(conditionFieldName);
        Iterator<String> iterator = node.fieldNames();
        while (iterator.hasNext()) {
            String prop = iterator.next();
            JsonNode valueList = node.get(prop);
            if (valueList == null || valueList.isNull()) {
                continue;
            }
            Iterator<JsonNode> valueNodeIterator = valueList.iterator();
            while (valueNodeIterator.hasNext()) {
                JsonNode valueNode = valueNodeIterator.next();
                JsonNode idNode = valueNode.get(FIELD_ID);
                if (idNode == null || idNode.isNull()) {
                    continue;
                }
                Long fieldId = idNode.asLong();
                String relationship = Relationship.AND.toString();
                String opt = Operation.IN.toString();
                Pair<Value, Value> pair = Pair.of(null, null);
                JsonNode value = valueNode.get(VALUE);
                switch (prop) {
                    case OPTION:
                        pair = readCustomFieldOptionValue(pair, value);
                        break;
                    case NUMBER:
                        opt = Operation.EQUAL.toString();
                        pair = readCustomFieldNumberValue(pair, value);
                        break;
                    case STRING:
                    case TEXT:
                        opt = Operation.LIKE.toString();
                        pair = readCustomFieldStringValue(pair, value);
                        break;
                    case DATE:
                    case DATE_HMS:
                        opt = Operation.BETWEEN.toString();
                        pair = readCustomFieldDateValue(prop, valueNode, pair);
                        break;
                    default:
                        break;
                }
                Value first = pair.getFirst();
                Value second = pair.getSecond();
                if (first == null && second == null) {
                    //只存了字段id,用于界面展示
                    addEmptyCondition(searchParamVO, null, fieldId);
                } else if (second == null) {
                    Condition condition =
                            new Condition()
                                    .setField(new Field().setFieldId(fieldId).setPredefined(false))
                                    .setRelationship(relationship)
                                    .setOperation(opt)
                                    .setValue(first);
                    conditions.add(condition);
                } else {
                    Condition condition =
                            new Condition()
                                    .setField(new Field().setFieldId(fieldId).setPredefined(false))
                                    .setRelationship(relationship)
                                    .setOperation(opt)
                                    .setBetweenValues(pair);
                    conditions.add(condition);
                }
            }
        }
    }

    private Pair<Value, Value> readCustomFieldDateValue(String prop, JsonNode valueNode, Pair<Value, Value> pair) {
        JsonNode start = valueNode.get(START_DATE);
        JsonNode end = valueNode.get(END_DATE);
        if (start != null && !start.isNull() && end != null && !end.isNull()) {
            String pattern = BaseConstants.Pattern.DATETIME;
            if (DATE_HMS.equals(prop)) {
                pattern = BaseConstants.Pattern.TIME_SS;
            }
            pair = generatePairDate(start, end, pattern);
        }
        return pair;
    }

    private Pair<Value, Value> generatePairDate(JsonNode start, JsonNode end, String pattern) {
        Pair<Value, Value> pair;
        SimpleDateFormat formatter = new SimpleDateFormat(pattern);
        try {
            Date startDate = formatter.parse(start.asText());
            Date endDate = formatter.parse(end.asText());
            pair = Pair.of(new Value().setValueDate(startDate), new Value().setValueDate(endDate));
        } catch (ParseException e) {
            throw new CommonException("error.parse.custom.field.date");
        }
        return pair;
    }

    private Pair<Value, Value> readCustomFieldStringValue(Pair<Value, Value> pair, JsonNode value) {
        if (value != null && !value.isNull()) {
            String valueStr = value.asText();
            pair = Pair.of(new Value().setValueStr(valueStr), null);
        }
        return pair;
    }

    private Pair<Value, Value> readCustomFieldNumberValue(Pair<Value, Value> pair, JsonNode value) {
        if (value != null && !value.isNull()) {
            BigDecimal number = new BigDecimal(value.asText());
            pair = Pair.of(new Value().setValueDecimal(number), null);
        }
        return pair;
    }

    private Pair<Value, Value> readCustomFieldOptionValue(Pair<Value, Value> pair, JsonNode value) {
        if (value != null && !value.isNull()) {
            List<Long> valueIdList = null;
            try {
                valueIdList = objectMapper.readValue(value.traverse(), new TypeReference<List<Long>>() {
                });
            } catch (IOException e) {
                throw new CommonException("error.read.option.to.list", e);
            }
            pair = Pair.of(new Value().setValueIdList(valueIdList), null);
        }
        return pair;
    }

    private void processDropDown(JsonNode conditionNode,
                                 SearchParamVO searchParamVO,
                                 List<Condition> conditions,
                                 String conditionFieldName) {
        JsonNode option = conditionNode.get(conditionFieldName);
        String fieldCode = OPTION_FIELD_MAPPING.get(conditionFieldName);
        Assert.notNull(fieldCode, "error.field.code.null");
        if (option == null || option.isNull()) {
            addEmptyCondition(searchParamVO, fieldCode, null);
        } else {
            Condition condition;
            try {
                if (FieldCode.ENVIRONMENT.equals(fieldCode)) {
                    //环境，字符串类型数据
                    List<String> valueStrList = objectMapper.readValue(option.traverse(), new TypeReference<List<String>>() {
                    });
                    condition =
                            new Condition()
                                    .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                    .setRelationship(Relationship.AND.toString())
                                    .setOperation(Operation.IN.toString())
                                    .setValue(new Value().setValueStrList(valueStrList));

                } else {
                    List<Long> valueIds = objectMapper.readValue(option.traverse(), new TypeReference<List<Long>>() {
                    });
                    boolean containsNull = valueIds.contains(0L);
                    valueIds.remove(0L);
                    if (!valueIds.isEmpty()) {
                        if (containsNull) {
                            //括号的情况
                            condition =
                                    new Condition()
                                            .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                            .setRelationship(Relationship.AND.toString())
                                            .setOperation(Operation.BRACKET.toString())
                                            .setSubConditions(
                                                    Arrays.asList(
                                                            new Condition()
                                                                    .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                                                    .setRelationship(Relationship.AND.toString())
                                                                    .setOperation(Operation.IN.toString())
                                                                    .setValue(new Value().setValueIdList(valueIds)),
                                                            new Condition()
                                                                    .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                                                    .setRelationship(Relationship.OR.toString())
                                                                    .setOperation(Operation.IS_NULL.toString())));
                        } else {
                            condition =
                                    new Condition()
                                            .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                            .setRelationship(Relationship.AND.toString())
                                            .setOperation(Operation.IN.toString())
                                            .setValue(new Value().setValueIdList(valueIds));
                        }
                    } else {
                        //只有为空条件
                        condition =
                                new Condition()
                                        .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                        .setRelationship(Relationship.AND.toString())
                                        .setOperation(Operation.IS_NULL.toString());
                    }
                }
            } catch (IOException e) {
                throw new CommonException("error.read.option.to.list", e);
            }
            conditions.add(condition);
        }
    }

    private void processNumberField(JsonNode conditionNode,
                                    SearchParamVO searchParamVO,
                                    List<Condition> conditions,
                                    String conditionFieldName) {
        JsonNode number = conditionNode.get(conditionFieldName);
        String fieldCode = conditionFieldName;
        if (number == null || number.isNull()) {
            //只存储了筛选的列
            addEmptyCondition(searchParamVO, fieldCode, null);
        } else {
            String value = number.asText();
            BigDecimal bigDecimal = new BigDecimal(value);
            Condition condition =
                    new Condition()
                            .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                            .setRelationship(Relationship.AND.toString())
                            .setOperation(Operation.EQUAL.toString())
                            .setValue(new Value().setValueDecimal(bigDecimal));
            conditions.add(condition);
        }
    }

    private void processTagField(JsonNode conditionNode,
                                 SearchParamVO searchParamVO,
                                 List<Condition> conditions,
                                 String conditionFieldName) {
        JsonNode tag = conditionNode.get(conditionFieldName);
        String fieldCode = conditionFieldName;
        if (tag == null || tag.isNull()) {
            addEmptyCondition(searchParamVO, fieldCode, null);
        } else {
            try {
                List<TagVO> tags = objectMapper.readValue(tag.traverse(), new TypeReference<List<TagVO>>() {
                });
                Condition condition =
                        new Condition()
                                .setField(new Field().setFieldCode(fieldCode).setPredefined(true))
                                .setRelationship(Relationship.AND.toString())
                                .setOperation(Operation.IN.toString())
                                .setValue(new Value().setObjectList(tags));
                conditions.add(condition);
            } catch (IOException e) {
                throw new CommonException("error.read.tag.to.list", e);
            }
        }
    }

    private void processYqCloudField(JsonNode conditionNode, List<Condition> conditions) {
        JsonNode yqCloudFlagNode = conditionNode.get(YQ_CLOUD_FLAG);
        JsonNode yqCloudNumNode = conditionNode.get(YQ_CLOUD_NUM);
        if (yqCloudFlagNode != null) {
            boolean yqCloudFlag = yqCloudFlagNode.asBoolean();
            Operation operation = yqCloudFlag ? Operation.IS_NOT_NULL : Operation.IS_NULL;
            Condition condition =
                    new Condition()
                            .setField(new Field().setFieldCode(SearchConstant.Field.YQ_CLOUD_NUM).setPredefined(true))
                            .setRelationship(SearchConstant.Relationship.AND.toString())
                            .setOperation(operation.toString());
            conditions.add(condition);
        } else if (yqCloudNumNode != null) {
            String yqCloudNum = yqCloudNumNode.asText();
            Condition condition =
                    new Condition()
                            .setField(new Field().setFieldCode(SearchConstant.Field.YQ_CLOUD_NUM).setPredefined(true))
                            .setRelationship(SearchConstant.Relationship.AND.toString())
                            .setOperation(Operation.LIKE.toString())
                            .setValue(new Value().setValueStr(yqCloudNum));
            conditions.add(condition);
        }
    }

    private void addEmptyCondition(SearchParamVO searchParamVO,
                                   String fieldCode,
                                   Long fieldId) {
        EmptyCondition emptyCondition = Optional.ofNullable(searchParamVO.getEmptyCondition()).orElse(new EmptyCondition());
        searchParamVO.setEmptyCondition(emptyCondition);
        if (fieldCode != null) {
            List<String> predefinedFieldCode = Optional.ofNullable(emptyCondition.getPredefinedFieldCode()).orElse(new ArrayList<>());
            emptyCondition.setPredefinedFieldCode(predefinedFieldCode);
            if (!predefinedFieldCode.contains(fieldCode)) {
                predefinedFieldCode.add(fieldCode);
            }
        }
        if (fieldId != null) {
            List<Long> customFieldId = Optional.ofNullable(emptyCondition.getCustomFieldIds()).orElse(new ArrayList<>());
            emptyCondition.setCustomFieldIds(customFieldId);
            if (!customFieldId.contains(fieldId)) {
                customFieldId.add(fieldId);
            }
        }
    }
}
